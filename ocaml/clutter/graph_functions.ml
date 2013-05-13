open Grammar_types
open Unit_constraint_intersection
open Zone_stubs
open UDBM_utilities
open Clock_constraint_utilities
open Clock_constraint_clutter
open ZVG_tree

let init_zone_list_array ta =
  (Array.init
     ta.numlocations
     (function i -> [{zone_location1 = i; zone_constraint1 = [True]}])
  )

let init_tree_array ta = 
  (Array.init
     ta.numlocations
     (function i -> init_tree ())
  )

let string_of_queue queue =
  "["
  ^
    (String.concat
       "; "
       (List.map
          string_of_int
          queue)
    )
  ^
    "]"

let enqueue_without_repetition queue location =
  if
    (List.exists
       ((=) location)
       queue
    )
  then
    queue
  else
    (Printf.printf
       "enqueue %s in %s\n\n"
       (string_of_int location)
       (string_of_queue queue)
    ;
     flush stdout;
     location::queue
    )

let dequeue ta (queue, zone_list_array, tree_array) =
  let queueref = ref queue in
  let split_using_parent qhd (parent, edge) =
    (let
        constraint_list =
       (List.map
          (function zone ->
            clock_constraint_after_clock_resets
              zone.zone_constraint1
              edge.clock_resets
          ) (*Why reset the clocks on the parent's zones? In order
              to ensure that the zones in this location are made
              correctly.*)
          (List.filter
             (function zone ->
               (clock_constraint_haveIntersection
                  ta.clock_names
                  zone.zone_constraint1
                  edge.condition
               )
             )
             zone_list_array.(parent)
          ) (*Why filter? To make sure we don't unncessarily split
              states when stability does not require us to do so.*)
       )
     in
     (zone_list_array.(qhd) <- (
       Printf.printf
         "qhd = %s, zone_list length = %s before split\n"
         (string_of_int qhd)
         (string_of_int (List.length zone_list_array.(qhd)));
       (Printf.printf
          "Tree top is %s, constraint_list length = %s\n"
          (string_of_int parent)
          (string_of_int (List.length zone_list_array.(parent)))
       );
       flush stdout;
       (split_zone_list_on_constraint_list
          zone_list_array.(qhd)
          constraint_list
          ta)
      ));
     Printf.printf
       "qhd = %s, zone_list length = %s after split\n"
       (string_of_int qhd)
       (string_of_int (List.length zone_list_array.(qhd))))
  in
  let self_split qhd =
    (let
        constraint_list =
       (ta.locations.(qhd).invariant
        ::
          (List.concat
             (List.map
                (function departure ->
                  [departure.condition
                  ;
                   (clock_constraint_without_reset_clocks
                      ta.locations.(departure.next_location).invariant
                      departure.clock_resets
                   )
                  ]
                )
                (Array.to_list ta.locations.(qhd).departures)
             )
          )
       )
     in
     (Printf.printf
        "qhd = %s, zone_list length = %s before split\n"
        (string_of_int qhd)
        (string_of_int (List.length zone_list_array.(qhd))));
     (Printf.printf
        "Self-splitting, constraint_list length = %s\n"
        (string_of_int (List.length constraint_list))
     );
     flush stdout;
     (zone_list_array.(qhd) <-
        (split_zone_list_on_constraint_list
           zone_list_array.(qhd)
           constraint_list
           ta)
     );
     (tree_array.(qhd) <-
        add_element_to_tree tree_array.(qhd) qhd
     );
     (Printf.printf
        "qhd = %s, zone_list length = %s after split\n"
        (string_of_int qhd)
        (string_of_int (List.length zone_list_array.(qhd)))))
  in
  let process_tree qhd =
    (Printf.printf
       "Starting with the tree of qhd = %s.\n"
       (string_of_tree tree_array.(qhd));
     flush stdout;
     (List.iter
        (function tree_element ->
          (Printf.printf "constraint_list length = %s\n"
             (string_of_int (List.length zone_list_array.(qhd))));
          flush stdout;
          let
              constraint_list =
            (List.map
               (function zone ->
                 zone.zone_constraint1
               )
               zone_list_array.(qhd)
            )
          in
          let
              changed_zone_list =
            (split_zone_list_on_constraint_list
               zone_list_array.(tree_element)
               constraint_list
               ta
            )
          in
          Printf.printf
            "tree_array.(tree_element) = %s\n"
            (string_of_tree tree_array.(tree_element))
          ;
          Printf.printf
            "tree_array.(qhd) = %s\n"
            (string_of_tree tree_array.(qhd))
          ;
          queueref :=
            if
              (tree_element_difference
                 tree_array.(qhd)
                 tree_array.(tree_element)
              )
            then
              (enqueue_without_repetition !queueref tree_element)
            else
              (!queueref)
          ;
          zone_list_array.(tree_element) <-
            changed_zone_list
          ;
          tree_array.(tree_element) <-
            add_element_to_tree tree_array.(tree_element) qhd;
        )
        (get_elements tree_array.(qhd))
     );
     (Printf.printf "Done with elements of the tree of qhd.\n");
     flush stdout)
  in
  let process_successors qhd =
    ((Printf.printf "Starting with successors of qhd.\n");
     flush stdout;
     (List.iter
        (function departure ->
          let successor = departure.next_location in
          Printf.printf "Successor=%s arrived.\n" (string_of_int successor);
          Printf.printf
            "tree_array.(successor) = %s\n"
            (string_of_tree tree_array.(successor))
          ;
          Printf.printf
            "tree_array.(qhd) = %s\n"
            (string_of_tree tree_array.(qhd))
          ;
          flush stdout;
          queueref :=
            if
              tree_element_difference tree_array.(qhd) tree_array.(successor)
            then
              (enqueue_without_repetition !queueref successor)
            else
              (!queueref)
          ;
          Printf.printf "Successor=%s left.\n" (string_of_int successor);
          flush stdout;
          tree_array.(successor) <-
            (add_parent_with_edge_to_tree
               tree_array.(successor)
               qhd
               departure
            );
        )
        (Array.to_list ta.locations.(qhd).departures)
     );
     (Printf.printf "Done with the successors of qhd.\n");
     (flush stdout))
  in
  match !queueref with
    [] -> (!queueref, zone_list_array, tree_array)
  | qhd::qtl ->
    Printf.printf "dequeue %s!\n\n" (string_of_int qhd);
    flush stdout;
    queueref := qtl;
    flush stdout;
    (
      match
        get_parent_with_edge tree_array.(qhd)
      with
        None -> ()
      | Some (parent, edge) ->
        (split_using_parent qhd (parent, edge))
    ) ;
    flush stdout
    ;
    (if (not (List.mem qhd (get_elements tree_array.(qhd))))
     then
        (self_split qhd)
     else
        () 
    );
    process_tree qhd;
    process_successors qhd;
    Printf.printf "queue now = %s\n" (string_of_queue !queueref);
    flush stdout;
    (!queueref,
     zone_list_array,
     tree_array)

let rec empty_queue ta (queue, zone_list_array, tree_array) =
  match queue with
    [] -> (queue, zone_list_array, tree_array)
  | qhd::qtl -> (empty_queue
                   ta
                   (dequeue
                      ta
                      (queue, zone_list_array, tree_array)
                   )
  )

let generate_zone_valuation_graph ta =
  let zone_list_array = 
    match
      (empty_queue
         ta
         ([ta.numinit],
          (init_zone_list_array ta),
          (init_tree_array ta)
         )
      )
    with
      (_, zone_list_array, _) -> zone_list_array
  in
  let
      graph = 
    Array.map
      (function zone_list ->
        List.map
          (function zone ->
            (zone,
             let
                 departures =
               {action = -1;
                condition = [True];
                clock_resets = [||];
                next_location = zone.zone_location1
               } (*This one is a time transition.*)
               ::
                 (List.filter
                    (function departure ->
                      clock_constraint_haveIntersection
                        ta.clock_names
                        zone.zone_constraint1
                        departure.condition
                    )
                    (Array.to_list
                       ta.locations.(zone.zone_location1).departures)
                 )
             in
             List.map
               (function departure ->
                 (departure,
                  if (departure.action >= 0) then (*action transition*)
                    (List.filter
                       (function arrival_zone ->
                         clock_constraint_haveIntersection
                           ta.clock_names
                           (clock_constraint_after_clock_resets
                              zone.zone_constraint1
                              departure.clock_resets
                           )
                           arrival_zone.zone_constraint1
                       )
                       zone_list_array.(departure.next_location)
                    )
                  else (*time transition*)
                    (List.filter
                       (function arrival_zone ->
                         (* clock_constraint_haveIntersection *)
                         (*   ta.clock_names *)
                         (*   zone.zone_constraint1 (\*TODO: make this upward unbounded!*\) *)
                         (*   arrival_zone.zone_constraint1 *)
                         match
                           (clock_constraint_to_dbm_option
                              ta.clock_names
                              zone.zone_constraint1)
                         with
                           None -> false
                         | Some dst ->
                           (match
                               (clock_constraint_to_dbm_option
                                  ta.clock_names
                                  arrival_zone.zone_constraint1)
                            with
                              None -> false
                            | Some src ->
                              (dbm_haveIntersection
                                 (dbm_up dst)
                                 src)
                           )
                        )
                         zone_list_array.(departure.next_location)
                       )
                    )
                 )
                   departures
               )
            )
              zone_list
          )
          zone_list_array
       in
      graph


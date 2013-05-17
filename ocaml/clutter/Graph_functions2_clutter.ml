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

(*We earlier thought that uncut zones cannot exist, but here's an
  example to prove that they do: the successor having one zone, the
  future of x=2 and y=0, and the predecessor having one zone, the
  future of x=0 and y=0*)
let cut_and_uncut_successor_zones_from_predecessor
    ta
    predecessor_zone_list
    edge
    successor_zone_list =
  let dim = 1 + ta.numclocks in
  List.partition
    (function z1 ->
      List.for_all
        (function z2 ->
          dbm_haveIntersection
            z1.zone_constraint2
            z2.zone_constraint2
        )
        successor_zone_list
    )
    (successor_zones_from_predecessor
       ta
       predecessor_zone_list
       edge
    )

let new_successor_zones_from_predecessor
    ta
    predecessor
    predecessor_zone_list
    edge
    successor
    successor_zone_list
    (cut, uncut) =
  let dim = 1 + ta.numclocks in
  (List.filter
     (function z1 ->
       List.for_all
         (function z2 ->
           not (dbm_haveIntersection
                  z1.zone_constraint2
                  z2.zone_constraint2
           )
         )
         successor_zone_list
     )
     (split_zone_list_on_dbm_list
        dim
        successor
        cut
        (List.map
           (function zone -> zone.zone_constraint2)
           successor_zone_list
        )
     )
     , uncut)

let dequeue ta (queue, zone_list_array, tree_array) =
  let dim = 1 + ta.numclocks in
  let queueref = ref queue in
  (* let split_using_parent qhd (parent, edge) = *)
  (*   (let *)
  (*       constraint_list = *)
  (*      (List.map *)
  (*         (function zone -> *)
  (*           clock_constraint_after_clock_resets *)
  (*             zone.zone_constraint2 *)
  (*             edge.clock_resets *)
  (*         ) (\*Why reset the clocks on the parent's zones? In order *)
  (*             to ensure that the zones in this location are made *)
  (*             correctly.*\) *)
  (*         (List.filter *)
  (*            (function zone -> *)
  (*              (clock_constraint_haveIntersection *)
  (*                 ta.clock_names *)
  (*                 zone.zone_constraint2 *)
  (*                 edge.condition *)
  (*              ) *)
  (*            ) *)
  (*            zone_list_array.(parent) *)
  (*         ) (\*Why filter? To make sure we don't unncessarily split *)
  (*             states when stability does not require us to do so.*\) *)
  (*      ) *)
  (*    in *)
  (*    (zone_list_array.(qhd) <- ( *)
  (*      Printf.printf *)
  (*        "qhd = %s, zone_list length = %s before split\n" *)
  (*        (string_of_int qhd) *)
  (*        (string_of_int (List.length zone_list_array.(qhd))); *)
  (*      (Printf.printf *)
  (*         "Tree top is %s, constraint_list length = %s\n" *)
  (*         (string_of_int parent) *)
  (*         (string_of_int (List.length zone_list_array.(parent))) *)
  (*      ); *)
  (*      flush stdout; *)
  (*      (split_zone_list_on_constraint_list *)
  (*         zone_list_array.(qhd) *)
  (*         constraint_list *)
  (*         ta) *)
  (*     )); *)
  (*    Printf.printf *)
  (*      "qhd = %s, zone_list length = %s after split\n" *)
  (*      (string_of_int qhd) *)
  (*      (string_of_int (List.length zone_list_array.(qhd)))) *)
  (* in *)
  (* let process_tree qhd = *)
  (*   (Printf.printf *)
  (*      "Starting with the tree of qhd = %s.\n" *)
  (*      (string_of_tree tree_array.(qhd)); *)
  (*    flush stdout; *)
  (*    (List.iter *)
  (*       (function tree_element -> *)
  (*         (Printf.printf "constraint_list length = %s\n" *)
  (*            (string_of_int (List.length zone_list_array.(qhd)))); *)
  (*         flush stdout; *)
  (*         let *)
  (*             constraint_list = *)
  (*           (List.map *)
  (*              (function zone -> *)
  (*                zone.zone_constraint2 *)
  (*              ) *)
  (*              zone_list_array.(qhd) *)
  (*           ) *)
  (*         in *)
  (*         let *)
  (*             changed_zone_list = *)
  (*           (split_zone_list_on_constraint_list *)
  (*              zone_list_array.(tree_element) *)
  (*              constraint_list *)
  (*              ta *)
  (*           ) *)
  (*         in *)
  (*         Printf.printf *)
  (*           "tree_array.(tree_element) = %s\n" *)
  (*           (string_of_tree tree_array.(tree_element)) *)
  (*         ; *)
  (*         Printf.printf *)
  (*           "tree_array.(qhd) = %s\n" *)
  (*           (string_of_tree tree_array.(qhd)) *)
  (*         ; *)
  (*         queueref := *)
  (*           if *)
  (*             (tree_element_difference *)
  (*                tree_array.(qhd) *)
  (*                tree_array.(tree_element) *)
  (*             ) *)
  (*           then *)
  (*             (enqueue_without_repetition !queueref tree_element) *)
  (*           else *)
  (*             (!queueref) *)
  (*         ; *)
  (*         zone_list_array.(tree_element) <- *)
  (*           changed_zone_list *)
  (*         ; *)
  (*         tree_array.(tree_element) <- *)
  (*           add_element_to_tree tree_array.(tree_element) qhd; *)
  (*       ) *)
  (*       (get_elements tree_array.(qhd)) *)
  (*    ); *)
  (*    (Printf.printf "Done with elements of the tree of qhd.\n"); *)
  (*    flush stdout) *)
  (* in *)
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
          zone_list_array.(successor) <-
            (new_successor_zones
               ta
               qhd
               zone_list_array.(qhd)
               departure
               successor
               zone_list_array.(successor)
            )
          ;
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
          tree_array.(successor) <-
            (augment_tree_with_tree
               tree_array.(successor)
               tree_array.(qhd)
            );
        )
        (Array.to_list ta.locations.(qhd).departures)
     );
     (Printf.printf "Done with the successors of qhd.\n");
     (flush stdout))
  in
  let rec
      backward_propagate () =
    let
        new_zone = ref false
    in
    Array.iter
      (function l1 ->
        Array.iter
          (function departure ->
            let
                (splittable, unsplittable) =
              List.partition
                (function zone ->
                  match
                    (clock_constraint_to_dbm_option
                       ta.clock_names
                       departure.condition
                    )
                  with
                  | None -> false
                  | Some departure_condition_dbm -> 
                    dbm_haveIntersection
                      (zone.zone_constraint2)
                      (departure_condition_dbm)
                )
                zone_list_array.(l1.location_index)
            in
            let
                changed_zone_list =
              split_zone_list_on_dbm_list
                dim
                l1.location_index
                splittable
                (List.map
                   (function zone ->
                     (dbm_without_reset_clocks
                        ta.clock_names
                        departure.clock_resets
                        zone.zone_constraint2
                     )
                   )
                   zone_list_array.(departure.next_location)
                )
            in
            (if
                (List.length changed_zone_list >
                   List.length splittable
                )
             then
                (new_zone := true;
                 zone_list_array.(l1.location_index) <-
                   changed_zone_list @ unsplittable
                )
             else
                ()
            )
            ;
          )
          l1.departures
      )
      (ta.locations)
    ;
    if
      !new_zone
    then
      backward_propagate ()
    else
      ()
  in
  match !queueref with
    [] -> (!queueref, zone_list_array, tree_array)
  | qhd::qtl ->
    Printf.printf "dequeue %s!\n\n" (string_of_int qhd);
    flush stdout;
    queueref := qtl;
    flush stdout;
    (* ( *)
    (*   match *)
    (*     get_parent_with_edge tree_array.(qhd) *)
    (*   with *)
    (*     None -> () *)
    (*   | Some (parent, edge) -> *)
    (*     (split_using_parent qhd (parent, edge)) *)
    (* ) ; *)
    (* flush stdout *)
    (* ; *)
    (if (not (List.mem qhd (get_elements tree_array.(qhd))))
     then
        (Printf.printf
           "location = %s, zone_list length = %s before split\n"
           (string_of_int qhd)
           (string_of_int (List.length zone_list_array.(qhd)))
        ; 
         zone_list_array.(qhd) <-
           (self_split ta qhd zone_list_array.(qhd));
         Printf.printf
           "location = %s, zone_list length = %s after split\n"
           (string_of_int qhd)
           (string_of_int (List.length zone_list_array.(qhd)));
         tree_array.(qhd) <-
           (add_element_to_tree tree_array.(qhd) qhd)
        )
     else
        ()
    );
    (* process_tree qhd; *)
    process_successors qhd;
    backward_propagate ();
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

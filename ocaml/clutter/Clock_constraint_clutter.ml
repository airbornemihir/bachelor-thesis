open Grammar_types
open UDBM_utilities

let unit_clock_constraint_max unit_clock_constraint cn =
  match
    unit_clock_constraint
  with
    True
  | False -> Lstrict 0
  | Lt (cn1, n1)
  | Le (cn1, n1)
  | Eq (cn1, n1)
  | Ge (cn1, n1)
  | Gt (cn1, n1) -> if (cn1 = cn) then Lslack n1 else Lstrict 0

let combine_max lbound1 lbound2 =
  match
    (lbound1, lbound2)
  with
    (Lstrict n1, Lstrict n2) -> if (n1 < n2)
      then (Lstrict n2)
      else (Lstrict n1)
  | (Lstrict n1, Lslack n2) -> if (n1 < n2)
    then (Lslack n2)
    else (Lstrict n1)
  | (Lslack n1, Lstrict n2) -> if (n2 < n1)
    then (Lslack n1)
    else (Lstrict n2)
  | (Lslack n1, Lslack n2) -> if (n2 < n2)
    then (Lslack n2)
    else (Lslack n1)

let clock_constraint_max clock_constraint cn =
  List.fold_left
    combine_max
    (Lstrict 0)
    clock_constraint

let split_on_unit_clock_constraint unit_clock_constraint =
  match
    unit_clock_constraint
  with
    True
  | False -> [True]
  | Lt (cn1, n1)
  | Ge (cn1, n1) ->
    if (n1 <= 0) then [True] else [Lt (cn1, n1); Ge (cn1, n1)]
  | Le (cn1, n1) 
  | Gt (cn1, n1) ->
    if (n1 < 0) then [True] else [Le (cn1, n1); Gt (cn1, n1)]
  | Eq (cn1, n1) ->
    if
      (n1 < 0)
    then
      [True]
    else
      if
        (n1 = 0)
      then
        [Le (cn1, n1); Gt (cn1, n1)]
      else
        [Lt (cn1, n1); Eq (cn1, n1); Gt (cn1, n1)]

let minimise_clock_constraint clock_names clock_constraint =
  let
      clock_name_list = Array.to_list clock_names
  in
  let
      phase1 clock_constraint =
    if
      List.exists
        ((=) False)
        clock_constraint
    then
      [False]
    else
      clock_constraint
  in
  let
      phase2 clock_constraint =
    List.filter
      ((<>) True)
      clock_constraint
  in
  let
      phase3 clock_constraint =
      List.concat
        (List.map
           (function unit_clock_constraint ->
             match
               unit_clock_constraint
             with
             | Eq (cn, c) -> [Le (cn, c); Ge (cn, c)]
             | _ -> [unit_clock_constraint]
           )
           clock_constraint
        )
  in
  let
      phase4 clock_constraint =
    List.fold_left
      (function clock_constraint -> function cn ->
        match
          (List.fold_left
             (function (partial_clock_constraint,
                        current_lt,
                        current_le,
                        current_ge,
                        current_gt) ->
               function unit_clock_constraint ->
                 let
                     default =
                   (unit_clock_constraint::partial_clock_constraint,
                    current_lt,
                    current_le,
                    current_ge,
                    current_gt
                   )
                 in
                 let
                     same =
                   (partial_clock_constraint,
                    current_lt,
                    current_le,
                    current_ge,
                    current_gt
                   )
                 in
                 match unit_clock_constraint
                 with
                 | Lt (cn1, n1) ->
                   if (cn1 = cn)
                   then
                     (partial_clock_constraint,
                      (if n1 < current_lt then n1 else current_lt),
                      current_le,
                      current_ge,
                      current_gt
                     )
                   else
                     default
                 | Le (cn1, n1) ->
                   if (cn1 = cn)
                   then
                     (partial_clock_constraint,
                      current_lt,
                      (if n1 < current_le then n1 else current_le),
                      current_ge,
                      current_gt
                     )
                   else
                     default
                 | Eq (cn1, n1) ->
                   if (cn1 = cn)
                   then
                     (partial_clock_constraint,
                      current_lt,
                      (if n1 < current_ge then n1 else current_le),
                      (if n1 > current_ge then n1 else current_ge),
                      current_gt
                     )
                   else
                     default
                 | Ge (cn1, n1) ->
                   if (cn1 = cn)
                   then
                     (partial_clock_constraint,
                      current_lt,
                      current_le,
                      (if n1 > current_ge then n1 else current_ge),
                      current_gt
                     )
                   else
                     default
                 | Gt (cn1, n1) ->
                   if (cn1 = cn)
                   then
                     (partial_clock_constraint,
                      current_lt,
                      current_le,
                      current_ge,
                      (if n1 > current_gt then n1 else current_gt)
                     )
                   else
                     default
                 | True ->
                   same
                 | False ->
                   default
             )
             ([], max_int, max_int, 0, 0)
             (clock_constraint)
          )
        with
        | (remainder, final_lt, final_le, final_ge, final_gt) ->
          (if (final_lt <= final_le)
           then
              (if (final_lt < max_int) then [Lt (cn, final_lt)] else [])
           else
              (if (final_le < max_int) then [Le (cn, final_le)] else [])
          )
            @
            (if (final_gt >= final_ge)
             then
                (if (final_gt > 0) then [Gt (cn, final_gt)] else [])
             else
                (if (final_ge > 0) then [Ge (cn, final_ge)] else [])
            )
            @
            remainder
      )
      clock_constraint
      clock_name_list
  in
  (phase4 (phase3 (phase2 (phase1 clock_constraint))))

let clock_constraint_intersection clock_names c1 c2 =
  minimise_clock_constraint clock_names (c1 @ c2)

let split_zone_on_clock_constraint zone clock_constraint clock_names=
  List.fold_left (*This is where we split by each of the constituents
                   of the constraint, one by one.*)
    (function zone_list ->
      function unit_clock_constraint -> (*return a zone list
                                          containing these zones split
                                          by this unit clock
                                          constraint.*)
        List.fold_left
          (function partial_zone_list -> 
            function zone -> (*return the partial zone list augmented
                               with the zones we get from the
                               splitting of this zone.*)
              (List.filter
                 (function zone ->
                   match
                     (clock_constraint_to_dbm_option clock_names zone.zone_constraint1)
                   with
                     None -> false
                   | _ -> true
                 )
                 (List.map
                    (function unit_clock_constraint -> (*Yep, variable overuse.*)
                      {zone_location1 = zone.zone_location1;
                       zone_constraint1 =
                          minimise_clock_constraint
                            clock_names
                            (unit_clock_constraint ::
                               zone.zone_constraint1)
                      }
                    )
                    (split_on_unit_clock_constraint unit_clock_constraint)
                 ) (*OK, this list contains all the possible zones,
                     but some have to go.*)
              )
              @
                partial_zone_list
          )
          []
          zone_list
    )
    [zone]
    clock_constraint

let split_zone_list_on_constraint_list zone_list constraint_list ta =
  List.fold_left
    (function zone_list ->
      function clock_constraint ->
        List.fold_left
          (function partial_zone_list ->
            function zone ->
              (List.map
                 (function zone ->
                   {zone_location1 = zone.zone_location1;
                    zone_constraint1 =
                       (minimise_clock_constraint
                          ta.clock_names
                          zone.zone_constraint1
                       )
                   }
                 )
                 ((split_zone_on_clock_constraint zone clock_constraint ta.clock_names)
                  @
                    partial_zone_list)
              )
          )
          []
          zone_list
    )
    zone_list
    constraint_list

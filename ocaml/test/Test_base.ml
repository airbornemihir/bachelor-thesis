open Grammar_types
open Clock_constraint_utilities

let match_clock_constraints found expected =
  (List.for_all
     (function expectedelem ->
       (List.exists
          ((=) expectedelem)
          found
       )
       ||
         (expectedelem = True)
     )
     expected
  )
  &&
    (List.for_all
       (function foundelem ->
         (List.exists
            ((=) foundelem)
            expected)
         ||
           (foundelem = True)
       )
       found
    )

let match_minimised clock_constraint clock_name_list expected =
  (minimise_clock_constraint clock_constraint clock_name_list) = expected

(* CAV paper example *)
let ta1 = {
  numlocations = 3;
  numtrans = 3;
  numclocks = 1;
  numactions = 3;
  numinit = 0;
  clock_names = [|"X"|];
  locations =
    [|
      {
        location_index = 0;
        invariant = [True];
        departures =
          [|
            {
              action = 0;
              condition = [Gt ("X", 2)];
              clock_resets = [||];
              next_location = 1
            }
          |]
      };
      {
        location_index = 1;
        invariant = [True];
        departures =
          [|
            {
              action = 1;
              condition = [Gt ("X", 5)];
              clock_resets = [|"X"|];
              next_location = 2
            }
          |]
      };
      {
        location_index = 2;
        invariant = [True];
        departures =
          [|
            {
              action = 2;
              condition = [Gt ("X", 8)];
              clock_resets = [||];
              next_location = 0
            }
          |]
      }
    |]
}

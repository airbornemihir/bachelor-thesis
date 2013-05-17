open Grammar_types
open Graph_functions2
open Test_base
open Zone_stubs
open Zone_stubs_test
open UDBM_utilities
open UDBM_utilities_test

let dim05 = 2

let dbm05 =
  match
    (constraint_list_to_dbm_option
       dim05
       [(1, 0, false, 8)]
    )
  with
  | None -> (dbm_init dim05) (* This is not supposed to happen! *)
  | Some dbm05 -> dbm05

let test67 =
  if
    verify_dbm
      dim05
      dbm05
      [(1, 0, false, 8)]
  then
    "test67 passed"
  else
    "test67 failed"

let dim06 = 2

let dbm06 =
  match
    (constraint_list_to_dbm_option
       dim06
       [(0, 1, true, -8)]
    )
  with
  | None -> (dbm_init dim06) (* This is not supposed to happen! *)
  | Some dbm06 -> dbm06

let test68 =
  if
    verify_dbm
      dim06
      dbm06
      [(0, 1, true, -8)]
  then
    "test68 passed"
  else
    "test68 failed"

let dim07 = 2

let dbm07 =
  match
    (constraint_list_to_dbm_option
       dim07
       [(1, 0, false, 2)]
    )
  with
  | None -> (dbm_init dim07) (* This is not supposed to happen! *)
  | Some dbm07 -> dbm07

let test69 =
  if
    verify_dbm
      dim07
      dbm07
      [(1, 0, false, 2)]
  then
    "test69 passed"
  else
    "test69 failed"

let dim08 = 2

let dbm08 =
  match
    (constraint_list_to_dbm_option
       dim08
       [(0, 1, true, -2)]
    )
  with
  | None -> (dbm_init dim08) (* This is not supposed to happen! *)
  | Some dbm08 -> dbm08

let test70 =
  if
    verify_dbm
      dim08
      dbm08
      [(0, 1, true, -2)]
  then
    "test70 passed"
  else
    "test70 failed"

let test71 =
  let
      found =
    List.map
      (function zone -> zone.zone_constraint2)
      (new_successor_zones
         ta1
         2
         [{zone_location2 = 2; zone_constraint2 = dbm05};
          {zone_location2 = 2; zone_constraint2 = dbm06}
         ]
         {
           action = 2;
           condition = [Gt ("X", 8)];
           clock_resets = [||];
           next_location = 0
         }
         0
         [{zone_location2 = 0; zone_constraint2 = dbm07};
          {zone_location2 = 0; zone_constraint2 = dbm08}
         ]
      )
  in
  let
      expected =
    [[(1, 0, false, 2)]; [(0, 1, true, -2); (1, 0, false, 8)]; [(0, 1, true, -8)]]
  in
  if
    verify_split 2 found expected
  then
    "test71 passed"
  else
    "test71 failed"
    (* ("test71 failed, dbm are " ^ (string_of_int (List.length found)) ^ *)
    (*     " in number.\n" ^ *)
    (*     "test71 failed, dbm are [" ^ *)
    (*     (String.concat *)
    (*        "; " *)
    (*        (List.map *)
    (*           (function dbm -> dbm_to_string [|"X"; "Y"|] dbm) *)
    (*           (List.filter (function dbm -> not (dbm_isEmpty dbm)) found) *)
    (*        ) *)
    (*     ) *)
    (*  ^ "]") *)

let ta2 = {
  numlocations = 1;
  numtrans = 1;
  numclocks = 2;
  numactions = 1;
  numinit = 0;
  clock_names = [|"X"; "Y"|];
  locations =
    [|
      {
        location_index = 0;
        invariant = [True];
        departures =
          [|
            {
              action = 0;
              condition = [Eq("Y", 1)];
              clock_resets = [|"Y"|];
              next_location = 0
            }
          |]
      }
    |]
}

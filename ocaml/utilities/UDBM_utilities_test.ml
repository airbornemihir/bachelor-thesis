open Grammar_types
open Test_base
open UDBM_utilities
open Zone_stubs
open Zone_stubs_test

let dim01 = 3

let dbm01 =
  (dbm_updateValue
       (dbm_updateValue
          (dbm_init dim01)
          dim01
          2
          3
       )
       dim01
       1
       2
    )

let test45 =
  if
    verify_raw_t
      dim01
      dbm01
      [(2, 0, false, 3); (1, 2, false, -1); (0, 1, false, -2)]
  then
    "test45 passed"
  else
    "test45 failed"

let dim02 = dim01

let dbm02 = raw_t_without_reset_clocks [|"X"; "Y"|] [|"X"|] dbm01 

let test46 =
  if
    verify_raw_t
      dim02
      dbm02
      [(2, 0, false, 3); (0, 2, false, -3)]
  then
    "test46 passed"
  else
    "test46 failed"

let dim03 = dim01

let dbm03 = raw_t_after_clock_resets [|"X"; "Y"|] [|"X"|] dbm01 

let test47 =
  if
    verify_raw_t
      dim03
      dbm03
      [(2, 0, false, 3); (1, 2, false, -3)]
  then
    "test47 passed"
  else
    ("test47 failed, found  = " ^ (raw_t_to_string [|"X"; "Y"|] dbm02))

let test48 =
  if
    (match
        (constraint_list_to_raw_t_option
           2
           [(0, 1, false, -3); (1, 0, true, 3)]
        )
     with
     | None -> true
     | _ -> false
    )
  then
    "test48 passed"
  else
    "test48 failed"

let test49 =
  let dim = 2 in
  if
    (match
        (constraint_list_to_raw_t_option
           dim
           [(0, 1, false, -3); (1, 0, false, 3)]
        )
     with
     | None -> false
     | Some dbm ->
       verify_raw_t
         dim
         dbm
         [(0, 1, false, -3); (1, 0, false, 3)]
    )
  then
    "test49 passed"
  else
    "test49 failed"

let dim04 = 3

let dbm04 =
  match
    (constraint_list_to_raw_t_option
       dim04
       [(2, 0, false, 3); (1, 0, false, 3)]
    )
  with
  | None -> (dbm_init dim04) (* This is not supposed to happen! *)
  | Some dbm04 -> dbm04

let test50 =
  if
    verify_raw_t
      dim04
      dbm04
      [(2, 0, false, 3); (1, 0, false, 3)]
  then
    "test50 passed"
  else
    "test50 failed"

let verify_split dim found expected = 
  (List.length found = List.length expected) &&
    (List.for_all
       (function e ->
         (List.length
            (List.filter
               (function f ->
                 verify_raw_t dim f e
               )
               found)
         )
         =
           (List.length (List.filter ((=) e) expected))
       )
       expected
    )

let test51 =
  let
      found =
    split_raw_t_on_constraint dim04 dbm04 (1, 2, false, 2)
  in
  let
      expected =
    [[(2, 0, false, 3); (1, 0, false, 3); (1, 2, false, 2)];
     [(1, 0, false, 3); (2, 1, true, -2)]]
  in
  if
    verify_split dim04 found expected
  then
    "test51 passed"
  else
    "test51 failed"
      
let test52 =
  let
      found =
    split_raw_t_on_constraint dim04 dbm04 (2, 1, true, -2)
  in
  let
      expected =
    [[(2, 0, false, 3); (1, 0, false, 3); (1, 2, false, 2)];
     [(1, 0, false, 3); (2, 1, true, -2)]]
  in
  if
    verify_split dim04 found expected
  then
    "test52 passed"
  else
    "test52 failed"
      
let test53 =
  let
      found =
    split_raw_t_on_constraint dim04 dbm04 (2, 1, true, -3)
  in
  let
      expected =
    [[(2, 0, false, 3); (1, 0, false, 3)]]
  in
  if
    verify_split dim04 found expected
  then
    "test53 passed"
  else
    "test53 failed"
      
let test54 =
  let
      found =
    split_raw_t_list_on_constraint_list
      dim04
      [dbm04]
      [(2, 1, true, -1); (2, 1, true, 1)]
  in
  let
      expected =
    [[(1, 0, false, 3); (2, 1, true, -1)];
     [(2, 0, false, 3); (1, 0, false, 3); (1, 2, false, 1); (2, 1, true, 1)];
     [(2, 0, false, 3); (1, 2, false, -1)]
    ]
  in
  if
    verify_split dim04 found expected
  then
    "test54 passed"
  else
    ("test54 failed, dbm are [" ^
        (String.concat
           "; "
           (List.map
              (raw_t_to_string [|"X"; "Y"|])
              found
           )
        )
     ^ "]")

let dim05 = dim04

let dbm05 =
  match
    (constraint_list_to_raw_t_option
       dim04
       [(1, 2, false, 2); (2, 1, false, 2)]
    )
  with
  | None -> (dbm_init dim05) (* This is not supposed to happen! *)
  | Some dbm05 -> dbm05

let test55 =
  if
    verify_raw_t
      dim05
      dbm05
      [(1, 2, false, 2); (2, 1, false, 2)]
  then
    "test55 passed"
  else
    "test55 failed"
      
let test56 =
  let
      found =
    split_raw_t_list_on_raw_t
      dim04
      [dbm04]
      dbm05
  in
  let
      expected =
    [[(1, 0, false, 3); (2, 1, true, -2)];
     [(2, 0, false, 3); (1, 0, false, 3); (1, 2, false, 2); (2, 1, false, 2)];
     [(2, 0, false, 3); (1, 2, true, -2)]
    ]
  in
  if
    verify_split dim04 found expected
  then
    "test56 passed"
  else
    ("test56 failed, dbm are [" ^
        (String.concat
           "; "
           (List.map
              (raw_t_to_string [|"X"; "Y"|])
              found
           )
        )
     ^ "]")
      
let test57 =
  let
      found =
    split_raw_t_list_on_clock_constraint
      [|"X"; "Y"|]
      [dbm05]
      [Le ("X", 3); Le ("Y", 3)]
  in
  let
      expected =
    [[(2, 0, false, 3); (1, 0, false, 3); (1, 2, false, 2); (2, 1, false, 2)];
     [(0, 2, true, -3); (1, 0, false, 3); (2, 1, false, 2)];
     [(2, 0, false, 3); (0, 1, true, -3); (1, 2, false, 2)];
     [(0, 2, true, -3); (0, 1, true, -3); (1, 2, false, 2); (2, 1, false, 2)]
    ]
  in
  if
    verify_split dim04 found expected
  then
    "test57 passed"
  else
    ("test57 failed, dbm are [" ^
        (String.concat
           "; "
           (List.map
              (raw_t_to_string [|"X"; "Y"|])
              found
           )
        )
     ^ "]")

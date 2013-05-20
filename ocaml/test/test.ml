open Grammar_types
open Clock_constraint_utilities
open Graph_functions3_test
open Unit_constraint_intersection_test
open Zone_stubs
open Zone_stubs_test
open UDBM_utilities
open UDBM_utilities_test
open Test_base
open Test_base_clutter
open Clock_constraint_clutter_test
open PCQueue_test
open Graph_functions2_test
open Alt_Fernandez_modules_test
  
let test14 =
  if
    0 = clock_name_to_index "X" [|"X"; "Y"|]
  then
    "test14 passed"
  else
    "test14 failed"

let test15 =
  if
    1 = clock_name_to_index "Y" [|"X"; "Y"|]
  then
    "test15 passed"
  else
    "test15 failed"

let test16 =
  if
    -1 = clock_name_to_index "Z" [|"X"; "Y"|]
  then
    "test16 passed"
  else
    "test16 failed"

let test17 =
  if
    None <>
    (clock_constraint_to_dbm_option
       [|"X"; "Y"|]
       [Le("X", 4); Ge ("X", 3)]
    )
  then
    "test17 passed"
  else
    "test17 failed"

let test18 =
  if
    None = (clock_constraint_to_dbm_option
       [|"X"; "Y"|]
       [Le("X", 3); Ge ("X", 4)]
    )
  then
    "test18 passed"
  else
    "test18 failed"

let test19 =
  if
    [Eq ("X1", 0); Eq ("X3", 0); Gt ("X2", 3); Lt
      ("X2", 5)]
      =
    (clock_constraint_after_clock_resets
       [Ge ("X1", 5); Gt ("X2", 3); Le ("X1", 7); Lt ("X3", 5); Lt
         ("X2", 5)]
       [|"X1"; "X3"|])
  then
    "test19 passed"
  else
    "test19 failed"

let _ =
  print_string test2;
  print_newline ();
  print_string test3;
  print_newline ();
  print_string test4;
  print_newline ();
  print_string test5;
  print_newline ();
  print_string test6;
  print_newline ();
  print_string "test6 should be 3.\n";
  print_string test8;
  print_newline ();
  print_string test9;
  print_newline ();
  print_string test10;
  print_newline ();
  print_string test11;
  print_newline ();
  print_string test12;
  print_newline ();
  print_string test13;
  print_newline ();
  print_string test14;
  print_newline ();
  print_string test15;
  print_newline ();
  print_string test16;
  print_newline ();
  print_string test17;
  print_newline ();
  print_string test18;
  print_newline ();
  print_string test19;
  print_newline ();
  print_string test20;
  print_newline ();
  print_string test21;
  print_newline ();
  print_string test24;
  print_newline (); 
  print_string test26;
  print_newline ();
  print_string test27;
  print_newline ();
  print_string test31;
  print_newline ();
  print_string test32;
  print_newline ();
  print_string test33;
  print_newline ();
  print_string test34;
  print_newline ();
  print_string test35;
  print_newline ();
  print_string test36;
  print_newline ();
  print_string test37;
  print_newline ();
  print_string test38;
  print_newline ();
  print_string test39;
  print_newline ();
  print_string test41;
  print_newline ();
  print_string test42;
  print_newline ();
  print_string test43;
  print_newline ();
  print_string test44;
  print_newline ();
  print_string test45;
  print_newline ();
  print_string test46;
  print_newline ();
  print_string test47;
  print_newline ();
  print_string test48;
  print_newline ();
  print_string test49;
  print_newline ();
  print_string test50;
  print_newline ();
  print_string test51;
  print_newline ();
  print_string test52;
  print_newline ();
  print_string test53;
  print_newline ();
  print_string test54;
  print_newline ();
  print_string test55;
  print_newline ();
  print_string test56;
  print_newline ();
  print_string test57;
  print_newline ();
  print_string test58;
  print_newline ();
  print_string test59;
  print_newline ();
  print_string test60;
  print_newline ();
  print_string test61;
  print_newline ();
  print_string test62;
  print_newline ();
  print_string test63;
  print_newline ();
  print_string test64;
  print_newline ();
  print_string test65;
  print_newline ();
  print_string test66;
  print_newline ();
  print_string test67;
  print_newline ();
  print_string test68;
  print_newline ();
  print_string test69;
  print_newline ();
  print_string test70;
  print_newline ();
  print_string test71;
  print_newline ();
  print_string test72;
  print_newline ();
  print_string test73;
  print_newline ();
  print_string test74;
  print_newline ();
  print_string test75;
  print_newline ();
  print_string test76;
  print_newline ();
  print_string test77;
  print_newline ();
  print_string test78;
  print_newline ();
  print_string test79;
  print_newline ();
  print_string test80;
  print_newline ();
  print_string test81;
  print_newline ();
  print_string test82;
  print_newline ();
  print_string test83;
  print_newline ();
  print_string test84;
  print_newline ();
  print_string test85;
  print_newline ();
  print_string test86;
  print_newline ();
  print_string test87;
  print_newline ();
  print_string test88;
  print_newline ();
  print_string test89;
  print_newline ();
  print_string test90;
  print_newline ();
  print_string test91;
  print_newline ();
  print_string test92;
  print_newline ();
  print_string test93;
  print_newline ();
  print_string test94;
  print_newline ();
  print_string test95;
  print_newline ();
  print_string test96;
  print_newline ();
  exit 0

(* File calc.ml *)
open Parse_timed_automaton
open Clock_constraint_utilities
open Graph_functions2
open Grammar_types
open UDBM_utilities
open Zone_stubs
open ZVG_modules

let text_dump ta g = 
  let txt_out = open_out "/tmp/lts.txt" in
  Printf.fprintf txt_out "#locations %s\n" (string_of_int ta.numlocations);
  Printf.fprintf txt_out "#trans %s\n" (string_of_int ta.numtrans);
  Printf.fprintf txt_out "#clocks %s\n" (string_of_int ta.numclocks);
  Printf.fprintf txt_out "#actions %s\n" (string_of_int ta.numactions);
  Printf.fprintf txt_out "#init %s\n" (string_of_int ta.numinit);
  let len = (Array.length g) in
  for i = 0 to len - 1 do
    List.iter
      (function (zone, edges_of_zone) ->
        Printf.fprintf txt_out "\nlocation: %s\n" (string_of_int i);
        Printf.fprintf txt_out
          "invar: %s\n"
          (dbm_to_string ta.clock_names zone.zone_constraint2)
        ;
        Printf.fprintf txt_out "trans:\n";
        List.iter
          (function (departure, _) ->
            Printf.fprintf
              txt_out
              "ACT %s; RESET { %s }; goto %s\n"
              (string_of_int departure.action)
              (String.concat
                 " "
                 (Array.to_list departure.clock_resets)
              )
              (string_of_int departure.next_location)
          )
          edges_of_zone
      )
      g.(i)
    ;
  done;
  flush txt_out;
  close_out txt_out

let _ =
  let result = parse_timed_automaton stdin in
  let g = generate_zone_valuation_graph result in
  text_dump result g;
  let l = lts_of_zone_valuation_graph result in
  (* let *)
  (*     zone_list_list = *)
  (*   List.map *)
  (*     (function zone_edge_list -> *)
  (*       List.map *)
  (*         (function (zone, _) -> zone) *)
  (*         zone_edge_list *)
  (*     ) *)
  (*     (Array.to_list g) *)
  (* in *)
  (* let partition = ZVGLTS2.fernandez_specifying_partition l zone_list_list in *)
  (ZVGLTS2.print_dot
     l
     "/tmp/lts.dot");
  let partition = ZVGLTS2.fernandez l in
  (ZVGLTS2.print_quotient_dot
     l
     partition
     "/tmp/lts_quotient.dot");
  let q = ZVGLTS2.quotient_lts l partition in
  (ZVGQuotientLTS2.print_dot q "/tmp/quotient_lts.dot");
  exit 0


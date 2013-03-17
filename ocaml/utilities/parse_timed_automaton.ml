open Grammar_types
open Zone_stubs

exception Insane_automaton

let is_sane_unit_clock_constraint ta unit_clock_constraint =
  let f (cn, n) = (n >= 0)
    &&
      (List.exists
	 ((=) cn)
	 (Array.to_list ta.clock_names)
      )
  in
  match unit_clock_constraint with
    True -> true
  | False -> false
  | Lt (cn, n) -> f (cn, n)
  | Le (cn, n) -> f (cn, n)
  | Eq (cn, n) -> f (cn, n)
  | Ge (cn, n) -> f (cn, n)
  | Gt (cn, n) -> f (cn, n)

let rec is_sane_clock_constraint ta clock_constraint = 
  List.fold_left
    (function partial_sanity -> 
      function unit_clock_constraint ->
	partial_sanity
	&&
	  (is_sane_unit_clock_constraint
	     ta
	     unit_clock_constraint))
    true
    clock_constraint

let is_sane_timed_automaton ta = 
  ta.numlocations == Array.length ta.locations
  &&
    ta.numtrans ==
    (Array.fold_left
       (function partial_sum ->
	 (function location -> partial_sum + (Array.length
						location.departures)))
       0
       ta.locations
    )
  &&
    ta.numclocks == Array.length ta.clock_names
  &&
    ta.numinit >= 0
  &&
    ta.numinit < ta.numlocations
  &&
    (Array.fold_left
       (function truth ->
	 (function location ->
	   truth
	   &&
	     location.location_index >= 0
	   &&
	     location.location_index < ta.numlocations
	   &&
	     is_sane_clock_constraint ta location.invariant
	   &&
	     (Array.fold_left
		(function truth ->
		  (function transition ->
		    truth
		    &&
		      is_sane_clock_constraint ta transition.condition
		    &&
		      transition.next_location >= 0
		    &&
		      transition.next_location < ta.numlocations
		  )
		)
		true
		location.departures)
	 )
       )
       true
       ta.locations)
    
let parse_timed_automaton channel =
  let
      lexbuf = Lexing.from_channel channel
  in
  let
      ta = Timed_automaton_parser.main Timed_automaton_lexer.token lexbuf
  in
  if
    is_sane_timed_automaton ta
  then
    ta
  else
    raise Insane_automaton


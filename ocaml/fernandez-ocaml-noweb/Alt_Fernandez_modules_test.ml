open Alt_Fernandez_modules

module V =
  struct
    type t = int
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal = Pervasives.(=)
    let state_name = string_of_int
  end

module E1 =
  struct
    type t = int
    let compare = Pervasives.compare
    let default = 0
    type action = t
    module ActionMap = Map.Make(
      struct
        type t = action
        let compare = Pervasives.compare
      end
    )
    let action_names =
      ActionMap.add 2 "2" (ActionMap.add 1 "1" (ActionMap.add 0 "0" ActionMap.empty))
  end

module IntIntLTS1 = LTS_Functor (V) (E1)

let test93 =
  try
    match
      IntIntLTS1.add_edge IntIntLTS1.empty 0 1
    with
    | _ -> "test93 passed"
  with
  | Invalid_argument _ -> "test93 failed"

let test94 =
  try
    match
      IntIntLTS1.add_edge_e IntIntLTS1.empty (0, 0, 1)
    with
    | _ -> "test94 passed"
  with
  | Invalid_argument _ -> "test94 failed"

let test95 =
  try
    match
      IntIntLTS1.add_edge_e IntIntLTS1.empty (0, -1, 1)
    with
    | _ -> "test95 failed"
  with
  | Invalid_argument _ -> "test95 passed"

module E2 =
  struct
    include E1
    let action_names = ActionMap.add 3 "3" (ActionMap.add 2 "2" (ActionMap.add 1 "1" ActionMap.empty))
  end

module IntIntLTS2 = LTS_Functor (V) (E2)

module IntIntLTS1Dot = LTS_Dot_Functor (IntIntLTS1)

module IntIntLTS2Dot = LTS_Dot_Functor (IntIntLTS2)

let test96 =
  try
    match
      IntIntLTS2.add_edge IntIntLTS2.empty 0 1
    with
    | g -> "test96 failed"
  with
  | Invalid_argument _ -> "test96 passed"

let test97 =
  try
    match
      IntIntLTS2.add_edge_e IntIntLTS2.empty (0, 3, 1)
    with
    | g -> "test97 passed"
  with
  | Invalid_argument _ -> "test97 failed"

let g01 =
  List.fold_left
    (fun g e -> IntIntLTS1.add_edge_e g e)
    (IntIntLTS1.add_vertex IntIntLTS1.empty 7)
    [(0, 0, 1); (1, 0, 1); (2, 1, 3); (3, 1, 4); (4, 1, 2); (5, 0, 0);
     (6, 0, 3); (5, 1, 6)]

let () =
  let
      c = open_out "/tmp/alt_lts.dot"
  in
  let
      () = IntIntLTS1Dot.output_graph c g01
  in
  close_out c

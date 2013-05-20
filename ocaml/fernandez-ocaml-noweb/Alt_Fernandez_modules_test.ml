open Alt_Fernandez_modules

module V =
  struct
    type t = int
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal = Pervasives.(=)
  end

module E1 =
  struct
    type t = int
    let compare = Pervasives.compare
    let default = 0
    type action = t
    module ActionSet = Set.Make(
      struct
        type t = action
        let compare = Pervasives.compare
      end
    )
    let actions = ActionSet.add 2 (ActionSet.add 1 (ActionSet.add 0 ActionSet.empty))
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
    let actions = ActionSet.add 3 (ActionSet.add 2 (ActionSet.add 1 ActionSet.empty))
  end

module IntIntLTS2 = LTS_Functor (V) (E2)

let test96 =
  try
    match
      IntIntLTS2.add_edge IntIntLTS2.empty 0 1
    with
    | _ -> "test96 failed"
  with
  | Invalid_argument _ -> "test96 passed"

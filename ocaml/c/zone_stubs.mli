type raw_t (*This usually stores pointers to raw_t*)
type constraint_t
external dbm_init: int -> raw_t = "zone_dbm_init"
external dbm_finish: raw_t -> unit = "zone_dbm_finish"
external dbm_constrainC: raw_t -> int -> constraint_t -> raw_t =
  "zone_dbm_constrainC"
external dbm_constraint2: int -> int -> int -> bool -> constraint_t =
  "zone_dbm_constraint2"
external dbm_isEmpty: raw_t -> int -> bool = "zone_dbm_isEmpty"
external dbm_haveIntersection: raw_t -> raw_t -> int -> bool = "zone_dbm_haveIntersection"
external dbm_intersection: raw_t -> raw_t -> int -> raw_t = "zone_dbm_intersection"
external dbm_freeClock: raw_t -> int -> int -> raw_t =
  "zone_dbm_freeClock"
external dbm_updateValue: raw_t -> int -> int -> int -> raw_t =
  "zone_dbm_updateValue"
external dbm_up: raw_t -> int -> raw_t = "zone_dbm_up"
external dbm_toString: raw_t -> int -> string = "zone_dbm_toString"
external dbm_zero: raw_t -> int -> raw_t = "zone_dbm_zero"
external dbm_toConstraintList: raw_t -> int -> ((int * int * bool * int) list) =
  "zone_dbm_toConstraintList"
external dbm_raw2bound: raw_t -> int = "zone_dbm_raw2bound"
(*This is intended to be experimental in nature.*)
external int_toList: int -> (int * bool) list = "zone_int_toList"

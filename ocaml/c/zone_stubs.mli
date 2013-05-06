type dbm_struct_t (*This usually dbm_struct_t values, which
                    themselves contain malloc()-ated pointers.*)
type constraint_t
external dbm_init: int -> dbm_struct_t = "zone_dbm_init"
external dbm_constrainC: dbm_struct_t -> constraint_t -> dbm_struct_t =
  "zone_dbm_constrainC"
external dbm_constraint2: int -> int -> int -> bool -> constraint_t =
  "zone_dbm_constraint2"
external dbm_isEmpty: dbm_struct_t -> bool = "zone_dbm_isEmpty"
external dbm_haveIntersection: dbm_struct_t -> dbm_struct_t -> bool = "zone_dbm_haveIntersection"
external dbm_intersection: dbm_struct_t -> dbm_struct_t -> dbm_struct_t = "zone_dbm_intersection"
external dbm_freeClock: dbm_struct_t -> int -> dbm_struct_t =
  "zone_dbm_freeClock"
external dbm_updateValue: dbm_struct_t -> int -> int -> dbm_struct_t =
  "zone_dbm_updateValue"
external dbm_up: dbm_struct_t -> dbm_struct_t = "zone_dbm_up"
external dbm_toString: dbm_struct_t -> int -> string = "zone_dbm_toString"
external dbm_zero: dbm_struct_t -> int -> dbm_struct_t = "zone_dbm_zero"
external dbm_isZeroIncluded: dbm_struct_t -> int -> bool = "zone_dbm_isZeroIncluded"
external dbm_toConstraintList: dbm_struct_t -> int -> ((int * int * bool * int) list) =
  "zone_dbm_toConstraintList"
external dbm_toLargerConstraintList: dbm_struct_t -> int -> ((int * int * bool * int) list) =
  "zone_dbm_toLargerConstraintList"
(*This is intended to be experimental in nature.*)
external dbm_areEqual: dbm_struct_t -> dbm_struct_t -> int -> bool = "zone_dbm_areEqual"
external int_toList: int -> (int * bool) list = "zone_int_toList"

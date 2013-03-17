type raw_t (*This usually stores pointers to raw_t*)
type constraint_t
external dbm_init: int -> raw_t = "zone_dbm_init"
external dbm_finish: raw_t -> unit = "zone_dbm_finish"
external dbm_constrainC: raw_t -> int -> constraint_t -> raw_t =
  "zone_dbm_constrainC"
external dbm_constraint2: int -> int -> int -> bool -> constraint_t =
  "zone_dbm_constraint2"
external dbm_isEmpty: raw_t -> int -> bool = "zone_dbm_isEmpty"

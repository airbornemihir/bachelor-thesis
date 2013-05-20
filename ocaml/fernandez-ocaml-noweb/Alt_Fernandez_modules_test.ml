open Alt_Fernandez_modules

module V =
  struct
    type t = int
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal = Pervasives.(=)
  end

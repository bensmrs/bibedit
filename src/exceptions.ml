exception Unknown_policy of string
exception Unknown_key of string
exception Unknown_value of string

let unknown_policy p = raise (Unknown_policy ("Policy `" ^ Option.get p ^ "' unknown. Aborting."))
let unknown_key k p = raise (Unknown_key ("Key `" ^ Option.get k ^ "' unknown for policy `" ^ Option.get p ^ "'. Aborting."))
let unknown_value v p = raise (Unknown_value ("Value `" ^ Option.get v ^ "' unknown for policy `" ^ Option.get p ^ "'. Aborting."))
let unknown_value_for k v p = raise (Unknown_value ("Value `" ^ Option.get v ^ "' unknown for key `" ^ Option.get k ^ "' of policy `" ^ Option.get p ^ "'. Aborting."))

let get_default e = function
  | Some e' -> e'
  | None    -> e

let get_string = get_default ""

let string_of_chars chars = String.of_seq (List.to_seq chars)

let string_to_chars str = List.of_seq (String.to_seq str)

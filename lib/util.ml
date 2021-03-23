let rewind lexbuf n =
  (* TODO we should ensure that n is not too large *)
  let open Lexing in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum - n; } ;
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - n

let get_default e = function
  | Some e' -> e'
  | None    -> e

let get_string = get_default ""

let string_of_chars chars = String.of_seq (List.to_seq chars)

{
  open Parser
}

let text = ['a'-'z''A'-'Z''0'-'9']+
let whitespace = [' ''\t''\n']

rule token = parse
  | '-' '-'   { transformation lexbuf }
  | text as s { FILE(s) }

and transformation = parse
  | text as s whitespace* { global_state := Value; TRANSFORMATION(s) }

and chain = parse
  | ',' { COMMA }
  | '=' { EQUAL }
  | whitespace

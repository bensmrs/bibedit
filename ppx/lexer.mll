{
  open Parser
}

let integer = ['0'-'9']+
let whitespace = [' ''\t''\n']
let separator = [^' ''\t''\n''<''>''{''}']+

rule token = parse
  | eof                                { EOF }
  | '<' (integer as s) '>'             { ARG (int_of_string s) }
  | separator as s '<' '.' '.' '.' '>' { ETC s }
  | '{'                                { LBRACE }
  | '}'                                { RBRACE }
  | whitespace                         { THEN }
  | separator as s                     { SEP s }

{
  open Parser
  open Types
  type lexer_state = Default | BeforeOpen | Entry | String of int * str_delim
  let global_state = ref Default
}

let comment_type = ['c''C']['o''O']['m''M']['m''M']['e''E']['n''N']['t''T']
let string_type = ['s''S']['t''T']['r''R']['i''I']['n''N']['g''G']
let preamble_type = ['p''P']['r''R']['e''E']['a''A']['m''M']['b''B']['l''L']['e''E']
let other_type = ['a'-'z''A'-'Z']+
let key = ['a'-'z''A'-'Z''_''0'-'9']['a'-'z''A'-'Z''0'-'9''.''-''_']*
let integer = ['0'-'9']+
let not_newline = [^'\n''\r']
let whitespace = [' ''\t''\n']

rule token = parse
  | eof { EOF }
  | _   { Util.rewind lexbuf 1;
          match !global_state with
          | Default     -> default lexbuf
          | BeforeOpen  -> before_open lexbuf
          | Entry       -> entry lexbuf
          | String(i,d) -> str i d lexbuf}

and default = parse
  | eof                      { EOF }
  | '@' (comment_type as s) whitespace* as s' (['{''('] not_newline*) as s''
                             { COMMENTTY(s, s', s'') }
  | '@' (preamble_type as s) { global_state := BeforeOpen; PREAMBLETY(s) }
  | '@' (string_type as s)   { global_state := BeforeOpen; STRINGTY(s) }
  | '@' (other_type as s)    { global_state := BeforeOpen; OTHERTY(s) }
  | whitespace+ as s         { WHITESPACE(s) }
  | _ as c                   { COMMENT(c) }

and before_open = parse
  | '('                { global_state := Entry; LPAREN }
  | '{'                { global_state := Entry; LBRACE }
  | whitespace+ as s   { WHITESPACE(s) }

and entry = parse
  | ','              { COMMA }
  | '='              { EQUAL }
  | '"'              { global_state := String(0, Quote); LQUOTE }
  | '{'              { global_state := String(0, Brace); LBRACE }
  | key as s         { KEY(s) }
  | integer as s     { INTEGER(int_of_string s) }
  | ')'              { global_state := Default; RPAREN }
  | '}'              { global_state := Default; RBRACE }
  | '#'              { HASH }
  | whitespace+ as s { WHITESPACE(s) }

and str level d = parse
  | '"'      { if d = Quote && level = 0 then (global_state := Entry; RQUOTE) else CHAR('"') }
  | '@'      { if d = Quote then CHAR('@') else failwith "Illegal '@' in string definition" }
  | '\\' '{' { MULTICHAR("\\{") }
  | '\\' '}' { MULTICHAR("\\}") }
  | '{'      { global_state := String(level+1,d); LBRACE }
  | '}'      { global_state := if level = 0 then Entry else String(level-1,d); RBRACE }
  | _ as c   { CHAR(c) }

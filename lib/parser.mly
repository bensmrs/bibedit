%{
  open Types
  open Util
%}

%token EOF

/* Pairs */
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LQUOTE RQUOTE

/* Entry types */
%token <string * string * string> COMMENTTY
%token <string> PREAMBLETY STRINGTY OTHERTY
%token <char> COMMENT

/* Separators */
%token COMMA EQUAL HASH
%token <string> WHITESPACE

/* Entry elements */
%token <string> KEY MULTICHAR
%token <int> INTEGER
%token <char> CHAR

/* Entrypoint */
%start start
%type <bentry list> start

%%

start: entry* EOF { $1 }

entry:
  | out_comment                                  { BComment(None, $1) }
  | COMMENTTY WHITESPACE?                        { let (e,w,d) = $1 in BComment(Some (e, w), d ^ get_string $2) }
  | PREAMBLETY WHITESPACE? lparen strval rparen  { BPreEntry(($1, EParen, (get_string $2, $3, $5)), $4) }
  | PREAMBLETY WHITESPACE? lbrace strval rbrace  { BPreEntry(($1, EBrace, (get_string $2, $3, $5)), $4) }
  | STRINGTY WHITESPACE? lparen key_value rparen { BStrEntry(($1, EParen, (get_string $2, $3, $5)), $4) }
  | STRINGTY WHITESPACE? lbrace key_value rbrace { BStrEntry(($1, EBrace, (get_string $2, $3, $5)), $4) }
  | OTHERTY WHITESPACE? lparen entry_data rparen { BEntry(($1, EParen, (get_string $2, $3, $5)), $4) }
  | OTHERTY WHITESPACE? lbrace entry_data rbrace { BEntry(($1, EBrace, (get_string $2, $3, $5)), $4) }

out_comment:
  | COMMENT WHITESPACE?             {String.make 1 $1 ^ get_string $2}
  | COMMENT WHITESPACE? out_comment {String.make 1 $1 ^ get_string $2 ^ $3}

lparen: LPAREN WHITESPACE? { get_string $2 }
rparen: RPAREN WHITESPACE? { get_string $2 }
lbrace: LBRACE WHITESPACE? { get_string $2 }
rbrace: RBRACE WHITESPACE? { get_string $2 }
rquote: RQUOTE WHITESPACE? { get_string $2 }
comma: COMMA WHITESPACE?   { get_string $2 }
equal: EQUAL WHITESPACE?   { get_string $2 }
hash: HASH WHITESPACE?     { get_string $2 }
key: KEY WHITESPACE?       { ($1, get_string $2) }

entry_data: key comma key_value_list { ($1, $2, $3) }

key_value_list:
  | key_value comma?               { [($1, $2)] }
  | key_value comma key_value_list { ($1, Some $2)::$3 }

key_value: key equal value { ($1, $2, $3) }

value:
  | INTEGER WHITESPACE? { BInt($1, get_string $2) }
  | strval              { BStrVal($1) }

strval:
  | LQUOTE str rquote             { BString(Quote, $2, $3) }
  | LQUOTE str rquote hash strval { BCat(BString(Quote, $2, $3), $4, $5) }
  | LBRACE str rbrace             { BString(Brace, $2, $3) }
  | key                           { bvar_of $1 }
  | key hash strval               { BCat(bvar_of $1, $2, $3) }

str:
  |                       { "" }
  | CHAR str              { String.make 1 $1 ^ $2 }
  | MULTICHAR str         { $1 ^ $2 }
  | LBRACE str RBRACE str { "{" ^ $2 ^ "}" ^ $4 }

%{
  open Types

  let (@!) (args, rules) (args', rules') = (args @ args', rules @ rules')
%}

%token EOF, LBRACE, RBRACE, THEN
%token <int> ARG
%token <string> SEP, ETC

%start start
%type <int list * rule list> start

%%

start: expr EOF { $1 }

arg:
  | ARG     { ([$1-1], []) }
  | SEP ARG { ([$2-1], [Prefix $1]) }

expr:
  | SEP ARG              { ([$2-1], [Prefix $1]) }
  | SEP ARG THEN l2_expr { ([$2-1], [Prefix $1; Then]) @! $4 }
  | SEP ARG SEP l2_expr  { ([$2-1], [Prefix $1; Key_value $3]) @! $4 }

l2_expr:
  | sub_expr                   { $1 }
  | LBRACE sub_expr RBRACE ETC { ([], [Split_on $4]) @! $2 }

sub_expr:
  | arg             { $1 }
  | arg SEP l2_expr { $1 @! ([], [Key_value $2]) @! $3 }

let to_string = Processing.string_of_bib
let from_channel c = Parser.start Lexer.token (Lexing.from_channel c)
let apply_policies = Processing.apply_policies

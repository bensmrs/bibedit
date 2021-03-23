module Ty = Types
module P = Parser
module L = Lexer
open Ppxlib
open Ast_builder.Default
open Util

let unsupported_pattern loc = Location.raise_errorf ~loc "Unsupported pattern"
let unsupported_expression loc = Location.raise_errorf ~loc "Unsupported expression"
let unsupported_payload loc = Location.raise_errorf ~loc "Unsupported payload"

type doc_cat = { cat_name: string; arg_id: int; choices: doc_item list }
and doc_item = { item_name: string option; item_keys: string list; desc: string option; doc: string option; children: doc_cat list }
type state = First | Default of string option | In_tuple

let zipmap f el =
  let (xl, yl) = List.fold_left
                   (fun (xl, yl) e -> let (x, y) = f e in (x::xl, y::yl))
                   ([], []) el in
  (List.rev xl, List.rev yl)

let parse_attribute allowed attr  =
  let name = attr.attr_name.txt in
  let loc = attr.attr_loc in
  if List.exists ((=) name) allowed
  then match attr.attr_payload with
       | PStr l -> if List.length l = 1
                   then (match (List.hd l).pstr_desc with
                         | Pstr_eval (e, _) -> (match e.pexp_desc with
                                                | Pexp_constant (Pconst_string (s,_,_)) -> Some (name, s)
                                                | _                                     -> unsupported_payload loc)
                         | _                -> unsupported_payload loc)
                   else unsupported_payload loc
       | _      -> unsupported_payload loc
  else None

let singular_of s = List.hd (String.split_on_char '|' s)

let plural_of s =
  let split_s = String.split_on_char '|' s in
  if List.length split_s = 1
  then s ^ "s"
  else List.hd (List.tl split_s)

let fill_item item = function
  | "desc", desc -> { item with desc = Some desc }
  | "doc", doc   -> { item with doc = Some doc }
  | _, _         -> item

let rec parse_item_attributes item = function
  | hd::tl -> parse_item_attributes
                (match parse_attribute ["desc"; "doc"] hd with
                 | Some p -> fill_item item p
                 | None   -> item)
                tl
  | []     -> item

let rec parse_cat_attributes arg_id = function
  | hd::tl -> (match parse_attribute ["kind"] hd with
               | Some (_, cat_name) -> { cat_name; arg_id; choices = [] }
               | None               -> parse_cat_attributes arg_id tl)
  | []     -> { cat_name = "argument"; arg_id; choices = [] }

let rec flatten_ppat p = match p.ppat_desc with
  | Ppat_or (pat, pat') -> let (pats, attrs)   = flatten_ppat pat in
                           let (pats', attrs') = flatten_ppat pat' in
                           (pats @ pats', attrs @ attrs' @ [p.ppat_attributes])
  | Ppat_alias (pat, _) -> let (pats, attrs) = flatten_ppat pat in
                                  (pats, attrs @ [p.ppat_attributes])
  | Ppat_constant _ | Ppat_any | Ppat_var _
                        -> ([p], [p.ppat_attributes])
  | _                   -> unsupported_pattern p.ppat_loc

let rec get_pat_value p = match p.ppat_desc with
  | Ppat_constant (Pconst_string (value, _, _)) -> Some value
  | Ppat_alias (pat, _)                         -> get_pat_value pat
  | Ppat_any | Ppat_var _                       -> None
  | _                                           -> unsupported_pattern p.ppat_loc

let rewrite_pattern p guard =
  let loc = p.ppat_loc in
  let guard_attrs = List.map (fun e -> e.pexp_attributes) (Option.to_list guard) in
  ((match p.ppat_desc with
    | Ppat_constant _ -> let key = get_pat_value p in
                         parse_item_attributes
                           { item_name = key; item_keys = Option.to_list key; desc = None; doc = None; children = [] }
                           p.ppat_attributes
    | Ppat_or _ | Ppat_alias _ | Ppat_var _
                      -> let (pats, attrs) = flatten_ppat p in
                         parse_item_attributes
                           { item_name = get_pat_value (List.hd pats); item_keys = List.flatten (List.map (fun x -> Option.to_list (get_pat_value x)) pats); desc = None; doc = None; children = [] } 
                           (try List.find (fun x -> List.length x != 0) (guard_attrs @ List.rev attrs)
                            with Not_found -> [])
    | Ppat_any        -> parse_item_attributes { item_name = None; item_keys = []; desc = None; doc = None; children = [] } p.ppat_attributes
    | _               -> unsupported_pattern p.ppat_loc),
   ppat_construct ~loc { txt = Lident "Some"; loc } (Some p))

let unknown_cat l =
  let format_cat = function
   | (name, Some value) -> singular_of name ^ " `" ^ value ^ "'"
   | (name, None)       -> singular_of name in
  (match List.map format_cat l with
   | []          -> failwith "Unreachable state"
   | hd::[]      -> String.capitalize_ascii hd ^ " required"
   | hd::hd'::[] -> String.capitalize_ascii hd' ^ " requires a " ^ hd
   | hd::hd'::tl -> String.capitalize_ascii hd' ^ " of " ^ String.concat " in " tl ^ " requires a " ^ hd)
  ^ ". Please consult the manual."

let unknown_value l =
  let format_cat (n, v) =
   let name = List.hd (String.split_on_char '|' n) in
   match v with
   | Some value -> name ^ " `" ^ value ^ "'"
   | None       -> name in
  (match List.map format_cat l with
   | []          -> failwith "Unreachable state"
   | hd::[]      -> ("Unknown " ^ hd ^ " `", "'.")
   | hd::hd'::[] -> ("Unknown " ^ hd ^ " `", "' for " ^ hd' ^ ". Please consult the manual.")
   | hd::hd'::tl -> ("Unknown " ^ hd ^ " `", "' for " ^ hd' ^ " of " ^ String.concat " in " tl ^ ". Please consult the manual."))

let make_fail_value loc l =
  let (before, after) = unknown_value l in
  let after = if List.length l = 1
              then eapply ~loc (evar ~loc "^") [estring ~loc (after ^ "\n\nConsult `");
                                                eapply ~loc (evar ~loc "^") [eapply ~loc (evar ~loc "Array.get") [evar ~loc "Sys.argv";
                                                                                                                  eint ~loc 0];
                                                                             estring ~loc " --help' for more information."]
                                               ]
              else estring ~loc after in
  eapply ~loc (evar ~loc "failwith") [eapply ~loc (evar ~loc "^") [estring ~loc before;
                                                                   eapply ~loc (evar ~loc "^") [evar ~loc "v"; after]]]

let color_of_int = function
                   | 0 -> Ty.Cyan
                   | 1 -> Green
                   | 2 -> Yellow
                   | 3 -> Red
                   | 4 -> Magenta
                   | 5 -> Blue
                   | _ -> Default

let colorize color s = match color with
  | Ty.Red  -> "\x1b[31m" ^ s ^ "\x1b[0m"
  | Green   -> "\x1b[32m" ^ s ^ "\x1b[0m"
  | Yellow  -> "\x1b[33m" ^ s ^ "\x1b[0m"
  | Blue    -> "\x1b[34m" ^ s ^ "\x1b[0m"
  | Magenta -> "\x1b[35m" ^ s ^ "\x1b[0m"
  | Cyan    -> "\x1b[36m" ^ s ^ "\x1b[0m"
  | Default -> s

let rec rewrite_expression cat_names name expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_match (e, cases)
                 -> let arg_id = (match e.pexp_desc with
                                  | Pexp_extension ({ txt = "arg"; _ }, PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_integer (i, None)); _ }, _); _}])
                                      -> int_of_string i - 1
                                  | _ -> Location.raise_errorf ~loc:e.pexp_loc ("The match can only be done on [%%arg <int>] arguments")) in
                    (match name with
                     | First | Default _ -> let cat_attr = parse_cat_attributes arg_id e.pexp_attributes in
                                            let (choices, asf) = rewriter_cases ((cat_attr.cat_name, None)::cat_names) cases in
                                            let cat = [{ cat_attr with choices }] in
                                            let match_exp = pexp_match ~loc e asf in
                                            (cat, match name with
                                                  | Default (Some cat_name) -> pexp_variant ~loc (String.capitalize_ascii cat_name) (Some match_exp)
                                                  | Default None            -> unsupported_expression loc
                                                  | _                       -> match_exp)
                     | In_tuple         -> expr_mapper false cat_names expr)
  | Pexp_tuple l -> if List.for_all (fun e -> match e.pexp_desc with Pexp_match _ -> true | _ -> false) l
                    then let (cats, asf) = zipmap (rewrite_expression cat_names In_tuple) l in
                         (List.flatten cats, match name with
                                             | Default (Some cat_name) -> pexp_variant ~loc (String.capitalize_ascii cat_name) (Some (pexp_tuple ~loc asf))
                                             | _                       -> unsupported_expression loc)
                    else ([], expr)
  | _            -> ([], expr)

and rewrite_case cat_names case =
  let (item, pc_lhs) = rewrite_pattern case.pc_lhs case.pc_guard in
  let pc_guard = if Option.is_none case.pc_guard then Some (ebool ~loc:case.pc_lhs.ppat_loc true) else case.pc_guard in
  let cat_names = match cat_names with
                  | (name, None)::tl -> (name, item.item_name)::tl
                  | _                -> failwith "Unreachable state" in
  let (children, pc_rhs) = rewrite_expression cat_names (Default item.item_name) case.pc_rhs in
  ({ item with children }, { pc_lhs; pc_guard; pc_rhs })

and rewriter_cases cat_names cases =
  let loc = (List.hd cases).pc_lhs.ppat_loc in
  let (items, cases') = zipmap (rewrite_case cat_names) cases in
  (items, case ~lhs:(ppat_construct ~loc { txt = Lident "None"; loc } None)
               ~guard:None
               ~rhs:(eapply ~loc (evar ~loc "failwith") [estring ~loc (unknown_cat cat_names)])::cases' @
          [case ~lhs:(ppat_construct ~loc { txt = Lident "Some"; loc } (Some (pvar ~loc "v")))
                ~guard:None
                ~rhs:(make_fail_value loc cat_names)])

and build_help cats loc =
  let repeat n s = String.concat "" (List.init (max 0 n) (fun _ -> s)) in
  let fill_with s = (repeat 80 s) ^ "\n" in
  let subsection level s = repeat level "│ " ^ s ^ "\n" ^ repeat level "│ " ^ "┌\n" in
  let fill_until n s s' = s' ^ repeat (n - String.length s') s in
  let split_words width s =
    let lstrip s = if String.length s > 0 && s.[0] = ' '
                   then String.sub s 1 (String.length s - 1)
                   else s in
    let rec split_words_rec (consumed, current) = function
    | ('-' | ',' | ';' | ':' | '.') as c::tl
                       -> if String.length consumed + String.length current + 1 <= width
                          then split_words_rec (consumed ^ current ^ (String.make 1 c), "") tl
                          else if consumed = ""
                               then (current, Some (string_of_chars (c::tl)))
                               else (consumed, Some (lstrip (current ^ string_of_chars (c::tl))))
    | (' ' | '\t')::tl -> if String.length consumed = 0 && String.length current = 0
                          then split_words_rec (consumed, current) tl
                          else if String.length consumed + String.length current = width
                               then (consumed ^ current, Some (string_of_chars tl))
                               else split_words_rec (consumed ^ current, " ") tl
    | c::tl            -> if String.length consumed + String.length current + 1 <= width
                          then split_words_rec (consumed, current ^ String.make 1 c) tl
                          else if consumed = ""
                               then (current, Some (string_of_chars (c::tl)))
                               else (consumed, Some (lstrip (current ^ string_of_chars (c::tl))))
    | []               -> (consumed ^ current, None) in
    split_words_rec ("", "") (string_to_chars s) in
  let apply_color (color, box) =  List.map (colorize color) box in
  let boxed level widths ?(colors=[]) l =
    let rec boxed_rec =
      let rec boxed_rec_rec width = function
        | hd::tl when String.length hd <= width -> fill_until width " " hd::boxed_rec_rec width tl
        | hd::tl                                -> let (first, other) = split_words width hd in
                                                   (fill_until width " " first)::(match other with
                                                                                  | None   -> tl
                                                                                  | Some s -> boxed_rec_rec width (s::tl))
        | []                                    -> []
      in function
      | (w, s)::tl -> boxed_rec_rec w (String.split_on_char '\n' s)::boxed_rec tl
      | []         -> [] in
    let boxes = boxed_rec (List.combine widths l) in
    let size = List.fold_left (fun x y -> max x (List.length y)) 0 boxes in
    let boxes = List.mapi (fun i l -> l @ (List.init (size - List.length l) (fun _ -> repeat (List.nth widths i) " "))) boxes in
    let boxes = if List.length colors = List.length boxes
                then List.map apply_color (List.combine colors boxes)
                else boxes in
    String.concat "\n" (List.fold_left (fun x y -> List.map (fun (x, y) -> x ^ "  " ^ y) (List.combine x y)) (List.map (fun x -> repeat level "│ " ^ x) (List.hd boxes)) (List.tl boxes)) in
  let rec dump_tree level = function
    | { cat_name; arg_id; choices }::tl -> subsection level (String.capitalize_ascii (plural_of cat_name) ^
                                                                                     colorize (color_of_int arg_id)
                                                                                              (" <" ^ string_of_int (arg_id + 1) ^ ">")) ^
                                             dump_choices true arg_id (level+1) choices ^
                                             repeat level "│ " ^
                                             "└\n" ^
                                             (match tl, choices with
                                              | [], _ -> ""
                                              | _, _   -> repeat (match tl with [] -> level-1 | _ -> level) "│ " ^ "\n") ^ dump_tree level tl
    | []                        -> ""
  and dump_choices ignore_space arg_id level = function
    | { item_keys; desc; doc; children; _ }::tl -> let keys = if List.length item_keys = 0
                                                              then ["<else>"]
                                                              else item_keys in
                                                   (if ignore_space then "" else repeat level "│ " ^ "\n") ^
                                                   boxed level
                                                         [20 - 2 * level; 22; 34]
                                                         ~colors:[color_of_int arg_id; Default; Default]
                                                         [String.concat ",\n" keys; get_string desc; get_string doc] ^ "\n" ^ 
                                                         dump_tree (level+1) children ^ dump_choices false arg_id level tl
    | [] -> "" in
  { pc_lhs = ppat_construct ~loc { txt = Lident "Some"; loc } (Some (pstring ~loc "help"));
    pc_guard = None;
    pc_rhs = pexp_sequence ~loc (eapply ~loc (evar ~loc "print_endline")
                                             [eapply ~loc (evar ~loc "^")
                                                          [eapply ~loc (evar ~loc "^")
                                                                       [eapply ~loc (evar ~loc "^")
                                                                                    [estring ~loc "Usage: ";
                                                                                     eapply ~loc (evar ~loc "Array.get")
                                                                                                 [evar ~loc "Sys.argv";
                                                                                                  eint ~loc 0]];
                                                                        estring ~loc (" <option|file>*\n" ^ fill_with "─" ^ "┌─────────┒\n│ Options ┃  ")];
                                                           eapply ~loc (evar ~loc "^")
                                                                       [eapply ~loc (evar ~loc "!")
                                                                                    [evar ~loc "__cmdargs_parsestr"];
                                                                        estring ~loc ("\n┕━━━━━━━━━┛\n\n" ^ dump_tree 0 cats)]]])
                                (eapply ~loc (evar ~loc "exit") [eint ~loc 0]) }

and expr_mapper gen_help cat_names expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_match _ -> let (cats, e) = rewrite_expression cat_names First expr in
                    (cats, { e with pexp_desc = (match e.pexp_desc with
                                                 | Pexp_match (exp, cases) -> Pexp_match(exp, if gen_help
                                                                                              then (build_help cats loc)::cases
                                                                                              else cases)
                                                 | _                       -> failwith "Unreachable state") })
  | _ -> Location.raise_errorf ~loc "`cmdargs' can only be used with `match'"

let cmdargs_mapper =
  Extension.declare "cmdargs" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ expr -> let (_,ast) = expr_mapper true [] expr in
                                let loc = expr.pexp_loc in
                                pexp_fun ~loc Nolabel None (ppat_var ~loc { txt = "_arg_list"; loc }) ast)

let rewrite_arg expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_constant (Pconst_integer (i, None)) -> eapply ~loc (evar ~loc "List.nth") [evar ~loc "_arg_list"; eint ~loc (int_of_string i - 1)]
  | _                                        -> Location.raise_errorf ~loc "`arg' must be given an integer parameter"

let arg_mapper =
  Extension.declare "arg" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ -> rewrite_arg)

let parse_code =
  {|
    type __cmdargs_rule = X__cmdargs_Prefix of string | X__cmdargs_Split_on of string | X__cmdargs_Key_value of string | X__cmdargs_Then
    type __cmdargs_matched = X__cmdargs_Matched of string | X__cmdargs_Unmatched of string

    let rec __cmdargs_parse rules short tokens =
      let is_then = function X__cmdargs_Then -> true | _ -> false in
      let is_prefix = function X__cmdargs_Prefix _ -> true | _ -> false in
      let burn = ref false in
      let rec parse_then rules current tokens =
        if List.exists ((=) current) short || List.length tokens = 0
        then None
        else Some (parse_rec rules (List.hd tokens))
      and parse_rec rules token =
        if token = ""
        then [[]]
        else match rules with
             | X__cmdargs_Prefix p::tl
                  -> let prefix_len = String.length p in
                     if String.length token >= prefix_len && String.sub token 0 prefix_len = p
                     then let tok = String.sub token prefix_len (String.length token - prefix_len) in
                          List.map (List.cons (X__cmdargs_Matched tok)) (parse_rec tl tok)
                     else [[X__cmdargs_Unmatched token]]
             | X__cmdargs_Split_on s::tl
                  -> List.flatten (List.map (parse_rec tl) (Str.split_delim (Str.regexp s) token))
             | X__cmdargs_Key_value s::tl
                  -> (match Str.bounded_split_delim (Str.regexp s) token 2 with
                      | key::value::_ -> List.map (List.cons (X__cmdargs_Matched key)) (parse_rec tl value)
                      | key::[]       -> [[X__cmdargs_Matched key]]
                      | []            -> failwith "Unreachable state")
             | X__cmdargs_Then::tl
                  -> (match parse_then tl token (List.tl tokens) with
                      | Some paths -> burn := true; paths
                      | None       -> [[]])
             | [] -> [[X__cmdargs_Matched token]]
      in let normalized_rules = match List.length (List.filter is_then rules) with
                                | 0 -> if is_prefix (List.hd rules) && List.length rules = 1
                                       then (List.hd rules)::[X__cmdargs_Then]
                                       else rules
                                | 1 -> if is_prefix (List.hd rules) && is_then (List.nth rules 1)
                                       then rules
                                       else failwith "Misplaced `Then'."
                                | _ -> failwith "Too many `Then'."
      in match tokens with
         | hd::tl -> let paths = (parse_rec normalized_rules hd) in
                     if !burn
                     then (burn := false;
                           (match tl with
                            | _::tl' -> paths @ (__cmdargs_parse rules short tl')
                            | []     -> paths))
                     else paths @ (__cmdargs_parse rules short tl)
         | _      -> []
    
    let rec __cmdargs_unwrap_matched n = function
      | X__cmdargs_Matched s::tl -> Some s::__cmdargs_unwrap_matched (n-1) tl
      | []                       -> List.init n (fun _ -> None)
      | _                        -> failwith "Unreachable state"
  |}

let dump_list f l = "[" ^ String.concat ";" (List.map f l) ^ "]"

let dump_int_list = dump_list string_of_int

let dump_instructions = dump_list (function
                                   | Ty.Prefix s -> "X__cmdargs_Prefix \"" ^ s ^ "\""
                                   | Then        -> "X__cmdargs_Then"
                                   | Split_on s  -> "X__cmdargs_Split_on \"" ^ s ^ "\""
                                   | Key_value s -> "X__cmdargs_Key_value \"" ^ s ^ "\"")

let invoke_parser_code name instructions args matcher =
  let args_length = string_of_int (List.length args) in
  {|
    let |} ^ name ^ {| argv =
      let rec lput e i = function
        | _::tl when i = 0 -> e::tl
        | hd::tl           -> hd::lput e (i-1) tl
        | []               -> [] in
      let reorder indices l =
        let rec reorder_rec acc indices l = match indices, l with
          | hd::tl, hd'::tl' -> reorder_rec (lput hd' hd acc) tl tl'
          | [], []           -> acc
          | _                -> failwith "Unreachable state"
        in reorder_rec (List.init |} ^ args_length ^ {| (fun _ -> None)) indices l in
      let l = Array.to_list (Array.sub argv 1 (Array.length argv - 1)) in
      let paths = __cmdargs_parse |} ^ dump_instructions instructions ^ {| ["help"] l in
      List.fold_left (fun (u, m) y -> match y with
                                      | X__cmdargs_Unmatched s::_ -> (s::u, m)
                                      | _              -> (u, m @ [|} ^ matcher ^ {| (reorder |} ^ dump_int_list args ^ {| (__cmdargs_unwrap_matched |} ^ args_length ^ {| y))])) ([], []) paths
  |}

let valid_args args = List.fold_left (fun x y -> x + y) 0 (List.mapi (fun i x -> abs (x-i)) (List.sort compare args)) = 0

let highlight_numbers s =
  Str.global_replace (Str.regexp "<1>") (colorize (color_of_int 0) "<1>") (Str.global_replace (Str.regexp "<2>") (colorize (color_of_int 1) "<2>") (Str.global_replace (Str.regexp "<3>") (colorize (color_of_int 2) "<3>") (Str.global_replace (Str.regexp "<4>") (colorize (color_of_int 3) "<4>") (Str.global_replace (Str.regexp "<5>") (colorize (color_of_int 4) "<5>") (Str.global_replace (Str.regexp "<6>") (colorize (color_of_int 5) "<6>") s)))))

let rec rewrite_structure acc items =
  match items with
  | { pstr_desc = Pstr_extension (({ txt; loc }, payl), _); _ } as hd::tl ->
        if txt = "parse"
        then (match payl with
              | PStr l -> if List.length l = 1
                          then (match (List.hd l) with
                                | { pstr_desc = Pstr_value (_, l'); pstr_loc; _ }
                                    -> if List.length l' = 1
                                       then (match List.hd l' with
                                             | { pvb_pat = { ppat_desc = Ppat_var { txt; _ }; _ }; pvb_expr = { pexp_desc; pexp_loc;_ }; _ }
                                                   -> (match pexp_desc with
                                                       | Pexp_apply ({ pexp_desc = Pexp_ident {txt = Lident matcher; _}; _ },
                                                                     [(Nolabel, { pexp_desc = Pexp_constant (Pconst_string (shape, _, _)); pexp_loc; _ })])
                                                                      -> let (args, instructions) = P.start L.token (Lexing.from_string shape) in
                                                                         if valid_args args
                                                                         then let prelude = Parse.implementation (Lexing.from_string parse_code) in
                                                                              let impl = Parse.implementation (Lexing.from_string (invoke_parser_code txt instructions args matcher)) in
                                                                              let loc = pexp_loc in
                                                                              let str_ref = { pstr_desc = Pstr_value (Nonrecursive, [{ pvb_pat = pvar ~loc "__cmdargs_parsestr"; pvb_expr = eapply ~loc (evar ~loc "ref") [estring ~loc (highlight_numbers shape)]; pvb_attributes = []; pvb_loc = loc }]); pstr_loc = loc } in
                                                                              (rewrite_structure (str_ref::acc @ prelude @ impl) tl)
                                                                         else Location.raise_errorf ~loc:pexp_loc "The n argument identifiers must go from 1 to n"
                                                       | _ -> Location.raise_errorf ~loc:pexp_loc "`parse' requires a function call of the form: <mapper function> <arguments shape>")
                                             | { pvb_pat = { ppat_loc; _ }; _ } -> Location.raise_errorf ~loc:ppat_loc "`parse' can only be used with simple value assignments")
                                       else Location.raise_errorf ~loc:pstr_loc "`parse' can only be used with single-valued `let' statements"
                                | { pstr_loc; _ } -> Location.raise_errorf ~loc:pstr_loc "`parse' can only be used with `let' statements")
                          else Location.raise_errorf ~loc "`parse' can only be used with single statements"
              | _      -> Location.raise_errorf ~loc "`parse' can only be used with structures")
        else rewrite_structure (acc @ [hd]) tl
  | hd::tl -> rewrite_structure (acc @ [hd]) tl
  | [] -> acc

let () = Driver.register_transformation "cmdargs" ~extensions:[cmdargs_mapper; arg_mapper] ~impl:(rewrite_structure [])

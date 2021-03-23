open Types

let open_str = function
  | Quote -> "\""
  | Brace -> "{"

let close_str = function
  | Quote -> "\""
  | Brace -> "}"

let open_entry = function
  | EParen -> "("
  | EBrace -> "{"

let close_entry = function
  | EParen -> ")"
  | EBrace -> "}"

let string_of_entry_data (ty, op, (sp, sp', sp'')) str = "@" ^ ty ^ sp ^ open_entry op ^ sp' ^ str ^ close_entry op ^ sp''

let rec string_of_bstring = function
  | BCat (s, sp, s')    -> string_of_bstring s ^ "#" ^ sp ^ string_of_bstring s'
  | BString (op, s, sp) -> open_str op ^ s ^ close_str op ^ sp
  | BVar (s, sp)        -> s ^ sp

let string_of_bvalue = function
  | BStrVal s    -> string_of_bstring s
  | BInt (i, sp) -> string_of_int i ^ sp

let string_of_bkey (s, sp) = s ^ sp

let string_of_bkv (k, sp, v) = string_of_bkey k ^ "=" ^ sp ^ string_of_bvalue v

let format_comma = function
  | Some sp -> "," ^ sp
  | None    -> ""

let string_of_bdata (key, sp, l) = string_of_bkey key ^ "," ^ sp ^ List.fold_left (fun x (kv, sp') -> x ^ string_of_bkv kv ^ format_comma sp') "" l

let string_of_bentry = function
  | BEntry (e, data)               -> string_of_entry_data e (string_of_bdata data)
  | BStrEntry (e, data)            -> string_of_entry_data e (string_of_bkv data)
  | BPreEntry (e, data)            -> string_of_entry_data e (string_of_bstring data)
  | BComment (Some (ty, sp), data) -> ty ^ sp ^ data
  | BComment (None, data)          -> data

let string_of_bib = List.fold_left (fun x y -> x ^ string_of_bentry y) ""

let apply_to_types f = function
  | BEntry ((ty, op, spl), data) -> BEntry ((f ty, op, spl), data)
  | e                            -> e

let apply_to_fields f =
  let rec apply_to_fields_rec = function
    | (((k, sp), sp', v), sp'')::tl -> (((f k, sp), sp', v), sp'')::apply_to_fields_rec tl
    | []                            -> [] in function
  | BEntry (e, (k, sp, l)) -> BEntry (e, (k, sp, apply_to_fields_rec l))
  | e                      -> e

let apply_to_variables f = function
  | BStrEntry (e, ((k, sp), sp', v)) -> BStrEntry (e, ((f k, sp), sp', v))
  | e                                -> e

let apply_to_keys f = function
  | BEntry (e, ((k, sp), sp', l)) -> BEntry (e, ((f k, sp), sp', l))
  | e                             -> e

let lowercase = String.lowercase_ascii
let capitalize = (fun x -> String.capitalize_ascii (String.lowercase_ascii x))
let uppercase = String.uppercase_ascii

let case_apply_to kind what =
  let f = match what with
    | `Lowercase  -> lowercase
    | `Uppercase  -> uppercase
    | `Capitalize -> capitalize
  in (match kind with
  | `Types     -> apply_to_types
  | `Fields    -> apply_to_fields
  | `Variables -> apply_to_variables
  | `Keys      -> apply_to_keys) f

let left_indent before_open after_open =
  let before_open_split = String.split_on_char '\n' before_open in
  let potential_left_indent = if List.length before_open_split = 1
                              then "" (* Padding may be needed afterwards *)
                              else List.hd (List.rev before_open_split) ^ " " in
  let after_open_split = String.split_on_char '\n' after_open in
  if List.length after_open_split = 1
  then potential_left_indent ^ List.hd after_open_split
  else List.hd (List.rev after_open_split)

let egyptian before_open after_open _ after_close =
  (" ", "\n" ^ left_indent before_open after_open, "\n", after_close)

let aligned before_open after_open _ after_close =
  ("\n", "\n" ^ left_indent before_open after_open, "\n", after_close)

let narrow _ _ _ after_close = ("", "", "", after_close)

let newlines i before_open _ before_close after_close = (before_open, String.make i '\n', before_close, after_close)

let spaces i before_open _ before_close after_close = (before_open, String.make i ' ', before_close, after_close)

let after_newlines i before_open after_open before_close _ = (before_open, after_open, before_close, String.make i '\n')

let after_spaces i before_open after_open before_close _ = (before_open, after_open, before_close, String.make i ' ')

let rec get_last_bstring_space = function
  | BCat (_, _, s)     -> get_last_bstring_space s
  | BString (_, _, sp) -> sp
  | BVar (_, sp)       -> sp

let rec set_last_bstring_space sp = function
  | BCat (s, sp', s') -> BCat (s, sp', set_last_bstring_space sp s')
  | BString (d, s, _) -> BString (d, s, sp)
  | BVar (s, _)       -> BVar (s, sp)

let get_last_bvalue_space = function
  | BStrVal s    -> get_last_bstring_space s
  | BInt (_, sp) -> sp

let set_last_bvalue_space sp = function
  | BStrVal s   -> BStrVal (set_last_bstring_space sp s)
  | BInt (i, _) -> BInt (i, sp)

let get_last_bkv_space (_, _, v) = get_last_bvalue_space v

let get_last_bkvlist_item_spaces (kv, sp) = (get_last_bkv_space kv, sp)

let set_last_bkv_space sp (k, sp', v) = (k, sp', set_last_bvalue_space sp v)

let set_bkv_eq_space sp sp' ((s, _), _, v) = ((s, sp), sp', v)

let get_last_bkvlist_space kvl = match get_last_bkvlist_item_spaces (List.hd (List.rev kvl)) with
  | (_, Some sp) -> sp
  | (sp, None)   -> sp

let get_last_bkvlist_kv_space kvl =
  match List.hd (List.rev kvl) with
  | (_, Some _)             -> None
  | ((k, _, _) as kv, None) -> Some (k, get_last_bkv_space kv)

let set_last_bkvlist_space sp kvl =
  let rev_kvl = List.rev kvl in
  let last_entry = match List.hd rev_kvl with
  | (kv, Some _) -> (kv, Some sp)
  | (kv, None)   -> (set_last_bkv_space sp kv, None)
  in List.rev (last_entry::(List.tl rev_kvl))

let set_last_bkvlist_pos_spaces (sp, sp') kvl =
  let rev_kvl = List.rev kvl in
  let (kv, _) = List.hd rev_kvl in
  List.rev ((set_last_bkv_space sp kv, sp')::(List.tl rev_kvl))

let set_last_bkvlist_kv_space sp kvl =
  let rev_kvl = List.rev kvl in
  let (kv, sp') = List.hd rev_kvl in
  let last_entry = (set_last_bkv_space sp kv, sp') in
  List.rev (last_entry::(List.tl rev_kvl))

let set_key_bkvlist_kv_space k sp kvl =
  let rec set_rec acc = function
    | ((k', _, _) as kv, sp')::tl when k = k' -> List.rev_append ((set_last_bkv_space sp kv, sp')::acc) tl
    | hd::tl                                  -> set_rec (hd::acc) tl
    | []                                      -> []
  in set_rec [] kvl

let set_last_line_spaces_if_multiline sp ml_sp =
  let ml_sp_split = String.split_on_char '\n' ml_sp in
  if List.length ml_sp_split = 1
  then ml_sp
  else String.concat "\n" (List.rev (sp::List.tl (List.rev ml_sp_split)))

let set_bkvlist_comma_space sp kvl =
  let rec set_bkvlist_comma_space_rec = function
    | (kv, Some _)::hd'::tl -> (set_last_bkv_space "" kv, Some sp)::set_bkvlist_comma_space_rec (hd'::tl)
    | l                     -> List.rev l
  in set_bkvlist_comma_space_rec kvl

let set_bkvlist_eq_space sp sp' kvl =
  let rec set_bkvlist_eq_space_rec = function
    | (kv, sp'')::tl -> (set_bkv_eq_space sp sp' kv, sp'')::set_bkvlist_eq_space_rec tl
    | []             -> []
  in set_bkvlist_eq_space_rec kvl

let set_bkvlist_left_space sp kvl =
  let rec set_bkvlist_left_space_rec = function
    | (kv, Some sp')::hd'::tl -> (kv, Some (set_last_line_spaces_if_multiline sp sp'))::set_bkvlist_left_space_rec (hd'::tl)
    | l                       -> List.rev l
  in set_bkvlist_left_space_rec kvl

let get_last_bdata_space (_, _, kvl) = get_last_bkvlist_space kvl

let set_last_bdata_space sp (k, sp', kvl) = (k, sp', set_last_bkvlist_space sp kvl)

let set_bdata_comma_space sp (k, _, kvl) = (k, sp, set_bkvlist_comma_space sp kvl)

let set_bdata_eq_space sp sp' (k, sp'', kvl) = (k, sp'', set_bkvlist_eq_space sp sp' kvl)

let set_bdata_left_space sp (k, sp', kvl) =
  (k, set_last_line_spaces_if_multiline sp sp', set_bkvlist_left_space sp kvl)

let spacing_braces_fun = function
  | `Egyptian -> egyptian
  | `Aligned  -> aligned
  | `Narrow   -> narrow

let spacing_keys_fun = function
  | `Newlines i -> newlines i
  | `Spaces i  -> spaces i

let spacing_after_fun = function
  | `Newlines i -> after_newlines i
  | `Spaces i   -> after_spaces i

let spacing f = function
  | BEntry ((d, op, (sp, sp', sp''')), data)  -> let (nsp, nsp', nsp'', nsp''') = f sp sp' (get_last_bdata_space data) sp''' in
                                                 BEntry((d, op, (nsp, nsp', nsp''')), set_last_bdata_space nsp'' data)
  | BStrEntry ((d, op, (sp, sp', sp''')), kv) -> let (nsp, nsp', nsp'', nsp''') = f sp sp' (get_last_bkv_space kv) sp''' in
                                                 BStrEntry ((d, op, (nsp, nsp', nsp''')), set_last_bkv_space nsp'' kv)
  | BPreEntry ((d, op, (sp, sp', sp''')), s)  -> let (nsp, nsp', nsp'', nsp''') = f sp sp' (get_last_bstring_space s) sp''' in
                                                 BPreEntry ((d, op, (nsp, nsp', nsp''')), set_last_bstring_space nsp'' s)
  | bc                                        -> bc

let make_spaces = function
  | `Newlines i -> String.make i '\n'
  | `Spaces i   -> String.make i ' '

let spacing_commas v = function
  | BEntry ((d, op, sp), data) -> BEntry ((d, op, sp), set_bdata_comma_space (make_spaces v) data)
  | e                          -> e

let spacing_eq v v' =
  let nsp = make_spaces v in
  let nsp' = make_spaces v' in
  function
  | BEntry ((d, op, sp), data)  -> BEntry ((d, op, sp), set_bdata_eq_space nsp nsp' data)
  | BStrEntry ((d, op, sp), kv) -> BStrEntry ((d, op, sp), set_bkv_eq_space nsp nsp' kv)
  | e                           -> e

let spacing_left v =
  let nsp = match v with
    | `Tabs      -> "\t"
    | `Spaces i -> String.make i ' '
  in function
  | BEntry ((d, op, (sp, sp', sp''')), data)  -> BEntry ((d, op, (sp, set_last_line_spaces_if_multiline nsp sp', sp''')), set_bdata_left_space nsp data)
  | BStrEntry ((d, op, (sp, sp', sp''')), kv) -> BStrEntry ((d, op, (sp, set_last_line_spaces_if_multiline nsp sp', sp''')), kv)
  | BPreEntry ((d, op, (sp, sp', sp''')), s)  -> BPreEntry ((d, op, (sp, set_last_line_spaces_if_multiline nsp sp', sp''')), s)
  | comment                                   -> comment

let sort_entries v =
  let rec wrap = function
    | BComment _ as c::(BEntry _ as e)::tl -> ECat (c, e)::wrap tl
    | e::tl                                -> ESingle e::wrap tl
    | []                                   -> []
  in let rec unwrap = function
    | ECat (c, e)::tl -> c::e::unwrap tl
    | ESingle e::tl   -> e::unwrap tl
    | []              -> []
  in let real_entry = function
    | ECat (_, e) -> e
    | ESingle e   -> e
  in let comp e e' = match real_entry e, real_entry e' with
    | BEntry (_, ((s, _), _, _)), BEntry (_, ((s', _), _, _))       -> compare s s'
    | BEntry _, _                                                   -> 1
    | BStrEntry _, BEntry _                                         -> -1
    | BStrEntry (_, ((s, _), _, _)), BStrEntry (_, ((s', _), _, _)) -> compare s s'
    | BStrEntry _, _                                                -> 1
    | BPreEntry _, BEntry _                                         -> -1
    | BPreEntry _, BPreEntry _                                      -> 0
    | BPreEntry _, _                                                -> 1
    | _, BComment _                                                 -> 0
    | _, _                                                          -> -1
  in let f = match v with
    | `Asc  -> comp
    | `Desc -> fun x y -> -comp x y
  in fun x -> unwrap (List.stable_sort f (wrap x))

let sort_bkvlist v l =
  let (kvs, sps) = List.fold_left (fun (kvs, sps) (kv, sp) -> (kv::kvs, sp::sps)) ([], []) l in
  let comp ((k, _), _, _) ((k', _), _, _) = compare (lowercase k) (lowercase k') in
  let last_space = get_last_bkvlist_kv_space l in
  let f = match v with
    | `Asc  -> fun x y -> -comp x y
    | `Desc -> comp
  in let l' = List.fold_left2 (fun l kv sp -> (kv, sp)::l) [] (List.sort f kvs) sps in
  match last_space with
    | Some (k, s) -> (match get_last_bkvlist_kv_space l' with
                      | Some (_, s') -> set_key_bkvlist_kv_space k s' (set_last_bkvlist_kv_space s l')
                      | None         -> l')
    | None        -> l'

let sort_fields v = function
  | BEntry (e, (k, sp, l)) -> BEntry (e, (k, sp, sort_bkvlist v l))
  | e                      -> e

let apply = fun f b -> List.rev (List.fold_left (fun x y -> f y::x) [] b)

let sort_apply_to kind what = match kind with
   | `Keys   -> sort_entries what
   | `Fields -> apply (sort_fields what)

let rec remove_comments f = function
  | BComment (_, s)::tl when f s -> remove_comments f tl
  | hd::tl                       -> hd::remove_comments f tl
  | []                           -> []

let all_blank_regex = Str.regexp "^[ \t\n]*$"
let alphanum_regex = Str.regexp "[a-zA-Z0-9]"

let match_blanks = fun x -> Str.string_match all_blank_regex x 0
let has_alphanum = fun x -> Str.string_match alphanum_regex x 0

let remove_comments = function
  | `All    -> remove_comments (fun _ -> true)
  | `Blanks -> remove_comments match_blanks
  | `Text   -> remove_comments (fun x -> not (match_blanks x))
  | `Header -> function _::tl -> tl | [] -> []

let process_header =
  let rec process_header_rec acc = function
    | hd::tl when has_alphanum hd -> (String.concat "\n" (List.rev tl), String.concat "\n" (hd::acc))
    | hd::tl                      -> process_header_rec (hd::acc) tl
    | []                          -> (String.concat "\n" (List.rev acc), "")
  in function
  | `First -> (function
               | hd::tl -> hd::BComment (None, "")::tl
               | [] -> [])
  | `Smart -> (function
               | BComment (d, s)::tl -> let hd_split = String.split_on_char '\n' s in
                                        let (header, comment) = process_header_rec [] (List.rev hd_split) in
                                        BComment(d, header)::BComment(None, comment)::tl
               | l                   -> l)
  | `None  -> fun x -> x

let remove_bkvlist_field v kvl =
  let rec remove_rec acc = function
    | (((k, _), _, _), _) as hd::tl when lowercase k = lowercase v
                             -> if List.length tl = 0
                                then set_last_bkvlist_pos_spaces (get_last_bkvlist_item_spaces hd) (List.rev acc)
                                else remove_rec acc tl
    | hd::tl                 -> remove_rec (hd::acc) tl
    | []                     -> List.rev acc
  in remove_rec [] kvl

let rename_bkvlist_field (src, dst) kvl =
  let rec rename_rec acc = function
    | (((k, sp), sp', v), sp'')::tl when lowercase k = lowercase src
                             -> List.rev_append ((((dst, sp), sp', v), sp'')::acc) tl
    | hd::tl                 -> rename_rec (hd::acc) tl
    | []                     -> List.rev acc
  in rename_rec [] kvl

let remove_field v = function
  | BEntry (e, (k, sp, l)) -> BEntry (e, (k, sp, remove_bkvlist_field v l))
  | e                      -> e

let rename_field v = function
  | BEntry (e, (k, sp, l)) -> BEntry (e, (k, sp, rename_bkvlist_field v l))
  | e                      -> e

let apply_policy policy =
  match policy with
    | `Case (k, v)           -> apply (fun b -> List.fold_left (fun x y -> case_apply_to y v x) b k)
    | `Spacing (`Braces v)   -> apply (spacing (spacing_braces_fun v))
    | `Spacing (`Keys v)     -> apply (spacing (spacing_keys_fun v))
    | `Spacing (`Commas v)   -> apply (spacing_commas v)
    | `Spacing (`Eq (v, v')) -> apply (spacing_eq v v')
    | `Spacing (`Left v)     -> apply (spacing_left v)
    | `Spacing (`After v)    -> apply (spacing (spacing_after_fun v))
    | `Sort (k, v)           -> fun b -> List.fold_left (fun x y -> sort_apply_to y k x) b v
    | `Comments (`Remove v)  -> remove_comments v
    | `Comments (`Header v)  -> process_header v
    | `Fields (`Remove v)    -> apply (remove_field v)
    | `Fields (`Rename v)    -> apply (rename_field v)

let apply_policies policies bib = List.fold_left (fun x y -> apply_policy y x) bib policies

type str_delim = Quote | Brace
type entry_opener = EParen | EBrace

type space = string
type comma_space = space
type hash_space = space
type equal_space = space
type three_spaces = string * string * string

type bstring = BCat of bstring * hash_space * bstring
             | BString of str_delim * string * space
             | BVar of string * space
type bvalue = BStrVal of bstring | BInt of int * space
type bkey = string * space
type bkv = bkey * equal_space * bvalue
type bkvlist = (bkv * comma_space option) list
type bdata = bkey * comma_space * bkvlist
type entry_data = string * entry_opener * three_spaces (* Declaration, opener, spaces *)
type bentry = BEntry of entry_data * bdata
            | BStrEntry of entry_data * bkv
            | BPreEntry of entry_data * bstring
            | BComment of (string * space) option * string

let bvar_of (s, s') = BVar (s, s')

type case_key = [`Types | `Fields | `Variables | `Keys]
type case_value = [`Lowercase | `Uppercase | `Capitalize]
type spacing_braces_value = [`Egyptian | `Aligned | `Narrow]
type spacing_value = [`Newlines of int | `Spaces of int]
type spacing_left_value = [`Tabs | `Spaces of int]
type spacing_strategy = [ `Braces of spacing_braces_value
                        | `Keys of spacing_value
                        | `Commas of spacing_value
                        | `Eq of (spacing_value * spacing_value)
                        | `Left of spacing_left_value
                        | `After of spacing_value ]
type sort_key = [`Keys | `Fields]
type sort_value = [`Asc | `Desc]
type comments_remove_value = [`All | `Blanks | `Text | `Header]
type comments_header_value = [`First | `Smart | `None]
type comments_strategy = [ `Remove of comments_remove_value
                         | `Header of comments_header_value ]
type fields_strategy = [ `Remove of string list
                       | `Rename of (string * string) list ]
type policy = [ `Case of case_key list * case_value
              | `Spacing of spacing_strategy
              | `Sort of sort_key list * sort_value
              | `Comments of comments_strategy
              | `Fields of fields_strategy ]

type pre_entry = ECat of bentry * bentry | ESingle of bentry

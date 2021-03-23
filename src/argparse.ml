open Exceptions

let on s = Str.regexp ("[ \t\n]*" ^ s ^ "[ \t\n]*")

let colon_to_pair s = let l = String.split_on_char ':' s in (List.hd l, List.hd (List.tl l))

let is_int_str s = try (ignore (int_of_string s); true) with _ -> false

let make_strategy =
  match%cmdargs [%arg 1] [@kind "policy|policies"] with
  | "case"  [@desc "Case modifiers"]
            [@doc  "This policy changes the case of the Bibtex structures."]
            -> (match [%arg 2] [@kind "strategy|strategies"] with
                | "all"       [@desc "Match all structural text"]
                              [@doc "This strategy matches Types, Fields, Variables and Keys."]
                              -> [`Types; `Fields; `Variables; `Keys]
                | "types"     [@desc "Match only Types"]
                              [@doc "This strategy matches only Types, i.e. the text right after the `@' symbols starting an entry."]
                              -> [`Types]
                | "fields"    [@desc "Match only Fields"]
                              [@doc "This strategy matches only Fields, i.e. the text right before the `=' symbols in a regular entry."]
                              -> [`Fields]
                | "variables" [@desc "Match only Variables"]
                              [@doc "This strategy matches only Variables, i.e. the text right before the `=' symbols in a @String entry."]
                              -> [`Variables]
                | "keys"      [@desc "Match only Keys"]
                              [@doc "This strategy matches only Keys, i.e. the text right after the opening `{' or `(' of a regular entry."]
                              -> [`Keys]),
               (match [%arg 3] [@kind "value"] with
                | "lower"      [@desc "Make the text lowercase"]
                               -> `Lowercase
                | "capitalize" [@desc "Make the first letter of the text Uppercase"]
                               -> `Capitalize
                | "upper"      [@desc "Make the text UPPERCASE"]
                               -> `Uppercase)
  | "spacing" [@desc "Spacing modifiers"]
              [@doc "This policy changes the spacings around the Bibtex structures."]
              -> (match [%arg 2] [@kind "strategy|strategies"] with
                  | "braces" [@desc "Change brace spacings"]
                             -> (match [%arg 3] [@kind "value"] with
                                 | "egyptian" | "yes" | "true" [@desc "Convert to Egyptian braces"]
                                                               [@doc "This value puts braces in the following form:\n  @type {\n     ...\n  }"]
                                                               -> `Egyptian
                                 | "aligned"                   [@desc "Align braces to the left"]
                                                               [@doc "This value puts braces in the following form:\n  @type\n  {\n     ...\n  }"]
                                                               -> `Aligned
                                 | "narrow" | "no" | "false"   [@desc "Convert to narrow braces"]
                                                               [@doc "This value puts braces in the following form:\n  @type{...}"]
                                                               -> `Narrow)
                  | "keys"   [@desc "Change key spacings"]
                             -> (match [%arg 3] [@kind "value"] with
                                 | "newline" | "yes" | "true" [@desc "Put keys on a new line"]
                                                              [@doc "This value formats keys like so:\n  @type {\n     key,\n     ...\n  }"]
                                                              -> `Newlines 1
                                 | "no" | "false"             [@desc "Keep keys on the same line as the opening braces"]
                                                              [@doc "This value formats keys like so:\n  @type {key,\n     ...\n  }"]
                                                              -> `Spaces 0
                                 | "space" | "spaces"         [@desc "Put a space between opening braces and keys"]
                                                              [@doc "This value formats keys like so:\n  @type { key,\n     ...\n  }"]
                                                              -> `Spaces 1
                                 | s when is_int_str s        [@desc "Put <else> spaces between opening braces and keys"]
                                                              [@doc "This value formats keys like so:\n  @type {<else>key,\n     ...\n  }"]
                                                              -> `Spaces (int_of_string s))
                  | "commas" [@desc "Change comma spacings"]
                             -> (match [%arg 3] [@kind "value"] with
                                 | "newline" | "yes" | "true" [@desc "Insert a line break after commas"]
                                                              [@doc "This value formats commas like so:\n  @type {...,\n     ...\n  }"]
                                                              -> `Newlines 1
                                 | "narrow" | "no" | "false"  [@desc "Remove all the spaces after commas"]
                                                              [@doc "This value formats commas like so:\n  @type {...,...\n  }"]
                                                              -> `Spaces 0
                                 | "space" | "spaces"         [@desc "Insert a space after commas"]
                                                              [@doc "This value formats commas like so:\n  @type {..., ...\n  }"]
                                                              -> `Spaces 1
                                 | s when is_int_str s        [@desc "Insert <else> spaces after commas"]
                                                              [@doc "This value formats commas like so:\n  @type {...,<else>...\n  }"]
                                                              -> `Spaces (int_of_string s))
                  | "left"   [@desc "Change spacings to the left"]
                             -> (match [%arg 3] [@kind "value"] with
                                 | "tabs"                    [@desc "Insert a tab at the beginning of each line in an entry"]
                                                             [@doc "This value formats entries like so:\n  @type {...,\n  [tab] ...\n  }"]
                                                             -> `Tabs
                                 | "no" | "false"            [@desc "Remove all the spaces at the beginning of lines inside entries"]
                                                             [@doc "This value formats entries like so:\n  @type {...,\n  ...\n  }"]
                                                             -> `Spaces 0
                                 | "spaces" | "yes" | "true" [@desc "Insert four spaces at the beginning of each line in an entry"]
                                                             [@doc "This value formats entries like so:\n  @type {...,\n      ...\n  }"]
                                                             -> `Spaces 4
                                 | "space"                   [@desc "Insert a space at the beginning of each line in an entry"]
                                                             [@doc "This value formats entries like so:\n  @type {...,\n   ...\n  }"]
                                                             -> `Spaces 1
                                 | s when is_int_str s       [@desc "Insert <else> spaces at the beginning of each line in an entry"]
                                                             [@doc "This value formats entries like so:\n  @type {...,\n  <else>...\n  }"]
                                                             -> `Spaces (int_of_string s))
                  | "eq"     [@desc "Change spacings around equal signs"]
                             -> (match [%arg 3] [@kind "value"] with
                                 | "narrow" | "no" [@desc "Remove all the spaces around equal signs"]
                                                   [@doc "This value formats entries like so:\n  @type {...,\n     ...=...,\n     ...\n  }"]
                                                   -> (`Spaces 0, `Spaces 0)
                                 | "space" | "spaces" | "yes" | "true"
                                                   [@desc "Insert a space before and after equal signs"]
                                                   [@doc "This value formats entries like so:\n  @type {...,\n     ... = ...,\n     ...\n  }"]
                                                   -> (`Spaces 1, `Spaces 1)
                                 | s               [@desc "<else> = <4> | <4>:<5>\nInsert <4> spaces before and after equal signs or <4> spaces before and <5> spaces after equal signs"]
                                                   [@doc "This value formats entries like so:\n  @type {...,\n     ...<4>=<5>...,\n     ...\n  }"]
                                                   -> (try (match [%arg 4] with
                                                            | None    -> let i = int_of_string s in (`Spaces i, `Spaces i)
                                                            | Some s' -> (`Spaces (int_of_string s), `Spaces (int_of_string s')))
                                                       with Failure _ -> unknown_value_for [%arg 2] [%arg 3] [%arg 1]))
                  | "after" [@desc "Change spacing after entries"]
                             -> (match [%arg 3] [@kind "value"] with
                                 | "break" | "linebreak"      [@desc "Insert a line break after entries"]
                                                              [@doc "This value formats entries like so:\n  @type {...}\n  @type2 {...}"]
                                                              -> `Newlines 1
                                 | "newline" | "yes" | "true" [@desc "Insert a line break and a new line after entries"]
                                                              [@doc "This value formats entries like so:\n  @type {...}\n\n  @type2 {...}"]
                                                              -> `Newlines 2
                                 | "no" | "false"             [@desc "Remove all the spaces after entries"]
                                                              [@doc "This value formats entries like so:\n  @type {...}@type2 {...}"]
                                                              -> `Spaces 0))
  | "sort" [@desc "Sort policy"]
           [@doc "This policy sorts sortable Bibtex structures."]
           -> (match [%arg 2] [@kind "strategy|strategies"] with
               | "asc"          [@desc "Sort increasingly"]
                                [@doc "The sort is made case-insensitively."]
                                -> `Asc
               | "desc" | "dsc" [@desc "Sort decreasingly"]
                                [@doc "The sort is made case-insensitively."]
                                -> `Desc),
              (match [%arg 3] [@kind "value"] with
               | "all"     [@desc "Match all keyed structures"]
                           [@doc "This strategy matches Entries and Fields."]
                           -> [`Keys; `Fields]
               | "keys"    [@desc "Match only Keys"]
                           [@doc "This strategy matches only Keys, i.e. the text right after the opening `{' or `(' of a regular entry."]
                           -> [`Keys]
               | "fields"  [@desc "Match only Fields"]
                           [@doc "This strategy matches only Fields, i.e. the text right before the `=' symbols in a regular entry."]
                           -> [`Fields])
  | "comments" [@desc "Comment manipulation"]
               [@doc "This policy applies transformations to comments."]
               -> (match [%arg 2] [@kind "strategy|strategies"] with
                   | "remove" [@desc "Comment removal strategy"]
                              -> (match [%arg 3] [@kind "value"] with
                                  | "all"              [@desc "Remove all comments"]
                                                       -> `All
                                  | "blanks"           [@desc "Remove only whitespace comments"]
                                                       -> `Blanks
                                  | "text"             [@desc "Remove only non-whitespace comments"]
                                                       -> `Text
                                  | "first" | "header" [@desc "Remove the first comment"]
                                                       -> `Header)
                   | "header" [@desc "Header definition strategy"]
                              [@doc "This strategy transforms the first entry to build a custom header comment."]
                              -> (match [%arg 3] [@kind "value"] with
                                  | "first" | "yes" | "true" [@desc "Make the first comment the header"]
                                                             -> `First
                                  | "auto" | "smart"         [@desc "Smartly deduce the header content"]
                                                             [@doc "This value tries to find a line break in the first comment splitting two meaningful pieces of text. The last meaningful line break is used as a pivot: what's before is the header and what's after becomes a new comment."]
                                                             -> `Smart
                                  | "none" | "no" | "false"  [@desc "Do not create a header"]
                                                             -> `None))
  | "fields" [@desc "Field manipulation"]
             [@doc "This policy applies transformations to Fields, i.e. to the text right before the `=' symbols in a regular entry."]
             -> (match [%arg 2] with
                 | "remove" [@desc "Requires <3>\nRemoves the Fields corresponding to <3>"]
                            [@doc "The match is made case-insensitively"]
                            -> `Remove (Option.get [%arg 3])
                 | "rename" [@desc "Requires <3>:<4>\nRenames the Fields corresponding to <3> into <4>"]
                            [@doc "The match is made case-insensitively"]
                            -> `Rename (Option.get [%arg 3], Option.get [%arg 4]))

let%parse parse = make_strategy "--<1> {<2>={<3>:<4>}+<...>},<...>"

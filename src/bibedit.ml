let () =
  let (files, policies) = Argparse.parse Sys.argv in
  if List.length files != 1 then failwith "bibedit requires exactly one input file";
  let bib = Bib.from_channel (open_in (List.hd files)) in
  print_endline (Bib.to_string (Bib.apply_policies policies bib))

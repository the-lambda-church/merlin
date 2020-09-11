let (^/) = Filename.concat

let gen_rule dir file =
  let path = dir ^/ file in
  let enabled_if =
    let ic = open_in path in
    let line =
      try input_line ic
      with End_of_file -> ""
    in
    if Std.String.is_prefixed line ~by:"(enabled_if" then
      line
    else
      ""
  in
  let alias_name =
    match Std.String.chop_prefix ~prefix:"./test-dirs/" path with
    | None -> assert false
    | Some path ->
      Std.String.replace_all ~pattern:"/" ~with_:"-" path
      |> Filename.chop_extension
  in
  Printf.printf {|
(alias
 (name %s)%s
 (package merlin)
 (deps (:t %s)
       (source_tree %s)
       %%{bin:ocamlmerlin}
       %%{bin:ocamlmerlin-server}
       %%{bin:dot-merlin-reader})
 (action
   (chdir %s
     (setenv MERLIN %%{exe:merlin-wrapper}
     (setenv OCAMLC %%{ocamlc}
       (progn
         (run %%{bin:mdx} test --syntax=cram %%{t})
         (diff? %%{t} %%{t}.corrected)))))))
(alias (name runtest) (deps (alias %s)))
|} alias_name enabled_if path dir dir alias_name

let rec scan_fs dir =
  let content =
    try Sys.readdir dir
    with Sys_error _ -> [||]
  in
  Array.sort String.compare content;
  Array.iter (fun file ->
    let path = dir ^/ file in
    if Sys.is_directory path then
      scan_fs path
    else if Filename.check_suffix file ".t" then
      gen_rule dir file
  ) content

let () =
  scan_fs ".";
  flush_all ();
  exit 0

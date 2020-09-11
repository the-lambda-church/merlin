let ocaml_version_val =
  match
    Scanf.sscanf Sys.argv.(1) "%s@.%s@.%d" (fun maj min p -> maj, min, p)
  with
  | "4", "02", _ ->
      "`OCaml_4_02_3"
  | "4", "07", p ->
      Printf.sprintf "`OCaml_4_07_%d" p
  | maj, min, _ ->
      Printf.sprintf "`OCaml_%s_%s_0" maj min

let () =
  Printf.printf {|
let version = "%%VERSION%%"
let ocamlversion :
  [ `OCaml_4_02_0 | `OCaml_4_02_1 | `OCaml_4_02_2 | `OCaml_4_02_3
  | `OCaml_4_03_0 | `OCaml_4_04_0 | `OCaml_4_05_0 | `OCaml_4_06_0
  | `OCaml_4_07_0 | `OCaml_4_07_1 | `OCaml_4_08_0 | `OCaml_4_09_0
  | `OCaml_4_10_0 | `OCaml_4_11_0 ] = %s
|} ocaml_version_val

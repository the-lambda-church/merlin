let ccomp_type   = Sys.argv.(1)
let pre_flags_f  = Sys.argv.(2)
let post_flags_f = Sys.argv.(3)

let pre_flags, post_flags =
  if Str.string_match (Str.regexp "msvc") ccomp_type 0 then
    "/Fe", "advapi32.lib"
  else
    "-o", ""

let write_lines f s =
  let oc = open_out f in
  output_string oc s;
  close_out oc

let () =
  write_lines pre_flags_f pre_flags;
  write_lines post_flags_f post_flags

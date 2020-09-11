open Std

module Cmi = struct
  type error =
    | Not_an_interface of string
    | Wrong_version_interface of string * string
    | Corrupted_interface of string

  exception Error of error

  let to_version_opt = function
    | "Caml1999I017" -> Some "4.02"
    | "Caml1999I020" -> Some "4.03"
    | "Caml1999I021" -> Some "4.04 or 4.05"
    | "Caml1999I022" -> Some "4.06"
    | "Caml1999I023" -> Some "4.07.0"
    | "Caml1999I024" -> Some "4.07.1"
    | "Caml1999I025" -> Some "4.08"
    | "Caml1999I026" -> Some "4.09"
    | "Caml1999I027" -> Some "4.10"
    | "Caml1999I028" -> Some "4.11"
    | _ -> None

  open Format

  let report_error ppf = function
    | Not_an_interface filename ->
        fprintf ppf "%a@ is not a compiled interface"
          Location.print_filename filename
    | Wrong_version_interface (filename, compiler_magic) ->
      begin match to_version_opt compiler_magic with
      | None ->
        fprintf ppf
          "%a@ seems to be compiled with a version of OCaml that is not@.\
           supported by Merlin."
          Location.print_filename filename
      | Some version ->
        fprintf ppf
          "%a@ seems to be compiled with OCaml %s.@.\
           But this instance of Merlin handles OCaml %s."
          Location.print_filename filename version
          (Option.get @@ to_version_opt Config.cmi_magic_number)
      end
    | Corrupted_interface filename ->
        fprintf ppf "Corrupted compiled interface@ %a"
          Location.print_filename filename

  let () =
    Location.register_error_of_exn
      (function
        | Error err -> Some (Location.error_of_printer_file report_error err)
        | _ -> None
      )
end

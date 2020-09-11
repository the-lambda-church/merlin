open MenhirSdk
open Printf

module G = Cmly_read.Read(struct let filename = Sys.argv.(1) end)

open G

let print_header () =
  let name = Filename.chop_extension (Filename.basename Sys.argv.(1)) in
  printf "open %s\n" (String.capitalize name)

let attributes_at st =
  List.fold_left
    (fun attrs (prod, pos) ->
       if pos > 0 then
         let _, _, attrs' = (G.Production.rhs prod).(pos - 1) in
         attrs' @ attrs
       else
         attrs)
    [] (Lr0.items (Lr1.lr0 st))

let print_named_items () =
  let print_item st =
    match List.filter (Attribute.has_label "item") (attributes_at st) with
    | [] -> ()
    | (x :: _) as xs ->
      let xs = List.map Attribute.payload xs |> List.sort_uniq compare in
      if List.length xs > 1 then
        eprintf "Warning: state %d has multiple items, %s.\n"
          (Lr1.to_int st) (String.concat " " xs);
      printf "  | %d -> %s\n"
        (Lr1.to_int st) (Attribute.payload x)
  in
  printf "let named_item_at = function\n";
  Lr1.iter print_item;
  printf "  | _ -> raise Not_found\n\n"

let print_nullable () =
  let print_n n =
    if Nonterminal.nullable n then
      printf "  | N_%s -> true\n" (Nonterminal.mangled_name n)
  in
  printf "let nullable (type a) : a MenhirInterpreter.nonterminal -> bool =\n\
         \  let open MenhirInterpreter in function\n";
  Nonterminal.iter print_n;
  printf "  | _ -> false\n"

let () =
  print_header ();
  print_named_items ();
  print_nullable ()

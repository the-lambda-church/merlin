(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module SMap = Misc.String.Map

(* Mapping from basenames to full filenames *)
type registry = string SMap.t ref

open Local_store.Compiler

let files : registry = srefk SMap.empty
let files_uncap : registry = srefk SMap.empty

module Dir = struct
  type t = {
    path : string;
    files : string list;
  }

  let path t = t.path
  let files t = t.files

  let create path =
    { path; files = Array.to_list (Directory_content_cache.read path) }
end

let dirs = srefk []

let reset () =
  assert (Local_store.is_bound compiler_state);
  files := SMap.empty;
  files_uncap := SMap.empty;
  dirs := []

let get () = !dirs
let get_paths () = List.map Dir.path !dirs

let add dir =
  assert (Local_store.is_bound compiler_state);
  let add_file base =
    let fn = Filename.concat dir.Dir.path base in
    files := SMap.add base fn !files;
    files_uncap := SMap.add (String.uncapitalize_ascii base) fn !files_uncap;
  in
  List.iter add_file dir.Dir.files;
  dirs := dir :: !dirs

let remove_dir dir =
  assert (Local_store.is_bound compiler_state);
  let new_dirs = List.filter (fun d -> Dir.path d <> dir) !dirs in
  if new_dirs <> !dirs then begin
    reset ();
    List.iter add (List.rev new_dirs)
  end

let add_dir dir = add (Dir.create dir)

let init l =
  reset ();
  List.iter add_dir (List.rev l)

let is_basename fn = Filename.basename fn = fn

let find fn =
  assert (Local_store.is_bound compiler_state);
  if is_basename fn then
    SMap.find fn !files
  else
    Misc.find_in_path (get_paths ()) fn

let find_uncap fn =
  assert (Local_store.is_bound compiler_state);
  if is_basename fn then
    SMap.find (String.uncapitalize_ascii fn) !files_uncap
  else
    Misc.find_in_path_uncap (get_paths ()) fn

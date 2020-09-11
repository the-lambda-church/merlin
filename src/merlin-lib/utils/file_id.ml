type t = Unix.stats

let null_stat =
  { Unix.
    st_dev = -1; st_ino = -1; st_kind = Unix.S_REG; st_nlink = -1;
    st_perm = -1; st_uid = -1; st_gid = -1; st_rdev = -1; st_size = -1;
    st_atime = nan; st_mtime = nan; st_ctime = nan }

let get filename =
  try Unix.stat filename
  with _ -> null_stat

let check a b =
  a == b || (
    (a != null_stat) && (b != null_stat) &&
    let open Unix in
    a.st_mtime = b.st_mtime &&
    a.st_size = b.st_size &&
    a.st_ino = b.st_ino &&
    a.st_dev = b.st_dev
  )

let cache = ref None

let with_cache k =
  Std.let_ref cache (Some (Hashtbl.create 7)) k

let get filename =
  match !cache with
  | None -> get filename
  | Some table ->
    match Hashtbl.find table filename with
    | stats ->
      Logger.log ~section:"stat_cache" ~title:"reuse cache" "%s" filename;
      stats
    | exception Not_found ->
      let stats = get filename in
      Hashtbl.add table filename stats;
      stats

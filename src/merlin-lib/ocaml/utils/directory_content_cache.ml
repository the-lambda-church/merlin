include File_cache.Make (struct
    let cache_name = "Directory_content_cache"
    type t = string array

    (* For backward compatibility reason, simulate the behavior of
       [Misc.find_in_path]: silently ignore directories that don't exist
       + treat [""] as the current directory. *)
    let read dir =
      try
        Sys.readdir (if dir = "" then Filename.current_dir_name else dir)
      with Sys_error _ ->
        [||]
  end)


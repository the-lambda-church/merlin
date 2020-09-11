open Std

let {Logger. log} = Logger.for_section "Pipeline"

let time_shift = ref 0.0

let timed_lazy r x =
  lazy (
    let start = Misc.time_spent () in
    let time_shift0 = !time_shift in
    let update () =
      let delta = Misc.time_spent () -. start in
      let shift = !time_shift -. time_shift0 in
      time_shift := time_shift0 +. delta;
      r := !r +. delta -. shift;
    in
    match Lazy.force x with
    | x -> update (); x
    | exception exn -> update (); Std.reraise exn
  )

module Cache = struct
  let cache = ref []

  (* Values from configuration that are used as a key for the cache.
     These values should:
     - allow to maximize reuse; associating a single typechecker instance to a
       filename and directory is natural, but keying also based on verbosity
       makes no sense
     - prevent reuse in different environments (if there is a change in
       loadpath, a new typechecker should be produced).

     It would be better to guarantee that the typechecker was well-behaved
     when the loadpath changes (so that we can reusing the same instance, and
     let the typechecker figure which part of its internal state should be
     invalidated).
     However we already had many bug related to that.  There are subtle changes
     in the type checker behavior accross the different versions of OCaml.
     It is simpler to create new instances upfront.
  *)

  let key config =
    Mconfig.(
      config.query.filename,
      config.query.directory,
      config.ocaml,
      {config.merlin with log_file = None; log_sections = []}
    )

  let get config =
    let title = "pop_cache" in
    let key = key config in
    match List.assoc key !cache with
    | state ->
      cache := (key, state) :: List.remove_assoc key !cache;
      log ~title "found entry for this configuration";
      state
    | exception Not_found ->
      log ~title "nothing cached for this configuration";
      let state = Mocaml.new_state () in
      cache := (key, state) :: List.take_n 5 !cache;
      state
end

module Typer = struct
  type t = {
    errors : exn list lazy_t;
    result : Mtyper.result;
  }
end

module Ppx = struct
  type t = {
    config : Mconfig.t;
    errors : exn list;
    parsetree : Mreader.parsetree;
  }
end

type t = {
  config : Mconfig.t;
  state  : Mocaml.typer_state;
  raw_source : Msource.t;
  source : Msource.t lazy_t;
  reader : (Mreader.result * Mconfig.t) lazy_t;
  ppx    : Ppx.t lazy_t;
  typer  : Typer.t lazy_t;

  pp_time     : float ref;
  reader_time : float ref;
  ppx_time    : float ref;
  typer_time  : float ref;
  error_time  : float ref;
}

let raw_source t = t.raw_source

let input_config t = t.config
let input_source t = Lazy.force t.source

let with_pipeline t f =
  Mocaml.with_state t.state @@ fun () ->
  Mreader.with_ambient_reader t.config (input_source t) f

let get_lexing_pos t pos =
  Msource.get_lexing_pos
    (input_source t) ~filename:(Mconfig.filename t.config) pos

let reader t = Lazy.force t.reader

let ppx    t = Lazy.force t.ppx
let typer  t = Lazy.force t.typer

let reader_config    t = (snd (reader t))
let reader_parsetree t = (fst (reader t)).Mreader.parsetree
let reader_comments  t = (fst (reader t)).Mreader.comments
let reader_lexer_errors  t = (fst (reader t)).Mreader.lexer_errors
let reader_parser_errors t = (fst (reader t)).Mreader.parser_errors
let reader_no_labels_for_completion t =
  (fst (reader t)).Mreader.no_labels_for_completion

let ppx_parsetree t = (ppx t).Ppx.parsetree
let ppx_errors    t = (ppx t).Ppx.errors

let final_config  t = (ppx t).Ppx.config

let typer_result t = (typer t).Typer.result
let typer_errors t = Lazy.force (typer t).Typer.errors

let process
    ?state
    ?(pp_time=ref 0.0)
    ?(reader_time=ref 0.0)
    ?(ppx_time=ref 0.0)
    ?(typer_time=ref 0.0)
    ?(error_time=ref 0.0)
    ?for_completion
    config raw_source =
  let state = match state with
    | None -> Cache.get config
    | Some state -> state
  in
  let source = timed_lazy pp_time (lazy (
      match Mconfig.(config.ocaml.pp) with
      | None -> raw_source
      | Some { workdir; workval } ->
        let source = Msource.text raw_source in
        let source =
          Pparse.apply_pp
            ~workdir ~filename:Mconfig.(config.query.filename)
            ~source ~pp:workval
        in
        Msource.make source
    )) in
  let reader = timed_lazy reader_time (lazy (
      let lazy source = source in
      let result = Mreader.parse ?for_completion config source in
      let config = result.Mreader.config in
      let config = Mreader.apply_directives config result.Mreader.parsetree in
      let config = Mconfig.normalize config in
      result, config
    )) in
  let ppx = timed_lazy ppx_time (lazy (
      let lazy ({Mreader.parsetree; _}, config) = reader in
      let caught = ref [] in
      Msupport.catch_errors Mconfig.(config.ocaml.warnings) caught @@ fun () ->
      let config, parsetree = Mppx.rewrite config parsetree in
      { Ppx. config; parsetree; errors = !caught }
    )) in
  let typer = timed_lazy typer_time (lazy (
      let lazy { Ppx. config; parsetree; _ } = ppx in
      let result = Mtyper.run config parsetree in
      let errors = timed_lazy error_time (lazy (Mtyper.get_errors result)) in
      { Typer. errors; result }
    )) in
  { config; state; raw_source; source; reader; ppx; typer;
    pp_time; reader_time; ppx_time; typer_time; error_time }

let make config source =
  process (Mconfig.normalize config) source

let for_completion position
    {config; state; raw_source;
     pp_time; reader_time; ppx_time; typer_time; error_time; _} =
  process config raw_source ~for_completion:position
    ~state ~pp_time ~reader_time ~ppx_time ~typer_time ~error_time

let timing_information t = [
  "pp"     , !(t.pp_time);
  "reader" , !(t.reader_time);
  "ppx"    , !(t.ppx_time);
  "typer"  , !(t.typer_time);
  "error"  , !(t.error_time);
]

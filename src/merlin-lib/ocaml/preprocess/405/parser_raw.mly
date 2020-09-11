/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* The parser definition */

%{
[@@@ocaml.warning "-9"]

open Location
open Asttypes
open Longident
open Parsetree
open Ast_helper
open Docstrings
open Docstrings.WithMenhir
open Msupport_parsing

let mkloc = Location.mkloc
let mknoloc = Location.mknoloc

let mktyp ~loc d = Typ.mk ~loc d
let mkpat ~loc d = Pat.mk ~loc d
let mkexp ~loc d = Exp.mk ~loc d
let mkmty ?attrs ~loc d = Mty.mk ?attrs ~loc d
let mksig ~loc d = Sig.mk ~loc d
let mkmod ?attrs ~loc d = Mod.mk ?attrs ~loc d
let mkstr ~loc d = Str.mk ~loc d
let mkclass ?attrs ~loc d = Cl.mk ?attrs ~loc d
let mkcty ?attrs ~loc d = Cty.mk ~loc ?attrs d
let mkctf ~loc ?attrs ?docs d =
  Ctf.mk ~loc ?attrs ?docs d
let mkcf ~loc ?attrs ?docs d =
  Cf.mk ~loc ?attrs ?docs d

(* for now silently turn positions into locations *)
let rhs_loc pos = pos

let mkrhs rhs pos = mkloc rhs (rhs_loc pos)

let reloc_pat ~loc x = { x with ppat_loc = loc };;
let reloc_exp ~loc x = { x with pexp_loc = loc };;

let mkoperator name pos =
  let loc = rhs_loc pos in
  Exp.mk ~loc (Pexp_ident(mkloc (Lident name) loc))

let mkpatvar name pos =
  Pat.mk ~loc:(rhs_loc pos) (Ppat_var (mkrhs name pos))

(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitly in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -annot option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.
*)
let ghexp ~loc d = Exp.mk ~loc:{ loc with Location.loc_ghost = true } d
let ghpat ~loc d = Pat.mk ~loc:{ loc with Location.loc_ghost = true } d
let ghtyp ~loc d = Typ.mk ~loc:{ loc with Location.loc_ghost = true } d
let ghloc ~loc d = { txt = d; loc = { loc with Location.loc_ghost = true } }
let ghstr ~loc d = Str.mk ~loc:{ loc with Location.loc_ghost = true } d
let ghsig ~loc d = Sig.mk ~loc:{ loc with Location.loc_ghost = true } d

let mkinfix ~loc ~oploc arg1 name arg2 =
  mkexp ~loc (Pexp_apply(mkoperator name oploc, [Nolabel, arg1; Nolabel, arg2]))

let neg_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus ~loc ~oploc name arg =
  let mkexp = mkexp ~loc in
  match name, arg.pexp_desc with
  | "-", Pexp_constant(Pconst_integer (n,m)) ->
      mkexp(Pexp_constant(Pconst_integer(neg_string n,m)))
  | ("-" | "-."), Pexp_constant(Pconst_float (f, m)) ->
      mkexp(Pexp_constant(Pconst_float(neg_string f, m)))
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) oploc, [Nolabel, arg]))

let mkuplus ~loc ~oploc name arg =
  let mkexp = mkexp ~loc in
  let desc = arg.pexp_desc in
  match name, desc with
  | "+", Pexp_constant(Pconst_integer _)
  | ("+" | "+."), Pexp_constant(Pconst_float _) -> mkexp desc
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) oploc, [Nolabel, arg]))

let mkexp_cons consloc args loc =
  Exp.mk ~loc (Pexp_construct(mkloc (Lident "::") consloc, Some args))

let mkpat_cons consloc args loc =
  Pat.mk ~loc (Ppat_construct(mkloc (Lident "::") consloc, Some args))

let rec mktailexp nilloc = function
    [] ->
      let loc = { nilloc with loc_ghost = true } in
      let nil = { txt = Lident "[]"; loc = loc } in
      Exp.mk ~loc (Pexp_construct (nil, None))
  | e1 :: el ->
      let exp_el = mktailexp nilloc el in
      let loc = {loc_start = e1.pexp_loc.loc_start;
               loc_end = exp_el.pexp_loc.loc_end;
               loc_ghost = true}
      in
      let arg = Exp.mk ~loc (Pexp_tuple [e1; exp_el]) in
      mkexp_cons {loc with loc_ghost = true} arg loc

let rec mktailpat nilloc = function
    [] ->
      let loc = { nilloc with loc_ghost = true } in
      let nil = { txt = Lident "[]"; loc = loc } in
      Pat.mk ~loc (Ppat_construct (nil, None))
  | p1 :: pl ->
      let pat_pl = mktailpat nilloc pl in
      let loc = {loc_start = p1.ppat_loc.loc_start;
               loc_end = pat_pl.ppat_loc.loc_end;
               loc_ghost = true}
      in
      let arg = Pat.mk ~loc (Ppat_tuple [p1; pat_pl]) in
      mkpat_cons {loc with loc_ghost = true} arg loc

let mkstrexp e attrs =
  { pstr_desc = Pstr_eval (e, attrs); pstr_loc = e.pexp_loc }

let mkexp_constraint ~loc e (t1, t2) =
  let ghexp = ghexp ~loc in
  match t1, t2 with
  | Some t, None -> ghexp(Pexp_constraint(e, t))
  | _, Some t -> ghexp(Pexp_coerce(e, t1, t))
  | None, None -> assert false

let mkexp_opt_constraint ~loc e = function
  | None -> e
  | Some constraint_ -> mkexp_constraint ~loc e constraint_

let mkpat_opt_constraint ~loc p = function
  | None -> p
  | Some typ -> mkpat ~loc (Ppat_constraint(p, typ))

let array_function ~loc str name =
  ghloc ~loc (Ldot(Lident str, (if !Clflags.fast then "unsafe_" ^ name else name)))

let syntax_error loc =
  raise_error (Syntaxerr.Error(Syntaxerr.Other loc))

let not_expecting pos nonterm =
    raise_error Syntaxerr.(Error(Not_expecting(rhs_loc pos, nonterm)))

let bigarray_function ~loc str name =
  ghloc ~loc (Ldot(Ldot(Lident "Bigarray", str), name))

let bigarray_untuplify = function
    { pexp_desc = Pexp_tuple explist; pexp_loc = _ } -> explist
  | exp -> [exp]

let bigarray_get ~loc arr arg =
  let mkexp, ghexp = mkexp ~loc, ghexp ~loc in
  let bigarray_function = bigarray_function ~loc in
  let get = if !Clflags.fast then "unsafe_get" else "get" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" get)),
                       [Nolabel, arr; Nolabel, c1]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" get)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" get)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2; Nolabel, c3]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "get")),
                       [Nolabel, arr; Nolabel, ghexp(Pexp_array coords)]))

let bigarray_set ~loc arr arg newval =
  let mkexp, ghexp = mkexp ~loc, ghexp ~loc in
  let bigarray_function = bigarray_function ~loc in
  let set = if !Clflags.fast then "unsafe_set" else "set" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" set)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, newval]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" set)),
                       [Nolabel, arr; Nolabel, c1;
                        Nolabel, c2; Nolabel, newval]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" set)),
                       [Nolabel, arr; Nolabel, c1;
                        Nolabel, c2; Nolabel, c3; Nolabel, newval]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "set")),
                       [Nolabel, arr;
                        Nolabel, ghexp(Pexp_array coords);
                        Nolabel, newval]))

let lapply p1 p2 =
  if not !Clflags.applicative_functors then
    raise_error (Syntaxerr.Error(Syntaxerr.Applicative_path (symbol_rloc())));
  Lapply(p1, p2)

let exp_of_label ~loc lbl pos =
  mkexp ~loc (Pexp_ident(mkrhs (Lident(Longident.last lbl)) pos))

let pat_of_label ~loc lbl pos =
  mkpat ~loc (Ppat_var (mkrhs (Longident.last lbl) pos))

let mk_newtypes ~loc newtypes exp =
  let mkexp = mkexp ~loc in
  List.fold_right (fun newtype exp -> mkexp (Pexp_newtype (newtype, exp)))
    newtypes exp

let wrap_type_annotation ~loc newtypes core_type body =
  let mkexp, ghtyp = mkexp ~loc, ghtyp ~loc in
  let mk_newtypes = mk_newtypes ~loc in
  let exp = mkexp(Pexp_constraint(body,core_type)) in
  let exp = mk_newtypes newtypes exp in
  (exp, ghtyp(Ptyp_poly(newtypes,Typ.varify_constructors newtypes core_type)))

let wrap_exp_attrs ~loc body (ext, attrs) =
  let ghexp = ghexp ~loc in
  (* todo: keep exact location for the entire attribute *)
  let body = {body with pexp_attributes = attrs @ body.pexp_attributes} in
  match ext with
  | None -> body
  | Some id -> ghexp(Pexp_extension (id, PStr [mkstrexp body []]))

let mkexp_attrs ~loc d attrs =
  wrap_exp_attrs ~loc (mkexp ~loc d) attrs

let wrap_typ_attrs ~loc typ (ext, attrs) =
  let ghtyp = ghtyp ~loc in
  (* todo: keep exact location for the entire attribute *)
  let typ = {typ with ptyp_attributes = attrs @ typ.ptyp_attributes} in
  match ext with
  | None -> typ
  | Some id -> ghtyp(Ptyp_extension (id, PTyp typ))

let mktyp_attrs ~loc d attrs =
  wrap_typ_attrs ~loc (mktyp ~loc d) attrs

let wrap_pat_attrs ~loc pat (ext, attrs) =
  let ghpat = ghpat ~loc in
  (* todo: keep exact location for the entire attribute *)
  let pat = {pat with ppat_attributes = attrs @ pat.ppat_attributes} in
  match ext with
  | None -> pat
  | Some id -> ghpat (Ppat_extension (id, PPat (pat, None)))

let mkpat_attrs ~loc d attrs =
  wrap_pat_attrs ~loc (mkpat ~loc d) attrs

let wrap_class_attrs body attrs =
  {body with pcl_attributes = attrs @ body.pcl_attributes}
let wrap_mod_attrs body attrs =
  {body with pmod_attributes = attrs @ body.pmod_attributes}
let wrap_mty_attrs body attrs =
  {body with pmty_attributes = attrs @ body.pmty_attributes}

let wrap_str_ext ~loc body ext =
  match ext with
  | None -> body
  | Some id -> ghstr ~loc (Pstr_extension ((id, PStr [body]), []))

let mkstr_ext ~loc d ext =
  wrap_str_ext ~loc (mkstr ~loc d) ext

let wrap_sig_ext ~loc body ext =
  match ext with
  | None -> body
  | Some id -> ghsig ~loc (Psig_extension ((id, PSig [body]), []))

let mksig_ext ~loc d ext =
  wrap_sig_ext ~loc (mksig ~loc d) ext

let text_str pos = Str.text (rhs_text pos)
let text_sig pos = Sig.text (rhs_text pos)
let text_cstr pos = Cf.text (rhs_text pos)
let text_csig pos = Ctf.text (rhs_text pos)

let extra_text startpos endpos text items =
  let pre_extras = rhs_pre_extra_text startpos in
  let post_extras = rhs_post_extra_text endpos in
    text pre_extras @ items @ text post_extras

let extra_str p1 p2 items = extra_text p1 p2 Str.text items
let extra_sig p1 p2 items = extra_text p1 p2 Sig.text items
let extra_cstr p1 p2 items = extra_text p1 p2 Cf.text items
let extra_csig p1 p2 items = extra_text p1 p2 Ctf.text items

let extra_rhs_core_type ct pos =
  let docs = rhs_info pos in
  { ct with ptyp_attributes = add_info_attrs docs ct.ptyp_attributes }

let mklb first ~loc (p, e) attrs =
  { lb_pattern = p;
    lb_expression = e;
    lb_attributes = attrs;
    lb_docs = symbol_docs_lazy loc.loc_start loc.loc_end;
    lb_text = if first then empty_text_lazy
              else symbol_text_lazy loc.loc_start;
    lb_loc = loc; }

let mklbs ~loc ext rf lb =
  { lbs_bindings = [lb];
    lbs_rec = rf;
    lbs_extension = ext ;
    lbs_loc = loc; }

let addlb lbs lb =
  { lbs with lbs_bindings = lb :: lbs.lbs_bindings }

let val_of_let_bindings ~loc lbs =
  let bindings =
    List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           ~docs:(Lazy.force lb.lb_docs)
           ~text:(Lazy.force lb.lb_text)
           lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
  let str = mkstr ~loc (Pstr_value(lbs.lbs_rec, List.rev bindings)) in
  match lbs.lbs_extension with
  | None -> str
  | Some id -> ghstr ~loc (Pstr_extension((id, PStr [str]), []))

let expr_of_let_bindings ~loc lbs body =
  let bindings =
    List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
    mkexp_attrs ~loc (Pexp_let(lbs.lbs_rec, List.rev bindings, body))
      (lbs.lbs_extension, [])

let class_of_let_bindings ~loc lbs body =
  let bindings =
    List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
    if lbs.lbs_extension <> None then
      raise_error Syntaxerr.(Error(Not_expecting(lbs.lbs_loc, "extension")));
    mkclass ~loc (Pcl_let (lbs.lbs_rec, List.rev bindings, body))


(* Alternatively, we could keep the generic module type in the Parsetree
   and extract the package type during type-checking. In that case,
   the assertions below should be turned into explicit checks. *)
let package_type_of_module_type pmty =
  let err loc s =
    raise_error (Syntaxerr.Error (Syntaxerr.Invalid_package_type (loc, s)))
  in
  let rec map_cstr = function
    | [] -> []
    | Pwith_type (lid, ptyp) :: xs ->
        let loc = ptyp.ptype_loc in
        if ptyp.ptype_params <> [] then
          err loc "parametrized types are not supported";
        if ptyp.ptype_cstrs <> [] then
          err loc "constrained types are not supported";
        if ptyp.ptype_private <> Public then
          err loc "private types are not supported";

        (* restrictions below are checked by the 'with_constraint' rule *)
        assert (ptyp.ptype_kind = Ptype_abstract);
        assert (ptyp.ptype_attributes = []);
        let ty =
          match ptyp.ptype_manifest with
          | Some ty -> ty
          | None -> assert false
        in
        (lid, ty) :: map_cstr xs
    | _ :: xs ->
        err pmty.pmty_loc "only 'with type t =' constraints are supported";
        map_cstr xs
  in
  match pmty with
  | {pmty_desc = Pmty_ident lid} -> (lid, [])
  | {pmty_desc = Pmty_with({pmty_desc = Pmty_ident lid}, cstrs)} ->
      (lid, map_cstr cstrs)
  | _ ->
      err pmty.pmty_loc
        "only module type identifier and 'with type' constraints are supported";
      (mknoloc (Lident "_"), [])

let make_loc startpos endpos = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let merloc startpos ?endpos x =
  let endpos = match endpos with
    | None -> x.pexp_loc.Location.loc_end
    | Some endpos -> endpos
  in
  let str = mkloc "merlin.loc" (make_loc startpos endpos) in
  { x with pexp_attributes = (str , PStr []) :: x.pexp_attributes }

let val_of_lwt_bindings ~loc lbs =
  let bindings =
    List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           ~docs:(Lazy.force lb.lb_docs)
           ~text:(Lazy.force lb.lb_text)
           lb.lb_pattern (Fake.app Fake.Lwt.un_lwt lb.lb_expression))
      lbs.lbs_bindings
  in
  let str = mkstr ~loc (Pstr_value(lbs.lbs_rec, List.rev bindings)) in
  match lbs.lbs_extension with
  | None -> str
  | Some id -> ghstr ~loc (Pstr_extension((id, PStr [str]), []))

let expr_of_lwt_bindings ~loc lbs body =
  let bindings =
    List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           lb.lb_pattern (Fake.app Fake.Lwt.un_lwt lb.lb_expression))
      lbs.lbs_bindings
  in
  Fake.app Fake.Lwt.in_lwt
    (mkexp_attrs ~loc (Pexp_let(lbs.lbs_rec, List.rev bindings, body))
       (lbs.lbs_extension, []))

%}

%[@printer.header
  let string_of_INT = function
    | (s, None) -> Printf.sprintf "INT(%s)" s
    | (s, Some c) -> Printf.sprintf "INT(%s%c)" s c

  let string_of_FLOAT = function
    | (s, None) -> Printf.sprintf "FLOAT(%s)" s
    | (s, Some c) -> Printf.sprintf "FLOAT(%s%c)" s c

  let string_of_STRING = function
    | s, Some s' -> Printf.sprintf "STRING(%S,%S)" s s'
    | s, None -> Printf.sprintf "STRING(%S)" s
]

%[@recovery.header
  open Parsetree
  open Ast_helper

  let default_loc = ref Location.none

  let default_expr () =
    let id = Location.mkloc "merlin.hole" !default_loc in
    Exp.mk ~loc:!default_loc (Pexp_extension (id, PStr []))

  let default_pattern () = Pat.any ~loc:!default_loc ()

  let default_module_expr () = Mod.structure ~loc:!default_loc[]
  let default_module_type () = Mty.signature ~loc:!default_loc[]
]
%token AMPERAMPER [@symbol "&&"]
%token AMPERSAND [@symbol "&"]
%token AND [@symbol "and"]
%token AS [@symbol "as"]
%token ASSERT [@symbol "assert"]
%token BACKQUOTE [@symbol "`"]
%token BANG [@symbol "!"]
%token BAR [@symbol "|"]
%token BARBAR [@symbol "||"]
%token BARRBRACKET [@symbol "|]"]
%token BEGIN [@symbol "begin"]
%token <char> CHAR [@cost 2] [@recovery '_']
%token CLASS [@symbol "class"]
%token COLON [@symbol ":"]
%token COLONCOLON [@symbol "::"]
%token COLONEQUAL [@symbol ":="]
%token COLONGREATER [@symbol ":>"]
%token COMMA [@symbol ","]
%token CONSTRAINT [@symbol "constraint"]
%token DO [@symbol "do"]
%token DONE [@symbol "done"]
%token DOT [@symbol "."]
%token DOTDOT [@symbol ".."]
%token DOWNTO [@symbol "downto"]
%token ELSE [@symbol "else"]
%token END [@symbol "end"]
%token EOF
%token EQUAL [@symbol "="]
%token EXCEPTION [@symbol "exception"]
%token EXTERNAL [@symbol "external"]
%token FALSE [@symbol "false"]
%token <string * char option> FLOAT [@cost 2] [@recovery ("0.",None)]
                                    [@printer string_of_FLOAT]

%token FOR [@symbol "for"]
%token FUN [@symbol "fun"]
%token FUNCTION [@symbol "function"]
%token FUNCTOR [@symbol "functor"]
%token GREATER [@symbol ">"]
%token GREATERRBRACE [@symbol ">}"]
%token GREATERRBRACKET [@symbol ">]"]
%token IF [@symbol "if"]
%token IN [@symbol "in"]
%token INCLUDE [@symbol "include"]
%token <string> INFIXOP0 [@cost 2] [@recovery "_"][@printer Printf.sprintf "INFIXOP0(%S)"]
%token <string> INFIXOP1 [@cost 2] [@recovery "_"][@printer Printf.sprintf "INFIXOP1(%S)"]
%token <string> INFIXOP2 [@cost 2] [@recovery "_"][@printer Printf.sprintf "INFIXOP2(%S)"]
%token <string> INFIXOP3 [@cost 2] [@recovery "_"][@printer Printf.sprintf "INFIXOP3(%S)"]
%token <string> INFIXOP4 [@cost 2] [@recovery "_"][@printer Printf.sprintf "INFIXOP4(%S)"]
%token INHERIT [@symbol "inherit"]
%token INITIALIZER [@symbol "initializer"]
%token <string * char option> INT [@cost 1] [@recovery ("0",None)]
                                  [@printer string_of_INT]
%token <string> LABEL [@cost 2] [@recovery "_"][@printer Printf.sprintf "LABEL(%S)"] [@symbol "label"]
%token LAZY [@symbol "lazy"]
%token LBRACE [@symbol "{"]
%token LBRACELESS [@symbol "{<"]
%token LBRACKET [@symbol "["]
%token LBRACKETBAR [@symbol "[|"]
%token LBRACKETLESS [@symbol "[<"]
%token LBRACKETGREATER [@symbol "[>"]
%token LBRACKETPERCENT [@symbol "[%"]
%token LBRACKETPERCENTPERCENT [@symbol "[%%"]
%token LESS [@symbol "<"]
%token LESSMINUS [@symbol "<-"] [@cost 2]
%token LET [@symbol "let"]
%token <string> LIDENT [@cost 2] [@recovery "_"][@printer Printf.sprintf "LIDENT(%S)"]
%token LPAREN [@symbol ")"]
%token LBRACKETAT [@symbol "[@"]
%token LBRACKETATAT [@symbol "[@@"]
%token LBRACKETATATAT [@symbol "[@@@"]
%token MATCH [@symbol "match"]
%token METHOD [@symbol "method"]
%token MINUS [@symbol "-"]
%token MINUSDOT [@symbol "-."]
%token MINUSGREATER [@symbol "->"]
%token MODULE [@symbol "module"]
%token MUTABLE [@symbol "mutable"]
%token NEW [@symbol "new"]
%token NONREC [@cost 1] [@symbol "nonrec"]
%token OBJECT [@symbol "object"]
%token OF [@symbol "of"]
%token OPEN [@symbol "open"]
%token <string> OPTLABEL [@cost 2] [@recovery "_"][@printer Printf.sprintf "OPTLABEL(%S)"] [@symbol "?<label>"]
%token OR [@symbol "or"]

%token PERCENT [@symbol "%"]
%token PLUS [@symbol "+"]
%token PLUSDOT [@symbol "+."]
%token PLUSEQ [@symbol "+="]
%token <string> PREFIXOP [@cost 2] [@recovery "!"][@printer Printf.sprintf "PREFIXOP(%S)"] [@symbol "!"]
%token PRIVATE [@symbol "private"]
%token QUESTION [@symbol "?"]
%token QUESTIONQUESTION [@symbol "??"]
%token QUOTE [@symbol "'"]
%token RBRACE [@symbol "}"]
%token RBRACKET [@symbol "]"]
%token REC [@symbol "rec"]
%token RPAREN [@symbol ")"]
%token SEMI [@symbol ";"]
%token SEMISEMI [@symbol ";;"]
%token SHARP [@symbol "#"]
%token <string> HASHOP [@cost 2] [@recovery ""][@printer Printf.sprintf "HASHOP(%S)"] [@symbol "#<op>"]
%token SIG [@symbol "sig"]
%token STAR [@symbol "*"]
%token <string * string option> STRING [@cost 1] [@recovery ("", None)][@printer string_of_STRING]
%token STRUCT [@symbol "struct"]
%token THEN [@symbol "then"]
%token TILDE [@symbol "~"]
%token TO [@symbol "to"]
%token TRUE [@symbol "true"]
%token TRY [@symbol "try"]
%token TYPE [@symbol "type"]
%token <string> UIDENT [@cost 2][@recovery "_"][@printer Printf.sprintf "UIDENT(%S)"]
%token UNDERSCORE [@symbol "_"]
%token VAL [@symbol "val"]
%token VIRTUAL [@symbol "virtual"]
%token WHEN [@symbol "when"]
%token WHILE [@symbol "while"]
%token WITH [@symbol "with"]
%token <string * Location.t> COMMENT [@cost 2][@recovery ("", Location.none)]
%token <Docstrings.docstring> DOCSTRING

%token EOL

%token LET_LWT [@cost 1] [@symbol "lwt"]
%token TRY_LWT [@cost 1] [@symbol "try_lwt"]
%token MATCH_LWT [@cost 1] [@symbol "match_lwt"]
%token FINALLY_LWT [@cost 1] [@symbol "finally"]
%token FOR_LWT [@cost 1] [@symbol "for_lwt"]
%token WHILE_LWT [@cost 1] [@symbol "while_lwt"]

%token DOTLESS [@cost 1] [@symbol ".<"]
%token DOTTILDE [@cost 1] [@symbol ".~"]
%token GREATERDOT [@cost 1] [@symbol ">."]
%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI
%nonassoc LET LET_LWT
%nonassoc below_WITH
%nonassoc FUNCTION WITH
%nonassoc FINALLY_LWT
%nonassoc AND
%nonassoc THEN
%nonassoc ELSE
%nonassoc LESSMINUS
%right COLONEQUAL
%nonassoc AS
%left BAR
%nonassoc below_COMMA
%left COMMA
%right MINUSGREATER
%right OR BARBAR
%right AMPERSAND AMPERAMPER
%nonassoc below_EQUAL
%left INFIXOP0 EQUAL LESS GREATER
%right INFIXOP1
%nonassoc below_LBRACKETAT
%nonassoc LBRACKETAT
%nonassoc LBRACKETATAT
%right COLONCOLON
%left INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ
%left PERCENT INFIXOP3 STAR
%right INFIXOP4
%nonassoc prec_unary_minus prec_unary_plus
%nonassoc prec_constant_constructor
%nonassoc prec_constr_appl
%left prec_escape
%nonassoc below_SHARP
%nonassoc SHARP
%left HASHOP
%nonassoc below_DOT
%nonassoc DOT

%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW PREFIXOP STRING TRUE UIDENT
          LBRACKETPERCENT LBRACKETPERCENTPERCENT
          DOTLESS DOTTILDE GREATERDOT QUESTIONQUESTION




%start implementation
%type <Parsetree.structure> implementation
%start interface
%type <Parsetree.signature> interface


%start parse_expression
%type <Parsetree.expression> parse_expression

%%


%inline extra_str(symb): symb { extra_str $startpos $endpos $1 };
%inline extra_sig(symb): symb { extra_sig $startpos $endpos $1 };
%inline extra_cstr(symb): symb { extra_cstr $startpos $endpos $1 };
%inline extra_csig(symb): symb { extra_csig $startpos $endpos $1 };
%inline extra_text(symb): symb { extra_text $startpos $endpos $1 };

%inline mkrhs(symb): symb
    {
      (* Semantically we could use $symbolstartpos instead of $startpos
         here, but the code comes from calls to (Parsing.rhs_loc p) for
         some position p, which rather corresponds to
         $startpos, so we kept it for compatibility.

         I do not know if mkrhs is ever used in a situation where $startpos
         and $symbolpos do not coincide. *)
      mkrhs $1 (make_loc $startpos $endpos) }
;



implementation:
    extra_str(structure) EOF { $1 }
;
interface:
    extra_sig(signature) EOF { $1 }
;
parse_core_type:
    core_type EOF { $1 }
;
parse_expression:
    seq_expr EOF { $1 }
;
parse_pattern:
    pattern EOF { $1 }
;



functor_arg:
    LPAREN RPAREN
      { (mkrhs "*" (make_loc $startpos($2) $endpos($2))), None }
  | LPAREN mkrhs(functor_arg_name) COLON module_type RPAREN
      { $2, Some $4 }
;

functor_arg_name:
    UIDENT { $1 }
  | UNDERSCORE { "_" }
;

functor_args [@recovery []]:
    functor_args functor_arg
      { $2 :: $1 }
  | functor_arg
      { [ $1 ] }
;

module_expr [@recovery default_module_expr ()]:
    mkrhs(mod_longident)
      { (mkmod ~loc:(make_loc $symbolstartpos $endpos))(Pmod_ident $1) }
  | STRUCT attributes extra_str(structure) END
      { (mkmod ~loc:(make_loc $symbolstartpos $endpos)) ~attrs:$2 (Pmod_structure($3)) }
  (*| STRUCT attributes structure error
      { unclosed("struct", 1, "end", 3) }*)
  | FUNCTOR attributes functor_args MINUSGREATER module_expr
      { let modexp =
          List.fold_left
            (fun acc (n, t) -> (mkmod ~loc:(make_loc $symbolstartpos $endpos))(Pmod_functor(n, t, acc)))
            $5 $3
        in wrap_mod_attrs modexp $2 }
  | module_expr paren_module_expr
      { (mkmod ~loc:(make_loc $symbolstartpos $endpos))(Pmod_apply($1, $2)) }
  | module_expr LPAREN RPAREN
      { (mkmod ~loc:(make_loc $symbolstartpos $endpos))(Pmod_apply($1, (mkmod ~loc:(make_loc $symbolstartpos $endpos)) (Pmod_structure []))) }
  | paren_module_expr
      { $1 }
  | module_expr attribute
      { Mod.attr $1 $2 }
  | extension
      { (mkmod ~loc:(make_loc $symbolstartpos $endpos))(Pmod_extension $1) }
;

paren_module_expr:
    LPAREN module_expr COLON module_type RPAREN
      { (mkmod ~loc:(make_loc $symbolstartpos $endpos))(Pmod_constraint($2, $4)) }
  (*| LPAREN module_expr COLON module_type error
      { unclosed("(", 2, ")", 4) }*)
  | LPAREN module_expr RPAREN
      { $2 }
  (*| LPAREN module_expr error
      { unclosed("(", 1, ")", 3) }*)
  | LPAREN VAL attributes expr RPAREN
      { (mkmod ~loc:(make_loc $symbolstartpos $endpos)) ~attrs:$3 (Pmod_unpack $4) }
  | LPAREN VAL attributes expr COLON package_type RPAREN
      { (mkmod ~loc:(make_loc $symbolstartpos $endpos)) ~attrs:$3 (Pmod_unpack(
              (ghexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_constraint($4, (ghtyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_package $6))))) }
  | LPAREN VAL attributes expr COLON package_type COLONGREATER package_type RPAREN
      { (mkmod ~loc:(make_loc $symbolstartpos $endpos)) ~attrs:$3 (Pmod_unpack(
              (ghexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_coerce($4, Some((ghtyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_package $6)),
                                    (ghtyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_package $8))))) }
  | LPAREN VAL attributes expr COLONGREATER package_type RPAREN
      { (mkmod ~loc:(make_loc $symbolstartpos $endpos)) ~attrs:$3 (Pmod_unpack(
              (ghexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_coerce($4, None, (ghtyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_package $6))))) }
  (*| LPAREN VAL attributes expr COLON error
      { unclosed("(", 1, ")", 6) }*)
  (*| LPAREN VAL attributes expr COLONGREATER error
      { unclosed("(", 1, ")", 6) }*)
  (*| LPAREN VAL attributes expr error
      { unclosed("(", 1, ")", 5) }*)
;

structure [@recovery []]:
    seq_expr post_item_attributes structure_tail
      { (mark_rhs_docs $startpos($1) $endpos($2));
        ((text_str $startpos($1))) @ mkstrexp $1 $2 :: $3 }
  | toplevel_directive structure { $1 :: $2 }
  | structure_tail { $1 }
;
structure_tail:
                         { [] }
  | SEMISEMI structure { ((text_str $startpos($1))) @ $2 }
  | structure_item structure_tail { ((text_str $startpos($1))) @ $1 :: $2 }
;
%public structure_item:
    let_bindings
      { (val_of_let_bindings ~loc:(make_loc $symbolstartpos $endpos)) $1 }
  | primitive_declaration
      { let (body, ext) = $1 in (mkstr_ext ~loc:(make_loc $symbolstartpos $endpos)) (Pstr_primitive body) ext }
  | value_description
      { let (body, ext) = $1 in (mkstr_ext ~loc:(make_loc $symbolstartpos $endpos)) (Pstr_primitive body) ext }
  | type_declarations
      { let (nr, l, ext) = $1 in (mkstr_ext ~loc:(make_loc $symbolstartpos $endpos)) (Pstr_type (nr, List.rev l)) ext }
  | str_type_extension
      { let (l, ext) = $1 in (mkstr_ext ~loc:(make_loc $symbolstartpos $endpos)) (Pstr_typext l) ext }
  | str_exception_declaration
      { let (l, ext) = $1 in (mkstr_ext ~loc:(make_loc $symbolstartpos $endpos)) (Pstr_exception l) ext }
  | module_binding
      { let (body, ext) = $1 in (mkstr_ext ~loc:(make_loc $symbolstartpos $endpos)) (Pstr_module body) ext }
  | rec_module_bindings
      { let (l, ext) = $1 in (mkstr_ext ~loc:(make_loc $symbolstartpos $endpos)) (Pstr_recmodule(List.rev l)) ext }
  | module_type_declaration
      { let (body, ext) = $1 in (mkstr_ext ~loc:(make_loc $symbolstartpos $endpos)) (Pstr_modtype body) ext }
  | open_statement
      { let (body, ext) = $1 in (mkstr_ext ~loc:(make_loc $symbolstartpos $endpos)) (Pstr_open body) ext }
  | class_declarations
      { let (l, ext) = $1 in (mkstr_ext ~loc:(make_loc $symbolstartpos $endpos)) (Pstr_class (List.rev l)) ext }
  | class_type_declarations
      { let (l, ext) = $1 in (mkstr_ext ~loc:(make_loc $symbolstartpos $endpos)) (Pstr_class_type (List.rev l)) ext }
  | str_include_statement
      { let (body, ext) = $1 in (mkstr_ext ~loc:(make_loc $symbolstartpos $endpos)) (Pstr_include body) ext }
  | item_extension post_item_attributes
      { (mkstr ~loc:(make_loc $symbolstartpos $endpos))(Pstr_extension ($1, (add_docs_attrs ((symbol_docs $symbolstartpos $endpos)) $2))) }
  | floating_attribute
      { (mark_symbol_docs $symbolstartpos $endpos);
        (mkstr ~loc:(make_loc $symbolstartpos $endpos))(Pstr_attribute $1) }
;
str_include_statement:
    INCLUDE ext_attributes module_expr post_item_attributes
      { let (ext, attrs) = $2 in
        Incl.mk $3 ~attrs:(attrs@$4)
            ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
      , ext }
;
module_binding_body:
    EQUAL module_expr
      { $2 }
  | COLON module_type EQUAL module_expr
      { (mkmod ~loc:(make_loc $symbolstartpos $endpos))(Pmod_constraint($4, $2)) }
  | functor_arg module_binding_body
      { (mkmod ~loc:(make_loc $symbolstartpos $endpos))(Pmod_functor(fst $1, snd $1, $2)) }
;
module_binding:
    MODULE ext_attributes mkrhs(UIDENT) module_binding_body post_item_attributes
      { let (ext, attrs) = $2 in
        Mb.mk $3 $4 ~attrs:(attrs@$5)
            ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
      , ext }
;
rec_module_bindings:
    rec_module_binding { let (b, ext) = $1 in ([b], ext) }
  | rec_module_bindings and_module_binding
      { let (l, ext) = $1 in ($2 :: l, ext) }
;
rec_module_binding:
    MODULE ext_attributes REC mkrhs(UIDENT) module_binding_body post_item_attributes
      { let (ext, attrs) = $2 in
        Mb.mk $4 $5 ~attrs:(attrs@$6)
            ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
      , ext }
;
and_module_binding:
    AND attributes mkrhs(UIDENT) module_binding_body post_item_attributes
      { Mb.mk $3 $4 ~attrs:($2@$5) ~loc:(((make_loc $symbolstartpos $endpos)))
               ~text:((symbol_text $startpos)) ~docs:((symbol_docs $symbolstartpos $endpos)) }
;



module_type [@recovery default_module_type ()]:
    mkrhs(mty_longident)
      { (mkmty ~loc:(make_loc $symbolstartpos $endpos))(Pmty_ident $1) }
  | SIG attributes extra_sig(signature) END
      { (mkmty ~loc:(make_loc $symbolstartpos $endpos)) ~attrs:$2 (Pmty_signature $3) }
  (*| SIG attributes signature error
      { unclosed("sig", 1, "end", 4) }*)
  | FUNCTOR attributes functor_args MINUSGREATER module_type
      %prec below_WITH
      { let mty =
          List.fold_left
            (fun acc (n, t) -> (mkmty ~loc:(make_loc $symbolstartpos $endpos))(Pmty_functor(n, t, acc)))
            $5 $3
        in wrap_mty_attrs mty $2 }
  | module_type MINUSGREATER module_type
      %prec below_WITH
      { (mkmty ~loc:(make_loc $symbolstartpos $endpos))(Pmty_functor(mknoloc "_", Some $1, $3)) }
  | module_type WITH with_constraints
      { (mkmty ~loc:(make_loc $symbolstartpos $endpos))(Pmty_with($1, List.rev $3)) }
  | MODULE TYPE OF attributes module_expr %prec below_LBRACKETAT
      { (mkmty ~loc:(make_loc $symbolstartpos $endpos)) ~attrs:$4 (Pmty_typeof $5) }


  | LPAREN module_type RPAREN
      { $2 }
  (*| LPAREN module_type error
      { unclosed("(", 1, ")", 3) }*)
  | extension
      { (mkmty ~loc:(make_loc $symbolstartpos $endpos))(Pmty_extension $1) }
  | module_type attribute
      { Mty.attr $1 $2 }
;
signature:
                         { [] }
  | SEMISEMI signature { ((text_sig $startpos($1))) @ $2 }
  | signature_item signature { ((text_sig $startpos($1))) @ $1 :: $2 }
;
signature_item:
    value_description
      { let (body, ext) = $1 in (mksig_ext ~loc:(make_loc $symbolstartpos $endpos)) (Psig_value body) ext }
  | primitive_declaration
      { let (body, ext) = $1 in (mksig_ext ~loc:(make_loc $symbolstartpos $endpos)) (Psig_value body) ext }
  | type_declarations
      { let (nr, l, ext) = $1 in (mksig_ext ~loc:(make_loc $symbolstartpos $endpos)) (Psig_type (nr, List.rev l)) ext }
  | sig_type_extension
      { let (l, ext) = $1 in (mksig_ext ~loc:(make_loc $symbolstartpos $endpos)) (Psig_typext l) ext }
  | sig_exception_declaration
      { let (l, ext) = $1 in (mksig_ext ~loc:(make_loc $symbolstartpos $endpos)) (Psig_exception l) ext }
  | module_declaration
      { let (body, ext) = $1 in (mksig_ext ~loc:(make_loc $symbolstartpos $endpos)) (Psig_module body) ext }
  | module_alias
      { let (body, ext) = $1 in (mksig_ext ~loc:(make_loc $symbolstartpos $endpos)) (Psig_module body) ext }
  | rec_module_declarations
      { let (l, ext) = $1 in (mksig_ext ~loc:(make_loc $symbolstartpos $endpos)) (Psig_recmodule (List.rev l)) ext }
  | module_type_declaration
      { let (body, ext) = $1 in (mksig_ext ~loc:(make_loc $symbolstartpos $endpos)) (Psig_modtype body) ext }
  | open_statement
      { let (body, ext) = $1 in (mksig_ext ~loc:(make_loc $symbolstartpos $endpos)) (Psig_open body) ext }
  | sig_include_statement
      { let (body, ext) = $1 in (mksig_ext ~loc:(make_loc $symbolstartpos $endpos)) (Psig_include body) ext }
  | class_descriptions
      { let (l, ext) = $1 in (mksig_ext ~loc:(make_loc $symbolstartpos $endpos)) (Psig_class (List.rev l)) ext }
  | class_type_declarations
      { let (l, ext) = $1 in (mksig_ext ~loc:(make_loc $symbolstartpos $endpos)) (Psig_class_type (List.rev l)) ext }
  | item_extension post_item_attributes
      { (mksig ~loc:(make_loc $symbolstartpos $endpos))(Psig_extension ($1, (add_docs_attrs ((symbol_docs $symbolstartpos $endpos)) $2))) }
  | floating_attribute
      { (mark_symbol_docs $symbolstartpos $endpos);
        (mksig ~loc:(make_loc $symbolstartpos $endpos))(Psig_attribute $1) }
;
open_statement:
  | OPEN override_flag ext_attributes mkrhs(mod_longident) post_item_attributes
      { let (ext, attrs) = $3 in
        Opn.mk $4 ~override:$2 ~attrs:(attrs@$5)
          ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
      , ext }
;
sig_include_statement:
    INCLUDE ext_attributes module_type post_item_attributes %prec below_WITH
      { let (ext, attrs) = $2 in
        Incl.mk $3 ~attrs:(attrs@$4)
            ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
      , ext }
;
module_declaration_body:
    COLON module_type
      { $2 }
  | LPAREN mkrhs(UIDENT) COLON module_type RPAREN module_declaration_body
      { (mkmty ~loc:(make_loc $symbolstartpos $endpos))(Pmty_functor($2, Some $4, $6)) }
  | LPAREN RPAREN module_declaration_body
      { (mkmty ~loc:(make_loc $symbolstartpos $endpos))(Pmty_functor((mkrhs "*" (make_loc $startpos($1) $endpos($1))), None, $3)) }
;
module_declaration:
    MODULE ext_attributes mkrhs(UIDENT) module_declaration_body post_item_attributes
      { let (ext, attrs) = $2 in
        Md.mk $3 $4 ~attrs:(attrs@$5)
          ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
      , ext }
;
module_alias:
    MODULE ext_attributes mkrhs(UIDENT) EQUAL mkrhs(mod_longident) post_item_attributes
      { let (ext, attrs) = $2 in
        Md.mk $3
          (Mty.alias ~loc:$5.Location.loc $5) ~attrs:(attrs@$6)
             ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
      , ext }
;
rec_module_declarations:
    rec_module_declaration
      { let (body, ext) = $1 in ([body], ext) }
  | rec_module_declarations and_module_declaration
      { let (l, ext) = $1 in ($2 :: l, ext) }
;
rec_module_declaration:
    MODULE ext_attributes REC mkrhs(UIDENT) COLON module_type post_item_attributes
      { let (ext, attrs) = $2 in
        Md.mk $4 $6 ~attrs:(attrs@$7)
            ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
      , ext }
;
and_module_declaration:
    AND attributes mkrhs(UIDENT) COLON module_type post_item_attributes
      { Md.mk $3 $5 ~attrs:($2@$6) ~loc:(((make_loc $symbolstartpos $endpos)))
              ~text:((symbol_text $startpos)) ~docs:((symbol_docs $symbolstartpos $endpos)) }
;
module_type_declaration_body:
                              { None }
  | EQUAL module_type { Some $2 }
;
module_type_declaration:
    MODULE TYPE ext_attributes mkrhs(ident) module_type_declaration_body
    post_item_attributes
      { let (ext, attrs) = $3 in
        Mtd.mk $4 ?typ:$5 ~attrs:(attrs@$6)
          ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
      , ext }
;


class_declarations:
    class_declaration
      { let (body, ext) = $1 in ([body], ext) }
  | class_declarations and_class_declaration
      { let (l, ext) = $1 in ($2 :: l, ext) }
;
class_declaration:
    CLASS ext_attributes virtual_flag class_type_parameters mkrhs(LIDENT)
    class_fun_binding post_item_attributes
      { let (ext, attrs) = $2 in
        Ci.mk $5 $6 ~virt:$3 ~params:$4 ~attrs:(attrs@$7)
            ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
      , ext }
;
and_class_declaration:
    AND attributes virtual_flag class_type_parameters mkrhs(LIDENT) class_fun_binding
    post_item_attributes
      { Ci.mk $5 $6 ~virt:$3 ~params:$4
         ~attrs:($2@$7) ~loc:(((make_loc $symbolstartpos $endpos)))
         ~text:((symbol_text $startpos)) ~docs:((symbol_docs $symbolstartpos $endpos)) }
;
class_fun_binding:
    EQUAL class_expr
      { $2 }
  | COLON class_type EQUAL class_expr
      { (mkclass ~loc:(make_loc $symbolstartpos $endpos))(Pcl_constraint($4, $2)) }
  | labeled_simple_pattern class_fun_binding
      { let (l,o,p) = $1 in (mkclass ~loc:(make_loc $symbolstartpos $endpos))(Pcl_fun(l, o, p, $2)) }
;
class_type_parameters:
                                                { [] }
  | LBRACKET type_parameter_list RBRACKET { List.rev $2 }
;
class_fun_def:
    labeled_simple_pattern MINUSGREATER class_expr
      { let (l,o,p) = $1 in (mkclass ~loc:(make_loc $symbolstartpos $endpos))(Pcl_fun(l, o, p, $3)) }
  | labeled_simple_pattern class_fun_def
      { let (l,o,p) = $1 in (mkclass ~loc:(make_loc $symbolstartpos $endpos))(Pcl_fun(l, o, p, $2)) }
;
class_expr:
    class_simple_expr
      { $1 }
  | FUN attributes class_fun_def
      { wrap_class_attrs $3 $2 }
  | class_simple_expr simple_labeled_expr_list
      { (mkclass ~loc:(make_loc $symbolstartpos $endpos))(Pcl_apply($1, List.rev $2)) }
  | let_bindings IN class_expr
      { (class_of_let_bindings ~loc:(make_loc $symbolstartpos $endpos)) $1 $3 }
  | class_expr attribute
      { Cl.attr $1 $2 }
  | extension
      { (mkclass ~loc:(make_loc $symbolstartpos $endpos))(Pcl_extension $1) }
;
class_simple_expr:
    LBRACKET core_type_comma_list RBRACKET class_longident
      { (mkclass ~loc:(make_loc $symbolstartpos $endpos))(Pcl_constr(mkloc $4 ((make_loc $startpos($4) $endpos($4))), List.rev $2)) }
  | mkrhs(class_longident)
      { (mkclass ~loc:(make_loc $symbolstartpos $endpos))(Pcl_constr($1, [])) }
  | OBJECT attributes class_structure END
      { (mkclass ~loc:(make_loc $symbolstartpos $endpos)) ~attrs:$2 (Pcl_structure($3)) }
  (*| OBJECT attributes class_structure error
      { unclosed("object", 1, "end", 4) }*)
  | LPAREN class_expr COLON class_type RPAREN
      { (mkclass ~loc:(make_loc $symbolstartpos $endpos))(Pcl_constraint($2, $4)) }
  (*| LPAREN class_expr COLON class_type error
      { unclosed("(", 1, ")", 5) }*)
  | LPAREN class_expr RPAREN
      { $2 }
  (*| LPAREN class_expr error
      { unclosed("(", 1, ")", 3) }*)
;
class_structure:
  | class_self_pattern extra_cstr(class_fields)
       { Cstr.mk $1 (List.rev $2) }
;
class_self_pattern:
    LPAREN pattern RPAREN
      { (reloc_pat ~loc:(make_loc $symbolstartpos $endpos)) $2 }
  | LPAREN pattern COLON core_type RPAREN
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_constraint($2, $4)) }
  |
      { (ghpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_any) }
;
class_fields:

      { [] }
  | class_fields class_field
      { $2 :: ((text_cstr $startpos($2))) @ $1 }
;
class_field:
  | INHERIT override_flag attributes class_expr parent_binder
    post_item_attributes
      { (mkcf ~loc:(make_loc $symbolstartpos $endpos)) (Pcf_inherit ($2, $4, $5)) ~attrs:($3@$6) ~docs:((symbol_docs $symbolstartpos $endpos)) }
  | VAL value post_item_attributes
      { let v, attrs = $2 in
        (mkcf ~loc:(make_loc $symbolstartpos $endpos)) (Pcf_val v) ~attrs:(attrs@$3) ~docs:((symbol_docs $symbolstartpos $endpos)) }
  | METHOD method_ post_item_attributes
      { let meth, attrs = $2 in
        (mkcf ~loc:(make_loc $symbolstartpos $endpos)) (Pcf_method meth) ~attrs:(attrs@$3) ~docs:((symbol_docs $symbolstartpos $endpos)) }
  | CONSTRAINT attributes constrain_field post_item_attributes
      { (mkcf ~loc:(make_loc $symbolstartpos $endpos)) (Pcf_constraint $3) ~attrs:($2@$4) ~docs:((symbol_docs $symbolstartpos $endpos)) }
  | INITIALIZER attributes seq_expr post_item_attributes
      { (mkcf ~loc:(make_loc $symbolstartpos $endpos)) (Pcf_initializer $3) ~attrs:($2@$4) ~docs:((symbol_docs $symbolstartpos $endpos)) }
  | item_extension post_item_attributes
      { (mkcf ~loc:(make_loc $symbolstartpos $endpos)) (Pcf_extension $1) ~attrs:$2 ~docs:((symbol_docs $symbolstartpos $endpos)) }
  | floating_attribute
      { (mark_symbol_docs $symbolstartpos $endpos);
        (mkcf ~loc:(make_loc $symbolstartpos $endpos)) (Pcf_attribute $1) }
;
parent_binder:
    AS mkrhs(LIDENT)
          { Some $2 }
  |
          { None }
;
value:

    override_flag attributes MUTABLE VIRTUAL label COLON core_type
      { if $1 = Override then (syntax_error (make_loc $startpos($1) $endpos($1)));
        (mkloc $5 ((make_loc $startpos($5) $endpos($5))), Mutable, Cfk_virtual $7), $2 }
  | override_flag attributes VIRTUAL mutable_flag mkrhs(label) COLON core_type
      { if $1 = Override then (syntax_error (make_loc $startpos($1) $endpos($1)));
        ($5, $4, Cfk_virtual $7), $2 }
  | override_flag attributes mutable_flag mkrhs(label) EQUAL seq_expr
      { ($4, $3, Cfk_concrete ($1, $6)), $2 }
  | override_flag attributes mutable_flag mkrhs(label) type_constraint EQUAL seq_expr
      {
       let e = (mkexp_constraint ~loc:(make_loc $symbolstartpos $endpos)) $7 $5 in
       ($4, $3, Cfk_concrete ($1, e)), $2
      }
;
method_:

    override_flag attributes PRIVATE VIRTUAL label COLON poly_type
      { if $1 = Override then (syntax_error (make_loc $startpos($1) $endpos($1)));
        (mkloc $5 ((make_loc $startpos($5) $endpos($5))), Private, Cfk_virtual $7), $2 }
  | override_flag attributes VIRTUAL private_flag label COLON poly_type
      { if $1 = Override then (syntax_error (make_loc $startpos($1) $endpos($1)));
        (mkloc $5 ((make_loc $startpos($5) $endpos($5))), $4, Cfk_virtual $7), $2 }
  | override_flag attributes private_flag label strict_binding
      { (mkloc $4 ((make_loc $startpos($4) $endpos($4))), $3,
         Cfk_concrete ($1, (ghexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_poly ($5, None)))), $2 }
  | override_flag attributes private_flag label COLON poly_type EQUAL seq_expr
      { (mkloc $4 ((make_loc $startpos($4) $endpos($4))), $3,
        Cfk_concrete ($1, (ghexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_poly($8, Some $6)))), $2 }
  | override_flag attributes private_flag label COLON TYPE lident_list
    DOT core_type EQUAL seq_expr
      { let exp, poly = (wrap_type_annotation ~loc:(make_loc $symbolstartpos $endpos)) $7 $9 $11 in
        (mkloc $4 ((make_loc $startpos($4) $endpos($4))), $3,
         Cfk_concrete ($1, (ghexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_poly(exp, Some poly)))), $2 }
;



class_type:
    class_signature
      { $1 }
  | QUESTION LIDENT COLON simple_core_type_or_tuple MINUSGREATER
    class_type
      { (mkcty ~loc:(make_loc $symbolstartpos $endpos))(Pcty_arrow(Optional $2 , $4, $6)) }
  | OPTLABEL simple_core_type_or_tuple MINUSGREATER class_type
      { (mkcty ~loc:(make_loc $symbolstartpos $endpos))(Pcty_arrow(Optional $1, $2, $4)) }
  | LIDENT COLON simple_core_type_or_tuple MINUSGREATER class_type
      { (mkcty ~loc:(make_loc $symbolstartpos $endpos))(Pcty_arrow(Labelled $1, $3, $5)) }
  | simple_core_type_or_tuple MINUSGREATER class_type
      { (mkcty ~loc:(make_loc $symbolstartpos $endpos))(Pcty_arrow(Nolabel, $1, $3)) }
 ;
class_signature:
    LBRACKET core_type_comma_list RBRACKET clty_longident
      { (mkcty ~loc:(make_loc $symbolstartpos $endpos))(Pcty_constr (mkloc $4 ((make_loc $startpos($4) $endpos($4))), List.rev $2)) }
  | mkrhs(clty_longident)
      { (mkcty ~loc:(make_loc $symbolstartpos $endpos))(Pcty_constr ($1, [])) }
  | OBJECT attributes class_sig_body END
      { (mkcty ~loc:(make_loc $symbolstartpos $endpos)) ~attrs:$2 (Pcty_signature $3) }
  (*| OBJECT attributes class_sig_body error
      { unclosed("object", 1, "end", 4) }*)
  | class_signature attribute
      { Cty.attr $1 $2 }
  | extension
      { (mkcty ~loc:(make_loc $symbolstartpos $endpos))(Pcty_extension $1) }
;
class_sig_body:
    class_self_type extra_csig(class_sig_fields)
      { Csig.mk $1 (List.rev $2) }
;
class_self_type:
    LPAREN core_type RPAREN
      { $2 }
  |
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_any) }
;
class_sig_fields:
                                                { [] }
| class_sig_fields class_sig_field { $2 :: ((text_csig $startpos($2))) @ $1 }
;
class_sig_field:
    INHERIT attributes class_signature post_item_attributes
      { (mkctf ~loc:(make_loc $symbolstartpos $endpos)) (Pctf_inherit $3) ~attrs:($2@$4) ~docs:((symbol_docs $symbolstartpos $endpos)) }
  | VAL attributes value_type post_item_attributes
      { (mkctf ~loc:(make_loc $symbolstartpos $endpos)) (Pctf_val $3) ~attrs:($2@$4) ~docs:((symbol_docs $symbolstartpos $endpos)) }
  | METHOD attributes private_virtual_flags mkrhs(label) COLON poly_type
    post_item_attributes
      {
       let (p, v) = $3 in
       (mkctf ~loc:(make_loc $symbolstartpos $endpos)) (Pctf_method ($4, p, v, $6)) ~attrs:($2@$7) ~docs:((symbol_docs $symbolstartpos $endpos))
      }
  | CONSTRAINT attributes constrain_field post_item_attributes
      { (mkctf ~loc:(make_loc $symbolstartpos $endpos)) (Pctf_constraint $3) ~attrs:($2@$4) ~docs:((symbol_docs $symbolstartpos $endpos)) }
  | item_extension post_item_attributes
      { (mkctf ~loc:(make_loc $symbolstartpos $endpos)) (Pctf_extension $1) ~attrs:$2 ~docs:((symbol_docs $symbolstartpos $endpos)) }
  | floating_attribute
      { (mark_symbol_docs $symbolstartpos $endpos);
        (mkctf ~loc:(make_loc $symbolstartpos $endpos))(Pctf_attribute $1) }
;
value_type:
    VIRTUAL mutable_flag mkrhs(label) COLON core_type
      { $3, $2, Virtual, $5 }
  | MUTABLE virtual_flag mkrhs(label) COLON core_type
      { $3, Mutable, $2, $5 }
  | mkrhs(label) COLON core_type
      { $1, Immutable, Concrete, $3 }
;
constrain:
        core_type EQUAL core_type { $1, $3, (make_loc $symbolstartpos $endpos) }
;
constrain_field:
        core_type EQUAL core_type { $1, $3 }
;
class_descriptions:
    class_description
      { let (body, ext) = $1 in ([body],ext) }
  | class_descriptions and_class_description
      { let (l, ext) = $1 in ($2 :: l, ext) }
;
class_description:
    CLASS ext_attributes virtual_flag class_type_parameters mkrhs(LIDENT) COLON
    class_type post_item_attributes
      { let (ext, attrs) = $2 in
        Ci.mk $5 $7 ~virt:$3 ~params:$4 ~attrs:(attrs@$8)
            ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
     , ext }
;
and_class_description:
    AND attributes virtual_flag class_type_parameters mkrhs(LIDENT) COLON class_type
    post_item_attributes
      { Ci.mk $5 $7 ~virt:$3 ~params:$4
              ~attrs:($2@$8) ~loc:(((make_loc $symbolstartpos $endpos)))
              ~text:((symbol_text $startpos)) ~docs:((symbol_docs $symbolstartpos $endpos)) }
;
class_type_declarations:
    class_type_declaration
      { let (body, ext) = $1 in ([body],ext) }
  | class_type_declarations and_class_type_declaration
      { let (l, ext) = $1 in ($2 :: l, ext) }
;
class_type_declaration:
    CLASS TYPE ext_attributes virtual_flag class_type_parameters mkrhs(LIDENT) EQUAL
    class_signature post_item_attributes
      { let (ext, attrs) = $3 in
        Ci.mk $6 $8 ~virt:$4 ~params:$5 ~attrs:(attrs@$9)
            ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
      , ext}
;
and_class_type_declaration:
    AND attributes virtual_flag class_type_parameters mkrhs(LIDENT) EQUAL
    class_signature post_item_attributes
      { Ci.mk $5 $7 ~virt:$3 ~params:$4
         ~attrs:($2@$8) ~loc:(((make_loc $symbolstartpos $endpos)))
         ~text:((symbol_text $startpos)) ~docs:((symbol_docs $symbolstartpos $endpos)) }
;



seq_expr:
  | expr %prec below_SEMI { $1 }
  | expr SEMI { (reloc_exp ~loc:(make_loc $symbolstartpos $endpos)) $1 }
  | expr SEMI seq_expr { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_sequence($1, $3)) }
  | expr SEMI PERCENT attr_id seq_expr
      { let seq = (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_sequence ($1, $5)) in
        let payload = PStr [mkstrexp seq []] in
        (mkexp ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_extension ($4, payload)) }
;
labeled_simple_pattern:
    QUESTION LPAREN label_let_pattern opt_default RPAREN
      { (Optional (fst $3), $4, snd $3) }
  | QUESTION label_var
      { (Optional (fst $2), None, snd $2) }
  | OPTLABEL LPAREN let_pattern opt_default RPAREN
      { (Optional $1, $4, $3) }
  | OPTLABEL pattern_var
      { (Optional $1, None, $2) }
  | TILDE LPAREN label_let_pattern RPAREN
      { (Labelled (fst $3), None, snd $3) }
  | TILDE label_var
      { (Labelled (fst $2), None, snd $2) }
  | LABEL simple_pattern
      { (Labelled $1, None, $2) }
  | simple_pattern
      { (Nolabel, None, $1) }
;
pattern_var:
    mkrhs(LIDENT) { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_var $1) }
  | UNDERSCORE { (mkpat ~loc:(make_loc $symbolstartpos $endpos)) Ppat_any }
;
opt_default:
                                        { None }
  | EQUAL seq_expr { Some $2 }
;
label_let_pattern:
    label_var
      { $1 }
  | label_var COLON core_type
      { let (lab, pat) = $1 in (lab, (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_constraint(pat, $3))) }
;
label_var:
    mkrhs(LIDENT) { ($1.Location.txt, (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_var $1)) }
;
let_pattern [@recovery default_pattern ()]:
    pattern
      { $1 }
  | pattern COLON core_type
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_constraint($1, $3)) }
;
%public expr [@recovery default_expr ()]:
    simple_expr %prec below_SHARP
      { $1 }
  | simple_expr simple_labeled_expr_list
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_apply($1, List.rev $2)) }
  | let_bindings IN seq_expr
      { (expr_of_let_bindings ~loc:(make_loc $symbolstartpos $endpos)) $1 (merloc $endpos($2) $3) }
  | LET MODULE ext_attributes mkrhs(UIDENT) module_binding_body IN seq_expr
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_letmodule($4, $5, (merloc $endpos($6) $7))) $3 }
  | LET EXCEPTION ext_attributes let_exception_declaration IN seq_expr
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_letexception($4, $6)) $3 }
  | LET OPEN override_flag ext_attributes mkrhs(mod_longident) IN seq_expr
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_open($3, $5, (merloc $endpos($6) $7))) $4 }
  | FUNCTION ext_attributes opt_bar match_cases
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_function(List.rev $4)) $2 }
  | FUN ext_attributes labeled_simple_pattern fun_def
      { let (l,o,p) = $3 in
        (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_fun(l, o, p, $4)) $2 }
  | FUN ext_attributes LPAREN TYPE lident_list RPAREN fun_def
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) ((mk_newtypes ~loc:(make_loc $symbolstartpos $endpos)) $5 $7).pexp_desc $2 }
  | MATCH ext_attributes seq_expr WITH opt_bar match_cases
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_match($3, List.rev $6)) $2 }
  | TRY ext_attributes seq_expr WITH opt_bar match_cases
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_try($3, List.rev $6)) $2 }
  (*| TRY ext_attributes seq_expr WITH error
      { (syntax_error (make_loc $startpos($) $endpos($))) }*)
  | expr_comma_list %prec below_COMMA
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_tuple(List.rev $1)) }
  | mkrhs(constr_longident) simple_expr %prec below_SHARP
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_construct($1, Some $2)) }
  | name_tag simple_expr %prec below_SHARP
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_variant($1, Some $2)) }
  | IF ext_attributes seq_expr THEN expr ELSE expr
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos))(Pexp_ifthenelse($3, (merloc $endpos($4) $5), Some (merloc $endpos($6) $7))) $2 }
  | IF ext_attributes seq_expr THEN expr
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_ifthenelse($3, (merloc $endpos($4) $5), None)) $2 }
  | WHILE ext_attributes seq_expr DO seq_expr DONE
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_while($3, (merloc $endpos($4) $5))) $2 }
  | FOR ext_attributes pattern EQUAL seq_expr direction_flag seq_expr DO
    seq_expr DONE
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos))(Pexp_for($3, (merloc $endpos($4) $5), (merloc $endpos($6) $7), $6, (merloc $endpos($8) $9))) $2 }
  | expr COLONCOLON expr
      { mkexp_cons ((make_loc $startpos($2) $endpos($2))) ((ghexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_tuple[$1;(merloc $endpos($2) $3)])) ((make_loc $symbolstartpos $endpos)) }
  | LPAREN COLONCOLON RPAREN LPAREN expr COMMA expr RPAREN
      { mkexp_cons ((make_loc $startpos($2) $endpos($2))) ((ghexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_tuple[$5;$7])) ((make_loc $symbolstartpos $endpos)) }
  | expr INFIXOP0 expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 $2 $3 }
  | expr INFIXOP1 expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 $2 $3 }
  | expr INFIXOP2 expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 $2 $3 }
  | expr INFIXOP3 expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 $2 $3 }
  | expr INFIXOP4 expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 $2 $3 }
  | expr PLUS expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 "+" $3 }
  | expr PLUSDOT expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 "+." $3 }
  | expr PLUSEQ expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 "+=" $3 }
  | expr MINUS expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 "-" $3 }
  | expr MINUSDOT expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 "-." $3 }
  | expr STAR expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 "*" $3 }
  | expr PERCENT expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 "%" $3 }
  | expr EQUAL expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 "=" $3 }
  | expr LESS expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 "<" $3 }
  | expr GREATER expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 ">" $3 }
  | expr OR expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 "or" $3 }
  | expr BARBAR expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 "||" $3 }
  | expr AMPERSAND expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 "&" $3 }
  | expr AMPERAMPER expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 "&&" $3 }
  | expr COLONEQUAL expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 ":=" $3 }
  | subtractive expr %prec prec_unary_minus
      { (mkuminus ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($1) $endpos($1))) $1 $2 }
  | additive expr %prec prec_unary_plus
      { (mkuplus ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($1) $endpos($1))) $1 $2 }
  | simple_expr DOT mkrhs(label_longident) LESSMINUS expr
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_setfield($1, $3, $5)) }
  | simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS expr
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_apply((ghexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_ident((array_function ~loc:(make_loc $symbolstartpos $endpos)) "Array" "set")),
                         [Nolabel,$1; Nolabel,$4; Nolabel,$7])) }
  | simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS expr
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_apply((ghexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_ident((array_function ~loc:(make_loc $symbolstartpos $endpos)) "String" "set")),
                         [Nolabel,$1; Nolabel,$4; Nolabel,$7])) }
  | simple_expr DOT LBRACE expr RBRACE LESSMINUS expr
      { (bigarray_set ~loc:(make_loc $symbolstartpos $endpos)) $1 $4 $7 }
  | mkrhs(label) LESSMINUS expr
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_setinstvar($1, $3)) }
  | ASSERT ext_attributes simple_expr %prec below_SHARP
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_assert $3) $2 }
  | LAZY ext_attributes simple_expr %prec below_SHARP
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_lazy $3) $2 }
  | OBJECT ext_attributes class_structure END
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_object $3) $2 }
  (*| OBJECT ext_attributes class_structure error
      { unclosed("object", 1, "end", 4) }*)
  | expr attribute
      { Exp.attr $1 $2 }
  (*| UNDERSCORE
      { (not_expecting (make_loc $startpos($1) $endpos($1)) "wildcard \"_\"") }*)
;
%public simple_expr:
    mkrhs(val_longident)
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_ident $1) }
  | constant
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_constant $1) }
  | mkrhs(constr_longident) %prec prec_constant_constructor
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_construct($1, None)) }
  | name_tag %prec prec_constant_constructor
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_variant($1, None)) }
  | LPAREN seq_expr RPAREN
      { (reloc_exp ~loc:(make_loc $symbolstartpos $endpos)) $2 }
  (*| LPAREN seq_expr error
      { unclosed("(", 1, ")", 3) }*)
  | BEGIN ext_attributes seq_expr END
      { (wrap_exp_attrs ~loc:(make_loc $symbolstartpos $endpos)) ((reloc_exp ~loc:(make_loc $symbolstartpos $endpos)) $3) $2 (* check location *) }
  | BEGIN ext_attributes END
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_construct (mkloc (Lident "()") ((make_loc $symbolstartpos $endpos)),
                               None)) $2 }
  (*| BEGIN ext_attributes seq_expr error
      { unclosed("begin", 1, "end", 3) }*)
  | LPAREN seq_expr type_constraint RPAREN
      { (mkexp_constraint ~loc:(make_loc $symbolstartpos $endpos)) $2 $3 }
  | simple_expr DOT mkrhs(label_longident)
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_field($1, $3)) }
  | mkrhs(mod_longident) DOT LPAREN seq_expr RPAREN
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_open(Fresh, $1, $4)) }
  | mkrhs(mod_longident) DOT LPAREN RPAREN
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_open(Fresh, $1,
                        (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_construct((mkrhs (Lident "()") (make_loc $startpos($1) $endpos($1))), None)))) }
  (*| mod_longident DOT LPAREN seq_expr error
      { unclosed("(", 3, ")", 5) }*)
  | simple_expr DOT LPAREN seq_expr RPAREN
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_apply((ghexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_ident((array_function ~loc:(make_loc $symbolstartpos $endpos)) "Array" "get")),
                         [Nolabel,$1; Nolabel,$4])) }
  (*| simple_expr DOT LPAREN seq_expr error
      { unclosed("(", 3, ")", 5) }*)
  | simple_expr DOT LBRACKET seq_expr RBRACKET
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_apply((ghexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_ident((array_function ~loc:(make_loc $symbolstartpos $endpos)) "String" "get")),
                         [Nolabel,$1; Nolabel,$4])) }
  (*| simple_expr DOT LBRACKET seq_expr error
      { unclosed("[", 3, "]", 5) }*)
  | simple_expr DOT LBRACE expr RBRACE
      { (bigarray_get ~loc:(make_loc $symbolstartpos $endpos)) $1 $4 }
  (*| simple_expr DOT LBRACE expr_comma_list error
      { unclosed("{", 3, "}", 5) }*)
  | LBRACE record_expr RBRACE
      { let (exten, fields) = $2 in (mkexp ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_record(fields, exten)) }
  (*| LBRACE record_expr error
      { unclosed("{", 1, "}", 3) }*)
  | mkrhs(mod_longident) DOT LBRACE record_expr RBRACE
      { let (exten, fields) = $4 in
        let rec_exp = (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_record(fields, exten)) in
        (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_open(Fresh, $1, rec_exp)) }
  (*| mod_longident DOT LBRACE record_expr error
      { unclosed("{", 3, "}", 5) }*)
  | LBRACKETBAR expr_semi_list opt_semi BARRBRACKET
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_array(List.rev $2)) }
  (*| LBRACKETBAR expr_semi_list opt_semi error
      { unclosed("[|", 1, "|]", 4) }*)
  | LBRACKETBAR BARRBRACKET
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_array []) }
  | mkrhs(mod_longident) DOT LBRACKETBAR expr_semi_list opt_semi BARRBRACKET
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_open(Fresh, $1, (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_array(List.rev $4)))) }
  | mkrhs(mod_longident) DOT LBRACKETBAR BARRBRACKET
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_open(Fresh, $1, (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_array []))) }
  (*| mod_longident DOT LBRACKETBAR expr_semi_list opt_semi error
      { unclosed("[|", 3, "|]", 6) }*)
  | LBRACKET expr_semi_list opt_semi RBRACKET
      { (reloc_exp ~loc:(make_loc $symbolstartpos $endpos)) (mktailexp ((make_loc $startpos($4) $endpos($4))) (List.rev $2)) }
  (*| LBRACKET expr_semi_list opt_semi error
      { unclosed("[", 1, "]", 4) }*)
  | mkrhs(mod_longident) DOT LBRACKET expr_semi_list opt_semi RBRACKET
      { let list_exp = (reloc_exp ~loc:(make_loc $symbolstartpos $endpos)) (mktailexp ((make_loc $startpos($6) $endpos($6))) (List.rev $4)) in
        (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_open(Fresh, $1, list_exp)) }
  | mkrhs(mod_longident) DOT LBRACKET RBRACKET
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_open(Fresh, $1,
                        (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_construct((mkrhs (Lident "[]") (make_loc $startpos($1) $endpos($1))), None)))) }
  (*| mod_longident DOT LBRACKET expr_semi_list opt_semi error
      { unclosed("[", 3, "]", 6) }*)
  | PREFIXOP simple_expr
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_apply((mkoperator $1 (make_loc $startpos($1) $endpos($1))), [Nolabel,$2])) }
  | BANG simple_expr
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_apply((mkoperator "!" (make_loc $startpos($1) $endpos($1))), [Nolabel,$2])) }
  | NEW ext_attributes mkrhs(class_longident)
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_new $3) $2 }
  | LBRACELESS field_expr_list GREATERRBRACE
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_override $2) }
  (*| LBRACELESS field_expr_list error
      { unclosed("{<", 1, ">}", 3) }*)
  | LBRACELESS GREATERRBRACE
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_override [])}
  | mkrhs(mod_longident) DOT LBRACELESS field_expr_list GREATERRBRACE
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_open(Fresh, $1, (mkexp ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_override $4)))}
  (*| mod_longident DOT LBRACELESS field_expr_list error
      { unclosed("{<", 3, ">}", 5) }*)
  | simple_expr SHARP mkrhs(label)
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_send($1, $3)) }
  | simple_expr HASHOP simple_expr
      { (mkinfix ~loc:(make_loc $symbolstartpos $endpos) ~oploc:(make_loc $startpos($2) $endpos($2))) $1 $2 $3 }
  | LPAREN MODULE ext_attributes module_expr RPAREN
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_pack $4) $3 }
  | LPAREN MODULE ext_attributes module_expr COLON package_type RPAREN
      { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_constraint ((ghexp ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_pack $4),
                                      (ghtyp ~loc:(make_loc $symbolstartpos $endpos)) (Ptyp_package $6)))
                    $3 }
  (*| LPAREN MODULE ext_attributes module_expr COLON error
      { unclosed("(", 1, ")", 6) }*)
  | mkrhs(mod_longident) DOT LPAREN MODULE ext_attributes module_expr COLON package_type RPAREN
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_open(Fresh, $1,
        (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_constraint ((ghexp ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_pack $6),
                                (ghtyp ~loc:(make_loc $symbolstartpos $endpos)) (Ptyp_package $8)))
                    $5 )) }
  (*| mod_longident DOT LPAREN MODULE ext_attributes module_expr COLON error
      { unclosed("(", 3, ")", 8) }*)
  | extension
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_extension $1) }
  | QUESTIONQUESTION
      { let id = mkloc "merlin.hole" ((make_loc $symbolstartpos $endpos)) in
        (mkexp ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_extension (id, PStr [])) }
;
simple_labeled_expr_list:
    labeled_simple_expr
      { [$1] }
  | simple_labeled_expr_list labeled_simple_expr
      { $2 :: $1 }
;
labeled_simple_expr:
    simple_expr %prec below_SHARP
      { (Nolabel, $1) }
  | label_expr
      { $1 }
;
label_expr:
    LABEL simple_expr %prec below_SHARP
      { (Labelled $1, $2) }
  | TILDE label_ident
      { (Labelled (fst $2), snd $2) }
  | QUESTION label_ident
      { (Optional (fst $2), snd $2) }
  | OPTLABEL simple_expr %prec below_SHARP
      { (Optional $1, $2) }
;
label_ident:
    LIDENT { ($1, (mkexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_ident((mkrhs (Lident $1) (make_loc $startpos($1) $endpos($1)))))) }
;
lident_list:
    mkrhs(LIDENT) { [$1] }
  | mkrhs(LIDENT) lident_list { $1 :: $2 }
;
let_binding_body:
    val_ident fun_binding
      { ((mkpatvar $1 (make_loc $startpos($1) $endpos($1))), $2) }
  | val_ident COLON typevar_list DOT core_type EQUAL seq_expr
      { ((ghpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_constraint((mkpatvar $1 (make_loc $startpos($1) $endpos($1))),
                               (ghtyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_poly(List.rev $3,$5)))),
         $7) }
  | val_ident COLON TYPE lident_list DOT core_type EQUAL seq_expr
      { let exp, poly = (wrap_type_annotation ~loc:(make_loc $symbolstartpos $endpos)) $4 $6 $8 in
        ((ghpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_constraint((mkpatvar $1 (make_loc $startpos($1) $endpos($1))), poly)), exp) }
  | pattern_no_exn EQUAL seq_expr
      { ($1, $3) }
  | simple_pattern_not_ident COLON core_type EQUAL seq_expr
      { ((ghpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_constraint($1, $3)), $5) }
;
let_bindings:
    let_binding { $1 }
  | let_bindings and_let_binding { addlb $1 $2 }
;
let_binding:
    LET ext_attributes rec_flag let_binding_body post_item_attributes
      { let (ext, attr) = $2 in
        (mklbs ~loc:(make_loc $symbolstartpos $endpos)) ext $3 ((mklb ~loc:(make_loc $symbolstartpos $endpos)) true $4 (attr@$5)) }
;
and_let_binding:
    AND attributes let_binding_body post_item_attributes
      { (mklb ~loc:(make_loc $symbolstartpos $endpos)) false $3 ($2@$4) }
;
fun_binding:
    strict_binding
      { $1 }
  | type_constraint EQUAL seq_expr
      { (mkexp_constraint ~loc:(make_loc $symbolstartpos $endpos)) $3 $1 }
;
strict_binding:
    EQUAL seq_expr
      { $2 }
  | labeled_simple_pattern fun_binding
      { let (l, o, p) = $1 in (ghexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_fun(l, o, p, $2)) }
  | LPAREN TYPE lident_list RPAREN fun_binding
      { (mk_newtypes ~loc:(make_loc $symbolstartpos $endpos)) $3 $5 }
;
match_cases [@recovery []]:
    match_case { [$1] }
  | match_cases BAR match_case { $3 :: $1 }
;
match_case:
    pattern MINUSGREATER seq_expr
      { Exp.case $1 (merloc $endpos($2) $3) }
  | pattern WHEN seq_expr MINUSGREATER seq_expr
      { Exp.case $1 ~guard:(merloc $endpos($2) $3) (merloc $endpos($4) $5) }
  | pattern MINUSGREATER DOT [@cost infinity]
      { Exp.case $1 (merloc $endpos($2)
                       (Exp.unreachable ~loc:((make_loc $startpos($3) $endpos($3))) ())) }
;
fun_def:
    MINUSGREATER seq_expr
      { (merloc $endpos($1) $2) }
  | COLON simple_core_type MINUSGREATER seq_expr
      { (mkexp ~loc:(make_loc $symbolstartpos $endpos)) (Pexp_constraint ((merloc $endpos($3) $4), $2)) }

  | labeled_simple_pattern fun_def
      {
       let (l,o,p) = $1 in
       (ghexp ~loc:(make_loc $symbolstartpos $endpos))(Pexp_fun(l, o, p, $2))
      }
  | LPAREN TYPE lident_list RPAREN fun_def
      { (mk_newtypes ~loc:(make_loc $symbolstartpos $endpos)) $3 $5 }
;
expr_comma_list:
    expr_comma_list COMMA expr { $3 :: $1 }
  | expr COMMA expr { [$3; $1] }
;
record_expr:
    simple_expr WITH lbl_expr_list { (Some $1, $3) }
  | lbl_expr_list { (None, $1) }
;
lbl_expr_list:
     lbl_expr { [$1] }
  | lbl_expr SEMI lbl_expr_list { $1 :: $3 }
  | lbl_expr SEMI { [$1] }
;
lbl_expr:
    mkrhs(label_longident) opt_type_constraint EQUAL expr
      { ($1, (mkexp_opt_constraint ~loc:(make_loc $symbolstartpos $endpos)) $4 $2) }
  | mkrhs(label_longident) opt_type_constraint
      { ($1, (mkexp_opt_constraint ~loc:(make_loc $symbolstartpos $endpos)) ((exp_of_label ~loc:(make_loc $symbolstartpos $endpos) $1.Location.txt (make_loc $startpos($1) $endpos($1)))) $2) }
;
field_expr_list:
    field_expr opt_semi { [$1] }
  | field_expr SEMI field_expr_list { $1 :: $3 }
;
field_expr:
    mkrhs(label) EQUAL expr
      { ($1, $3) }
  | mkrhs(label)
      { ($1, (exp_of_label ~loc:(make_loc $symbolstartpos $endpos) (Lident $1.Location.txt) (make_loc $startpos($1) $endpos($1)))) }
;
expr_semi_list:
    expr { [$1] }
  | expr_semi_list SEMI expr { $3 :: $1 }
;
type_constraint:
    COLON core_type { (Some $2, None) }
  | COLON core_type COLONGREATER core_type { (Some $2, Some $4) }
  | COLONGREATER core_type { (None, Some $2) }
  (*| COLON error { (syntax_error (make_loc $startpos($) $endpos($))) }*)
  (*| COLONGREATER error { (syntax_error (make_loc $startpos($) $endpos($))) }*)
;
opt_type_constraint:
    type_constraint { Some $1 }
  | { None }
;



pattern [@recovery default_pattern ()]:
  | pattern AS mkrhs(val_ident)
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_alias($1, $3)) }
  (*| pattern AS error
      { expecting(3, "identifier") }*)
  | pattern_comma_list %prec below_COMMA
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_tuple(List.rev $1)) }
  | pattern COLONCOLON pattern
      { mkpat_cons ((make_loc $startpos($2) $endpos($2))) ((ghpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_tuple[$1;$3])) ((make_loc $symbolstartpos $endpos)) }
  (*| pattern COLONCOLON error
      { expecting(3, "pattern") }*)
  | pattern BAR pattern
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_or($1, $3)) }
  (*| pattern BAR error
      { expecting(3, "pattern") }*)
  | EXCEPTION ext_attributes pattern %prec prec_constr_appl
      { (mkpat_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Ppat_exception $3) $2 }
  | pattern attribute
      { Pat.attr $1 $2 }
  | pattern_gen { $1 }
;
pattern_no_exn:
  | pattern_no_exn AS mkrhs(val_ident)
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_alias($1, $3)) }
  (*| pattern_no_exn AS error
      { expecting 3 "identifier" }*)
  | pattern_no_exn_comma_list %prec below_COMMA
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_tuple(List.rev $1)) }
  | pattern_no_exn COLONCOLON pattern
      { mkpat_cons ((make_loc $startpos($2) $endpos($2))) ((ghpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_tuple[$1;$3])) ((make_loc $symbolstartpos $endpos)) }
  (*| pattern_no_exn COLONCOLON error
      { expecting 3 "pattern" }*)
  | pattern_no_exn BAR pattern
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_or($1, $3)) }
  (*| pattern_no_exn BAR error
      { expecting 3 "pattern" }*)
  | pattern_no_exn attribute
      { Pat.attr $1 $2 }
  | pattern_gen { $1 }
;
pattern_gen:
    simple_pattern
      { $1 }
  | mkrhs(constr_longident) pattern %prec prec_constr_appl
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_construct($1, Some $2)) }
  | name_tag pattern %prec prec_constr_appl
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_variant($1, Some $2)) }
  | LPAREN COLONCOLON RPAREN LPAREN pattern COMMA pattern RPAREN
      { mkpat_cons ((make_loc $startpos($2) $endpos($2))) ((ghpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_tuple[$5;$7])) ((make_loc $symbolstartpos $endpos)) }
  (*| LPAREN COLONCOLON RPAREN LPAREN pattern COMMA pattern error
      { unclosed "(" 4 ")" 8 }*)
  | LAZY ext_attributes simple_pattern
      { (mkpat_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Ppat_lazy $3) $2}
;
simple_pattern:
    mkrhs(val_ident) %prec below_EQUAL
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_var $1) }
  | simple_pattern_not_ident { $1 }
;
simple_pattern_not_ident:
  | UNDERSCORE
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_any) }
  | signed_constant
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_constant $1) }
  | signed_constant DOTDOT signed_constant
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_interval ($1, $3)) }
  | mkrhs(constr_longident)
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_construct($1, None)) }
  | name_tag
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_variant($1, None)) }
  | SHARP mkrhs(type_longident)
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_type $2) }
  | simple_delimited_pattern
      { $1 }
  | mkrhs(mod_longident) DOT simple_delimited_pattern
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos)) @@ Ppat_open($1, $3) }
  | mkrhs(mod_longident) DOT LBRACKET RBRACKET
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos)) @@ Ppat_open($1, (mkpat ~loc:(make_loc $symbolstartpos $endpos)) @@
               Ppat_construct ((mkrhs (Lident "[]") (make_loc $startpos($4) $endpos($4))), None)) }
  | mkrhs(mod_longident) DOT LPAREN RPAREN
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos)) @@ Ppat_open($1, (mkpat ~loc:(make_loc $symbolstartpos $endpos)) @@
                 Ppat_construct ((mkrhs (Lident "()") (make_loc $startpos($4) $endpos($4))), None) ) }
  | mkrhs(mod_longident) DOT LPAREN pattern RPAREN
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos)) @@ Ppat_open ($1, $4) }
  (*| mod_longident DOT LPAREN pattern error
      {unclosed "(" 3 ")" 5 }*)
  (*| mod_longident DOT LPAREN error
      { expecting 4 "pattern" }*)
  | LPAREN pattern RPAREN
      { (reloc_pat ~loc:(make_loc $symbolstartpos $endpos)) $2 }
  (*| LPAREN pattern error
      { unclosed("(", 1, ")", 3) }*)
  | LPAREN pattern COLON core_type RPAREN
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_constraint($2, $4)) }
  (*| LPAREN pattern COLON core_type error
      { unclosed("(", 1, ")", 5) }*)
  (*| LPAREN pattern COLON error
      { expecting(4, "type") }*)
  | LPAREN MODULE ext_attributes mkrhs(UIDENT) RPAREN
      { (mkpat_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Ppat_unpack $4) $3 }
  | LPAREN MODULE ext_attributes mkrhs(UIDENT) COLON package_type RPAREN
      { (mkpat_attrs ~loc:(make_loc $symbolstartpos $endpos))
          (Ppat_constraint((mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_unpack $4),
                           (ghtyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_package $6)))
          $3 }
  (*| LPAREN MODULE ext_attributes UIDENT COLON package_type error
      { unclosed("(", 1, ")", 7) }*)
  | extension
      { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_extension $1) }
;

simple_delimited_pattern:
  | LBRACE lbl_pattern_list RBRACE
    { let (fields, closed) = $2 in (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_record(fields, closed)) }
  (*| LBRACE lbl_pattern_list error
    { unclosed "{" 1 "}" 3 }*)
  | LBRACKET pattern_semi_list opt_semi RBRACKET
    { (reloc_pat ~loc:(make_loc $symbolstartpos $endpos)) (mktailpat ((make_loc $startpos($4) $endpos($4))) (List.rev $2)) }
  (*| LBRACKET pattern_semi_list opt_semi error
    { unclosed "[" 1 "]" 4 }*)
  | LBRACKETBAR pattern_semi_list opt_semi BARRBRACKET
    { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_array(List.rev $2)) }
  | LBRACKETBAR BARRBRACKET
    { (mkpat ~loc:(make_loc $symbolstartpos $endpos))(Ppat_array []) }
  (*| LBRACKETBAR pattern_semi_list opt_semi error
    { unclosed "[|" 1 "|]" 4 }*)

pattern_comma_list:
    pattern_comma_list COMMA pattern { $3 :: $1 }
  | pattern COMMA pattern { [$3; $1] }
  (*| pattern COMMA error { expecting(3, "pattern") }*)
;
pattern_semi_list:
    pattern { [$1] }
  | pattern_semi_list SEMI pattern { $3 :: $1 }
;
pattern_no_exn_comma_list:
    pattern_no_exn_comma_list COMMA pattern { $3 :: $1 }
  | pattern_no_exn COMMA pattern { [$3; $1] }
  (*| pattern_no_exn COMMA error { expecting 3 "pattern" }*)
;
lbl_pattern_list:
    lbl_pattern { [$1], Closed }
  | lbl_pattern SEMI { [$1], Closed }
  | lbl_pattern SEMI UNDERSCORE opt_semi { [$1], Open }
  | lbl_pattern SEMI lbl_pattern_list
      { let (fields, closed) = $3 in $1 :: fields, closed }
;
lbl_pattern:
    mkrhs(label_longident) opt_pattern_type_constraint EQUAL pattern
      { ($1, (mkpat_opt_constraint ~loc:(make_loc $symbolstartpos $endpos)) $4 $2) }
  | mkrhs(label_longident) opt_pattern_type_constraint
      { ($1, (mkpat_opt_constraint ~loc:(make_loc $symbolstartpos $endpos)) ((pat_of_label ~loc:(make_loc $symbolstartpos $endpos) $1.Location.txt (make_loc $startpos($1) $endpos($1)))) $2) }
;
opt_pattern_type_constraint:
    COLON core_type { Some $2 }
  | { None }
;



value_description:
    VAL ext_attributes mkrhs(val_ident) COLON core_type post_item_attributes
      { let (ext, attrs) = $2 in
        Val.mk $3 $5 ~attrs:(attrs@$6)
               ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
      , ext }
;



primitive_declaration_body:
    STRING { [fst $1] }
  | STRING primitive_declaration_body { fst $1 :: $2 }
;
primitive_declaration:
    EXTERNAL ext_attributes mkrhs(val_ident) COLON core_type EQUAL
    primitive_declaration_body post_item_attributes
      { let (ext, attrs) = $2 in
        Val.mk $3 $5 ~prim:$7 ~attrs:(attrs@$8)
               ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
      , ext }
;



type_declarations:
    type_declaration
      { let (nonrec_flag, ty, ext) = $1 in (nonrec_flag, [ty], ext) }
  | type_declarations and_type_declaration
      { let (nonrec_flag, tys, ext) = $1 in (nonrec_flag, $2 :: tys, ext) }
;

type_declaration:
    TYPE ext_attributes nonrec_flag optional_type_parameters mkrhs(LIDENT)
    type_kind constraints post_item_attributes
      { let (kind, priv, manifest) = $6 in
        let (ext, attrs) = $2 in
        let ty =
          Type.mk $5 ~params:$4 ~cstrs:(List.rev $7) ~kind
            ~priv ?manifest ~attrs:(attrs@$8)
            ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
        in
          ($3, ty, ext) }
;
and_type_declaration:
    AND attributes optional_type_parameters mkrhs(LIDENT) type_kind constraints
    post_item_attributes
      { let (kind, priv, manifest) = $5 in
          Type.mk $4 ~params:$3 ~cstrs:(List.rev $6)
            ~kind ~priv ?manifest ~attrs:($2@$7) ~loc:(((make_loc $symbolstartpos $endpos)))
            ~text:((symbol_text $startpos)) ~docs:((symbol_docs $symbolstartpos $endpos)) }
;
constraints:
        constraints CONSTRAINT constrain { $3 :: $1 }
      | { [] }
;
type_kind:

      { (Ptype_abstract, Public, None) }
  | EQUAL core_type
      { (Ptype_abstract, Public, Some $2) }
  | EQUAL PRIVATE core_type
      { (Ptype_abstract, Private, Some $3) }
  | EQUAL constructor_declarations
      { (Ptype_variant(List.rev $2), Public, None) }
  | EQUAL PRIVATE constructor_declarations
      { (Ptype_variant(List.rev $3), Private, None) }
  | EQUAL DOTDOT
      { (Ptype_open, Public, None) }
  | EQUAL private_flag LBRACE label_declarations RBRACE
      { (Ptype_record $4, $2, None) }
  | EQUAL core_type EQUAL private_flag constructor_declarations
      { (Ptype_variant(List.rev $5), $4, Some $2) }
  | EQUAL core_type EQUAL DOTDOT
      { (Ptype_open, Public, Some $2) }
  | EQUAL core_type EQUAL private_flag LBRACE label_declarations RBRACE
      { (Ptype_record $6, $4, Some $2) }
;
optional_type_parameters:
                                                { [] }
  | optional_type_parameter { [$1] }
  | LPAREN optional_type_parameter_list RPAREN { List.rev $2 }
;
optional_type_parameter:
    type_variance optional_type_variable { $2, $1 }
;
optional_type_parameter_list:
    optional_type_parameter { [$1] }
  | optional_type_parameter_list COMMA optional_type_parameter { $3 :: $1 }
;
optional_type_variable:
    QUOTE ident { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_var $2) }
  | UNDERSCORE { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_any) }
;


type_parameters:
                                                { [] }
  | type_parameter { [$1] }
  | LPAREN type_parameter_list RPAREN { List.rev $2 }
;
type_parameter:
    type_variance type_variable { $2, $1 }
;
type_variance:
                                                { Invariant }
  | PLUS { Covariant }
  | MINUS { Contravariant }
;
type_variable:
    QUOTE ident { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_var $2) }
;
type_parameter_list:
    type_parameter { [$1] }
  | type_parameter_list COMMA type_parameter { $3 :: $1 }
;
constructor_declarations:
    constructor_declaration { [$1] }
  | bar_constructor_declaration { [$1] }
  | constructor_declarations bar_constructor_declaration { $2 :: $1 }
;
constructor_declaration:
  | mkrhs(constr_ident) generalized_constructor_arguments attributes
      {
       let args,res = $2 in
       Type.constructor $1 ~args ?res ~attrs:$3
         ~loc:(((make_loc $symbolstartpos $endpos))) ~info:((symbol_info $endpos))
      }
;
bar_constructor_declaration:
  | BAR mkrhs(constr_ident) generalized_constructor_arguments attributes
      {
       let args,res = $3 in
       Type.constructor $2 ~args ?res ~attrs:$4
         ~loc:(((make_loc $symbolstartpos $endpos))) ~info:((symbol_info $endpos))
      }
;
str_exception_declaration:
  | sig_exception_declaration { $1 }
  | EXCEPTION ext_attributes mkrhs(constr_ident) EQUAL mkrhs(constr_longident) attributes
    post_item_attributes
      { let (ext, attrs) = $2 in
        Te.rebind $3 $5 ~attrs:(attrs @ $6 @ $7)
          ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
        , ext }
;
sig_exception_declaration:
  | EXCEPTION ext_attributes mkrhs(constr_ident) generalized_constructor_arguments
    attributes post_item_attributes
      { let args, res = $4 in
        let (ext,attrs) = $2 in
          Te.decl $3 ~args ?res ~attrs:(attrs @ $5 @ $6)
            ~loc:(((make_loc $symbolstartpos $endpos))) ~docs:((symbol_docs $symbolstartpos $endpos))
        , ext }
;
let_exception_declaration:
    mkrhs(constr_ident) generalized_constructor_arguments attributes
      { let args, res = $2 in
        Te.decl $1 ~args ?res ~attrs:$3 ~loc:(((make_loc $symbolstartpos $endpos))) }
;
generalized_constructor_arguments:
                                  { (Pcstr_tuple [],None) }
  | OF constructor_arguments { ($2,None) }
  | COLON constructor_arguments MINUSGREATER simple_core_type
                                  { ($2,Some $4) }
  | COLON simple_core_type
                                  { (Pcstr_tuple [],Some $2) }
;

constructor_arguments:
  | core_type_list { Pcstr_tuple (List.rev $1) }
  | LBRACE label_declarations RBRACE { Pcstr_record $2 }
;
label_declarations:
    label_declaration { [$1] }
  | label_declaration_semi { [$1] }
  | label_declaration_semi label_declarations { $1 :: $2 }
;
label_declaration:
    mutable_flag mkrhs(label) COLON poly_type_no_attr attributes
      {
       Type.field $2 $4 ~mut:$1 ~attrs:$5
         ~loc:(((make_loc $symbolstartpos $endpos))) ~info:((symbol_info $endpos))
      }
;
label_declaration_semi:
    mutable_flag mkrhs(label) COLON poly_type_no_attr attributes SEMI attributes
      {
       let info =
         match (rhs_info $endpos($5)) with
         | Some _ as info_before_semi -> info_before_semi
         | None -> (symbol_info $endpos)
       in
       Type.field $2 $4 ~mut:$1 ~attrs:($5 @ $7)
         ~loc:(((make_loc $symbolstartpos $endpos))) ~info
      }
;



str_type_extension:
  TYPE ext_attributes nonrec_flag optional_type_parameters mkrhs(type_longident)
  PLUSEQ private_flag str_extension_constructors post_item_attributes
      { let (ext, attrs) = $2 in
        if $3 <> Recursive then (not_expecting (make_loc $startpos($3) $endpos($3)) "nonrec flag");
        Te.mk $5 (List.rev $8) ~params:$4 ~priv:$7
          ~attrs:(attrs@$9) ~docs:((symbol_docs $symbolstartpos $endpos))
        , ext }
;
sig_type_extension:
  TYPE ext_attributes nonrec_flag optional_type_parameters mkrhs(type_longident)
  PLUSEQ private_flag sig_extension_constructors post_item_attributes
      { let (ext, attrs) = $2 in
        if $3 <> Recursive then (not_expecting (make_loc $startpos($3) $endpos($3)) "nonrec flag");
        Te.mk $5 (List.rev $8) ~params:$4 ~priv:$7
          ~attrs:(attrs @ $9) ~docs:((symbol_docs $symbolstartpos $endpos))
        , ext }
;
str_extension_constructors:
    extension_constructor_declaration { [$1] }
  | bar_extension_constructor_declaration { [$1] }
  | extension_constructor_rebind { [$1] }
  | bar_extension_constructor_rebind { [$1] }
  | str_extension_constructors bar_extension_constructor_declaration
      { $2 :: $1 }
  | str_extension_constructors bar_extension_constructor_rebind
      { $2 :: $1 }
;
sig_extension_constructors:
    extension_constructor_declaration { [$1] }
  | bar_extension_constructor_declaration { [$1] }
  | sig_extension_constructors bar_extension_constructor_declaration
      { $2 :: $1 }
;
extension_constructor_declaration:
  | mkrhs(constr_ident) generalized_constructor_arguments attributes
      { let args, res = $2 in
        Te.decl $1 ~args ?res ~attrs:$3
          ~loc:(((make_loc $symbolstartpos $endpos))) ~info:((symbol_info $endpos)) }
;
bar_extension_constructor_declaration:
  | BAR mkrhs(constr_ident) generalized_constructor_arguments attributes
      { let args, res = $3 in
        Te.decl $2 ~args ?res ~attrs:$4
           ~loc:(((make_loc $symbolstartpos $endpos))) ~info:((symbol_info $endpos)) }
;
extension_constructor_rebind:
  | mkrhs(constr_ident) EQUAL mkrhs(constr_longident) attributes
      { Te.rebind $1 $3 ~attrs:$4
          ~loc:(((make_loc $symbolstartpos $endpos))) ~info:((symbol_info $endpos)) }
;
bar_extension_constructor_rebind:
  | BAR mkrhs(constr_ident) EQUAL mkrhs(constr_longident) attributes
      { Te.rebind $2 $4 ~attrs:$5
          ~loc:(((make_loc $symbolstartpos $endpos))) ~info:((symbol_info $endpos)) }
;



with_constraints:
    with_constraint { [$1] }
  | with_constraints AND with_constraint { $3 :: $1 }
;
with_constraint:
    TYPE type_parameters label_longident with_type_binder core_type_no_attr
    constraints
      { Pwith_type
          ((mkrhs $3 (make_loc $startpos($3) $endpos($3))),
           (Type.mk ((mkrhs (Longident.last $3) (make_loc $startpos($3) $endpos($3))))
              ~params:$2
              ~cstrs:(List.rev $6)
              ~manifest:$5
              ~priv:$4
              ~loc:((make_loc $symbolstartpos $endpos)))) }


  | TYPE type_parameters mkrhs(label) COLONEQUAL core_type_no_attr
      { Pwith_typesubst
          (Type.mk $3
             ~params:$2
             ~manifest:$5
             ~loc:((make_loc $symbolstartpos $endpos))) }
  | MODULE mkrhs(mod_longident) EQUAL mkrhs(mod_ext_longident)
      { Pwith_module ($2, $4) }
  | MODULE mkrhs(UIDENT) COLONEQUAL mkrhs(mod_ext_longident)
      { Pwith_modsubst ($2, $4) }
;
with_type_binder:
    EQUAL { Public }
  | EQUAL PRIVATE { Private }
;



typevar_list:
        QUOTE mkrhs(ident) { [$2] }
      | typevar_list QUOTE mkrhs(ident) { $3 :: $1 }
;
poly_type:
        core_type
          { $1 }
      | typevar_list DOT core_type
          { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_poly(List.rev $1, $3)) }
;
poly_type_no_attr:
        core_type_no_attr
          { $1 }
      | typevar_list DOT core_type_no_attr
          { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_poly(List.rev $1, $3)) }
;



core_type:
    core_type_no_attr
      { $1 }
  | core_type attribute
      { Typ.attr $1 $2 }
;
core_type_no_attr:
    core_type2 %prec MINUSGREATER
      { $1 }
  | core_type2 AS QUOTE ident
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_alias($1, $4)) }
;
core_type2:
    simple_core_type_or_tuple
      { $1 }
  | QUESTION LIDENT COLON core_type2 MINUSGREATER core_type2
      { let param = (extra_rhs_core_type $4 $endpos($4)) in
        (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_arrow(Optional $2, param, $6)) }
  | OPTLABEL core_type2 MINUSGREATER core_type2
      { let param = (extra_rhs_core_type $2 $endpos($2)) in
        (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_arrow(Optional $1, param, $4)) }
  | LIDENT COLON core_type2 MINUSGREATER core_type2
      { let param = (extra_rhs_core_type $3 $endpos($3)) in
        (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_arrow(Labelled $1, param, $5)) }
  | core_type2 MINUSGREATER core_type2
      { let param = (extra_rhs_core_type $1 $endpos($1)) in
        (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_arrow(Nolabel, param, $3)) }
;

simple_core_type:
    simple_core_type2 %prec below_SHARP
      { $1 }
  | LPAREN core_type_comma_list RPAREN %prec below_SHARP
      { match $2 with [sty] -> sty | _ ->
        raise_error (Syntaxerr.Error(Syntaxerr.Other (make_loc $symbolstartpos $endpos)));
        (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_any) }
;

simple_core_type2:
    QUOTE ident
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_var $2) }
  | UNDERSCORE
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_any) }
  | mkrhs(type_longident)
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_constr($1, [])) }
  | simple_core_type2 mkrhs(type_longident)
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_constr($2, [$1])) }
  | LPAREN core_type_comma_list RPAREN mkrhs(type_longident)
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_constr($4, List.rev $2)) }
  | LESS meth_list GREATER
      { let (f, c) = $2 in (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_object (f, c)) }
  | LESS GREATER
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_object ([], Closed)) }
  | SHARP mkrhs(class_longident)
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_class($2, [])) }
  | simple_core_type2 SHARP mkrhs(class_longident)
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_class($3, [$1])) }
  | LPAREN core_type_comma_list RPAREN SHARP mkrhs(class_longident)
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_class($5, List.rev $2)) }
  | LBRACKET tag_field RBRACKET
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_variant([$2], Closed, None)) }




  | LBRACKET BAR row_field_list RBRACKET
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_variant(List.rev $3, Closed, None)) }
  | LBRACKET row_field BAR row_field_list RBRACKET
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_variant($2 :: List.rev $4, Closed, None)) }
  | LBRACKETGREATER opt_bar row_field_list RBRACKET
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_variant(List.rev $3, Open, None)) }
  | LBRACKETGREATER RBRACKET
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_variant([], Open, None)) }
  | LBRACKETLESS opt_bar row_field_list RBRACKET
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_variant(List.rev $3, Closed, Some [])) }
  | LBRACKETLESS opt_bar row_field_list GREATER name_tag_list RBRACKET
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_variant(List.rev $3, Closed, Some (List.rev $5))) }
  | LPAREN MODULE ext_attributes package_type RPAREN
      { (mktyp_attrs ~loc:(make_loc $symbolstartpos $endpos)) (Ptyp_package $4) $3 }
  | extension
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos)) (Ptyp_extension $1) }
;
package_type:
    module_type { package_type_of_module_type $1 }
;
row_field_list:
    row_field { [$1] }
  | row_field_list BAR row_field { $3 :: $1 }
;
row_field:
    tag_field { $1 }
  | simple_core_type { Rinherit $1 }
;
tag_field:
    name_tag OF opt_ampersand amper_type_list attributes
      { Rtag ($1, add_info_attrs ((symbol_info $endpos)) $5, $3, List.rev $4) }
  | name_tag attributes
      { Rtag ($1, add_info_attrs ((symbol_info $endpos)) $2, true, []) }
;
opt_ampersand:
    AMPERSAND { true }
  | { false }
;
amper_type_list:
    core_type_no_attr { [$1] }
  | amper_type_list AMPERSAND core_type_no_attr { $3 :: $1 }
;
name_tag_list:
    name_tag { [$1] }
  | name_tag_list name_tag { $2 :: $1 }
;
simple_core_type_or_tuple:
    simple_core_type { $1 }
  | simple_core_type STAR core_type_list
      { (mktyp ~loc:(make_loc $symbolstartpos $endpos))(Ptyp_tuple($1 :: List.rev $3)) }
;
core_type_comma_list:
    core_type { [$1] }
  | core_type_comma_list COMMA core_type { $3 :: $1 }
;
core_type_list:
    simple_core_type { [$1] }
  | core_type_list STAR simple_core_type { $3 :: $1 }
;
meth_list:
    field_semi meth_list { let (f, c) = $2 in ($1 :: f, c) }
  | field_semi { [$1], Closed }
  | field { [$1], Closed }
  | DOTDOT { [], Open }
;
field:
  mkrhs(label) COLON poly_type_no_attr attributes
    { ($1, add_info_attrs ((symbol_info $endpos)) $4, $3) }
;

field_semi:
  mkrhs(label) COLON poly_type_no_attr attributes SEMI attributes
    { let info =
        match (rhs_info $endpos($4)) with
        | Some _ as info_before_semi -> info_before_semi
        | None -> (symbol_info $endpos)
      in
      ($1, add_info_attrs info ($4 @ $6), $3) }
;

label:
    LIDENT { $1 }
;



constant:
  | INT { let (n, m) = $1 in Pconst_integer (n, m) }
  | CHAR { Pconst_char $1 }
  | STRING { let (s, d) = $1 in Pconst_string (s, d) }
  | FLOAT { let (f, m) = $1 in Pconst_float (f, m) }
;
signed_constant:
    constant { $1 }
  | MINUS INT { let (n, m) = $2 in Pconst_integer("-" ^ n, m) }
  | MINUS FLOAT { let (f, m) = $2 in Pconst_float("-" ^ f, m) }
  | PLUS INT { let (n, m) = $2 in Pconst_integer (n, m) }
  | PLUS FLOAT { let (f, m) = $2 in Pconst_float(f, m) }
;



ident:
    UIDENT { $1 }
  | LIDENT { $1 }
;
val_ident:
    LIDENT { $1 }
  | LPAREN operator RPAREN { $2 }
  (*| LPAREN operator error { unclosed("(", 1, ")", 3) }*)
  (*| LPAREN error { expecting(2, "operator") }*)
  (*| LPAREN MODULE error { expecting(3, "module-expr") }*)
;
operator:
    PREFIXOP { $1 }
  | INFIXOP0 { $1 }
  | INFIXOP1 { $1 }
  | INFIXOP2 { $1 }
  | INFIXOP3 { $1 }
  | INFIXOP4 { $1 }
  | HASHOP { $1 }
  | BANG { "!" }
  | PLUS { "+" }
  | PLUSDOT { "+." }
  | MINUS { "-" }
  | MINUSDOT { "-." }
  | STAR { "*" }
  | EQUAL { "=" }
  | LESS { "<" }
  | GREATER { ">" }
  | OR { "or" }
  | BARBAR { "||" }
  | AMPERSAND { "&" }
  | AMPERAMPER { "&&" }
  | COLONEQUAL { ":=" }
  | PLUSEQ { "+=" }
  | PERCENT { "%" }
;
constr_ident:
    UIDENT { $1 }
  | LBRACKET RBRACKET { "[]" }
  | LPAREN RPAREN { "()" }

  | LPAREN COLONCOLON RPAREN { "::" }
  | FALSE { "false" }
  | TRUE { "true" }
;

val_longident:
    val_ident { Lident $1 }
  | mod_longident DOT val_ident { Ldot($1, $3) }
;
constr_longident:
    mod_longident %prec below_DOT { $1 }
  | LBRACKET RBRACKET { Lident "[]" }
  | LPAREN RPAREN { Lident "()" }
  | FALSE { Lident "false" }
  | TRUE { Lident "true" }
;
label_longident:
    LIDENT { Lident $1 }
  | mod_longident DOT LIDENT { Ldot($1, $3) }
;
type_longident:
    LIDENT { Lident $1 }
  | mod_ext_longident DOT LIDENT { Ldot($1, $3) }
;
mod_longident:
    UIDENT { Lident $1 }
  | mod_longident DOT UIDENT { Ldot($1, $3) }
;
mod_ext_longident:
    UIDENT { Lident $1 }
  | mod_ext_longident DOT UIDENT { Ldot($1, $3) }
  | mod_ext_longident LPAREN mod_ext_longident RPAREN { lapply $1 $3 }
;
mty_longident:
    ident { Lident $1 }
  | mod_ext_longident DOT ident { Ldot($1, $3) }
;
clty_longident:
    LIDENT { Lident $1 }
  | mod_ext_longident DOT LIDENT { Ldot($1, $3) }
;
class_longident:
    LIDENT { Lident $1 }
  | mod_longident DOT LIDENT { Ldot($1, $3) }
;



%inline toplevel_payload:
                { None }
  | STRING { let (s, d) = $1 in
                  Some (Exp.constant (Pconst_string (s, d) )) }
  | INT { let (n, m) = $1 in
                  Some (Exp.constant (Pconst_integer (n, m))) }
  | val_longident | mod_longident { Some (Exp.ident (mkloc $1 ((make_loc $symbolstartpos $endpos)))) }
  | FALSE { Some (Exp.construct (mkloc (Longident.Lident "false") ((make_loc $symbolstartpos $endpos))) None) }
  | TRUE { Some (Exp.construct (mkloc (Longident.Lident "true") ((make_loc $symbolstartpos $endpos))) None) }
;

toplevel_directive:
  SHARP ident toplevel_payload
    { let id = mkloc ("merlin.directive." ^ $2) ((make_loc $startpos($2) $endpos($2))) in
      let payload =
        match $3 with
        | None -> PStr []
        | Some exp -> PStr [mkstrexp exp []]
      in
      (mkstr ~loc:(make_loc $symbolstartpos $endpos))(Pstr_attribute (id,payload))
    }
;



name_tag:
    BACKQUOTE ident { $2 }
;
rec_flag:
                                                { Nonrecursive }
  | REC { Recursive }
;
nonrec_flag:
                                                { Recursive }
  | NONREC { Nonrecursive }
;
direction_flag:
    TO { Upto }
  | DOWNTO { Downto }
;
private_flag:
                                                { Public }
  | PRIVATE { Private }
;
mutable_flag:
                                                { Immutable }
  | MUTABLE { Mutable }
;
virtual_flag:
                                                { Concrete }
  | VIRTUAL { Virtual }
;
private_virtual_flags:
                 { Public, Concrete }
  | PRIVATE { Private, Concrete }
  | VIRTUAL { Public, Virtual }
  | PRIVATE VIRTUAL { Private, Virtual }
  | VIRTUAL PRIVATE { Private, Virtual }
;
override_flag:
                                                { Fresh }
  | BANG { Override }
;
opt_bar:
                                                { () }
  | BAR { () }
;
opt_semi:
  | { () }
  | SEMI { () }
;
subtractive:
  | MINUS { "-" }
  | MINUSDOT { "-." }
;
additive:
  | PLUS { "+" }
  | PLUSDOT { "+." }
;



single_attr_id:
    LIDENT { $1 }
  | UIDENT { $1 }
  | AND { "and" }
  | AS { "as" }
  | ASSERT { "assert" }
  | BEGIN { "begin" }
  | CLASS { "class" }
  | CONSTRAINT { "constraint" }
  | DO { "do" }
  | DONE { "done" }
  | DOWNTO { "downto" }
  | ELSE { "else" }
  | END { "end" }
  | EXCEPTION { "exception" }
  | EXTERNAL { "external" }
  | FALSE { "false" }
  | FOR { "for" }
  | FUN { "fun" }
  | FUNCTION { "function" }
  | FUNCTOR { "functor" }
  | IF { "if" }
  | IN { "in" }
  | INCLUDE { "include" }
  | INHERIT { "inherit" }
  | INITIALIZER { "initializer" }
  | LAZY { "lazy" }
  | LET { "let" }
  | MATCH { "match" }
  | METHOD { "method" }
  | MODULE { "module" }
  | MUTABLE { "mutable" }
  | NEW { "new" }
  | NONREC { "nonrec" }
  | OBJECT { "object" }
  | OF { "of" }
  | OPEN { "open" }
  | OR { "or" }
  | PRIVATE { "private" }
  | REC { "rec" }
  | SIG { "sig" }
  | STRUCT { "struct" }
  | THEN { "then" }
  | TO { "to" }
  | TRUE { "true" }
  | TRY { "try" }
  | TYPE { "type" }
  | VAL { "val" }
  | VIRTUAL { "virtual" }
  | WHEN { "when" }
  | WHILE { "while" }
  | WITH { "with" }

;

attr_id:
    single_attr_id { mkloc $1 ((make_loc $symbolstartpos $endpos)) }
  | single_attr_id DOT attr_id { mkloc ($1 ^ "." ^ $3.txt) ((make_loc $symbolstartpos $endpos))}
;
attribute:
  LBRACKETAT attr_id payload RBRACKET { ($2, $3) }
;
post_item_attribute:
  LBRACKETATAT attr_id payload RBRACKET { ($2, $3) }
;
floating_attribute:
  LBRACKETATATAT attr_id payload RBRACKET { ($2, $3) }
;
post_item_attributes:
                 { [] }
  | post_item_attribute post_item_attributes { $1 :: $2 }
;
attributes:
               { [] }
  | attribute attributes { $1 :: $2 }
;
ext_attributes:
                 { None, [] }
  | attribute attributes { None, $1 :: $2 }
  | PERCENT attr_id attributes { Some $2, $3 }
;
extension:
  LBRACKETPERCENT attr_id payload RBRACKET { ($2, $3) }
;
item_extension:
  LBRACKETPERCENTPERCENT attr_id payload RBRACKET { ($2, $3) }
;
payload:
    structure { PStr $1 }
  | COLON signature { PSig $2 }
  | COLON core_type { PTyp $2 }
  | QUESTION pattern { PPat ($2, None) }
  | QUESTION pattern WHEN seq_expr { PPat ($2, Some $4) }
;

%public simple_expr:
| DOTLESS expr GREATERDOT
    { Fake.Meta.code $startpos $endpos $2 }
| DOTTILDE simple_expr %prec prec_escape
    { Fake.Meta.uncode $startpos $endpos $2 }
;

(* Lwt *)
%public structure_item:
| lwt_bindings
    { (val_of_lwt_bindings ~loc:(make_loc $symbolstartpos $endpos)) $1 }

lwt_binding:
    LET_LWT ext_attributes rec_flag let_binding_body post_item_attributes
      { let (ext, attr) = $2 in
        (mklbs ~loc:(make_loc $symbolstartpos $endpos)) ext $3 ((mklb ~loc:(make_loc $symbolstartpos $endpos)) true $4 (attr@$5)) }
;
lwt_bindings:
    lwt_binding { $1 }
  | lwt_bindings and_let_binding { addlb $1 $2 }
;

%public expr:
| lwt_bindings IN seq_expr
    { (expr_of_lwt_bindings ~loc:(make_loc $symbolstartpos $endpos)) $1 (merloc $endpos($2) $3) }
| MATCH_LWT ext_attributes seq_expr WITH opt_bar match_cases
    { let expr = (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos))
          (Pexp_match(Fake.app Fake.Lwt.un_lwt $3, List.rev $6)) $2 in
      Fake.app Fake.Lwt.in_lwt expr }
| TRY_LWT ext_attributes seq_expr %prec below_WITH
    { (reloc_exp ~loc:(make_loc $symbolstartpos $endpos)) (Fake.app Fake.Lwt.in_lwt $3) }
| TRY_LWT ext_attributes seq_expr WITH opt_bar match_cases
    { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos))
        (Pexp_try(Fake.app Fake.Lwt.in_lwt $3, List.rev $6)) $2 }
| TRY_LWT ext_attributes seq_expr FINALLY_LWT seq_expr
    { Fake.app (Fake.app Fake.Lwt.finally_ $3) $5 }
| TRY_LWT ext_attributes seq_expr WITH opt_bar match_cases FINALLY_LWT seq_expr
    { let expr = (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos))
        (Pexp_try (Fake.app Fake.Lwt.in_lwt $3, List.rev $6)) $2 in
      Fake.app (Fake.app Fake.Lwt.finally_ expr) $8 }
| WHILE_LWT ext_attributes seq_expr DO seq_expr DONE
  { let expr = Pexp_while ($3, Fake.(app Lwt.un_lwt $5)) in
    Fake.(app Lwt.to_lwt ((mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) expr $2)) }
| FOR_LWT ext_attributes pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
    { let expr = Pexp_for ($3, $5, $7, $6, Fake.(app Lwt.un_lwt $9)) in
      Fake.(app Lwt.to_lwt ((mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos)) expr $2)) }
| FOR_LWT ext_attributes pattern IN seq_expr DO seq_expr DONE
    { (mkexp_attrs ~loc:(make_loc $symbolstartpos $endpos))
          (Pexp_let (Nonrecursive, [Vb.mk $3 (Fake.(app Lwt.un_stream $5))],
             Fake.(app Lwt.unit_lwt $7)))
          $2
    }
;

%%

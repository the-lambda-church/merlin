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

open Asttypes
open Longident
open Parsetree
open Ast_helper


let rloc loc_start loc_end =
  { Location. loc_start; loc_end; loc_ghost = false; }
let gloc loc_start loc_end =
  { Location. loc_start; loc_end; loc_ghost = true; }
let mkloc =
  Location.mkloc

let mktyp startpos endpos d = Typ.mk ~loc:(rloc startpos endpos) d
let mkpat startpos endpos d = Pat.mk ~loc:(rloc startpos endpos) d
let mkexp startpos endpos d = Exp.mk ~loc:(rloc startpos endpos) d
let mkmty startpos endpos d = Mty.mk ~loc:(rloc startpos endpos) d
let mksig startpos endpos d = [Sig.mk ~loc:(rloc startpos endpos) d]
let mkmod startpos endpos d = Mod.mk ~loc:(rloc startpos endpos) d
let mkstr startpos endpos d = [Str.mk ~loc:(rloc startpos endpos) d]
let ghstr startpos endpos d = [Str.mk ~loc:(gloc startpos endpos) d]
let mkclass startpos endpos d = Cl.mk ~loc:(rloc startpos endpos) d
let mkcty startpos endpos d = Cty.mk ~loc:(rloc startpos endpos) d
let mkctf startpos endpos ?attrs d = Ctf.mk ~loc:(rloc startpos endpos) ?attrs d
let mkcf startpos endpos ?attrs d = [Cf.mk ~loc:(rloc startpos endpos) ?attrs d]

let mkrhs startpos endpos rhs = mkloc rhs (rloc startpos endpos)
let mkoption d =
  let loc = {d.ptyp_loc with Location. loc_ghost = true} in
  Typ.mk ~loc (Ptyp_constr(mkloc (Ldot (Lident "*predef*", "option")) loc,[d]))

let reloc_pat startpos endpos x= { x with ppat_loc = rloc startpos endpos };;
let reloc_exp startpos endpos x= { x with pexp_loc = rloc startpos endpos };;
let merloc startpos ?endpos x =
  let endpos = match endpos with
    | None -> x.pexp_loc.Location.loc_end
    | Some endpos -> endpos
  in
  let str = mkloc "merlin.loc" (rloc startpos endpos) in
  { x with pexp_attributes = (str , PStr []) :: x.pexp_attributes }

let mkoperator startpos endpos name =
  let loc = rloc startpos endpos in
  Exp.mk ~loc (Pexp_ident(mkloc (Lident name) loc))

let mkpatvar startpos endpos name =
  Pat.mk ~loc:(rloc startpos endpos) (Ppat_var (mkrhs startpos endpos name))

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
let ghexp startpos endpos d = Exp.mk ~loc:(gloc startpos endpos) d
let ghpat startpos endpos d = Pat.mk ~loc:(gloc startpos endpos) d
let ghtyp startpos endpos d = Typ.mk ~loc:(gloc startpos endpos) d
let ghloc startpos endpos d = { txt = d; loc = gloc startpos endpos }

let mkinfix startpos endpos arg1 startpos2 endpos2 name arg2 =
  mkexp startpos endpos
    (Pexp_apply(mkoperator startpos2 endpos2 name, ["", arg1; "", arg2]))

let neg_float_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus startpos endpos name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant(Const_int n) ->
      mkexp startpos endpos (Pexp_constant(Const_int(-n)))
  | "-", Pexp_constant(Const_int32 n) ->
      mkexp startpos endpos (Pexp_constant(Const_int32(Int32.neg n)))
  | "-", Pexp_constant(Const_int64 n) ->
      mkexp startpos endpos (Pexp_constant(Const_int64(Int64.neg n)))
  | "-", Pexp_constant(Const_nativeint n) ->
      mkexp startpos endpos (Pexp_constant(Const_nativeint(Nativeint.neg n)))
  | ("-" | "-."), Pexp_constant(Const_float f) ->
      mkexp startpos endpos (Pexp_constant(Const_float(neg_float_string f)))
  | _ ->
      mkexp startpos endpos (Pexp_apply(mkoperator startpos endpos ("~" ^ name), ["", arg]))

let mkuplus startpos endpos name arg =
  let desc = arg.pexp_desc in
  match name, desc with
  | "+", Pexp_constant(Const_int _)
  | "+", Pexp_constant(Const_int32 _)
  | "+", Pexp_constant(Const_int64 _)
  | "+", Pexp_constant(Const_nativeint _)
  | ("+" | "+."), Pexp_constant(Const_float _) -> mkexp startpos endpos desc
  | _ ->
      mkexp startpos endpos (Pexp_apply(mkoperator startpos endpos ("~" ^ name), ["", arg]))

let mkexp_cons consloc args loc =
  Exp.mk ~loc (Pexp_construct(mkloc (Lident "::") consloc, Some args))

let mkpat_cons consloc args loc =
  Pat.mk ~loc (Ppat_construct(mkloc (Lident "::") consloc, Some args))

let rec mktailexp startpos endpos = function
    [] ->
      let loc = gloc startpos endpos in
      let nil = { txt = Lident "[]"; loc = loc } in
      Exp.mk ~loc (Pexp_construct (nil, None))
  | e1 :: el ->
      let open Location in
      let exp_el = mktailexp e1.pexp_loc.loc_end endpos el in
      let loc = gloc e1.pexp_loc.loc_start exp_el.pexp_loc.loc_end in
      let arg = Exp.mk ~loc (Pexp_tuple [e1; exp_el]) in
      mkexp_cons loc arg loc

let rec mktailpat startpos endpos = function
    [] ->
      let loc = gloc startpos endpos in
      let nil = { txt = Lident "[]"; loc = loc } in
      Pat.mk ~loc (Ppat_construct (nil, None))
  | p1 :: pl ->
      let open Location in
      let pat_pl = mktailpat p1.ppat_loc.loc_end endpos pl in
      let loc = gloc p1.ppat_loc.loc_start pat_pl.ppat_loc.loc_end in
      let arg = Pat.mk ~loc (Ppat_tuple [p1; pat_pl]) in
      mkpat_cons loc arg loc

let mkstrexp e attrs =
  { pstr_desc = Pstr_eval (e, attrs); pstr_loc = e.pexp_loc }

let mkexp_constraint startpos endpos e (t1, t2) =
  match t1, t2 with
  | Some t, None -> mkexp startpos endpos (Pexp_constraint(e, t))
  | _, Some t -> mkexp startpos endpos (Pexp_coerce(e, t1, t))
  | None, None -> e

let array_function startpos endpos str name =
  ghloc startpos endpos
    (Ldot(Lident str, (if !Clflags.fast then "unsafe_" ^ name else name)))

let raise_error = Msupport_parsing.raise_error

let syntax_error startpos endpos =
  raise_error (Syntaxerr.Escape_error (rloc startpos endpos))

let not_expecting startpos endpos nonterm =
  raise_error
    Syntaxerr.(Error (Not_expecting (rloc startpos endpos, nonterm)))

let bigarray_function startpos endpos str name =
  ghloc startpos endpos (Ldot(Ldot(Lident "Bigarray", str), name))

let bigarray_untuplify = function
    { pexp_desc = Pexp_tuple explist; pexp_loc = _ } -> explist
  | exp -> [exp]

let bigarray_get (startpos,endpos) (startop,endop) arr arg =
  let get = if !Clflags.fast then "unsafe_get" else "get" in
  let ghexp = ghexp startop endop in
  let mkexp = mkexp startpos endpos in
  let bigarray_function = bigarray_function startop endop in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" get)),
                       ["", arr; "", c1]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" get)),
                       ["", arr; "", c1; "", c2]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" get)),
                       ["", arr; "", c1; "", c2; "", c3]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "get")),
                       ["", arr; "", ghexp(Pexp_array coords)]))

let bigarray_set (startpos,endpos) (startop,endop) arr arg newval =
  let set = if !Clflags.fast then "unsafe_set" else "set" in
  let ghexp = ghexp startop endop in
  let mkexp = mkexp startpos endpos in
  let bigarray_function = bigarray_function startop endop in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" set)),
                       ["", arr; "", c1; "", newval]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" set)),
                       ["", arr; "", c1; "", c2; "", newval]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" set)),
                       ["", arr; "", c1; "", c2; "", c3; "", newval]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "set")),
                       ["", arr;
                        "", ghexp(Pexp_array coords);
                        "", newval]))

let lapply startpos endpos p1 p2 =
  if not !Clflags.applicative_functors then
    raise_error Syntaxerr.(Error(Applicative_path(rloc startpos endpos)));
  Lapply(p1, p2)

let exp_of_label startpos endpos lbl =
  mkexp startpos endpos (Pexp_ident(mkrhs startpos endpos (Lident(Longident.last lbl))))

let pat_of_label startpos endpos lbl =
  mkpat startpos endpos (Ppat_var (mkrhs startpos endpos (Longident.last lbl)))

let check_variable vl loc v =
  if List.mem v vl then
    raise_error Syntaxerr.(Error(Variable_in_scope(loc,v)))

let varify_constructors var_names t =
  let rec loop t =
    let desc =
      match t.ptyp_desc with
      | Ptyp_any -> Ptyp_any
      | Ptyp_var x ->
          check_variable var_names t.ptyp_loc x;
          Ptyp_var x
      | Ptyp_arrow (label,core_type1,core_type2) ->
          Ptyp_arrow(label, loop core_type1, loop core_type2)
      | Ptyp_tuple lst -> Ptyp_tuple (List.map loop lst)
      | Ptyp_constr( { txt = Lident s }, []) when List.mem s var_names ->
          Ptyp_var s
      | Ptyp_constr(longident, lst) ->
          Ptyp_constr(longident, List.map loop lst)
      | Ptyp_object (lst, o) ->
          Ptyp_object (List.map (fun (s, attrs, t) -> (s, attrs, loop t)) lst, o)
      | Ptyp_class (longident, lst) ->
          Ptyp_class (longident, List.map loop lst)
      | Ptyp_alias(core_type, string) ->
          check_variable var_names t.ptyp_loc string;
          Ptyp_alias(loop core_type, string)
      | Ptyp_variant(row_field_list, flag, lbl_lst_option) ->
          Ptyp_variant(List.map loop_row_field row_field_list,
                       flag, lbl_lst_option)
      | Ptyp_poly(string_lst, core_type) ->
          List.iter (check_variable var_names t.ptyp_loc) string_lst;
          Ptyp_poly(string_lst, loop core_type)
      | Ptyp_package(longident,lst) ->
          Ptyp_package(longident,List.map (fun (n,typ) -> (n,loop typ) ) lst)
      | Ptyp_extension (s, arg) ->
          Ptyp_extension (s, arg)
    in
    {t with ptyp_desc = desc}
  and loop_row_field =
    function
      | Rtag(label,attrs,flag,lst) ->
          Rtag(label,attrs,flag,List.map loop lst)
      | Rinherit t ->
          Rinherit (loop t)
  in
  loop t

let wrap_type_annotation startpos endpos newtypes core_type body =
  let mkexp = mkexp startpos endpos in
  let ghtyp = ghtyp startpos endpos in
  let exp = mkexp(Pexp_constraint(body,core_type)) in
  let exp =
    List.fold_right (fun newtype exp -> mkexp (Pexp_newtype (newtype, exp)))
      newtypes exp
  in
  (exp, ghtyp (Ptyp_poly(newtypes,varify_constructors newtypes core_type)))

let wrap_exp_attrs startpos endpos body (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let body = {body with pexp_attributes = attrs @ body.pexp_attributes} in
  match ext with
  | None -> body
  | Some id -> ghexp startpos endpos (Pexp_extension (id, PStr [mkstrexp body []]))

let mkexp_attrs startpos endpos d attrs =
  wrap_exp_attrs startpos endpos (mkexp startpos endpos d) attrs

let fake_tydecl tydecl = tydecl.ptype_name, tydecl
let tag_nonrec loc (_, a) =
  let attr = ({ txt = "nonrec"; loc }, PStr []) in
  {a with ptype_attributes = attr :: a.ptype_attributes}

let fake_vb_app f vb = {vb with pvb_expr = Fake.app f vb.pvb_expr}

let let_operator startpos endpos op bindings cont =
  let pat, expr =
    match bindings with
    | [] -> assert false
    | [x] -> (x.pvb_pat,x.pvb_expr)
    | l ->
      let pats, exprs =
        List.fold_right
          (fun {pvb_pat=p;pvb_expr=e} (ps,es) -> (p::ps,e::es)) l ([],[]) in
      ghpat startpos endpos (Ppat_tuple pats),
      ghexp startpos endpos (Pexp_tuple exprs)
  in
  let f = ghexp startpos endpos (Pexp_fun("", None, pat, cont)) in
  mkexp startpos endpos (Pexp_apply(op, [("", expr); ("", f)]))

%}

%[@printer.header

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



(* Tokens *)

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
%token <string> FLOAT [@cost 2] [@recovery "0."] [@printer Printf.sprintf "FLOAT(%S)"]
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
%token <int> INT [@cost 1] [@recovery 0][@printer Printf.sprintf "INT(%d)"]
%token <int32> INT32 [@cost 1] [@recovery 0l][@printer Printf.sprintf "INT(%ld)"]
%token <int64> INT64 [@cost 1] [@recovery 0L][@printer Printf.sprintf "INT64(%Ld)"]
%token <string> LABEL [@cost 2] [@recovery "_"][@printer Printf.sprintf "LABEL(%S)"] [@symbol "label"]
%token LAZY [@symbol "lazy"]
%token LBRACE [@symbol "{"]
%token LBRACELESS [@symbol "{<"]
%token LBRACKET [@symbol "["]
%token LBRACKETAT [@symbol "[@"]
%token LBRACKETATAT [@symbol "[@@"]
%token LBRACKETATATAT [@symbol "[@@@"]
%token LBRACKETBAR [@symbol "[|"]
%token LBRACKETGREATER [@symbol "[>"]
%token LBRACKETLESS [@symbol "[<"]
%token LBRACKETPERCENT [@symbol "[%"]
%token LBRACKETPERCENTPERCENT [@symbol "[%%"]
%token LESS [@symbol "<"]
%token LESSMINUS [@symbol "<-"] [@cost 2]
%token LET [@symbol "let"]
%token <string> LIDENT [@cost 2] [@recovery "_"][@printer Printf.sprintf "LIDENT(%S)"] [@symbol "<ident>"]
%token LPAREN [@symbol ")"]
%token MATCH [@symbol "match"]
%token METHOD [@symbol "method"]
%token MINUS [@symbol "-"]
%token MINUSDOT [@symbol "-."]
%token MINUSGREATER [@symbol "->"]
%token MODULE [@symbol "module"]
%token MUTABLE [@symbol "mutable"]
%token <nativeint> NATIVEINT [@cost 1] [@recovery 0n][@printer Printf.sprintf "NATIVEINT(%nd)"]
%token NEW [@symbol "new"]
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
%token <string> HASHOP [@cost 2] [@recovery ""][@printer Printf.sprintf "HASHOP(%S)"] [@symbol "#."]
%token SIG [@symbol "sig"]
%token STAR [@symbol "*"]
%token <string * string option> STRING [@cost 1] [@recovery ("", None)][@printer string_of_STRING] [@symbol "<string>"]
%token STRUCT [@symbol "struct"]
%token THEN [@symbol "then"]
%token TILDE [@symbol "~"]
%token TO [@symbol "to"]
%token TRUE [@symbol "true"]
%token TRY [@symbol "try"]
%token TYPE [@symbol "type"]
%token <string> UIDENT [@cost 2][@recovery "_"][@printer Printf.sprintf "UIDENT(%S)"] [@symbol "<ident>"]
%token UNDERSCORE [@symbol "_"]
%token VAL [@symbol "val"]
%token VIRTUAL [@symbol "virtual"]
%token WHEN [@symbol "when"]
%token WHILE [@symbol "while"]
%token WITH [@symbol "with"]
%token <string * Location.t> COMMENT [@cost 2][@recovery ("", Location.none)] [@symbol "COMMENT"]

%token EOL [@symbol "EOL"]

%token LET_LWT [@cost 1] [@symbol "lwt"]
%token TRY_LWT [@cost 1] [@symbol "try_lwt"]
%token MATCH_LWT [@cost 1] [@symbol "match_lwt"]
%token FINALLY_LWT [@cost 1] [@symbol "finally"]
%token FOR_LWT [@cost 1] [@symbol "for_lwt"]
%token WHILE_LWT [@cost 1] [@symbol "while_lwt"]
%token NONREC [@cost 1] [@symbol "nonrec"]

%token DOTLESS [@cost 1] [@symbol ".<"]
%token DOTTILDE [@cost 1] [@symbol ".~"]
%token GREATERDOT [@cost 1] [@symbol ">."]
%token <string> LETOP [@cost 1] [@recovery ""] [@printer Printf.sprintf "LETOP(%S)"] [@symbol "let<op>"]

(* Precedences and associativities.

Tokens and rules have precedences. A reduce/reduce conflict is resolved
in favor of the first rule (in source file order). A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind x * x -> x
for example, in the rules of the form expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*)

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI (* below EQUAL ({lbl=...; lbl=...}) *)
%nonassoc LET LET_LWT (* above SEMI ( ...; let ... in ...) *)
%nonassoc LETOP
%nonassoc below_WITH
%nonassoc FUNCTION WITH (* below BAR (match ... with ...) *)
%nonassoc FINALLY_LWT
%nonassoc AND (* above WITH (module rec A: SIG with ... and ...) *)
%nonassoc THEN (* below ELSE (if ... then ...) *)
%nonassoc ELSE (* (if ... then ... else ...) *)
%nonassoc LESSMINUS (* below COLONEQUAL (lbl <- x := e) *)
%right COLONEQUAL (* expr (e := e := e) *)
%nonassoc AS
%left BAR (* pattern (p|p|p) *)
%nonassoc below_COMMA
%left COMMA (* expr/expr_comma_list (e,e,e) *)
%right MINUSGREATER (* core_type2 (t -> t -> t) *)
%right OR BARBAR (* expr (e || e || e) *)
%right AMPERSAND AMPERAMPER (* expr (e && e && e) *)
%nonassoc below_EQUAL
%left INFIXOP0 EQUAL LESS GREATER (* expr (e OP e OP e) *)
%right INFIXOP1 (* expr (e OP e OP e) *)
%nonassoc below_LBRACKETAT
%nonassoc LBRACKETAT
%nonassoc LBRACKETATAT
%right COLONCOLON (* expr (e :: e :: e) *)
%left INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ (* expr (e OP e OP e) *)
%left INFIXOP3 STAR PERCENT (* expr (e OP e OP e) *)
%right INFIXOP4 (* expr (e OP e OP e) *)
%nonassoc prec_unary_minus prec_unary_plus (* unary - *)
%nonassoc prec_constant_constructor (* cf. simple_expr (C versus C x) *)
%nonassoc prec_constr_appl (* above AS BAR COLONCOLON COMMA *)
%left prec_escape
%nonassoc below_SHARP
%nonassoc SHARP (* simple_expr/toplevel_directive *)
%left HASHOP
%nonassoc below_DOT
%nonassoc DOT

(* Finally, the first tokens of simple_expr are above everything else. *)
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT
          LBRACKETPERCENT LBRACKETPERCENTPERCENT
          DOTLESS DOTTILDE GREATERDOT QUESTIONQUESTION

(* Entry points *)

(* for implementation files *)
%start implementation
%type <Parsetree.structure> implementation

(* for interface files *)
%start interface
%type <Parsetree.signature> interface

(* merlin: for inline expression *)
%start parse_expression
%type <Parsetree.expression> parse_expression


%%

(* Entry points *)

implementation:
| structure EOF
    { $1 }

interface:
| signature EOF
    { $1 }

parse_expression:
| seq_expr EOF
    { $1 }

(* Module expressions *)

functor_arg:
| LPAREN RPAREN
    { mkrhs $startpos($2) $endpos($2) "*", None }
| LPAREN functor_arg_name COLON module_type RPAREN
    { mkrhs $startpos($2) $endpos($2) $2, Some $4 }

functor_arg_name:
| UIDENT
    { $1 }
| UNDERSCORE
    { "_" }

functor_args [@recovery []]:
| functor_args functor_arg
    { $2 :: $1 }
| functor_arg
    { [ $1 ] }

module_expr [@recovery default_module_expr ()]:
| mod_longident
    { mkmod $startpos $endpos (Pmod_ident (mkrhs $startpos($1) $endpos($1) $1)) }
| STRUCT [@unclosed "struct"] structure END [@close]
    { mkmod $startpos $endpos (Pmod_structure($2)) }
| FUNCTOR functor_args MINUSGREATER module_expr
    { List.fold_left (fun acc (n, t) -> mkmod $startpos $endpos (Pmod_functor(n, t, acc))) $4 $2 }
| module_expr LPAREN module_expr RPAREN
    { mkmod $startpos $endpos (Pmod_apply($1, $3)) }
| module_expr LPAREN RPAREN
    { mkmod $startpos $endpos (Pmod_apply($1, mkmod $startpos $endpos (Pmod_structure []))) }
| LPAREN [@nclosed "("] module_expr COLON module_type RPAREN [@close]
    { mkmod $startpos $endpos (Pmod_constraint($2, $4)) }
| LPAREN [@unclosed "("] module_expr RPAREN [@close]
    { $2 }
| LPAREN [@unclosed "("] VAL expr RPAREN [@close]
    { mkmod $startpos $endpos (Pmod_unpack $3) }
| LPAREN [@unclosed "("] VAL expr COLON package_type RPAREN [@close]
    { mkmod $startpos $endpos (Pmod_unpack(
              ghexp $startpos $endpos (Pexp_constraint($3, ghtyp $startpos $endpos (Ptyp_package $5))))) }
| LPAREN [@unclosed "("] VAL expr COLON package_type COLONGREATER package_type RPAREN [@close]
    { mkmod $startpos $endpos (Pmod_unpack(
              ghexp $startpos $endpos (Pexp_coerce($3, Some(ghtyp $startpos $endpos (Ptyp_package $5)),
                                    ghtyp $startpos $endpos (Ptyp_package $7))))) }
| LPAREN [@unclosed "("] VAL expr COLONGREATER package_type RPAREN [@close]
    { mkmod $startpos $endpos (Pmod_unpack(
              ghexp $startpos $endpos (Pexp_coerce($3, None, ghtyp $startpos $endpos (Ptyp_package $5))))) }
| module_expr attribute
    { Mod.attr $1 $2 }
| extension
    { mkmod $startpos $endpos (Pmod_extension $1) }

structure [@recovery []]:
| v = structure_head
  { v }

structure_head :
| toplevel_directives seq_expr post_item_attributes structure_tail
    { List.rev_append $1 (mkstrexp $2 $3 :: $4) }
| toplevel_directives structure_tail
    { List.rev_append $1 $2 }

structure_tail:
| (* empty *)
    { [] }
| SEMISEMI structure_head
    { $2 }
| structure_item structure_tail
    { $1 @ $2 }

%public structure_item:
| LET [@item "let"] ext_attributes rec_flag let_bindings
    {
      let (extensions, attributes) = $2 in
      let str =
        match $4 with
          [ {pvb_pat = { ppat_desc = Ppat_any; ppat_loc = _ };
            pvb_expr = exp; pvb_attributes = attrs}] ->
            let exp = wrap_exp_attrs $startpos $endpos exp (None, attributes) in
            mkstr $startpos $endpos (Pstr_eval (exp, attrs))
        | l ->
          if attributes <> [] then
            not_expecting $startpos($2) $endpos($2) "attribute";
          mkstr $startpos $endpos (Pstr_value($3, List.rev l))
      in
      match extensions with
      | None -> str
      | Some id -> ghstr $startpos $endpos (Pstr_extension((id, PStr str), []))
    }
| EXTERNAL [@item "external"] val_ident COLON core_type EQUAL primitive_declaration
  post_item_attributes
    { mkstr $startpos $endpos
        (Pstr_primitive (Val.mk (mkrhs $startpos($2) $endpos($2) $2) $4
                           ~prim:$6 ~attrs:$7 ~loc:(rloc $startpos $endpos))) }
| TYPE [@item "type"] decls = type_declarations
    { mkstr $startpos $endpos (Pstr_type (List.rev decls) ) }
| TYPE [@item "type"] str_type_extension
    { mkstr $startpos $endpos (Pstr_typext $2) }
| EXCEPTION [@item "exception"] str_exception_declaration
    { mkstr $startpos $endpos (Pstr_exception $2) }
| MODULE [@item "module"] module_binding
    { mkstr $startpos $endpos (Pstr_module $2) }
| MODULE REC [@item "recursive module"] module_bindings
    { mkstr $startpos $endpos (Pstr_recmodule(List.rev $3)) }
| MODULE TYPE [@item "module type"] ident post_item_attributes
    { mkstr $startpos $endpos (Pstr_modtype (Mtd.mk (mkrhs $startpos($3) $endpos($3) $3)
                              ~attrs:$4 ~loc:(rloc $startpos $endpos))) }
| MODULE TYPE [@item "module type"] ident EQUAL module_type post_item_attributes
    { mkstr $startpos $endpos (Pstr_modtype (Mtd.mk (mkrhs $startpos($3) $endpos($3) $3)
                              ~typ:$5 ~attrs:$6 ~loc:(rloc $startpos $endpos))) }
| open_statement
    { mkstr $startpos $endpos (Pstr_open $1) }
| CLASS [@item "class"] class_declarations
    { mkstr $startpos $endpos (Pstr_class (List.rev $2)) }
| CLASS TYPE [@item "class type"] class_type_declarations
    { mkstr $startpos $endpos (Pstr_class_type (List.rev $3)) }
| INCLUDE [@item "include"] module_expr post_item_attributes
    { mkstr $startpos $endpos (Pstr_include (Incl.mk $2 ~attrs:$3
                                             ~loc:(rloc $startpos $endpos))) }
| item_extension post_item_attributes
    { mkstr $startpos $endpos (Pstr_extension ($1, $2)) }
| floating_attribute
    { mkstr $startpos $endpos (Pstr_attribute $1) }

module_binding_body :
| EQUAL module_expr
    { $2 }
| COLON module_type EQUAL module_expr
    { mkmod $startpos $endpos (Pmod_constraint($4, $2)) }
| functor_arg module_binding_body
    { mkmod $startpos $endpos (Pmod_functor(fst $1, snd $1, $2)) }

module_bindings :
| module_binding
    { [$1] }
| module_bindings AND module_binding
    { $3 :: $1 }

module_binding :
| UIDENT module_binding_body post_item_attributes
    { Mb.mk (mkrhs $startpos($1) $endpos($1) $1) $2 ~attrs:$3 ~loc:(rloc $startpos $endpos) }

(* Module types *)

module_type [@recovery default_module_type ()]:
| mty_longident
    { mkmty $startpos $endpos (Pmty_ident (mkrhs $startpos($1) $endpos($1) $1)) }
| SIG [@unclosed "sig"] signature END [@close]
    { mkmty $startpos $endpos (Pmty_signature $2) }
| FUNCTOR functor_args MINUSGREATER module_type %prec below_WITH
    { List.fold_left (fun acc (n, t) -> mkmty $startpos $endpos (Pmty_functor(n, t, acc))) $4 $2 }
| module_type WITH with_constraints
    { mkmty $startpos $endpos (Pmty_with($1, List.rev $3)) }
| MODULE TYPE OF module_expr %prec below_LBRACKETAT
    { mkmty $startpos $endpos (Pmty_typeof $4) }
(*| LPAREN [@unclosed "("] MODULE mod_longident RPAREN [@close]
    { mkmty $startpos $endpos (Pmty_alias (mkrhs $startpos($3) $endpos($3) $3)) }*)
| LPAREN [@unclosed "("] module_type RPAREN [@close]
    { $2 }
| extension
    { mkmty $startpos $endpos (Pmty_extension $1) }
| module_type attribute
    { Mty.attr $1 $2 }

signature :
| (* empty *)
    { [] }
| SEMISEMI signature
    { $2 }
| signature_item signature
    { $1 @ $2 }

%public signature_item:
| VAL [@item "val"] val_ident COLON core_type post_item_attributes
    { mksig $startpos $endpos (Psig_value
                (Val.mk (mkrhs $startpos($2) $endpos($2) $2) $4 ~attrs:$5 ~loc:(rloc $startpos $endpos))) }
| EXTERNAL [@item "external"]
  val_ident COLON core_type EQUAL primitive_declaration post_item_attributes
    { mksig $startpos $endpos (Psig_value
                (Val.mk (mkrhs $startpos($2) $endpos($2) $2) $4 ~prim:$6 ~attrs:$7
                   ~loc:(rloc $startpos $endpos))) }
| TYPE [@item "type"] type_declarations
    { mksig $startpos $endpos (Psig_type (List.rev $2)) }
| TYPE [@item "type"] sig_type_extension
    { mksig $startpos $endpos (Psig_typext $2) }
| EXCEPTION [@item "exception"] sig_exception_declaration
    { mksig $startpos $endpos (Psig_exception $2) }
| MODULE [@item "module"] UIDENT module_declaration post_item_attributes
    { mksig $startpos $endpos (Psig_module (Md.mk (mkrhs $startpos($2) $endpos($2) $2)
                             $3 ~attrs:$4 ~loc:(rloc $startpos $endpos))) }
| MODULE [@item "module"] UIDENT EQUAL mod_longident post_item_attributes
    { mksig $startpos $endpos (Psig_module (Md.mk (mkrhs $startpos($2) $endpos($2) $2)
                             (Mty.alias ~loc:(rloc $startpos($4) $endpos($4)) (mkrhs $startpos($4) $endpos($4) $4))
                             ~attrs:$5
                             ~loc:(rloc $startpos $endpos)
                          )) }
| MODULE REC [@item "recursive module"] rec_module_declarations
    { mksig $startpos $endpos (Psig_recmodule (List.rev $3)) }
| MODULE TYPE [@item "module type"] ident post_item_attributes
    { mksig $startpos $endpos (Psig_modtype (Mtd.mk (mkrhs $startpos($3) $endpos($3) $3)
                              ~attrs:$4 ~loc:(rloc $startpos $endpos))) }
| MODULE TYPE [@item "module type"] ident EQUAL module_type post_item_attributes
    { mksig $startpos $endpos (Psig_modtype (Mtd.mk (mkrhs $startpos($3) $endpos($3) $3) ~typ:$5
                              ~loc:(rloc $startpos $endpos)
                              ~attrs:$6)) }
| open_statement
    { mksig $startpos $endpos (Psig_open $1) }
| INCLUDE [@item "include"] module_type post_item_attributes %prec below_WITH
    { mksig $startpos $endpos (Psig_include (Incl.mk $2 ~attrs:$3
                                             ~loc:(rloc $startpos $endpos))) }
| CLASS [@item "class"] class_descriptions
    { mksig $startpos $endpos (Psig_class (List.rev $2)) }
| CLASS TYPE [@item "class type"]
  class_type_declarations
    { mksig $startpos $endpos (Psig_class_type (List.rev $3)) }
| item_extension post_item_attributes
    { mksig $startpos $endpos (Psig_extension ($1, $2)) }
| floating_attribute
    { mksig $startpos $endpos (Psig_attribute $1) }

open_statement:
| OPEN [@item "open"]
  override_flag mod_longident post_item_attributes
    { Opn.mk (mkrhs $startpos($3) $endpos($3) $3) ~override:$2 ~attrs:$4
        ~loc:(rloc $startpos $endpos) }

module_declaration :
| COLON module_type
    { $2 }
| LPAREN UIDENT COLON module_type RPAREN module_declaration
    { mkmty $startpos $endpos (Pmty_functor(mkrhs $startpos($2) $endpos($2) $2, Some $4, $6)) }
| LPAREN RPAREN module_declaration
    { mkmty $startpos $endpos (Pmty_functor(mkrhs $startpos($1) $endpos($1) "*", None, $3)) }

rec_module_declarations:
| module_rec_declaration
    { [$1] }
| rec_module_declarations AND module_rec_declaration
    { $3 :: $1 }

module_rec_declaration:
| UIDENT COLON module_type post_item_attributes
    { Md.mk (mkrhs $startpos($1) $endpos($1) $1) $3 ~attrs:$4 ~loc:(rloc $startpos $endpos) }

(* Class expressions *)

class_declarations :
| class_declarations AND class_declaration
    { $3 @ $1 }
| class_declaration
    { $1 }

class_declaration :
| virtual_flag class_type_parameters LIDENT class_fun_binding
  post_item_attributes
    {
      [Ci.mk (mkrhs $startpos($3) $endpos($3) $3) $4
         ~virt:$1 ~params:$2
         ~attrs:$5 ~loc:(rloc $startpos $endpos)]
    }

class_fun_binding:
| EQUAL class_expr
    { $2 }
| COLON class_type EQUAL class_expr
    { mkclass $startpos $endpos (Pcl_constraint($4, $2)) }
| labeled_simple_pattern class_fun_binding
    { let (l,o,p) = $1 in mkclass $startpos $endpos (Pcl_fun(l, o, p, $2)) }

class_type_parameters :
| (* empty *)
    { [] }
| LBRACKET type_parameter_list RBRACKET
    { List.rev $2 }

class_fun_def:
| labeled_simple_pattern MINUSGREATER class_expr
    { let (l,o,p) = $1 in mkclass $startpos $endpos (Pcl_fun(l, o, p, $3)) }
| labeled_simple_pattern class_fun_def
    { let (l,o,p) = $1 in mkclass $startpos $endpos (Pcl_fun(l, o, p, $2)) }

class_expr:
| class_simple_expr
    { $1 }
| FUN class_fun_def
    { $2 }
| class_simple_expr simple_labeled_expr_list
    { mkclass $startpos $endpos (Pcl_apply($1, List.rev $2)) }
| LET rec_flag let_bindings_no_attrs IN [@shift 2] class_expr
    { mkclass $startpos $endpos (Pcl_let ($2, List.rev $3, $5)) }
| class_expr attribute
    { Cl.attr $1 $2 }
| extension
    { mkclass $startpos $endpos (Pcl_extension $1) }

class_simple_expr:
| LBRACKET core_type_comma_list RBRACKET class_longident
    { mkclass $startpos $endpos (Pcl_constr(mkloc $4 (rloc $startpos($4) $endpos($4)), List.rev $2)) }
| class_longident
    { mkclass $startpos $endpos (Pcl_constr(mkrhs $startpos($1) $endpos($1) $1, [])) }
| OBJECT [@unclosed "object"] [@item "object"] class_structure END [@close]
    { mkclass $startpos $endpos (Pcl_structure($2)) }
| LPAREN [@unclosed "("] class_expr COLON class_type RPAREN [@close]
    { mkclass $startpos $endpos (Pcl_constraint($2, $4)) }
| LPAREN [@unclosed "("] class_expr RPAREN [@close]
    { $2 }

class_structure:
| class_self_pattern class_fields
    { Cstr.mk $1 (List.rev $2) }

class_self_pattern:
| LPAREN pattern RPAREN
    { reloc_pat $startpos $endpos $2 }
| LPAREN pattern COLON core_type RPAREN
    { mkpat $startpos $endpos (Ppat_constraint($2, $4)) }
| (* empty *)
    { ghpat $startpos $endpos (Ppat_any) }

class_fields :
| (* empty *)
    { [] }
| class_fields class_field
    { $2 @ $1 }

class_field :
| INHERIT override_flag class_expr parent_binder attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_inherit ($2, $3, $4)) ~attrs }
| VAL value attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_val $2) ~attrs }
| METHOD method_ attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_method $2) ~attrs }
| CONSTRAINT constrain_field attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_constraint $2) ~attrs }
| INITIALIZER seq_expr attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_initializer $2) ~attrs }
| item_extension attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_extension $1) ~attrs }
| floating_attribute
    { mkcf $startpos $endpos (Pcf_attribute $1) }

parent_binder:
| AS LIDENT
    { Some $2 }
| (* empty *)
    { None }

value:
(* TODO: factorize these rules (also with method): *)
| override_flag MUTABLE VIRTUAL label COLON core_type
    { if $1 = Override then syntax_error $startpos $endpos;
        mkloc $4 (rloc $startpos($4) $endpos($4)), Mutable, Cfk_virtual $6 }
| VIRTUAL mutable_flag label COLON core_type
    { mkrhs $startpos($3) $endpos($3) $3, $2, Cfk_virtual $5 }
| override_flag mutable_flag label EQUAL seq_expr
    { mkrhs $startpos($3) $endpos($3) $3, $2, Cfk_concrete ($1, $5) }
| override_flag mutable_flag label type_constraint EQUAL seq_expr
    {
       let e = mkexp_constraint $startpos $endpos $6 $4 in
       mkrhs $startpos($3) $endpos($3) $3, $2, Cfk_concrete ($1, e)
      }

method_:
(* TODO: factorize those rules... *)
| override_flag PRIVATE VIRTUAL label COLON poly_type
    { if $1 = Override then syntax_error $startpos $endpos;
        mkloc $4 (rloc $startpos($4) $endpos($4)), Private, Cfk_virtual $6 }
| override_flag VIRTUAL private_flag label COLON poly_type
    { if $1 = Override then syntax_error $startpos $endpos;
        mkloc $4 (rloc $startpos($4) $endpos($4)), $3, Cfk_virtual $6 }
| override_flag private_flag label strict_binding
    { mkloc $3 (rloc $startpos($3) $endpos($3)), $2, Cfk_concrete ($1, ghexp $startpos $endpos (Pexp_poly ($4, None))) }
| override_flag private_flag label COLON poly_type EQUAL seq_expr
    { mkloc $3 (rloc $startpos($3) $endpos($3)), $2, Cfk_concrete ($1, ghexp $startpos $endpos (Pexp_poly($7, Some $5))) }
| override_flag private_flag label COLON TYPE lident_list DOT core_type EQUAL v10 = seq_expr
    { let exp, poly = wrap_type_annotation $startpos $endpos $6 $8 v10 in
        mkloc $3 (rloc $startpos($3) $endpos($3)), $2, Cfk_concrete ($1, ghexp $startpos $endpos (Pexp_poly(exp, Some poly))) }

(* Class types *)
class_type:
| class_signature
    { $1 }
| QUESTION LIDENT COLON simple_core_type_or_tuple_no_attr MINUSGREATER class_type
    { mkcty $startpos $endpos (Pcty_arrow("?" ^ $2 , mkoption $4, $6)) }
| OPTLABEL simple_core_type_or_tuple_no_attr MINUSGREATER class_type
    { mkcty $startpos $endpos (Pcty_arrow("?" ^ $1, mkoption $2, $4)) }
| LIDENT COLON simple_core_type_or_tuple_no_attr MINUSGREATER class_type
    { mkcty $startpos $endpos (Pcty_arrow($1, $3, $5)) }
| simple_core_type_or_tuple_no_attr MINUSGREATER class_type
    { mkcty $startpos $endpos (Pcty_arrow("", $1, $3)) }

class_signature:
| LBRACKET core_type_comma_list RBRACKET clty_longident
    { mkcty $startpos $endpos (Pcty_constr (mkloc $4 (rloc $startpos($4) $endpos($4)), List.rev $2)) }
| clty_longident
    { mkcty $startpos $endpos (Pcty_constr (mkrhs $startpos($1) $endpos($1) $1, [])) }
| OBJECT [@unclosed "object"] [@item "object"] class_sig_body END [@close]
    { mkcty $startpos $endpos (Pcty_signature $2) }
| class_signature attribute
    { Cty.attr $1 $2 }
| extension
    { mkcty $startpos $endpos (Pcty_extension $1) }

class_sig_body:
| class_self_type class_sig_fields
    { Csig.mk $1 (List.rev $2) }

class_self_type:
| LPAREN core_type RPAREN
    { $2 }
| (* empty *)
    { mktyp $startpos $endpos (Ptyp_any) }

class_sig_fields:
| (* empty *)
    { [] }
| class_sig_fields class_sig_field
    { $2 :: $1 }

class_sig_field:
| INHERIT class_signature attrs = post_item_attributes
    { mkctf $startpos $endpos (Pctf_inherit $2) ~attrs }
| VAL value_type attrs = post_item_attributes
    { mkctf $startpos $endpos (Pctf_val $2) ~attrs }
| METHOD private_virtual_flags label COLON poly_type attrs = post_item_attributes
    {
      let (p, v) = $2 in
      mkctf $startpos $endpos (Pctf_method ($3, p, v, $5)) ~attrs
    }
| CONSTRAINT constrain_field attrs = post_item_attributes
    { mkctf $startpos $endpos (Pctf_constraint $2) ~attrs }
| item_extension attrs = post_item_attributes
    { mkctf $startpos $endpos (Pctf_extension $1) ~attrs }
| floating_attribute
    { mkctf $startpos $endpos (Pctf_attribute $1) }

value_type:
| VIRTUAL mutable_flag label COLON core_type
    { $3, $2, Virtual, $5 }
| MUTABLE virtual_flag label COLON core_type
    { $3, Mutable, $2, $5 }
| label COLON core_type
    { $1, Immutable, Concrete, $3 }

constrain:
| core_type EQUAL core_type
    { $1, $3, (rloc $startpos $endpos) }

constrain_field:
| core_type EQUAL core_type
    { $1, $3 }

class_descriptions :
| class_descriptions AND class_description
    { $3 @ $1 }
| class_description
    { $1 }

class_description :
| virtual_flag class_type_parameters LIDENT COLON class_type post_item_attributes
    {
      [Ci.mk (mkrhs $startpos($3) $endpos($3) $3) $5
         ~virt:$1 ~params:$2
         ~attrs:$6 ~loc:(rloc $startpos $endpos)]
    }

class_type_declarations :
| class_type_declarations AND class_type_declaration
    { $3 @ $1 }
| class_type_declaration
    { $1 }

class_type_declaration :
| virtual_flag class_type_parameters LIDENT EQUAL class_signature post_item_attributes
    {
      [Ci.mk (mkrhs $startpos($3) $endpos($3) $3) $5
         ~virt:$1 ~params:$2
         ~attrs:$6 ~loc:(rloc $startpos $endpos)]
    }

(* Core expressions *)

seq_expr :
| expr %prec below_SEMI
    { $1 }
| expr SEMI
    { reloc_exp $startpos $endpos $1 }
| expr SEMI [@shift 1] seq_expr
    { mkexp $startpos $endpos (Pexp_sequence($1, $3)) }

labeled_simple_pattern:
| QUESTION LPAREN label_let_pattern opt_default RPAREN
    { ("?" ^ fst $3, $4, snd $3) }
| QUESTION label_var
    { ("?" ^ fst $2, None, snd $2) }
| OPTLABEL LPAREN let_pattern opt_default RPAREN
    { ("?" ^ $1, $4, $3) }
| OPTLABEL pattern_var
    { ("?" ^ $1, None, $2) }
| TILDE LPAREN label_let_pattern RPAREN
    { (fst $3, None, snd $3) }
| TILDE label_var
    { (fst $2, None, snd $2) }
| LABEL simple_pattern
    { ($1, None, $2) }
| simple_pattern
    { ("", None, $1) }

pattern_var:
| LIDENT
    { mkpat $startpos $endpos (Ppat_var (mkrhs $startpos($1) $endpos($1) $1)) }
| UNDERSCORE
    { mkpat $startpos $endpos Ppat_any }

opt_default:
| (* empty *)
    { None }
| EQUAL seq_expr
    { Some $2 }

label_let_pattern :
| label_var
    { $1 }
| label_var COLON core_type
    { let (lab, pat) = $1 in (lab, mkpat $startpos $endpos (Ppat_constraint(pat, $3))) }

label_var :
| LIDENT
    { ($1, mkpat $startpos $endpos (Ppat_var (mkrhs $startpos($1) $endpos($1) $1))) }

let_pattern [@recovery default_pattern ()]:
| pattern
    { $1 }
| pattern COLON core_type
    { mkpat $startpos $endpos (Ppat_constraint($1, $3)) }

%public expr [@recovery default_expr ()]:
| simple_expr %prec below_SHARP
    { $1 }
| simple_expr simple_labeled_expr_list
    { mkexp $startpos $endpos (Pexp_apply($1, List.rev $2)) }
| LET [@item "let"] ext_attributes rec_flag let_bindings_no_attrs IN seq_expr
    { mkexp_attrs $startpos $endpos (Pexp_let($3, List.rev $4, (merloc $endpos($5) $6))) $2 }
| LET MODULE [@item "let module"]
  ext_attributes UIDENT module_binding_body IN seq_expr
    { mkexp_attrs $startpos $endpos (Pexp_letmodule(mkrhs $startpos($4) $endpos($4) $4, $5, (merloc $endpos($6) $7))) $3 }
| LET OPEN [@item "let open"] expr_open IN seq_expr
   { let (flag,id,ext) = $3 in
      mkexp_attrs $startpos $endpos (Pexp_open(flag, id, (merloc $endpos($4) $5))) ext }
| FUNCTION [@item "function"]
  ext_attributes opt_bar match_cases
    { mkexp_attrs $startpos $endpos (Pexp_function(List.rev $4)) $2 }
| FUN [@item "fun"]
  ext_attributes labeled_simple_pattern fun_def
    { let (l,o,p) = $3 in
        mkexp_attrs $startpos $endpos (Pexp_fun(l, o, p, $4)) $2 }
| FUN [@item "fun"]
  ext_attributes newtype fun_def
    { mkexp_attrs $startpos $endpos (Pexp_newtype($3, $4)) $2 }
| MATCH [@item "match"]
  ext_attributes seq_expr WITH opt_bar match_cases
    { mkexp_attrs $startpos $endpos (Pexp_match($3, List.rev $6)) $2 }
| TRY [@item "try"]
  ext_attributes seq_expr WITH opt_bar match_cases
    { mkexp_attrs $startpos $endpos (Pexp_try($3, List.rev $6)) $2 }
| expr_comma_list %prec below_COMMA
    { mkexp $startpos $endpos (Pexp_tuple(List.rev $1)) }
| constr_longident simple_expr %prec below_SHARP
    { mkexp $startpos $endpos (Pexp_construct(mkrhs $startpos($1) $endpos($1) $1, Some $2)) }
| name_tag simple_expr %prec below_SHARP
    { mkexp $startpos $endpos (Pexp_variant($1, Some $2)) }
| IF [@item "if"]
  ext_attributes seq_expr
  THEN [@item "then clause"] expr
  ELSE [@item "else clause"] expr
    { mkexp_attrs $startpos $endpos (Pexp_ifthenelse($3, (merloc $endpos($4) $5), Some (merloc $endpos($6) $7))) $2 }
| IF [@item "if"]
  ext_attributes seq_expr
  THEN [@item "then clause"] expr
    { mkexp_attrs $startpos $endpos (Pexp_ifthenelse($3, (merloc $endpos($4) $5), None)) $2 }
| WHILE [@item "while"]
  ext_attributes seq_expr DO [@item "while body"] seq_expr DONE
    { mkexp_attrs $startpos $endpos (Pexp_while($3, (merloc $endpos($4) $5))) $2 }
| FOR [@item "for"]
  ext_attributes pattern EQUAL seq_expr direction_flag seq_expr
  DO [@item "for body"] seq_expr DONE
    { mkexp_attrs $startpos $endpos (Pexp_for($3, (merloc $endpos($4) $5), (merloc $endpos($6) $7), $6, (merloc $endpos($8) $9))) $2 }
| expr COLONCOLON expr
    { mkexp_cons (rloc $startpos($2) $endpos($2)) (ghexp $startpos $endpos (Pexp_tuple[$1;$3])) (rloc $startpos $endpos) }
| LPAREN COLONCOLON RPAREN LPAREN expr COMMA expr RPAREN
    { mkexp_cons (rloc $startpos($2) $endpos($2)) (ghexp $startpos $endpos (Pexp_tuple[$5;$7])) (rloc $startpos $endpos) }
| expr INFIXOP0 expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| expr INFIXOP1 expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| expr INFIXOP2 expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| expr INFIXOP3 expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| expr INFIXOP4 expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| expr PLUS expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "+" $3 }
| expr PLUSDOT expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "+." $3 }
| expr PLUSEQ expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "+=" $3 }
| expr MINUS expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "-" $3 }
| expr MINUSDOT expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "-." $3 }
| expr STAR expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "*" $3 }
| expr PERCENT expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "%" $3 }
| expr EQUAL expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "=" $3 }
| expr LESS expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "<" $3 }
| expr GREATER expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) ">" $3 }
| expr OR expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "or" $3 }
| expr BARBAR expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "||" $3 }
| expr AMPERSAND expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "&" $3 }
| expr AMPERAMPER expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "&&" $3 }
| expr COLONEQUAL expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) ":=" $3 }
| subtractive expr %prec prec_unary_minus
    { mkuminus $startpos $endpos $1 $2 }
| additive expr %prec prec_unary_plus
    { mkuplus $startpos $endpos $1 $2 }
| simple_expr DOT label_longident LESSMINUS expr
    { mkexp $startpos $endpos (Pexp_setfield($1, mkrhs $startpos($3) $endpos($3) $3, $5)) }
| simple_expr _ops = DOT _ope = LPAREN seq_expr RPAREN LESSMINUS expr
    { mkexp $startpos $endpos
          (Pexp_apply(ghexp $startpos(_ops) $endpos(_ope)
                 (Pexp_ident(array_function $startpos(_ops) $endpos(_ope) "Array" "set")),
                         ["",$1; "",$4; "",$7])) }
| simple_expr _ops = DOT _ope = LBRACKET seq_expr RBRACKET LESSMINUS expr
    { mkexp $startpos $endpos
          (Pexp_apply(ghexp $startpos(_ops) $endpos(_ope)
                 (Pexp_ident(array_function $startpos(_ops) $endpos(_ope) "String" "set")),
                         ["",$1; "",$4; "",$7])) }
| simple_expr _ops = DOT _ope = LBRACE expr RBRACE LESSMINUS expr
    { bigarray_set ($startpos,$endpos) ($startpos(_ops),$endpos(_ope)) $1 $4 $7 }
| label LESSMINUS expr
    { mkexp $startpos $endpos (Pexp_setinstvar(mkrhs $startpos($1) $endpos($1) $1, $3)) }
| ASSERT ext_attributes simple_expr %prec below_SHARP
    { mkexp_attrs $startpos $endpos (Pexp_assert $3) $2 }
| LAZY ext_attributes simple_expr %prec below_SHARP
    { mkexp_attrs $startpos $endpos (Pexp_lazy $3) $2 }
| OBJECT [@unclosed "object"] [@item "object"] ext_attributes class_structure END [@close]
    { mkexp_attrs $startpos $endpos (Pexp_object $3) $2 }
| expr attribute
    { Exp.attr $1 $2 }

simple_expr:
| val_longident
    { mkexp $startpos $endpos (Pexp_ident (mkrhs $startpos($1) $endpos($1) $1)) }
| constant
    { mkexp $startpos $endpos (Pexp_constant $1) }
| constr_longident %prec prec_constant_constructor
    { mkexp $startpos $endpos (Pexp_construct(mkrhs $startpos($1) $endpos($1) $1, None)) }
| name_tag %prec prec_constant_constructor
    { mkexp $startpos $endpos (Pexp_variant($1, None)) }
| LPAREN [@unclosed "("] seq_expr RPAREN [@close]
    { reloc_exp $startpos $endpos $2 }
| DOTLESS expr GREATERDOT
    { Fake.Meta.code $startpos $endpos $2 }
| DOTTILDE simple_expr %prec prec_escape
    { Fake.Meta.uncode $startpos $endpos $2 }
| BEGIN ext_attributes seq_expr END
    { wrap_exp_attrs $startpos $endpos (reloc_exp $startpos $endpos $3) $2 (* check location *) }
| BEGIN [@unclosed "begin"] ext_attributes END [@close]
    { mkexp_attrs $startpos $endpos (Pexp_construct (mkloc (Lident "()") (rloc $startpos $endpos),
                               None)) $2 }
| LPAREN seq_expr type_constraint RPAREN
    { mkexp_constraint $startpos $endpos $2 $3 }
| simple_expr DOT label_longident
    { mkexp $startpos $endpos (Pexp_field($1, mkrhs $startpos($3) $endpos($3) $3)) }
| mod_longident DOT LPAREN [@unclosed "("] seq_expr RPAREN [@close]
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos($1) $endpos($1) $1, $4)) }
| simple_expr _ops = DOT _ope = LPAREN [@unclosed "("] seq_expr RPAREN [@close]
    { mkexp $startpos $endpos
          (Pexp_apply(ghexp $startpos(_ops) $endpos(_ope)
                 (Pexp_ident(array_function $startpos(_ops) $endpos(_ope) "Array" "get")),
                         ["",$1; "",$4])) }
| simple_expr _ops = DOT _ope = LBRACKET [@unclosed "["] seq_expr RBRACKET [@close]
    { mkexp $startpos $endpos
          (Pexp_apply(ghexp $startpos(_ops) $endpos(_ope)
                 (Pexp_ident(array_function $startpos(_ops) $endpos(_ope) "String" "get")),
                         ["",$1; "",$4])) }
| simple_expr _ops = DOT _ope = LBRACE [@unclosed "{"] expr RBRACE [@close]
    { bigarray_get ($startpos,$endpos) ($startpos(_ops),$endpos(_ope)) $1 $4 }
| LBRACE [@unclosed "{"] record_expr RBRACE [@close]
    { let (exten, fields) = $2 in mkexp $startpos $endpos (Pexp_record(fields, exten)) }
| mod_longident DOT LBRACE [@unclosed "{"] record_expr RBRACE [@close]
    { let (exten, fields) = $4 in
        let rec_exp = mkexp $startpos $endpos (Pexp_record(fields, exten)) in
        mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos($1) $endpos($1) $1, rec_exp)) }
| LBRACKETBAR [@unclosed "[|"] expr_semi_list opt_semi BARRBRACKET [@close]
    { mkexp $startpos $endpos (Pexp_array(List.rev $2)) }
| LBRACKETBAR BARRBRACKET
    { mkexp $startpos $endpos (Pexp_array []) }
| mod_longident DOT LBRACKETBAR [@unclosed "[|"] expr_semi_list opt_semi BARRBRACKET [@close]
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos($1) $endpos($1) $1, mkexp $startpos($4) $endpos($4) (Pexp_array(List.rev $4)))) }
| LBRACKET [@unclosed "["] expr_semi_list opt_semi RBRACKET [@close]
    { reloc_exp $startpos $endpos (mktailexp $startpos($4) $endpos($4) (List.rev $2)) }
| mod_longident DOT LBRACKET [@unclosed "["] expr_semi_list opt_semi RBRACKET [@close]
    { let list_exp = reloc_exp $startpos $endpos (mktailexp $startpos($6) $endpos($6) (List.rev $4)) in
        mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos($1) $endpos($1) $1, list_exp)) }
| PREFIXOP simple_expr
    { mkexp $startpos $endpos (Pexp_apply(mkoperator $startpos($1) $endpos($1) $1, ["",$2])) }
| BANG simple_expr
    { mkexp $startpos $endpos (Pexp_apply(mkoperator $startpos($1) $endpos($1) "!", ["",$2])) }
| NEW ext_attributes class_longident
    { mkexp_attrs $startpos $endpos (Pexp_new(mkrhs $startpos($3) $endpos($3) $3)) $2 }
| LBRACELESS [@unclosed "{<"] field_expr_list opt_semi GREATERRBRACE
    { mkexp $startpos $endpos (Pexp_override(List.rev $2)) }
| LBRACELESS GREATERRBRACE
    { mkexp $startpos $endpos (Pexp_override [])}
| mod_longident DOT LBRACELESS [@unclosed "{<"] field_expr_list opt_semi GREATERRBRACE [@close]
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos($1) $endpos($1) $1, mkexp $startpos($4) $endpos($4) (Pexp_override(List.rev $4)))) }
| simple_expr SHARP [@shift_token (1,LIDENT "")] label
    { mkexp $startpos $endpos (Pexp_send($1, $3)) }
| simple_expr HASHOP [@shift_token (1,LIDENT "")] simple_expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| LPAREN [@unclosed "("] MODULE module_expr RPAREN [@close]
    { mkexp $startpos $endpos (Pexp_pack $3) }
| LPAREN [@unclosed "("] MODULE module_expr COLON package_type RPAREN [@close]
    { mkexp $startpos $endpos (Pexp_constraint (ghexp $startpos $endpos (Pexp_pack $3),
                                ghtyp $startpos $endpos (Ptyp_package $5))) }
| mod_longident DOT LPAREN [@unclosed "("] MODULE module_expr COLON package_type RPAREN [@close]
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos($1) $endpos($1) $1,
        mkexp $startpos $endpos (Pexp_constraint (ghexp $startpos $endpos (Pexp_pack $5),
                                ghtyp $startpos $endpos (Ptyp_package $7))))) }
| extension
    { mkexp $startpos $endpos (Pexp_extension $1) }
| QUESTIONQUESTION
    { let id = mkloc "merlin.hole" (rloc $startpos $endpos) in
      mkexp $startpos $endpos (Pexp_extension (id, PStr [])) }

simple_labeled_expr_list:
| labeled_simple_expr
    { [$1] }
| simple_labeled_expr_list labeled_simple_expr
    { $2 :: $1 }

labeled_simple_expr:
| simple_expr %prec below_SHARP
    { ("", $1) }
| label_expr
    { $1 }

label_expr:
| LABEL simple_expr %prec below_SHARP
    { ($1, $2) }
| TILDE label_ident
    { $2 }
| QUESTION label_ident
    { ("?" ^ fst $2, snd $2) }
| OPTLABEL simple_expr %prec below_SHARP
    { ("?" ^ $1, $2) }

label_ident :
| LIDENT
    { ($1, mkexp $startpos $endpos (Pexp_ident(mkrhs $startpos($1) $endpos($1) (Lident $1)))) }

let_bindings :
| let_binding
    { [$1] }
| let_bindings AND let_binding
    { $3 :: $1 }

let_bindings_no_attrs :
| l = let_bindings
    { List.iter (fun vb -> if vb.pvb_attributes <> [] then
        raise_error
          Syntaxerr.(Error(Not_expecting(vb.pvb_loc,"item attribute"))))
        l;
      l }

lident_list :
| LIDENT
    { [$1] }
| LIDENT lident_list
    { $1 :: $2 }

let_binding :
| let_binding_ post_item_attributes
    { let (p, e) = $1 in Vb.mk ~loc:(rloc $startpos $endpos) ~attrs:$2 p e }

let_binding_:
| val_ident fun_binding
    { (mkpatvar $startpos($1) $endpos($1) $1, $2) }
| val_ident COLON typevar_list DOT core_type EQUAL seq_expr
    { (ghpat $startpos $endpos (Ppat_constraint(mkpatvar $startpos($1) $endpos($1) $1,
                               ghtyp $startpos $endpos (Ptyp_poly(List.rev $3,$5)))),
         $7) }
| val_ident COLON TYPE lident_list DOT core_type EQUAL seq_expr
    { let exp, poly = wrap_type_annotation $startpos $endpos $4 $6 $8 in
        (ghpat $startpos $endpos (Ppat_constraint(mkpatvar $startpos($1) $endpos($1) $1, poly)), exp) }
| pattern EQUAL seq_expr
    { ($1, $3) }
| simple_pattern_not_ident COLON core_type EQUAL seq_expr
    { (ghpat $startpos $endpos (Ppat_constraint($1, $3)), $5) }

fun_binding :
| strict_binding
    { $1 }
| type_constraint EQUAL seq_expr
    { mkexp_constraint $startpos $endpos $3 $1 }

strict_binding:
| EQUAL seq_expr
    { $2 }
| labeled_simple_pattern fun_binding
    { let (l, o, p) = $1 in ghexp $startpos $endpos (Pexp_fun(l, o, p, $2)) }
| LPAREN TYPE LIDENT RPAREN fun_binding
    { mkexp $startpos $endpos (Pexp_newtype($3, $5)) }

match_cases [@recovery []]:
| match_case
    { [$1] }
| match_cases [@indent (-2)] BAR match_case
    { $3 :: $1 }

match_case:
| pattern [@item "pattern"]
  MINUSGREATER [@item "match action"] seq_expr
    { Exp.case $1 (merloc $endpos($2) $3) }
| pattern [@item "pattern"]
  WHEN [@item "when guard"] seq_expr
  MINUSGREATER [@item "match action"] seq_expr
    { Exp.case $1 ~guard:(merloc $endpos($2) $3) (merloc $endpos($4) $5) }

fun_def:
| MINUSGREATER seq_expr
(* Cf #5939: we used to accept (fun p when e0 -> e) *)
    { (merloc $endpos($1) $2) }
| labeled_simple_pattern fun_def
    {
       let (l,o,p) = $1 in
       ghexp $startpos $endpos (Pexp_fun(l, o, p, $2))
      }
| LPAREN TYPE LIDENT RPAREN fun_def
    { mkexp $startpos $endpos (Pexp_newtype($3, $5)) }

expr_comma_list:
| expr_comma_list COMMA expr
    { $3 :: $1 }
| expr COMMA expr
    { [$3; $1] }

record_expr :
| simple_expr WITH lbl_expr_list
    { (Some $1, $3) }
| lbl_expr_list
    { (None, $1) }

lbl_expr_list :
| lbl_expr
    { [$1] }
| lbl_expr SEMI lbl_expr_list
    { $1 :: $3 }
| lbl_expr SEMI
    { [$1] }

lbl_expr:
| label_longident EQUAL expr
    { (mkrhs $startpos($1) $endpos($1) $1,$3) }
| label_longident
    { (mkrhs $startpos($1) $endpos($1) $1, exp_of_label $startpos($1) $endpos($1) $1) }

field_expr_list :
| label EQUAL expr
    { [mkrhs $startpos($1) $endpos($1) $1,$3] }
| field_expr_list SEMI label EQUAL expr
    { (mkrhs $startpos($3) $endpos($3) $3, $5) :: $1 }

expr_semi_list :
| expr
    { [$1] }
| expr_semi_list SEMI expr
    { $3 :: $1 }

type_constraint:
| COLON [@item "type constraint"]
  core_type
    { (Some $2, None) }
| COLON [@item "type constraint"]
  core_type COLONGREATER core_type
    { (Some $2, Some $4) }
| COLONGREATER [@item "type constraint"]
  core_type
    { (None, Some $2) }

(* Patterns *)

pattern [@recovery default_pattern ()]:
| simple_pattern
    { $1 }
| pattern AS val_ident
    { mkpat $startpos $endpos (Ppat_alias($1, mkrhs $startpos($3) $endpos($3) $3)) }
(*| pattern AS error
    { expecting $startpos($3) $endpos($3) "identifier" }*)
| pattern_comma_list %prec below_COMMA
    { mkpat $startpos $endpos (Ppat_tuple(List.rev $1)) }
| constr_longident pattern %prec prec_constr_appl
    { mkpat $startpos $endpos (Ppat_construct(mkrhs $startpos($1) $endpos($1) $1, Some $2)) }
| name_tag pattern %prec prec_constr_appl
    { mkpat $startpos $endpos (Ppat_variant($1, Some $2)) }
| pattern COLONCOLON pattern
    { mkpat_cons (rloc $startpos($2) $endpos($2)) (ghpat $startpos $endpos (Ppat_tuple[$1;$3])) (rloc $startpos $endpos) }
(*| pattern COLONCOLON error
    { expecting $startpos($3) $endpos($3) "pattern" }*)
| LPAREN COLONCOLON RPAREN LPAREN pattern COMMA pattern RPAREN
    { mkpat_cons (rloc $startpos($2) $endpos($2)) (ghpat $startpos $endpos (Ppat_tuple[$5;$7])) (rloc $startpos $endpos) }
| pattern BAR pattern
    { mkpat $startpos $endpos (Ppat_or($1, $3)) }
(*| pattern BAR error
    { expecting $startpos($3) $endpos($3) "pattern" }*)
| LAZY simple_pattern
    { mkpat $startpos $endpos (Ppat_lazy $2) }
| EXCEPTION pattern %prec prec_constr_appl
    { mkpat $startpos $endpos (Ppat_exception $2) }
| pattern attribute
    { Pat.attr $1 $2 }

simple_pattern :
| val_ident %prec below_EQUAL
    { mkpat $startpos $endpos (Ppat_var (mkrhs $startpos($1) $endpos($1) $1)) }
| simple_pattern_not_ident
    { $1 }

simple_pattern_not_ident:
| UNDERSCORE
    { mkpat $startpos $endpos (Ppat_any) }
| signed_constant
    { mkpat $startpos $endpos (Ppat_constant $1) }
| signed_constant DOTDOT signed_constant
    { mkpat $startpos $endpos (Ppat_interval ($1, $3)) }
| constr_longident
    { mkpat $startpos $endpos (Ppat_construct(mkrhs $startpos($1) $endpos($1) $1, None)) }
| name_tag
    { mkpat $startpos $endpos (Ppat_variant($1, None)) }
| SHARP type_longident
    { mkpat $startpos $endpos (Ppat_type (mkrhs $startpos($2) $endpos($2) $2)) }
| LBRACE [@unclosed "{"]
  lbl_pattern_list RBRACE [@close]
    { let (fields, closed) = $2 in mkpat $startpos $endpos (Ppat_record(fields, closed)) }
| LBRACKET [@unclosed "["]
  pattern_semi_list opt_semi RBRACKET [@close]
    { reloc_pat $startpos $endpos (mktailpat $startpos($4) $endpos($4) (List.rev $2)) }
| LBRACKETBAR [@unclosed "[|"]
  pattern_semi_list opt_semi BARRBRACKET [@close]
    { mkpat $startpos $endpos (Ppat_array(List.rev $2)) }
| LBRACKETBAR BARRBRACKET
    { mkpat $startpos $endpos (Ppat_array []) }
| LPAREN [@unclosed "("] pattern RPAREN [@close]
    { reloc_pat $startpos $endpos $2 }
| LPAREN [@unclosed "("] pattern COLON core_type RPAREN [@close]
    { mkpat $startpos $endpos (Ppat_constraint($2, $4)) }
(*| LPAREN pattern COLON error
    { expecting $startpos($4) $endpos($4) "type" }*)
| LPAREN MODULE [@unclosed "("] UIDENT RPAREN [@close]
    { mkpat $startpos $endpos (Ppat_unpack (mkrhs $startpos($3) $endpos($3) $3)) }
| LPAREN MODULE [@unclosed "("] UIDENT COLON package_type RPAREN [@close]
    { mkpat $startpos $endpos (Ppat_constraint(mkpat $startpos $endpos(Ppat_unpack (mkrhs $startpos($3) $endpos($3) $3)),
                              ghtyp $startpos $endpos (Ptyp_package $5))) }
| extension
    { mkpat $startpos $endpos (Ppat_extension $1) }

pattern_comma_list:
| pattern_comma_list COMMA pattern
    { $3 :: $1 }
| pattern COMMA pattern
    { [$3; $1] }
(*| pattern COMMA error
    { expecting $startpos($3) $endpos($3) "pattern" }*)

pattern_semi_list :
| pattern
    { [$1] }
| pattern_semi_list SEMI pattern
    { $3 :: $1 }

lbl_pattern_list :
| lbl_pattern
    { [$1], Closed }
| lbl_pattern SEMI
    { [$1], Closed }
| lbl_pattern SEMI UNDERSCORE opt_semi
    { [$1], Open }
| lbl_pattern SEMI lbl_pattern_list
    { let (fields, closed) = $3 in $1 :: fields, closed }

lbl_pattern:
| label_longident EQUAL pattern
    { (mkrhs $startpos($1) $endpos($1) $1,$3) }
| label_longident
    { (mkrhs $startpos($1) $endpos($1) $1, pat_of_label $startpos($1) $endpos($1) $1) }

(* Primitive declarations *)

primitive_declaration :
| STRING
    { [fst $1] }
| STRING primitive_declaration
    { fst $1 :: $2 }

(* Type declarations *)

type_declarations :
| type_declaration
    { [$1] }
| type_declarations AND type_declaration
    { $3 :: $1 }

type_declaration:
| optional_type_parameters LIDENT type_kind constraints post_item_attributes
    { let (kind, priv, manifest) = $3 in
        Type.mk (mkrhs $startpos($2) $endpos($2) $2)
          ~params:$1 ~cstrs:(List.rev $4)
          ~kind ~priv ?manifest ~attrs:$5 ~loc:(rloc $startpos $endpos)
       }

constraints:
| constraints CONSTRAINT constrain
    { $3 :: $1 }
| (* empty *)
    { [] }

type_kind:
| (* empty *)
    { (Ptype_abstract, Public, None) }
| EQUAL core_type
    { (Ptype_abstract, Public, Some $2) }
| EQUAL PRIVATE core_type
    { (Ptype_abstract, Private, Some $3) }
| EQUAL constructor_declarations
    { (Ptype_variant(List.rev $2), Public, None) }
| EQUAL PRIVATE constructor_declarations
    { (Ptype_variant(List.rev $3), Private, None) }
| EQUAL private_flag BAR constructor_declarations
    { (Ptype_variant(List.rev $4), $2, None) }
| EQUAL private_flag LBRACE label_declarations opt_semi RBRACE
    { (Ptype_record(List.rev $4), $2, None) }
| EQUAL core_type EQUAL private_flag opt_bar constructor_declarations
    { (Ptype_variant(List.rev $6), $4, Some $2) }
| EQUAL core_type EQUAL private_flag LBRACE label_declarations opt_semi RBRACE
    { (Ptype_record(List.rev $6), $4, Some $2) }
| EQUAL DOTDOT
    { (Ptype_open, Public, None) }
| EQUAL core_type EQUAL DOTDOT
    { (Ptype_open, Public, Some $2) }

optional_type_parameters :
| (* empty *)
    { [] }
| optional_type_parameter
    { [$1] }
| LPAREN optional_type_parameter_list RPAREN
    { List.rev $2 }

optional_type_parameter :
| type_variance optional_type_variable
    { $2, $1 }

optional_type_parameter_list :
| optional_type_parameter
    { [$1] }
| optional_type_parameter_list COMMA optional_type_parameter
    { $3 :: $1 }

optional_type_variable :
| QUOTE ident
    { mktyp $startpos $endpos (Ptyp_var $2) }
| UNDERSCORE
    { mktyp $startpos $endpos (Ptyp_any) }

type_parameters :
| (* empty *)
    { [] }
| type_parameter
    { [$1] }
| LPAREN type_parameter_list RPAREN
    { List.rev $2 }

type_parameter :
| type_variance type_variable
    { $2, $1 }

type_variance :
| (* empty *)
    { Invariant }
| PLUS
    { Covariant }
| MINUS
    { Contravariant }

type_variable :
| QUOTE ident
    { mktyp $startpos $endpos (Ptyp_var $2) }

type_parameter_list :
| type_parameter
    { [$1] }
| type_parameter_list COMMA type_parameter
    { $3 :: $1 }

constructor_declarations :
| constructor_declaration
    { [$1] }
| constructor_declarations [@indent (-2)] BAR constructor_declaration
    { $3 :: $1 }

constructor_declaration:
| constr_ident generalized_constructor_arguments attributes
    {
      let args,res = $2 in
      Type.constructor (mkrhs $startpos($1) $endpos($1) $1) ~args ?res ~loc:(rloc $startpos $endpos) ~attrs:$3
    }

str_exception_declaration:
| extension_constructor_declaration post_item_attributes
    {
      let ext = $1 in
      {ext with pext_attributes = ext.pext_attributes @ $2}
    }
| extension_constructor_rebind post_item_attributes
    {
      let ext = $1 in
      {ext with pext_attributes = ext.pext_attributes @ $2}
    }

sig_exception_declaration:
| extension_constructor_declaration post_item_attributes
    {
      let ext = $1 in
      {ext with pext_attributes = ext.pext_attributes @ $2}
    }

generalized_constructor_arguments :
| (* empty *)
    { ([],None) }
| OF core_type_list_no_attr
    { (List.rev $2,None) }
| COLON core_type_list_no_attr MINUSGREATER simple_core_type_no_attr
    { (List.rev $2,Some $4) }
| COLON simple_core_type_no_attr
    { ([],Some $2) }

label_declarations :
| label_declaration
    { [$1] }
| label_declarations SEMI label_declaration
    { $3 :: $1 }

label_declaration:
| mutable_flag label COLON poly_type_no_attr attributes
  {
    Type.field (mkrhs $startpos($2) $endpos($2) $2) $4 ~mut:$1 ~attrs:$5 ~loc:(rloc $startpos $endpos)
  }

(* Type extensions *)

str_type_extension:
| optional_type_parameters type_longident
  PLUSEQ private_flag opt_bar str_extension_constructors
  post_item_attributes
    { Te.mk (mkrhs $startpos($2) $endpos($2) $2) (List.rev $6)
        ~params:$1 ~priv:$4 ~attrs:$7 }

sig_type_extension:
| optional_type_parameters type_longident
  PLUSEQ private_flag opt_bar sig_extension_constructors
  post_item_attributes
    { Te.mk (mkrhs $startpos($2) $endpos($2) $2) (List.rev $6)
        ~params:$1 ~priv:$4 ~attrs:$7 }

str_extension_constructors:
| extension_constructor_declaration
    { [$1] }
| extension_constructor_rebind
    { [$1] }
| str_extension_constructors BAR extension_constructor_declaration
    { $3 :: $1 }
| str_extension_constructors BAR extension_constructor_rebind
    { $3 :: $1 }

sig_extension_constructors:
| extension_constructor_declaration
    { [$1] }
| sig_extension_constructors BAR extension_constructor_declaration
    { $3 :: $1 }

extension_constructor_declaration:
| constr_ident generalized_constructor_arguments attributes
    { let args, res = $2 in
      Te.decl (mkrhs $startpos($1) $endpos($1) $1) ~args ?res
              ~loc:(rloc $startpos $endpos) ~attrs:$3
    }

extension_constructor_rebind:
| constr_ident EQUAL constr_longident attributes
    { Te.rebind (mkrhs $startpos($1) $endpos($1) $1)
                (mkrhs $startpos($3) $endpos($3) $3)
                ~loc:(rloc $startpos $endpos) ~attrs:$4
    }

(* "with" constraints (additional type equations over signature components) *)

with_constraints :
| with_constraint
    { $1 }
| with_constraints AND with_constraint
    { $3 @ $1 }

with_constraint:
| TYPE type_parameters label_longident with_type_binder core_type_no_attr constraints
    { [Pwith_type
          (mkrhs $startpos($3) $endpos($3) $3,
           (Type.mk (mkrhs $startpos($3) $endpos($3) (Longident.last $3))
              ~params:$2
              ~cstrs:(List.rev $6)
              ~manifest:$5
              ~priv:$4
              ~loc:(rloc $startpos $endpos)))] }
| TYPE type_parameters label COLONEQUAL core_type_no_attr
    { [Pwith_typesubst
          (Type.mk (mkrhs $startpos($3) $endpos($3) $3)
             ~params:$2
             ~manifest:$5
             ~loc:(rloc $startpos $endpos))] }
| MODULE mod_longident EQUAL mod_ext_longident
    { [Pwith_module (mkrhs $startpos($2) $endpos($2) $2, mkrhs $startpos($4) $endpos($4) $4)] }
| MODULE UIDENT COLONEQUAL mod_ext_longident
    { [Pwith_modsubst (mkrhs $startpos($2) $endpos($2) $2, mkrhs $startpos($4) $endpos($4) $4)] }

with_type_binder :
| EQUAL
    { Public }
| EQUAL PRIVATE
    { Private }

(* Polymorphic types *)

typevar_list :
| QUOTE ident
    { [$2] }
| typevar_list QUOTE ident
    { $3 :: $1 }

poly_type :
| core_type
    { $1 }
| typevar_list DOT core_type
    { mktyp $startpos $endpos (Ptyp_poly(List.rev $1, $3)) }

poly_type_no_attr :
| core_type_no_attr
    { $1 }
| typevar_list DOT core_type_no_attr
    { mktyp $startpos $endpos (Ptyp_poly(List.rev $1, $3)) }

(* Core types *)

core_type :
| core_type_no_attr
    { $1 }
| core_type attribute
    { Typ.attr $1 $2 }

core_type_no_attr :
| core_type2
    { $1 }
| core_type2 AS QUOTE ident
    { mktyp $startpos $endpos (Ptyp_alias($1, $4)) }

core_type2 :
| simple_core_type_or_tuple
    { $1 }
| QUESTION LIDENT COLON core_type2 MINUSGREATER core_type2
    { mktyp $startpos $endpos (Ptyp_arrow("?" ^ $2 , mkoption $4, $6)) }
| OPTLABEL core_type2 MINUSGREATER core_type2
    { mktyp $startpos $endpos (Ptyp_arrow("?" ^ $1 , mkoption $2, $4)) }
| LIDENT COLON core_type2 MINUSGREATER core_type2
    { mktyp $startpos $endpos (Ptyp_arrow($1, $3, $5)) }
| core_type2 MINUSGREATER core_type2
    { mktyp $startpos $endpos (Ptyp_arrow("", $1, $3)) }

simple_core_type :
| simple_core_type2 %prec below_SHARP
    { $1 }
| LPAREN core_type_comma_list RPAREN %prec below_SHARP
    { match $2 with [sty] -> sty
                  | _ ->
                    syntax_error $startpos $endpos;
                    mktyp $startpos $endpos (Ptyp_any)
    }

simple_core_type_no_attr :
| simple_core_type2 %prec below_SHARP
    { $1 }
| LPAREN core_type_comma_list RPAREN %prec below_SHARP
    { match $2 with [sty] -> sty
                  | _ ->
                    syntax_error $startpos $endpos;
                    mktyp $startpos $endpos (Ptyp_any)
    }

simple_core_type2 :
| QUOTE ident
    { mktyp $startpos $endpos (Ptyp_var $2) }
| UNDERSCORE
    { mktyp $startpos $endpos (Ptyp_any) }
| type_longident
    { mktyp $startpos $endpos (Ptyp_constr(mkrhs $startpos($1) $endpos($1) $1, [])) }
| simple_core_type2 type_longident
    { mktyp $startpos $endpos (Ptyp_constr(mkrhs $startpos($2) $endpos($2) $2, [$1])) }
| LPAREN core_type_comma_list RPAREN type_longident
    { mktyp $startpos $endpos (Ptyp_constr(mkrhs $startpos($4) $endpos($4) $4, List.rev $2)) }
| LESS meth_list GREATER
    { let (f, c) = $2 in mktyp $startpos $endpos (Ptyp_object (f, c)) }
| LESS GREATER
    { mktyp $startpos $endpos (Ptyp_object ([], Closed)) }
| SHARP class_longident
    { mktyp $startpos $endpos (Ptyp_class(mkrhs $startpos($2) $endpos($2) $2, [])) }
| simple_core_type2 SHARP class_longident
    { mktyp $startpos $endpos (Ptyp_class(mkrhs $startpos($3) $endpos($3) $3, [$1])) }
| LPAREN core_type_comma_list RPAREN SHARP class_longident
    { mktyp $startpos $endpos (Ptyp_class(mkrhs $startpos($5) $endpos($5) $5, List.rev $2)) }
| LBRACKET tag_field RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant([$2], Closed, None)) }
(* PR#3835: this is not LR(1), would need lookahead=2
  | LBRACKET simple_core_type RBRACKET
      { mktyp $startpos $endpos (Ptyp_variant([$2], Closed, None)) }
*)
| LBRACKET BAR row_field_list RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant(List.rev $3, Closed, None)) }
| LBRACKET row_field BAR row_field_list RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant($2 :: List.rev $4, Closed, None)) }
| LBRACKETGREATER opt_bar row_field_list RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant(List.rev $3, Open, None)) }
| LBRACKETGREATER RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant([], Open, None)) }
| LBRACKETLESS opt_bar row_field_list RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant(List.rev $3, Closed, Some [])) }
| LBRACKETLESS opt_bar row_field_list GREATER name_tag_list RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant(List.rev $3, Closed, Some (List.rev $5))) }
| LPAREN MODULE package_type RPAREN
    { mktyp $startpos $endpos (Ptyp_package $3) }
| extension
    { mktyp $startpos $endpos (Ptyp_extension $1) }

package_type :
| mty_longident
    { (mkrhs $startpos($1) $endpos($1) $1, []) }
| mty_longident WITH package_type_cstrs
    { (mkrhs $startpos($1) $endpos($1) $1, $3) }

package_type_cstr:
| TYPE label_longident EQUAL core_type
    { (mkrhs $startpos($2) $endpos($2) $2, $4) }

package_type_cstrs :
| package_type_cstr
    { [$1] }
| package_type_cstr AND package_type_cstrs
    { $1::$3 }

row_field_list :
| row_field
    { [$1] }
| row_field_list BAR row_field
    { $3 :: $1 }

row_field:
| tag_field
    { $1 }
| simple_core_type
    { Rinherit $1 }

tag_field:
| name_tag OF opt_ampersand amper_type_list attributes
    { Rtag ($1, $5, $3, List.rev $4) }
| name_tag attributes
    { Rtag ($1, $2, true, []) }

opt_ampersand:
| AMPERSAND
    { true }
| (* empty *)
    { false }

amper_type_list :
| core_type_no_attr
    { [$1] }
| amper_type_list AMPERSAND core_type_no_attr
    { $3 :: $1 }

name_tag_list :
| name_tag
    { [$1] }
| name_tag_list name_tag
    { $2 :: $1 }

simple_core_type_or_tuple :
| simple_core_type %prec below_LBRACKETAT
    { $1 }
| simple_core_type STAR core_type_list
    { mktyp $startpos $endpos (Ptyp_tuple($1 :: List.rev $3)) }

simple_core_type_or_tuple_no_attr :
| simple_core_type_no_attr
    { $1 }
| simple_core_type_no_attr STAR core_type_list_no_attr
    { mktyp $startpos $endpos (Ptyp_tuple($1 :: List.rev $3)) }

core_type_comma_list :
| core_type
    { [$1] }
| core_type_comma_list COMMA core_type
    { $3 :: $1 }

core_type_list :
| simple_core_type %prec below_LBRACKETAT
    { [$1] }
| core_type_list STAR simple_core_type
    { $3 :: $1 }

core_type_list_no_attr :
| simple_core_type_no_attr
    { [$1] }
| core_type_list STAR simple_core_type_no_attr
    { $3 :: $1 }

meth_list :
| field SEMI meth_list
    { let (f, c) = $3 in ($1 :: f, c) }
| field opt_semi
    { [$1], Closed }
| DOTDOT
    { [], Open }

field:
| label COLON poly_type_no_attr attributes
    { ($1, $4, $3) }

label :
| LIDENT
    { $1 }

(* Constants *)

constant :
| INT
    { Const_int $1 }
| CHAR
    { Const_char $1 }
| STRING
    { let (s, d) = $1 in Pconst_string (s, d) }
| FLOAT
    { Const_float $1 }
| INT32
    { Const_int32 $1 }
| INT64
    { Const_int64 $1 }
| NATIVEINT
    { Const_nativeint $1 }

signed_constant :
| constant
    { $1 }
| MINUS INT
    { Const_int(- $2) }
| MINUS FLOAT
    { Const_float("-" ^ $2) }
| MINUS INT32
    { Const_int32(Int32.neg $2) }
| MINUS INT64
    { Const_int64(Int64.neg $2) }
| MINUS NATIVEINT
    { Const_nativeint(Nativeint.neg $2) }
| PLUS INT
    { Const_int $2 }
| PLUS FLOAT
    { Const_float $2 }
| PLUS INT32
    { Const_int32 $2 }
| PLUS INT64
    { Const_int64 $2 }
| PLUS NATIVEINT
    { Const_nativeint $2 }

(* Identifiers and long identifiers *)

ident :
| UIDENT
    { $1 }
| LIDENT
    { $1 }

val_ident :
| LIDENT
    { $1 }
| LPAREN [@unclosed "("] operator RPAREN [@close]
    { $2 }
(*| LPAREN $2 =error
    { expecting $startpos($2) $endpos($2) "operator" }*)
(*| LPAREN MODULE error
    { expecting $startpos($3) $endpos($3) "module-expr" }*)

operator :
| PREFIXOP
    { $1 }
| INFIXOP0
    { $1 }
| INFIXOP1
    { $1 }
| INFIXOP2
    { $1 }
| INFIXOP3
    { $1 }
| INFIXOP4
    { $1 }
| LETOP
    { $1 }
| HASHOP
    { $1 }
| BANG
    { "!" }
| PLUS
    { "+" }
| PLUSDOT
    { "+." }
| MINUS
    { "-" }
| MINUSDOT
    { "-." }
| STAR
    { "*" }
| EQUAL
    { "=" }
| LESS
    { "<" }
| GREATER
    { ">" }
| OR
    { "or" }
| BARBAR
    { "||" }
| AMPERSAND
    { "&" }
| AMPERAMPER
    { "&&" }
| COLONEQUAL
    { ":=" }
| PLUSEQ
    { "+=" }
| PERCENT
    { "%" }

constr_ident:
| UIDENT
    { $1 }
(* | LBRACKET RBRACKET { "[]" } *)
| LPAREN RPAREN
    { "()" }
| COLONCOLON
    { "::" }
(* | LPAREN COLONCOLON RPAREN { "::" } *)
| FALSE
    { "false" }
| TRUE
    { "true" }

val_longident :
| val_ident
    { Lident $1 }
| mod_longident DOT val_ident
    { Ldot($1, $3) }

constr_longident :
| mod_longident %prec below_DOT
    { $1 }
| LBRACKET RBRACKET
    { Lident "[]" }
| LPAREN RPAREN
    { Lident "()" }
| FALSE
    { Lident "false" }
| TRUE
    { Lident "true" }

label_longident :
| LIDENT
    { Lident $1 }
| mod_longident DOT LIDENT
    { Ldot($1, $3) }

type_longident :
| LIDENT
    { Lident $1 }
| mod_ext_longident DOT LIDENT
    { Ldot($1, $3) }

mod_longident :
| UIDENT
    { Lident $1 }
| mod_longident DOT UIDENT
    { Ldot($1, $3) }

mod_ext_longident :
| UIDENT
    { Lident $1 }
| mod_ext_longident DOT UIDENT
    { Ldot($1, $3) }
| mod_ext_longident LPAREN mod_ext_longident RPAREN
    { lapply $startpos $endpos $1 $3 }

mty_longident :
| ident
    { Lident $1 }
| mod_ext_longident DOT ident
    { Ldot($1, $3) }

clty_longident :
| LIDENT
    { Lident $1 }
| mod_ext_longident DOT LIDENT
    { Ldot($1, $3) }

class_longident :
| LIDENT
    { Lident $1 }
| mod_longident DOT LIDENT
    { Ldot($1, $3) }

(* Miscell aneous *)

name_tag:
| BACKQUOTE ident
    { $2 }

rec_flag :
| (* empty *)
    { Nonrecursive }
| REC
    { Recursive }

direction_flag :
| TO
    { Upto }
| DOWNTO
    { Downto }

private_flag:
| (* empty *)
    { Public }
| PRIVATE
    { Private }

mutable_flag:
| (* empty *)
    { Immutable }
| MUTABLE
    { Mutable }

virtual_flag:
| (* empty *)
    { Concrete }
| VIRTUAL
    { Virtual }

private_virtual_flags:
| (* empty *)
    { Public, Concrete }
| PRIVATE
    { Private, Concrete }
| VIRTUAL
    { Public, Virtual }
| PRIVATE VIRTUAL
    { Private, Virtual }
| VIRTUAL PRIVATE
    { Private, Virtual }

override_flag:
| (* empty *)
    { Fresh }
| BANG
    { Override }

opt_bar:
| (* empty *)
    { () }
| BAR
    { () }

opt_semi:
| (* empty *)
    { () }
| SEMI
    { () }

subtractive:
| MINUS
    { "-" }
| MINUSDOT
    { "-." }

additive:
| PLUS
    { "+" }
| PLUSDOT
    { "+." }


(* Attributes and extensions *)

single_attr_id:
| LIDENT
    { $1 }
| UIDENT
    { $1 }
| AND
    { "and" }
| AS
    { "as" }
| ASSERT
    { "assert" }
| BEGIN
    { "begin" }
| CLASS
    { "class" }
| CONSTRAINT
    { "constraint" }
| DO
    { "do" }
| DONE
    { "done" }
| DOWNTO
    { "downto" }
| ELSE
    { "else" }
| END
    { "end" }
| EXCEPTION
    { "exception" }
| EXTERNAL
    { "external" }
| FALSE
    { "false" }
| FOR
    { "for" }
| FUN
    { "fun" }
| FUNCTION
    { "function" }
| FUNCTOR
    { "functor" }
| IF
    { "if" }
| IN
    { "in" }
| INCLUDE
    { "include" }
| INHERIT
    { "inherit" }
| INITIALIZER
    { "initializer" }
| LAZY
    { "lazy" }
| LET
    { "let" }
| MATCH
    { "match" }
| METHOD
    { "method" }
| MODULE
    { "module" }
| MUTABLE
    { "mutable" }
| NEW
    { "new" }
| OBJECT
    { "object" }
| OF
    { "of" }
| OPEN
    { "open" }
| OR
    { "or" }
| PRIVATE
    { "private" }
| REC
    { "rec" }
| SIG
    { "sig" }
| STRUCT
    { "struct" }
| THEN
    { "then" }
| TO
    { "to" }
| TRUE
    { "true" }
| TRY
    { "try" }
| TYPE
    { "type" }
| VAL
    { "val" }
| VIRTUAL
    { "virtual" }
| WHEN
    { "when" }
| WHILE
    { "while" }
| WITH
    { "with" }
(* mod/land/lor/lxor/lsl/lsr/asr are not supported for now *)

attr_id :
| single_attr_id
    { mkloc $1 (rloc $startpos $endpos) }
| single_attr_id DOT attr_id
    { mkloc ($1 ^ "." ^ $3.txt) (rloc $startpos $endpos)}

attribute:
| LBRACKETAT [@item "attribute"] attr_id payload RBRACKET
    { ($2, $3) }

post_item_attribute:
| LBRACKETATAT [@item "attribute"] attr_id payload RBRACKET
    { ($2, $3) }

floating_attribute:
| LBRACKETATATAT [@item "attribute"] attr_id payload RBRACKET
    { ($2, $3) }

post_item_attributes :
| (* empty *)
    { [] }
| post_item_attribute post_item_attributes
    { $1 :: $2 }

attributes :
| (* empty *)
    { [] }
| attribute attributes
    { $1 :: $2 }

ext_attributes [@cost 1]:
| (* empty *)
    { None, [] }
| attribute attributes
    { None, $1 :: $2 }
| PERCENT attr_id attributes
    { Some $2, $3 }

extension:
| LBRACKETPERCENT [@item "extension"] attr_id payload RBRACKET
    { ($2, $3) }

item_extension:
| LBRACKETPERCENTPERCENT [@item "extension"] attr_id payload RBRACKET
    { ($2, $3) }

payload :
| structure
    { PStr $1 }
| COLON core_type
    { PTyp $2 }
| QUESTION pattern
    { PPat ($2, None) }
| QUESTION pattern WHEN seq_expr
    { PPat ($2, Some $4) }

(* Merlin refactoring *)

newtype :
| LPAREN TYPE LIDENT RPAREN
    { $3 }

expr_open :
| override_flag ext_attributes mod_longident
    { $1, mkrhs $startpos($3) $endpos($3) $3, $2 }

(* Caml p4 extensions *)
%public structure_item:
| LET_LWT [@item "lwt"] ext_attributes rec_flag let_bindings
    { match $4 with
    | [ {pvb_pat = { ppat_desc = Ppat_any; ppat_loc = _ };
         pvb_expr = exp; pvb_attributes = attrs} ] ->
        let exp = wrap_exp_attrs $startpos $endpos exp $2 in
        mkstr $startpos $endpos (Pstr_eval (Fake.app Fake.Lwt.un_lwt exp, attrs))
    | _ ->
      let str = mkstr $startpos $endpos
            (Pstr_value ($3, List.rev_map (fake_vb_app Fake.Lwt.un_lwt) $4))
      in
      let (ext, attrs) = $2 in
      if attrs <> [] then not_expecting $startpos($2) $endpos($2) "attribute";
      match ext with
      | None -> str
      | Some id -> ghstr $startpos $endpos (Pstr_extension((id, PStr str), []))
    }
| TYPE NONREC [@item "type nonrec"] decls = type_declarations
    { let ty = List.map fake_tydecl decls in
      let loc = rloc $startpos($2) $endpos($2) in
      mkstr $startpos $endpos (Pstr_type(List.rev_map (tag_nonrec loc) ty)) }

%public signature_item:
| TYPE NONREC [@item "type nonrec"] decls = type_declarations
    { let ty = List.map fake_tydecl decls in
      let loc = rloc $startpos($2) $endpos($2) in
      mksig $startpos $endpos (Psig_type (List.rev_map (tag_nonrec loc) ty)) }

%public expr:
| LET_LWT [@item "lwt"]
  ext_attributes rec_flag let_bindings IN [@shift 2] seq_expr
    { let expr = Pexp_let($3, List.rev_map (fake_vb_app Fake.Lwt.un_lwt) $4, (merloc $endpos($5) $6)) in
      Fake.app Fake.Lwt.in_lwt (mkexp_attrs $startpos $endpos expr $2) }
| MATCH_LWT [@item "match_lwt"]
  ext_attributes seq_expr WITH opt_bar match_cases
    { let expr = mkexp_attrs $startpos $endpos
          (Pexp_match(Fake.app Fake.Lwt.un_lwt $3, List.rev $6)) $2 in
      Fake.app Fake.Lwt.in_lwt expr }
| TRY_LWT [@item "try_lwt"]
  ext_attributes seq_expr %prec below_WITH
    { reloc_exp $startpos $endpos (Fake.app Fake.Lwt.in_lwt $3) }
| TRY_LWT [@item "try_lwt"]
  ext_attributes seq_expr WITH opt_bar match_cases
    { mkexp_attrs $startpos $endpos
        (Pexp_try(Fake.app Fake.Lwt.in_lwt $3, List.rev $6)) $2 }
| TRY_LWT [@item "try_lwt"]
  ext_attributes seq_expr FINALLY_LWT seq_expr
    { Fake.app (Fake.app Fake.Lwt.finally_ $3) $5 }
| TRY_LWT [@item "try_lwt"]
  ext_attributes seq_expr WITH opt_bar match_cases FINALLY_LWT seq_expr
    { let expr = mkexp_attrs $startpos $endpos
        (Pexp_try (Fake.app Fake.Lwt.in_lwt $3, List.rev $6)) $2 in
      Fake.app (Fake.app Fake.Lwt.finally_ expr) $8 }
| WHILE_LWT [@item "while_lwt"]
  ext_attributes seq_expr
  DO [@item "while_lwt body"] seq_expr DONE
  { let expr = Pexp_while ($3, Fake.(app Lwt.un_lwt $5)) in
    Fake.(app Lwt.to_lwt (mkexp_attrs $startpos $endpos expr $2)) }
| FOR_LWT [@item "for_lwt"]
  ext_attributes pattern EQUAL seq_expr direction_flag seq_expr
  DO [@item "for body"] seq_expr DONE
    { let expr = Pexp_for ($3, $5, $7, $6, Fake.(app Lwt.un_lwt $9)) in
      Fake.(app Lwt.to_lwt (mkexp_attrs $startpos $endpos expr $2)) }
| FOR_LWT [@item "for_lwt"]
  ext_attributes pattern IN seq_expr
  DO [@item "for body"] seq_expr DONE
    { mkexp_attrs $startpos $endpos
          (Pexp_let (Nonrecursive, [Vb.mk $3 (Fake.(app Lwt.un_stream $5))],
             Fake.(app Lwt.unit_lwt $7)))
          $2
    }
| let_operator [@item "meta-let"] ext_attributes let_bindings IN seq_expr
    { wrap_exp_attrs $startpos $endpos
      (let_operator $startpos $endpos $1 $3 $5) $2 }
;

(* Meta OCaml *)
let_operator:
| LETOP
  { mkexp $startpos $endpos
    (Pexp_ident(mkloc (Lident $1) (rloc $startpos $endpos))) }
| mod_longident DOT LETOP
  { mkexp $startpos $endpos
    (Pexp_ident(mkloc (Ldot ($1,$3)) (rloc $startpos $endpos))) }
;

(* Toplevel directives *)
%inline toplevel_payload:
                { None }
  | STRING { let (s, d) = $1 in
                  Some (Exp.constant (Pconst_string (s, d) )) }
  | INT { Some (Exp.constant (Const_int $1)) }
  | val_longident | mod_longident { Some (Exp.ident (mkloc $1 (rloc $startpos $endpos))) }
  | FALSE { Some (Exp.construct (mkloc (Longident.Lident "false") (rloc $startpos $endpos)) None) }
  | TRUE { Some (Exp.construct (mkloc (Longident.Lident "true") (rloc $startpos $endpos)) None) }
;

toplevel_directives:
    (* empty *) { [] }
  | toplevel_directives SHARP ident toplevel_payload
    { let id = mkloc ("merlin.directive." ^ $3) (rloc $startpos($3) $endpos($3)) in
      let payload =
        match $4 with
        | None -> PStr []
        | Some exp -> PStr [mkstrexp exp []]
      in
      mkstr $startpos $endpos (Pstr_attribute (id,payload)) @ $1
    }
;

%%

(enabled_if (>= %{ocaml_version} 4.04.0))

?? should be parsed as merlin.hole, and merlin.hole shouldn't be treated as a
type error.

  $ echo "let () = ??" | \
  > $MERLIN single errors -filename hole_0.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

This incomplete expression should generate only a parser error.
The hole is filled with merlin.hole.

  $ echo "let _ =" | \
  > $MERLIN single errors -filename hole_1.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 0
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "Syntax error, expecting expr"
      }
    ],
    "notifications": []
  }

  $ echo "let _ =" | \
  > $MERLIN single dump -what source -filename hole_1.ml | \
  > tr -d '\n' | jq '.value'
  "let _ = [%merlin.hole ]"

A bit trickier: the recovery is tempted to put a ->. (unreachable), but the
penalty should prevent it.

  $ echo "let f = function _ ->" | \
  > $MERLIN single errors -filename  "hole_2.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 0
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "Syntax error, expecting expr"
      }
    ],
    "notifications": []
  }

Issue #713: merlin would error when it cannot recover, but in some files there
really is nothing to recover.

Note: in 4.08 the suggestion changed from "exception" to "open" and the recovery
generates "let open struct end in [%merlin.hole ]".

FIXME: the syntax error message is off the mark.

  $ echo "let" | \
  > $MERLIN single errors -filename  "two_constr.ml" | \
  > sed 's/expecting.*/<unstable suggestion>/'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 0
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "Syntax error, <unstable suggestion>
      }
    ],
    "notifications": []
  }

  $ echo "let test x = match x with | None -> exit 1 | Some pkg -> pkg end" | \
  > $MERLIN single dump -what source -filename "lessminus.ml"
  {
    "class": "return",
    "value": "let test x =
    match x with
    | None -> ((exit 1)[@merlin.loc ])
    | Some pkg -> ((pkg)[@merlin.loc ])
  
  ",
    "notifications": []
  }

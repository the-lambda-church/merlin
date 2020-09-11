These tests ensure that all type errors are caught by the kernel, no exception
should reach top-level

  $ echo "type p = P : 'a -> 'a -> p" | \
  > $MERLIN single errors -filename ./incorrect_gadt.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 19
        },
        "end": {
          "line": 1,
          "col": 26
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The type constructor p expects 0 argument(s),
  but is here applied to 1 argument(s)"
      },
      {
        "start": {
          "line": 1,
          "col": 22
        },
        "end": {
          "line": 1,
          "col": 24
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "Syntax error"
      }
    ],
    "notifications": []
  }

  $ echo "let error : unknown_type_constructor = assert false" | \
  > $MERLIN single errors -filename  "unkown_constr.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 12
        },
        "end": {
          "line": 1,
          "col": 36
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound type constructor unknown_type_constructor"
      }
    ],
    "notifications": []
  }

  $ echo "val error : unknown_type_constructor" | \
  > $MERLIN single errors -filename  "unkown_constr.mli"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 12
        },
        "end": {
          "line": 1,
          "col": 36
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound type constructor unknown_type_constructor"
      }
    ],
    "notifications": []
  }

  $ echo "type t = A | A" | \
  > $MERLIN single errors -filename  "two_constr.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Two constructors are named A"
      }
    ],
    "notifications": []
  }

  $ echo "type t = A | A" | \
  > $MERLIN single errors -filename  "two_constr.mli"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Two constructors are named A"
      }
    ],
    "notifications": []
  }

  $ echo "let x = 4 val x : int" | \
  > $MERLIN single errors -filename  "ml_in_mli.mli"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 3
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "Syntax error"
      }
    ],
    "notifications": []
  }

vals are no allowed in ml files and detected during semantic analysis

  $ echo "val x : int" | \
  > $MERLIN single errors -filename  "mli_in_ml.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 11
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Value declarations are only allowed in signatures"
      }
    ],
    "notifications": []
  }

The code should raise a single error (for Bb typo), but shouldn't report the
unused case after

  $ $MERLIN single errors -filename "unused_case_after_error.ml" <<EOF \
  > type t = A | B | C \
  > let f = function \
  >   | A -> 1 \
  >   | Bb -> 1 \
  >   | C -> 1 \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 4
        },
        "end": {
          "line": 4,
          "col": 6
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This variant pattern is expected to have type t
  The constructor Bb does not belong to type t"
      }
    ],
    "notifications": []
  }

Syntax errors also shouldn't escape:

  $ echo "let f (_ : (module S with type 'a t = int)) = ()" | \
  > $MERLIN single errors -filename "invalid_package_type.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 11
        },
        "end": {
          "line": 1,
          "col": 42
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module type S"
      },
      {
        "start": {
          "line": 1,
          "col": 26
        },
        "end": {
          "line": 1,
          "col": 41
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "invalid package type: parametrized types are not supported"
      }
    ],
    "notifications": []
  }

Env initialization errors should also be caught, though it is currently
difficult to report them if the buffer is empty, therefore there should be
a different behavior between:

  $ echo "" | $MERLIN single errors -open Absent_unit -filename "env_init.ml"
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

And:

  $ echo "let x = 3" | \
  > $MERLIN single errors -open Absent_unit -filename "env_init.ml" | \
  > jq ".value |= (map(del(.start.line) | del(.end.line)))"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "col": -1
        },
        "end": {
          "col": -1
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module Absent_unit"
      }
    ],
    "notifications": []
  }

And of course, it should never leak for other requests:

  $ echo "" | $MERLIN single type-enclosing -position 1:0 -expression "3" \
  > -open Absent_unit -filename "env_init.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": -1
        },
        "end": {
          "line": 1,
          "col": 0
        },
        "type": "int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

When typing the Test module, Merlin will try to load the Foo dependency.
However foo.cmi is not a valid cmi file, we must make sure Merlin handle this
properly (this should also cover the "wrong magic number" case).

  $ $MERLIN single errors -filename test_use.ml < test_use.ml | \
  > tr '\r\n' ' ' | jq ".value |= (map(del(.start.line) | del(.end.line)))"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "col": -1
        },
        "end": {
          "col": -1
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Corrupted compiled interface tests/test-dirs/no-escape/foo.cmi"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single errors -filename test_open.ml -open Foo < test_open.ml | \
  > tr '\r\n' ' ' | jq ".value |= (map(del(.start.line) | del(.end.line)))"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "col": -1
        },
        "end": {
          "col": -1
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Corrupted compiled interface tests/test-dirs/no-escape/foo.cmi"
      }
    ],
    "notifications": []
  }

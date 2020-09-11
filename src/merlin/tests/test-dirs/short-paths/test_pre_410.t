(enabled_if (< %{ocaml_version} 4.10.0))

  $ $OCAMLC -c dep.mli

  $ $MERLIN single errors -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 21,
          "col": 36
        },
        "end": {
          "line": 21,
          "col": 49
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Some type variables are unbound in this type: class b : 'a -> a
  The method x has type 'a where 'a is unbound"
      },
      {
        "start": {
          "line": 23,
          "col": 46
        },
        "end": {
          "line": 23,
          "col": 47
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This class expression is not a class structure; it has type 'a -> a"
      },
      {
        "start": {
          "line": 27,
          "col": 0
        },
        "end": {
          "line": 30,
          "col": 3
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Some type variables are unbound in this type:
    class test : ?a:'a -> object method b : 'b end
  The method b has type 'b where 'b is unbound"
      },
      {
        "start": {
          "line": 27,
          "col": 12
        },
        "end": {
          "line": 27,
          "col": 13
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 16: this optional argument cannot be erased."
      },
      {
        "start": {
          "line": 29,
          "col": 13
        },
        "end": {
          "line": 29,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value c"
      },
      {
        "start": {
          "line": 34,
          "col": 0
        },
        "end": {
          "line": 37,
          "col": 3
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Some type variables are unbound in this type:
    class test : 'a -> object method b : 'b end
  The method b has type 'b where 'b is unbound"
      },
      {
        "start": {
          "line": 36,
          "col": 13
        },
        "end": {
          "line": 36,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value c"
      },
      {
        "start": {
          "line": 39,
          "col": 12
        },
        "end": {
          "line": 39,
          "col": 13
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 16: this optional argument cannot be erased."
      },
      {
        "start": {
          "line": 63,
          "col": 25
        },
        "end": {
          "line": 63,
          "col": 26
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type t = M.t but an expression was expected of type unit"
      },
      {
        "start": {
          "line": 82,
          "col": 13
        },
        "end": {
          "line": 86,
          "col": 6
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This functor has type functor (S : S) -> sig val bar : int -> S.t end
  The parameter cannot be eliminated in the result type.
  Please bind the argument to a module identifier."
      },
      {
        "start": {
          "line": 82,
          "col": 22
        },
        "end": {
          "line": 86,
          "col": 5
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Signature mismatch:
  Modules do not match:
    sig type t = int val foo : 'a -> string end
  is not included in
    S
  Values do not match:
    val foo : 'a -> string
  is not included in
    val foo : int -> t
  File \"test.ml\", line 72, characters 2-20: Expected declaration
  File \"test.ml\", line 85, characters 8-11: Actual declaration"
      },
      {
        "start": {
          "line": 90,
          "col": 18
        },
        "end": {
          "line": 90,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type int but an expression was expected of type Dep.M.t"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single errors -filename test.ml -short-paths < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 21,
          "col": 36
        },
        "end": {
          "line": 21,
          "col": 49
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Some type variables are unbound in this type: class b : 'a -> a
  The method x has type 'a where 'a is unbound"
      },
      {
        "start": {
          "line": 23,
          "col": 46
        },
        "end": {
          "line": 23,
          "col": 47
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This class expression is not a class structure; it has type 'a -> a"
      },
      {
        "start": {
          "line": 27,
          "col": 0
        },
        "end": {
          "line": 30,
          "col": 3
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Some type variables are unbound in this type:
    class test : ?a:'a -> object method b : 'b end
  The method b has type 'b where 'b is unbound"
      },
      {
        "start": {
          "line": 27,
          "col": 12
        },
        "end": {
          "line": 27,
          "col": 13
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 16: this optional argument cannot be erased."
      },
      {
        "start": {
          "line": 29,
          "col": 13
        },
        "end": {
          "line": 29,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value c"
      },
      {
        "start": {
          "line": 34,
          "col": 0
        },
        "end": {
          "line": 37,
          "col": 3
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Some type variables are unbound in this type:
    class test : 'a -> object method b : 'b end
  The method b has type 'b where 'b is unbound"
      },
      {
        "start": {
          "line": 36,
          "col": 13
        },
        "end": {
          "line": 36,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value c"
      },
      {
        "start": {
          "line": 39,
          "col": 12
        },
        "end": {
          "line": 39,
          "col": 13
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 16: this optional argument cannot be erased."
      },
      {
        "start": {
          "line": 63,
          "col": 25
        },
        "end": {
          "line": 63,
          "col": 26
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type N.O.t but an expression was expected of type unit"
      },
      {
        "start": {
          "line": 82,
          "col": 13
        },
        "end": {
          "line": 86,
          "col": 6
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This functor has type functor (S : S) -> sig val bar : int -> S.t end
  The parameter cannot be eliminated in the result type.
  Please bind the argument to a module identifier."
      },
      {
        "start": {
          "line": 82,
          "col": 22
        },
        "end": {
          "line": 86,
          "col": 5
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Signature mismatch:
  Modules do not match:
    sig type t = int val foo : 'a -> string end
  is not included in
    S
  Values do not match:
    val foo : 'a -> string
  is not included in
    val foo : t -> t
  File \"test.ml\", line 72, characters 2-20: Expected declaration
  File \"test.ml\", line 85, characters 8-11: Actual declaration"
      },
      {
        "start": {
          "line": 90,
          "col": 18
        },
        "end": {
          "line": 90,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type int but an expression was expected of type Dep.t"
      }
    ],
    "notifications": []
  }

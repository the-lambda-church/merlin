  $ $MERLIN single errors -filename foo.ml < foo.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 2
        },
        "end": {
          "line": 7,
          "col": 23
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "In this `with' constraint, the new definition of t
  does not match its original definition in the constrained signature:
  Type declarations do not match:
    type 'a t = 'a t constraint 'a = int
  is not included in
    type 'a t
  Their constraints differ.
  File \"foo.ml\", line 2, characters 2-11: Expected declaration
  File \"foo.ml\", line 6, characters 9-54: Actual declaration"
      }
    ],
    "notifications": []
  }

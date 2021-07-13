###############
## SUM TYPES ##
###############

Test 1.1 :

  $ cat >c1.ml <<EOF
  > let nice_candidate = Some 3
  > let nice_candidate_with_arg x = Some x
  > let nice_candidate_with_labeled_arg ~x = Some x
  > let y = 4
  > let x : int option = _
  > EOF

  $ $MERLIN single construct -position 5:22 \
  > -filename c1.ml <c1.ml | jq ".value"
  [
    {
      "start": {
        "line": 5,
        "col": 21
      },
      "end": {
        "line": 5,
        "col": 22
      }
    },
    [
      "(Some _)",
      "None"
    ]
  ]

With depth 2:

  $ $MERLIN single construct -depth 2 -position 5:22 \
  > -filename c1.ml <c1.ml | jq ".value"
  [
    {
      "start": {
        "line": 5,
        "col": 21
      },
      "end": {
        "line": 5,
        "col": 22
      }
    },
    [
      "(Some 0)",
      "None"
    ]
  ]

With values:

  $ $MERLIN single construct -with-values local -position 5:22 \
  > -filename c1.ml <c1.ml | jq ".value"
  [
    {
      "start": {
        "line": 5,
        "col": 21
      },
      "end": {
        "line": 5,
        "col": 22
      }
    },
    [
      "(Some _)",
      "None",
      "nice_candidate",
      "(nice_candidate_with_arg _)",
      "(nice_candidate_with_labeled_arg ~x:_)"
    ]
  ]

With depth 2 and values:

  $ $MERLIN single construct -depth 2 -with-values local \
  > -position 5:22 -filename c1.ml <c1.ml | jq ".value"
  [
    {
      "start": {
        "line": 5,
        "col": 21
      },
      "end": {
        "line": 5,
        "col": 22
      }
    },
    [
      "(Some 0)",
      "None",
      "(Some y)",
      "nice_candidate",
      "(nice_candidate_with_arg _)",
      "(nice_candidate_with_labeled_arg ~x:_)"
    ]
  ]

Test 1.2

  $ cat >c2.ml <<EOF
  > let x : int list = _
  > EOF

  $ $MERLIN single construct -position 1:20 \
  > -filename c2.ml <c2.ml | jq ".value"
  [
    {
      "start": {
        "line": 1,
        "col": 19
      },
      "end": {
        "line": 1,
        "col": 20
      }
    },
    [
      "(_ :: _)",
      "[]"
    ]
  ]

Test 1.3

  $ cat >c3.ml <<EOF
  > let x : 'a list = _
  > EOF

  $ $MERLIN single construct -position 1:19 \
  > -filename c3.ml <c3.ml | jq ".value"
  [
    {
      "start": {
        "line": 1,
        "col": 18
      },
      "end": {
        "line": 1,
        "col": 19
      }
    },
    [
      "(_ :: _)",
      "[]"
    ]
  ]


Test lazy

  $ cat >lazy.ml <<EOF
  > let x : int lazy = _
  > EOF

  $ $MERLIN single construct -position 1:20 \
  > -filename lazy.ml <lazy.ml | jq ".value"
  [
    {
      "start": {
        "line": 1,
        "col": 19
      },
      "end": {
        "line": 1,
        "col": 20
      }
    },
    [
      "(lazy _)"
    ]
  ]

#############
## RECORDS ##
#############

Test 2.1

  $ cat >c2.ml <<EOF
  > type r = { a : string; b : int option }
  > let nice_candidate = {a = "a"; b = None }
  > let x : r = _
  > EOF

  $ $MERLIN single construct -position 3:13 \
  > -filename c2.ml <c2.ml | jq ".value"
  [
    {
      "start": {
        "line": 3,
        "col": 12
      },
      "end": {
        "line": 3,
        "col": 13
      }
    },
    [
      "{ a = _; b = _ }"
    ]
  ]

#################
## ARROW TYPES ##
#################

Test 3.1

  $ cat >c31.ml <<EOF
  > let nice_candidate s = int_of_string s
  > let x : string -> int = _
  > EOF

  $ $MERLIN single construct -position 2:25 \
  > -filename c31.ml <c31.ml | jq ".value"
  [
    {
      "start": {
        "line": 2,
        "col": 24
      },
      "end": {
        "line": 2,
        "col": 25
      }
    },
    [
      "(fun string -> _)"
    ]
  ]

Test 3.2

  $ cat >c32.ml <<EOF
  > let type mytype = float
  > let x : v:string -> float -> mytype -> mytype -> int = _
  > EOF

  $ $MERLIN single construct -position 2:55 \
  > -filename c32.ml <c32.ml | jq ".value"
  [
    {
      "start": {
        "line": 2,
        "col": 55
      },
      "end": {
        "line": 2,
        "col": 56
      }
    },
    [
      "(fun ~v float mytype mytype_1 -> _)"
    ]
  ]

  $ $MERLIN single construct -depth 4 -position 2:55 \
  > -filename c32.ml <c32.ml | jq ".value"
  [
    {
      "start": {
        "line": 2,
        "col": 55
      },
      "end": {
        "line": 2,
        "col": 56
      }
    },
    [
      "(fun ~v float mytype mytype_1 -> 0)"
    ]
  ]

############
## TUPLES ##
############

Test 4.1

  $ cat >c41.ml <<EOF
  > type tup = int * float * (string option)
  > let some_float = 4.2
  > let x : tup = _
  > EOF

  $ $MERLIN single construct -position 3:14 \
  >  -filename c41.ml <c41.ml | jq '.value[1]'
  [
    "(_, _, _)"
  ]

####################
## POLY. VARIANTS ##
####################

TODO we need more tests here

Test 5.1

  $ cat >c51.ml <<EOF
  > type v = [ \`A | \`B of string ]
  > let some_v = \`B "totoro"
  > let x : v = _
  > EOF

  $ $MERLIN single construct -position 3:13 \
  >  -filename c51.ml <c51.ml | jq '.value[1]'
  [
    "(`B _)",
    "`A"
  ]

  $ $MERLIN single construct -with-values local -position 3:13 \
  >  -filename c51.ml <c51.ml | jq '.value[1]'
  [
    "(`B _)",
    "`A",
    "some_v"
  ]

###########
## GADTs ##
###########

Test 6.1

  $ cat >c61.ml <<EOF
  > type _ term =
  >  | Int : int -> int term
  >  | Float : float -> float term
  >  | Eq : 'a term * 'a term -> 'a term
  > 
  > let x : 'a term =
  >   _
  > let x : int term =
  >   _
  > EOF

  $ $MERLIN single construct -position 7:3 \
  > -filename c61.ml <c61.ml | jq '.value[1]'
  [
    "(Eq (_, _))",
    "(Float _)",
    "(Int _)"
  ]

Test 6.1b
Eq (Int _, Float) is wrong and should not appear (fixed)
  $ $MERLIN single construct -depth 2 -position 7:3 \
  > -filename c61.ml <c61.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 7,
          "col": 2
        },
        "end": {
          "line": 7,
          "col": 3
        }
      },
      [
        "(Eq ((Eq (_, _)), (Float _)))",
        "(Float 0.0)",
        "(Eq ((Float _), (Float _)))",
        "(Int 0)",
        "(Eq ((Float _), (Eq (_, _))))",
        "(Eq ((Eq (_, _)), (Eq (_, _))))",
        "(Eq ((Int _), (Eq (_, _))))",
        "(Eq ((Eq (_, _)), (Int _)))",
        "(Eq ((Int _), (Int _)))"
      ]
    ],
    "notifications": []
  }

Test 6.1c
  $ $MERLIN single construct -position 9:3 \
  > -filename c61.ml <c61.ml | jq '.value[1]'
  [
    "(Eq (_, _))",
    "(Int _)"
  ]

Test 6.2

  $ cat >c62.ml <<EOF
  > type _ term =
  >  | Int : int -> int term
  >  | Float : float -> float term
  >  | Add : (int -> int -> int) term
  >  | App : ('b -> 'a) term * 'b term -> 'a term
  > let v1 = Int 42
  > let v2 = Float 3.5
  > let x : 'a term =
  >   _
  > let x : int term =
  >   _
  > EOF

Test 6.2b
Fixed: v2 should appear
  $ $MERLIN single construct -with-values local -position 9:3 \
  > -filename c62.ml <c62.ml | jq '.value[1]'
  [
    "(App (_, _))",
    "Add",
    "(Float _)",
    "(Int _)",
    "v1",
    "v2"
  ]

Test 6.2c
only v1 should appear
  $ $MERLIN single construct -with-values local -position 11:3 \
  > -filename c62.ml <c62.ml | jq '.value[1]'
  [
    "(App (_, _))",
    "(Int _)",
    "v1",
    "x"
  ]

###################
## MISCELLANEOUS ##
###################

Test M.1 : Type vars

  $ cat >cM1.ml <<EOF
  > type 'a t = A of 'a
  > let x = A _
  > EOF

  $ $MERLIN single construct -position 2:11 \
  >  -filename cM1.ml <cM1.ml | jq '.value[1]'
  []

Test M.2 : FIXME wrong position

  $ cat >M2.ml <<EOF
  > let x : type a . a list = _
  > EOF

  $ $MERLIN single construct -position 1:27 \
  > -filename M2.ml <M2.ml | jq ".value"
  [
    {
      "start": {
        "line": 1,
        "col": 4
      },
      "end": {
        "line": 1,
        "col": 27
      }
    },
    [
      "(_ :: _)",
      "[]"
    ]
  ]

Test M.3 : Predef types

  $ cat >M3.ml <<EOF
  > let x : int =         _
  > let x : nativeint =   _
  > let x : int32 =       _
  > let x : int64 =       _
  > let x : float =       _
  > let x : char =        _
  > let x : string =      _
  > let x : bool =        _
  > let x : unit =        _
  > let x : exn =         _
  > let x : 'a array =    _
  > let x : 'a lazy_t =   _
  > EOF

  $ $MERLIN single construct -position 1:22 \
  > -filename M3.ml <M3.ml | jq ".value[1]"
  [
    "0"
  ]

  $ $MERLIN single construct -position 2:22 \
  > -filename M3.ml <M3.ml | jq ".value[1]"
  [
    "0n"
  ]

  $ $MERLIN single construct -position 3:22 \
  > -filename M3.ml <M3.ml | jq ".value[1]"
  [
    "0l"
  ]

  $ $MERLIN single construct -position 4:22 \
  > -filename M3.ml <M3.ml | jq ".value[1]"
  [
    "0L"
  ]

  $ $MERLIN single construct -position 5:22 \
  > -filename M3.ml <M3.ml | jq ".value[1]"
  [
    "0.0"
  ]

  $ $MERLIN single construct -position 6:22 \
  > -filename M3.ml <M3.ml | jq ".value[1]"
  [
    "'c'"
  ]

  $ $MERLIN single construct -position 7:22 \
  > -filename M3.ml <M3.ml | jq ".value[1]"
  [
    "\"\""
  ]

  $ $MERLIN single construct -position 8:22 \
  > -filename M3.ml <M3.ml | jq ".value[1]"
  [
    "true"
  ]

  $ $MERLIN single construct -position 9:22 \
  > -filename M3.ml <M3.ml | jq ".value[1]"
  [
    "()"
  ]

  $ $MERLIN single construct -position 10:22 \
  > -filename M3.ml <M3.ml | jq ".value[1]"
  [
    "exn"
  ]

  $ $MERLIN single construct -position 11:22 \
  > -filename M3.ml <M3.ml | jq ".value[1]"
  [
    "[||]"
  ]

  $ $MERLIN single construct -position 12:22 \
  > -filename M3.ml <M3.ml | jq ".value[1]"
  [
    "(lazy _)"
  ]

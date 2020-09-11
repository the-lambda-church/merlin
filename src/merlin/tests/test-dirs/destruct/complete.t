
  $ cat >typ.ml <<EOF \
  > type a = A | B of string \
  > type recd = { a : a } \
  > let f (x : recd) =    \
  >   match x with        \
  >   | { a = A } -> ()   \
  > EOF

  $ $MERLIN single case-analysis -start 5:4 -end 5:4 -filename typ.ml < typ.ml | \
  > sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 19
        },
        "end": {
          "line": 5,
          "col": 19
        }
      },
      "|{ a = B _ } -> (??)"
    ],
    "notifications": []
  }

  $ cat >typ2.ml <<EOF \
  > type basic_color = [ \`Blue | \`Red | \`Yellow ] \
  > let f (x : basic_color) = \
  >   match x with          \
  >   | \`Blue -> ()        \
  > EOF

  $ $MERLIN single case-analysis -start 4:5 -end 4:5 -filename typ2.ml <typ2.ml | \
  > sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 15
        },
        "end": {
          "line": 4,
          "col": 15
        }
      },
      "|`Yellow|`Red -> (??)"
    ],
    "notifications": []
  }

FIXME ?


  $ cat >typ3.ml <<EOF \
  > type a = A | B of string \
  > type recd = { a : a } \
  > let f (x : recd) =    \
  >   match x with        \
  >   | { a = A } -> ()   \
  >   | { a = B _ } -> () \
  > EOF

  $ $MERLIN single case-analysis -start 6:12 -end 6:12 -filename typ3.ml <typ3.ml | \
  > sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 6,
          "col": 4
        },
        "end": {
          "line": 6,
          "col": 15
        }
      },
      "{ a = B \"\" }|{ a = B _ }"
    ],
    "notifications": []
  }

  $ cat >typ4.ml <<EOF \
  > type b = C | D of string \
  > type a = A | B of b   \
  > type recd = { a : a } \
  > let f (x : recd) =    \
  >   match x with        \
  >   | { a = A } -> ()   \
  >   | { a = B _ } -> () \
  > EOF

  $ $MERLIN single case-analysis -start 7:12 -end 7:12 -filename typ4.ml <typ4.ml | \
  > sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 7,
          "col": 4
        },
        "end": {
          "line": 7,
          "col": 15
        }
      },
      "{ a = B (C) }|{ a = B (D _) }"
    ],
    "notifications": []
  }

  $ cat >typ3.ml <<EOF                              \
  > type _ term =                                   \
  >  | Int : int -> int term                        \
  >  | Add : (int -> int -> int) term               \
  >  | App : ('b -> 'a) term * 'b term -> 'a term   \
  > let eval : type a. a term -> a term =           \
  >   fun x : a term -> match x with                \
  >   | Int _ -> ()                                 \
  >   | Add -> ()                                   \
  > EOF

  $ $MERLIN single case-analysis -start 8:4 -end 8:4 -filename typ3.ml <typ3.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 8,
          "col": 13
        },
        "end": {
          "line": 8,
          "col": 13
        }
      },
      "|App (_,_) -> (??)"
    ],
    "notifications": []
  }

  $ cat >typ4.ml <<EOF                              \
  > type _ term =                                   \
  >  | Int : int -> int term                        \
  >  | Add : (int -> int -> int) term               \
  >  | App : ('b -> 'a) term * 'b term -> 'a term   \
  > let eval : type a. a term -> a term =           \
  >   function                                      \
  >   | Int _ -> ()                                 \
  >   | Add -> ()                                   \
  > EOF

  $ $MERLIN single case-analysis -start 8:4 -end 8:4 -filename typ4.ml <typ4.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 8,
          "col": 13
        },
        "end": {
          "line": 8,
          "col": 13
        }
      },
      "|App (_,_) -> (??)"
    ],
    "notifications": []
  }

FIXME

  $ cat >typ4b.ml <<EOF         \
  > type _ t =                  \
  >   | I : int t               \
  >   | B : bool t              \
  > let f : int t -> unit =     \
  >   function                  \
  >   | I -> ()                 \
  > EOF

  $ $MERLIN single case-analysis -start 6:4 -end 6:4 -filename typ4b.ml <typ4b.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 6,
          "col": 11
        },
        "end": {
          "line": 6,
          "col": 11
        }
      },
      "|B -> (??)"
    ],
    "notifications": []
  }

FIXME

  $ cat >typ5.ml <<EOF          \
  > type void = |               \
  > let f (x : void option) =   \
  >   match x with              \
  >   | None -> ()              \
  > EOF

  $ $MERLIN single case-analysis -start 4:4 -end 4:4 -filename typ5.ml <typ5.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 14
        },
        "end": {
          "line": 4,
          "col": 14
        }
      },
      "|Some _ -> (??)"
    ],
    "notifications": []
  }

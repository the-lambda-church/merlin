
  $ $MERLIN single case-analysis -start 3:2 -end 3:3 -filename typ.ml <<EOF | sed -e 's/,_)/, _)/g' \
  > type my_list = Atom | Elt of string * my_list \
  > let f x : my_list = \
  >   x \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 3
        }
      },
      "match (x : my_list) with | Atom -> (??) | Elt (_, _) -> (??)"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 3:2 -end 3:3 -filename typ.ml <<EOF \
  > type my_rec = { atom: string; elt: string * my_rec } \
  > let f x : my_rec = \
  >   x \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 3
        }
      },
      "match (x : my_rec) with | { atom; elt } -> (??)"
    ],
    "notifications": []
  }

FIXME
  $ $MERLIN single case-analysis -start 4:18 -end 4:21 -filename typ.ml <<EOF \
  > type my_rec = { atom: string; elt: string * my_rec } \
  > let f x : my_rec = \
  >   match (x : my_rec) with \
  >   | { atom; elt = _ } -> () \
  > EOF
  {
    "class": "error",
    "value": "Nothing to do",
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 3:2 -end 3:3 -filename typ.ml <<EOF \
  > type basic_color = [ \`Blue | \`Red | \`Yellow ] \
  > let f x : basic_color = \
  >   x \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 3
        }
      },
      "match (x : basic_color) with | `Blue -> (??) | `Yellow -> (??) | `Red -> (??)"
    ],
    "notifications": []
  }

  $ cat >typ.ml <<EOF             \
  > type my_list = Atom | Elt of string * my_list \
  > let f x : my_list =           \
  >   match (x : my_list) with    \
  >   | Atom -> ()                \
  >   | Elt (_, _) -> ()          \
  > EOF

  $ $MERLIN single case-analysis -start 5:12 -end 5:13 -filename typ.ml <typ.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 4
        },
        "end": {
          "line": 5,
          "col": 14
        }
      },
      "Elt (_,Atom)|Elt (_,Elt (_,_))"
    ],
    "notifications": []
  }

  $ cat >typ2.ml <<EOF                              \
  > type _ term =                                   \
  >  | Int : int -> int term                        \
  >  | Add : (int -> int -> int) term               \
  >  | App : ('b -> 'a) term * 'b term -> 'a term   \
  > let eval : type a. a term -> a term =           \
  >   fun x -> x                           \
  > EOF

  $ $MERLIN single case-analysis -start 6:10 -end 6:10 -filename typ2.ml <typ2.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "error",
    "value": "Destruct not allowed on core_type",
    "notifications": []
  }

  $ cat >typ3.ml <<EOF                              \
  > type _ term =                                   \
  >  | Int : int -> int term                        \
  >  | Add : (int -> int -> int) term               \
  >  | App : ('b -> 'a) term * 'b term -> 'a term   \
  > let eval : type a. a term -> a term =           \
  >   fun x : a term -> x                           \
  > EOF

  $ $MERLIN single case-analysis -start 6:20 -end 6:20 -filename typ3.ml <typ3.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 6,
          "col": 20
        },
        "end": {
          "line": 6,
          "col": 21
        }
      },
      "match (x : a term) with|Int _ -> (??)|Add -> (??)|App (_,_) -> (??)"
    ],
    "notifications": []
  }

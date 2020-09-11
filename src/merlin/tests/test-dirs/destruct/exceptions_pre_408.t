(enabled_if (< %{ocaml_version} 4.08.0))
FIXME
  $ $MERLIN single case-analysis -start 3:4 -end 3:8 -filename complete.ml <<EOF \
  > let _ = \
  >   match (None : int option) with \
  >   | exception _ -> () \
  >   | Some 3 -> () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 4,
          "col": 16
        }
      },
      "match match (None : int option) with | Some 3 -> () | exception _ -> () with
  | () -> (??)"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 4:4 -end 4:8 -filename complete.ml <<EOF \
  > let _ = \
  >   match (None : int option) with \
  >   | exception _ -> () \
  >   | Some _ -> () \
  > EOF
  {
    "class": "error",
    "value": "Nothing to do",
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 4:5 -end 4:5 -filename no_comp_pat.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | exception _ -> () \
  >   | None -> () \
  > EOF
  {
    "class": "error",
    "value": "Nothing to do",
    "notifications": []
  }

FIXME: `Some 0` certainly is a missing case but we can do better:

  $ $MERLIN single case-analysis -start 4:4 -end 4:8 -filename complete.ml <<EOF \
  > let _ = \
  >   match (None : int option) with \
  >   | exception _ -> () \
  >   | Some 3 -> () \
  > EOF
  {
    "class": "error",
    "value": "Nothing to do",
    "notifications": []
  }

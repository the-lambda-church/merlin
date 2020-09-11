(enabled_if (>= %{ocaml_version} 4.08.0))

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
          "line": 4,
          "col": 16
        },
        "end": {
          "line": 4,
          "col": 16
        }
      },
      "
  | Some 0|None -> (??)"
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
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 16
        },
        "end": {
          "line": 4,
          "col": 16
        }
      },
      "
  | None -> (??)"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 4:5 -end 4:5 -filename no_comp_pat.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | exception _ -> () \
  >   | None -> () \
  > EOF
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
      "
  | Some _ -> (??)"
    ],
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
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 16
        },
        "end": {
          "line": 4,
          "col": 16
        }
      },
      "
  | Some 0|None -> (??)"
    ],
    "notifications": []
  }

Same two tests but with the exception pattern at the end

  $ $MERLIN single case-analysis -start 4:9 -end 4:11 -filename no_comp_pat.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | None -> () \
  >   | exception _ -> () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 21
        },
        "end": {
          "line": 4,
          "col": 21
        }
      },
      "
  | Some _ -> (??)"
    ],
    "notifications": []
  }

FIXME: `Some 0` certainly is a missing case but we can do better

  $ $MERLIN single case-analysis -start 3:4 -end 3:8 -filename complete.ml <<EOF \
  > let _ = \
  >   match (None : int option) with \
  >   | Some 3 -> () \
  >   | exception _ -> () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 21
        },
        "end": {
          "line": 4,
          "col": 21
        }
      },
      "
  | Some 0|None -> (??)"
    ],
    "notifications": []
  }

Tests with exception in or-pattern

  $ $MERLIN single case-analysis -start 3:4 -end 3:4 -filename exp_or.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | None | exception _ -> () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 28
        },
        "end": {
          "line": 3,
          "col": 28
        }
      },
      "
  | Some _ -> (??)"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 3:11 -end 3:11 -filename exp_or.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | None | exception _ -> () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 28
        },
        "end": {
          "line": 3,
          "col": 28
        }
      },
      "
  | Some _ -> (??)"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 3:4 -end 3:4 -filename exp_or.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | exception _ | None -> () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 28
        },
        "end": {
          "line": 3,
          "col": 28
        }
      },
      "
  | Some _ -> (??)"
    ],
    "notifications": []
  }


  $ $MERLIN single case-analysis -start 3:4 -end 3:4 -filename exp_or.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | exception Not_found | None | exception _ -> () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 50
        },
        "end": {
          "line": 3,
          "col": 50
        }
      },
      "
  | Some _ -> (??)"
    ],
    "notifications": []
  }

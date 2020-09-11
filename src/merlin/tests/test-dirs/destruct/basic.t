TODO: test all error cases

  $ $MERLIN single case-analysis -start 4:9 -end 4:11 -filename nothing_to_do.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | None -> () \
  >   | Some () -> () \
  > EOF
  {
    "class": "error",
    "value": "Nothing to do",
    "notifications": []
  }


  $ $MERLIN single case-analysis -start 3:4 -end 3:8 -filename make_exhaustive.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | None -> () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 14
        },
        "end": {
          "line": 3,
          "col": 14
        }
      },
      "
  | Some _ -> (??)"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 4:9 -end 4:10 -filename refine_pattern.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | None -> () \
  >   | Some _ -> () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 9
        },
        "end": {
          "line": 4,
          "col": 10
        }
      },
      "()"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 4:2 -end 4:3 -filename unpack_module.ml <<EOF \
  > module type S = sig end \
  >  \
  > let g (x : (module S)) = \
  >   x \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 2
        },
        "end": {
          "line": 4,
          "col": 3
        }
      },
      "let module M = (val x) in (??)"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 2:2 -end 2:3 -filename record_exp.ml <<EOF \
  > let f (x : int ref) = \
  >   x \
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
          "line": 2,
          "col": 3
        }
      },
      "match x with | { contents } -> (??)"
    ],
    "notifications": []
  }

FIXME: put each case on a different line (if it doesn't require updating
pprintast).

  $ $MERLIN single case-analysis -start 2:2 -end 2:3 -filename variant_exp.ml <<EOF \
  > let f (x : int option) = \
  >   x \
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
          "line": 2,
          "col": 3
        }
      },
      "match x with | None -> (??) | Some _ -> (??)"
    ],
    "notifications": []
  }

  $ echo "let () = ()" | $MERLIN single case-analysis -start 1:4 -end 1:4 -filename stacktrace.ml | grep -E -v "Raised|Called|Re-raised"
  {
    "class": "exception",
    "value": "Destruct.Wrong_parent(\"get_every_pattern: [{\\\"filename\\\":\\\"stacktrace.ml\\\",\\\"start\\\":{\\\"line\\\":1,\\\"col\\\":0},\\\"end\\\":{\\\"line\\\":1,\\\"col\\\":11},\\\"ghost\\\":false,\\\"attrs\\\":[],\\\"kind\\\":\\\"value_binding\\\",\\\"children\\\":[{\\\"filename\\\":\\\"stacktrace.ml\\\",\\\"start\\\":{\\\"line\\\":1,\\\"col\\\":4},\\\"end\\\":{\\\"line\\\":1,\\\"col\\\":6},\\\"ghost\\\":false,\\\"attrs\\\":[],\\\"kind\\\":\\\"pattern (stacktrace.ml[1,0+4]..stacktrace.ml[1,0+6])\\\
    Tpat_construct \\\\\\\"()\\\\\\\"\\\
    []\\\
  \\\",\\\"children\\\":[]},{\\\"filename\\\":\\\"stacktrace.ml\\\",\\\"start\\\":{\\\"line\\\":1,\\\"col\\\":9},\\\"end\\\":{\\\"line\\\":1,\\\"col\\\":11},\\\"ghost\\\":false,\\\"attrs\\\":[],\\\"kind\\\":\\\"expression\\\",\\\"children\\\":[]}]}]\")
  ",
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 4:2 -end 4:1 -filename nonode.ml <<EOF | grep -B 1 Query_commands.No_nodes \
  > let f (x : int option) = \
  >   match w with    \
  >  | _ -> ()        \
  > EOF
    "class": "exception",
    "value": "Query_commands.No_nodes

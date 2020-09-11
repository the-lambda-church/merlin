  $ $MERLIN single type-enclosing -position 5:24 -verbosity 0 \
  > -filename ./issue864.ml < ./issue864.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 23
        },
        "end": {
          "line": 5,
          "col": 24
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 5,
          "col": 21
        },
        "end": {
          "line": 5,
          "col": 24
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 5,
          "col": 9
        },
        "end": {
          "line": 5,
          "col": 24
        },
        "type": "X.t -> int",
        "tail": "no"
      },
      {
        "start": {
          "line": 5,
          "col": 7
        },
        "end": {
          "line": 5,
          "col": 24
        },
        "type": "'a -> X.t -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 7:37 -verbosity 0 \
  > -filename ./issue864.ml < ./issue864.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 7,
          "col": 34
        },
        "end": {
          "line": 7,
          "col": 37
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 7,
          "col": 32
        },
        "end": {
          "line": 7,
          "col": 37
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 7,
          "col": 20
        },
        "end": {
          "line": 7,
          "col": 37
        },
        "type": "X.t -> int",
        "tail": "no"
      },
      {
        "start": {
          "line": 7,
          "col": 7
        },
        "end": {
          "line": 7,
          "col": 37
        },
        "type": "string -> X.t -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 9:35 -verbosity 0 \
  > -filename ./issue864.ml < ./issue864.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 9,
          "col": 34
        },
        "end": {
          "line": 9,
          "col": 35
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 9,
          "col": 32
        },
        "end": {
          "line": 9,
          "col": 35
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 9,
          "col": 20
        },
        "end": {
          "line": 9,
          "col": 35
        },
        "type": "X.t -> int",
        "tail": "no"
      },
      {
        "start": {
          "line": 9,
          "col": 7
        },
        "end": {
          "line": 9,
          "col": 35
        },
        "type": "string -> X.t -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

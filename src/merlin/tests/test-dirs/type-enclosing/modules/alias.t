  $ $MERLIN single type-enclosing -position 3:9 -verbosity 0 \
  > -filename ./alias.ml < ./alias.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 3,
        "col": 8
      },
      "end": {
        "line": 3,
        "col": 9
      },
      "type": "(module List)",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 8
      },
      "end": {
        "line": 3,
        "col": 12
      },
      "type": "int list -> int",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 3:11 -verbosity 0 \
  > -filename ./alias.ml < ./alias.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 12
        },
        "type": "'a list -> 'a",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 12
        },
        "type": "int list -> int",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 16
        },
        "type": "int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 3:14 -verbosity 0 \
  > -filename ./alias.ml < ./alias.ml
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
          "col": 15
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 13
        },
        "end": {
          "line": 3,
          "col": 16
        },
        "type": "int list",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 16
        },
        "type": "int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 3:15 -verbosity 0 \
  > -filename ./alias.ml < ./alias.ml
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
          "col": 15
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 13
        },
        "end": {
          "line": 3,
          "col": 16
        },
        "type": "int list",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 16
        },
        "type": "int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

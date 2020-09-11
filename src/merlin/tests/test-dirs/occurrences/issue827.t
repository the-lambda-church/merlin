Reproduction case:

  $ $MERLIN single occurrences -identifier-at 2:14 -filename ./issue827.ml < ./issue827.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 13
        },
        "end": {
          "line": 2,
          "col": 15
        }
      },
      {
        "start": {
          "line": 4,
          "col": 8
        },
        "end": {
          "line": 4,
          "col": 12
        }
      }
    ],
    "notifications": []
  }

  $ $MERLIN single occurrences -identifier-at 2:19 -filename ./issue827.ml < ./issue827.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 18
        },
        "end": {
          "line": 2,
          "col": 20
        }
      },
      {
        "start": {
          "line": 5,
          "col": 22
        },
        "end": {
          "line": 5,
          "col": 24
        }
      }
    ],
    "notifications": []
  }

Interestingly if you start from a use instead of the definition, it seems to
work:

  $ $MERLIN single occurrences -identifier-at 4:12 -filename ./issue827.ml < ./issue827.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 13
        },
        "end": {
          "line": 2,
          "col": 15
        }
      },
      {
        "start": {
          "line": 4,
          "col": 8
        },
        "end": {
          "line": 4,
          "col": 12
        }
      }
    ],
    "notifications": []
  }

  $ $MERLIN single occurrences -identifier-at 5:23 -filename ./issue827.ml < ./issue827.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 18
        },
        "end": {
          "line": 2,
          "col": 20
        }
      },
      {
        "start": {
          "line": 5,
          "col": 22
        },
        "end": {
          "line": 5,
          "col": 24
        }
      }
    ],
    "notifications": []
  }

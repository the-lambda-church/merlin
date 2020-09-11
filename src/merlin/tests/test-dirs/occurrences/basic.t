Test getting occurrences of a function arg:

  $ $MERLIN single occurrences -identifier-at 1:11 -filename ./basic.ml < ./basic.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 11
        },
        "end": {
          "line": 1,
          "col": 15
        }
      },
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 2,
          "col": 6
        }
      },
      {
        "start": {
          "line": 2,
          "col": 9
        },
        "end": {
          "line": 2,
          "col": 13
        }
      }
    ],
    "notifications": []
  }

Test getting occurrences of a function arg annotated with a type:

  $ $MERLIN single occurrences -identifier-at 4:19 -filename ./basic.ml < ./basic.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 19
        },
        "end": {
          "line": 4,
          "col": 23
        }
      },
      {
        "start": {
          "line": 5,
          "col": 2
        },
        "end": {
          "line": 5,
          "col": 6
        }
      },
      {
        "start": {
          "line": 5,
          "col": 9
        },
        "end": {
          "line": 5,
          "col": 13
        }
      }
    ],
    "notifications": []
  }

Test getting occurrences of a record pattern in a function arg:

  $ $MERLIN single occurrences -identifier-at 9:24 -filename ./basic.ml < ./basic.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 9,
          "col": 23
        },
        "end": {
          "line": 9,
          "col": 28
        }
      },
      {
        "start": {
          "line": 10,
          "col": 2
        },
        "end": {
          "line": 10,
          "col": 7
        }
      },
      {
        "start": {
          "line": 10,
          "col": 10
        },
        "end": {
          "line": 10,
          "col": 15
        }
      }
    ],
    "notifications": []
  }

Test getting occurrences of a function arg then used in record literal:

  $ $MERLIN single occurrences -identifier-at 12:23 -filename ./basic.ml < ./basic.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 12,
          "col": 22
        },
        "end": {
          "line": 12,
          "col": 25
        }
      },
      {
        "start": {
          "line": 13,
          "col": 11
        },
        "end": {
          "line": 13,
          "col": 14
        }
      }
    ],
    "notifications": []
  }

Test getting occurrences of a function arg then used in record literal punned:

  $ $MERLIN single occurrences -identifier-at 15:29 -filename ./basic.ml < ./basic.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 15,
          "col": 28
        },
        "end": {
          "line": 15,
          "col": 33
        }
      },
      {
        "start": {
          "line": 16,
          "col": 3
        },
        "end": {
          "line": 16,
          "col": 8
        }
      }
    ],
    "notifications": []
  }

Test getting occurrences of a function arg alias then used in record literal:

  $ $MERLIN single occurrences -identifier-at 18:25 -filename ./basic.ml < ./basic.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 18,
          "col": 24
        },
        "end": {
          "line": 18,
          "col": 27
        }
      },
      {
        "start": {
          "line": 19,
          "col": 2
        },
        "end": {
          "line": 19,
          "col": 5
        }
      },
      {
        "start": {
          "line": 19,
          "col": 8
        },
        "end": {
          "line": 19,
          "col": 11
        }
      }
    ],
    "notifications": []
  }

  $ echo "S .\nB .\nFLG -nopervasives" > .merlin
  $ $MERLIN single outline < foo.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 24,
          "col": 0
        },
        "end": {
          "line": 28,
          "col": 3
        },
        "name": "point",
        "kind": "Type",
        "type": null,
        "children": [
          {
            "start": {
              "line": 27,
              "col": 4
            },
            "end": {
              "line": 27,
              "col": 10
            },
            "name": "z",
            "kind": "Label",
            "type": null,
            "children": [],
            "deprecated": false
          },
          {
            "start": {
              "line": 26,
              "col": 4
            },
            "end": {
              "line": 27,
              "col": 3
            },
            "name": "y",
            "kind": "Label",
            "type": null,
            "children": [],
            "deprecated": false
          },
          {
            "start": {
              "line": 25,
              "col": 4
            },
            "end": {
              "line": 26,
              "col": 3
            },
            "name": "x",
            "kind": "Label",
            "type": null,
            "children": [],
            "deprecated": false
          }
        ],
        "deprecated": false
      },
      {
        "start": {
          "line": 20,
          "col": 0
        },
        "end": {
          "line": 22,
          "col": 16
        },
        "name": "eithery",
        "kind": "Type",
        "type": null,
        "children": [
          {
            "start": {
              "line": 22,
              "col": 2
            },
            "end": {
              "line": 22,
              "col": 16
            },
            "name": "Righty",
            "kind": "Constructor",
            "type": null,
            "children": [],
            "deprecated": false
          },
          {
            "start": {
              "line": 21,
              "col": 2
            },
            "end": {
              "line": 21,
              "col": 15
            },
            "name": "Lefty",
            "kind": "Constructor",
            "type": null,
            "children": [],
            "deprecated": false
          }
        ],
        "deprecated": false
      },
      {
        "start": {
          "line": 18,
          "col": 0
        },
        "end": {
          "line": 18,
          "col": 20
        },
        "name": "Ex",
        "kind": "Exn",
        "type": null,
        "children": [],
        "deprecated": false
      },
      {
        "start": {
          "line": 14,
          "col": 0
        },
        "end": {
          "line": 16,
          "col": 3
        },
        "name": "class_b",
        "kind": "Class",
        "type": null,
        "children": [
          {
            "start": {
              "line": 15,
              "col": 9
            },
            "end": {
              "line": 15,
              "col": 10
            },
            "name": "b",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false
          }
        ],
        "deprecated": false
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 8,
          "col": 3
        },
        "name": "Bar",
        "kind": "Module",
        "type": null,
        "children": [
          {
            "start": {
              "line": 3,
              "col": 1
            },
            "end": {
              "line": 7,
              "col": 4
            },
            "name": "S1",
            "kind": "Signature",
            "type": null,
            "children": [
              {
                "start": {
                  "line": 6,
                  "col": 3
                },
                "end": {
                  "line": 6,
                  "col": 21
                },
                "name": "foo",
                "kind": "Value",
                "type": "t -> int",
                "children": [],
                "deprecated": false
              },
              {
                "start": {
                  "line": 4,
                  "col": 3
                },
                "end": {
                  "line": 4,
                  "col": 9
                },
                "name": "t",
                "kind": "Type",
                "type": null,
                "children": [],
                "deprecated": false
              }
            ],
            "deprecated": false
          },
          {
            "start": {
              "line": 2,
              "col": 1
            },
            "end": {
              "line": 2,
              "col": 13
            },
            "name": "t",
            "kind": "Type",
            "type": null,
            "children": [],
            "deprecated": false
          }
        ],
        "deprecated": false
      }
    ],
    "notifications": []
  }
  $ $MERLIN single outline < path.ml | jq '.value[].type'
  "A.a"
  null
  $ $MERLIN single outline -short-paths < path.ml | jq '.value[].type'
  "a"
  null

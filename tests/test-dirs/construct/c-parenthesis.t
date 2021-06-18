###############
## SUM TYPES ##
###############

Test 1.1 :

  $ cat >c1.ml <<EOF
  > let x : int option option = Some (_)
  > EOF

  $ $MERLIN single construct -position 1:34 \
  > -filename c1.ml <c1.ml | jq ".value"
  [
    {
      "start": {
        "line": 1,
        "col": 33
      },
      "end": {
        "line": 1,
        "col": 36
      }
    },
    [
      "(Some _)",
      "None"
    ]
  ]

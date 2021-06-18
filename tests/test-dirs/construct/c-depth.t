  $ cat >d1.ml <<EOF
  > let x : int option option = _
  > EOF

Test 1.1
  $ $MERLIN single construct -depth 1 -position 1:28 -filename d1.ml <d1.ml |
  >  jq ".value[1]"
  [
    "(Some _)",
    "None"
  ]

Test 1.2
  $ $MERLIN single construct -depth 2 -position 1:28 -filename d1.ml <d1.ml |
  >  jq ".value[1]"
  [
    "(Some (Some _))",
    "None",
    "(Some None)"
  ]

Test 1.3
  $ $MERLIN single construct -depth 3 -position 1:28 -filename d1.ml <d1.ml |
  >  jq ".value[1]"
  [
    "(Some (Some 0))",
    "None",
    "(Some None)"
  ]

Test 1.4
  $ $MERLIN single construct -depth 4 -position 1:28 -filename d1.ml <d1.ml |
  >  jq ".value[1]"
  [
    "(Some (Some 0))",
    "None",
    "(Some None)"
  ]

  $ cat >d2.ml <<EOF
  > type t = { a : int option option; b : float option }
  > let x : t = _
  > EOF

Test 2.1
  $ $MERLIN single construct -depth 1 -position 2:12 -filename d2.ml <d2.ml |
  >  jq ".value[1]"
  [
    "{ a = _; b = _ }"
  ]

Test 2.2
  $ $MERLIN single construct -depth 2 -position 2:12 -filename d2.ml <d2.ml |
  >  jq ".value[1]"
  [
    "{ a = None; b = (Some _) }",
    "{ a = (Some _); b = (Some _) }",
    "{ a = (Some _); b = None }",
    "{ a = None; b = None }"
  ]

Test 2.3
  $ $MERLIN single construct -depth 3 -position 2:12 -filename d2.ml <d2.ml |
  >  jq ".value[1]"
  [
    "{ a = (Some None); b = (Some 0.0) }",
    "{ a = (Some (Some _)); b = (Some 0.0) }",
    "{ a = None; b = (Some 0.0) }",
    "{ a = None; b = None }",
    "{ a = (Some (Some _)); b = None }",
    "{ a = (Some None); b = None }"
  ]

Test 2.4
  $ $MERLIN single construct -depth 4 -position 2:12 -filename d2.ml <d2.ml |
  >  jq ".value[1]"
  [
    "{ a = (Some None); b = (Some 0.0) }",
    "{ a = (Some (Some 0)); b = (Some 0.0) }",
    "{ a = None; b = (Some 0.0) }",
    "{ a = None; b = None }",
    "{ a = (Some (Some 0)); b = None }",
    "{ a = (Some None); b = None }"
  ]

  $ cat >d3.ml <<EOF
  > type t = int option option * float option
  > let x : t = _
  > EOF

Test 3.1
  $ $MERLIN single construct -depth 2 -position 2:12 -filename d3.ml <d3.ml |
  >  jq ".value[1]"
  [
    "(None, (Some _))",
    "((Some _), (Some _))",
    "((Some _), None)",
    "(None, None)"
  ]

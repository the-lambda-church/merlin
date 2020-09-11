A simple name is not expanded

  $ echo | $MERLIN single dump-configuration -filename relative_path.ml -ppx test1 | \
  > jq '.value.ocaml.ppx'
  sh: ...
  [
    {
      "workdir": "tests/test-dirs/config",
      "workval": "test1"
    }
  ]

Neither is an absolute path

  $ echo | $MERLIN single dump-configuration -filename relative_path.ml -ppx /test2 | \
  > jq '.value.ocaml.ppx'
  sh: ...
  [
    {
      "workdir": "tests/test-dirs/config",
      "workval": "/test2"
    }
  ]

But relative names are

  $ echo | $MERLIN single dump-configuration -filename relative_path.ml -ppx ./test3 | \
  > jq '.value.ocaml.ppx'
  sh: ...
  [
    {
      "workdir": "tests/test-dirs/config",
      "workval": "./test3"
    }
  ]

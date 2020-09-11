Create a broke .merlin:

  $ cat > .merlin <<EOF \
  > FLG -principal \
  > PKG does-not-exist \
  > # some comment \
  > EOF

And look at merlin's config:

  $ echo | $MERLIN single dump-configuration -filename test.ml | \
  > jq ".value.merlin | {flags_applied: .flags_applied, failures: .failures}"
  {
    "flags_applied": [
      {
        "workdir": "tests/test-dirs/config/dot-merlin-reader",
        "workval": [
          "-principal"
        ]
      }
    ],
    "failures": [
      "Failed to load packages: does-not-exist"
    ]
  }

Also, see that the failure is reported to the user:

  $ echo | $MERLIN single errors -filename test.ml
  {
    "class": "return",
    "value": [
      {
        "type": "config",
        "sub": [],
        "valid": true,
        "message": "Failed to load packages: does-not-exist"
      }
    ],
    "notifications": []
  }

  $ rm .merlin

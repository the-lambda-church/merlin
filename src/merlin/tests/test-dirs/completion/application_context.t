(enabled_if (>= %{ocaml_version} 4.09.0))

  $ $MERLIN single complete-prefix -position 3:17 \
  > -filename application_context < application_context.ml \
  > | tr '\n' ' ' | jq ".value.context"
  [
    "application",
    {
      "argument_type": "'_weak1",
      "labels": [
        {
          "name": "~j",
          "type": "int"
        }
      ]
    }
  ]

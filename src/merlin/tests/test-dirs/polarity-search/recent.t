(enabled_if (>= %{ocaml_version} 4.07.0))

A few simple tests that show all the things we want to preserve or improve:

# To improve

- Presence of double underscores.

  $ echo "" | $MERLIN single search-by-polarity -query "-float +int64" \
  > -position 1:0 -filename test.ml | \
  > jq '.value.entries[] | del(.info) | del(.kind) | del(.deprecated)'
  {
    "name": "Stdlib__int64.bits_of_float",
    "desc": "float -> int64"
  }
  {
    "name": "Stdlib__int64.of_float",
    "desc": "float -> int64"
  }

- Duplicated elements

  $ echo "" | $MERLIN single search-by-polarity -safe-string \
  > -query "-int +string" -position 1:0 -filename test.ml | \
  > tr '\n' ' ' | jq '.value.entries |= (map(del(.deprecated)) | .[:2])'
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "string_of_int",
          "kind": "Value",
          "desc": "int -> string",
          "info": ""
        },
        {
          "name": "string_of_int",
          "kind": "Value",
          "desc": "int -> string",
          "info": ""
        }
      ],
      "context": null
    },
    "notifications": []
  }

# To keep

- Lower bound on function arity

  $ echo "" | $MERLIN single search-by-polarity \
  > -query "-float +fun +fun +float" -position 1:0 -filename test.ml | \
  > tr '\n' ' ' | jq '.value.entries |= (map(del(.info) | del(.kind) | del (.deprecated)) | .[0:11])'
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "**",
          "desc": "float -> float -> float"
        },
        {
          "name": "**",
          "desc": "float -> float -> float"
        },
        {
          "name": "*.",
          "desc": "float -> float -> float"
        },
        {
          "name": "*.",
          "desc": "float -> float -> float"
        },
        {
          "name": "+.",
          "desc": "float -> float -> float"
        },
        {
          "name": "+.",
          "desc": "float -> float -> float"
        },
        {
          "name": "-.",
          "desc": "float -> float -> float"
        },
        {
          "name": "-.",
          "desc": "float -> float -> float"
        },
        {
          "name": "/.",
          "desc": "float -> float -> float"
        },
        {
          "name": "/.",
          "desc": "float -> float -> float"
        },
        {
          "name": "atan2",
          "desc": "float -> float -> float"
        }
      ],
      "context": null
    },
    "notifications": []
  }

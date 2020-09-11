  $ $MERLIN single complete-prefix -position 11:10 -prefix "Z." \
  > -filename infix.ml < infix.ml | jq ".value.entries | sort_by(.name)"
  [
    {
      "name": "(>>)",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(>>=)",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(>>|)",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(|+)",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(|-)",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    }
  ]

(enabled_if (< %{ocaml_version} 4.11.0))

  $ $MERLIN single complete-prefix -position 11:15 -prefix MyList. \
  > -filename parenthesize.ml < parenthesize.ml | jq ".value.entries | sort_by(.name)"
  [
    {
      "name": "()",
      "kind": "Constructor",
      "desc": "MyList.u",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(::)",
      "kind": "Constructor",
      "desc": "'a * 'a MyList.t -> 'a MyList.t",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(mod)",
      "kind": "Value",
      "desc": "MyList.u",
      "info": "",
      "deprecated": false
    },
    {
      "name": "[]",
      "kind": "Constructor",
      "desc": "'a MyList.t",
      "info": "",
      "deprecated": false
    },
    {
      "name": "random",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "t",
      "kind": "Type",
      "desc": "type 'a t = (::) of 'a * 'a MyList.t | []",
      "info": "",
      "deprecated": false
    },
    {
      "name": "u",
      "kind": "Type",
      "desc": "type u = ()",
      "info": "",
      "deprecated": false
    }
  ]

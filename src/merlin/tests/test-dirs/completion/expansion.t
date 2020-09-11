  $ $MERLIN single expand-prefix -position 1:11 -prefix L.m \
  > -filename expansion.ml < expansion1.ml | jq ".value.entries | sort_by(.name)"
  [
    {
      "name": "List.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.map2",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mapi",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem_assoc",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem_assq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.memq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.merge",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.map2",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mapi",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem_assoc",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem_assq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.memq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.merge",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    }
  ]

  $ $MERLIN single expand-prefix -position 1:13 -prefix Lsi.m \
  > -filename expansion.ml < expansion2.ml | jq ".value.entries | sort_by(.name)"
  [
    {
      "name": "List.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.map2",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mapi",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem_assoc",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem_assq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.memq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.merge",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.map2",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mapi",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem_assoc",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem_assq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.memq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.merge",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    }
  ]


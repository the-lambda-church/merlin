Instances of the typechecker are cached based on configuration 
(values of type `Mconfig.t`).

Older versions of Merlin ignored some components resulting in possible
mismatches between the internal configuration of the typechecker (loadpath,
global modules visible from the environment) and Merlin configuration.

For instance, `-package` and `-cmi-path` were ignored.

The server might already be running, we kill it to make sure we start from a
clean slate:

  $ $MERLIN server stop-server

We build a dep which we will be revealed to Merlin later:

  $ $OCAMLC -c sub/dep.ml

First try with dep hidden:

  $ $MERLIN server errors -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 5
        },
        "end": {
          "line": 1,
          "col": 8
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module Dep"
      }
    ],
    "notifications": []
  }

For reference, the answer in single mode:

  $ $MERLIN single errors -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 5
        },
        "end": {
          "line": 1,
          "col": 8
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module Dep"
      }
    ],
    "notifications": []
  }


We try again after revealing the dependency:

  $ $MERLIN server errors -filename test.ml -cmi-path sub < test.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }


Reference:

  $ $MERLIN single errors -filename test.ml -cmi-path sub < test.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }


Well behaving versions of Merlin (>= 3.3.4) of should return the same answer as
reference.

We should check in the other direction too. Starting from a visible dep and
hidding it.  Older versions of the typechecker (before the 4.08 revamp of Env)
would accumulate dependencies and forget to flush the cache when a dependency
disappeared.
 
  $ $MERLIN server stop-server


Visible:

  $ $MERLIN server errors -filename test.ml -cmi-path sub < test.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }


Reference:

  $ $MERLIN single errors -filename test.ml -cmi-path sub < test.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }


Hidden:

  $ $MERLIN server errors -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 5
        },
        "end": {
          "line": 1,
          "col": 8
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module Dep"
      }
    ],
    "notifications": []
  }


Reference:

  $ $MERLIN single errors -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 5
        },
        "end": {
          "line": 1,
          "col": 8
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module Dep"
      }
    ],
    "notifications": []
  }


Now some cleanup.

  $ rm sub/dep.cm*

(enabled_if (>= %{ocaml_version} 4.08.0))

Let us take the following project, that defines a library "my_lib":

  $ cat > import.ml <<EOF \
  > module AB = struct\
  >   type t = A | B\
  > end\
  > let x = 3\
  > EOF

  $ cat > bar.ml <<EOF \
  > open Import\
  > let b = AB.B\
  > EOF

  $ echo "val x : int" > foo.mli
  $ cat > foo.ml <<EOF \
  > open Import\
  > type t = A\
  > let _a : AB.t * t = A, A\
  > let x, _ = x + 1, Bar.b\
  > EOF

  $ cat > my_lib.ml <<EOF \
  > module Bar = Bar\
  > module Foo = Foo\
  > let the_import_b = Bar.b\
  > EOF

And assume it is being built with dune:

  $ mkdir _build
  $ cp *.ml *.mli _build/
  $ cd _build && cat > my_lib__.ml <<EOF \
  > module Bar = My_lib__Bar\
  > module Foo = My_lib__Foo\
  > module Import = My_lib__Import\
  > EOF
  $ cd _build && $OCAMLC -c -no-alias-deps -w @a-40-41-42-49 -short-paths my_lib__.ml
  $ cd _build && $OCAMLC -c -w @a-40-41-42-49 -short-paths -open My_lib__ -o my_lib__Import import.ml
  $ cd _build && $OCAMLC -c -w @a-40-41-42-49 -short-paths -open My_lib__ -o my_lib__Bar bar.ml
  $ cd _build && $OCAMLC -c -w @a-40-41-42-49 -short-paths -open My_lib__ -o my_lib__Foo foo.mli
  $ cd _build && $OCAMLC -c -w @a-40-41-42-49 -short-paths -open My_lib__ -o my_lib__Foo foo.ml
  $ cd _build && $OCAMLC -c -w @a-40-41-42-49 -short-paths -open My_lib__ my_lib.ml
  $ cat > .merlin <<EOF \
  > EXCLUDE_QUERY_DIR \
  > FLG -w @a-40-41-42-49 -short-paths -open My_lib__ \
  > B _build \
  > S . \
  > EOF

Make sure merlin is happy:

  $ $MERLIN single errors -filename foo.ml < foo.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

Do an update that breaks the build:

  $ echo "let x = if x > 2 then 'c' else 'd'" >> import.ml
  $ cp *.ml *.mli _build/
  $ cd _build && $OCAMLC -c -w @a-40-41-42-49 -short-paths -open My_lib__ -o my_lib__Import import.ml
  $ cd _build && $OCAMLC -c -w @a-40-41-42-49 -short-paths -open My_lib__ -o my_lib__Bar bar.ml
  $ cd _build && $OCAMLC -c -w @a-40-41-42-49 -short-paths -open My_lib__ -o my_lib__Foo foo.ml
  File "foo.ml", line 4, characters 11-12:
  4 | let x, _ = x + 1, Bar.b
                 ^
  Error: This expression has type char but an expression was expected of type
           int
  [2]

Go to the file, and ask merlin to move you to the error:

  $ $MERLIN single errors -filename foo.ml < foo.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 11
        },
        "end": {
          "line": 4,
          "col": 12
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type char but an expression was expected of type int"
      }
    ],
    "notifications": []
  }

`Foo` does not depend on `My_lib`, but merlin tries to load it regardless.

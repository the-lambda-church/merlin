We work in a directory which is a symlink to another
  $ ln -s real link
  $ cd link

We need to set the MERLIN_LOG env variable for Merlin to log events prior
to the reading of the configuration.
  $ export MERLIN_LOG=-

Merlin should first try a path relative to where `dune ocaml-merlin` was
started and then an absolute path which may or may not include the symlink
  $ ocamlmerlin single dump-configuration -filename main.ml < main.ml \
  >  -log-section Mconfig -log-file - 2>&1 |
  > grep "Querying dune for file" |
  > sed s,/real/,/real_or_link/,g | sed s,/link/,/real_or_link/,g
  Querying dune for file: main.ml
  Querying dune for file: $TESTCASE_ROOT/real_or_link/main.ml

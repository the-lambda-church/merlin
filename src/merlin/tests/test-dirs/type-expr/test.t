  $ $MERLIN single type-expression -expression "y" -position start -filename test.ml < test.ml
  {
    "class": "return",
    "value": "Unbound value y",
    "notifications": []
  }
  $ $MERLIN single type-expression -expression "y" -position end -filename test.ml < test.ml
  {
    "class": "return",
    "value": "int",
    "notifications": []
  }

  $ $MERLIN single type-expression -expression "t" -position start -filename test.ml < test.ml
  {
    "class": "return",
    "value": "Unbound value t",
    "notifications": []
  }
  $ $MERLIN single type-expression -expression "t" -position end -filename test.ml < test.ml
  {
    "class": "return",
    "value": "Unbound value t",
    "notifications": []
  }

  $ $MERLIN single type-expression -expression "x + y" -position start -filename test.ml < test.ml
  {
    "class": "return",
    "value": "Unbound value x",
    "notifications": []
  }
  $ $MERLIN single type-expression -expression "x + y" -position end -filename test.ml < test.ml
  {
    "class": "return",
    "value": "int",
    "notifications": []
  }

  $ $MERLIN single type-expression -expression "T" -position start -filename test.ml < test.ml
  {
    "class": "return",
    "value": "Unbound constructor T",
    "notifications": []
  }
  $ $MERLIN single type-expression -expression "T" -position end -filename test.ml < test.ml
  {
    "class": "return",
    "value": "t",
    "notifications": []
  }

  $ $MERLIN single type-expression -expression "M" -position start -filename test.ml < test.ml
  {
    "class": "return",
    "value": "Unbound constructor M",
    "notifications": []
  }
  $ $MERLIN single type-expression -expression "M" -position end -filename test.ml < test.ml
  {
    "class": "return",
    "value": "(module List)",
    "notifications": []
  }

  $ $MERLIN single type-expression -expression "MT" -position start -filename test.ml < test.ml
  {
    "class": "return",
    "value": "Unbound constructor MT",
    "notifications": []
  }

  $ $MERLIN single type-expression -expression "MT" -position end -filename test.ml < test.ml | \
  > grep -v "^ *\(val\|type\) "
  {
    "class": "return",
    "value": "sig
  end",
    "notifications": []
  }

  $ $MERLIN single type-expression -expression "f (" -position start \
  > -filename test.ml < test.ml | \
  > sed 's/\("value": \)".*\.Error.*",/\1<syntax error>,/'
  {
    "class": "return",
    "value": <syntax error>,
    "notifications": []
  }

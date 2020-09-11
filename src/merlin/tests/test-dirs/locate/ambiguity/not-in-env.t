  $ $MERLIN single locate -look-for ml -position 2:10 -filename test.ml <<EOF \
  > let b = 10 \
  > let x = { b = 9 } \
  > EOF
  {
    "class": "return",
    "value": "Not in environment 'b'",
    "notifications": []
  }

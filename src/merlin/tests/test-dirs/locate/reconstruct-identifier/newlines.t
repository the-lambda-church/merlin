We need to be careful about newlines in Lexer_ident:

  $ $MERLIN single locate -look-for ml -position 7:20 \
  > -filename ./newline_in_quotes.ml < ./newline_in_quotes.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/reconstruct-identifier/newline_in_quotes.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 6:20 \
  > -filename ./escaped_newline.ml < ./escaped_newline.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/reconstruct-identifier/escaped_newline.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }

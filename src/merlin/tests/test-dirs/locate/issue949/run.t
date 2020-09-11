This test is for testing the behavior of identifiers with a . in them:

  $ $MERLIN single locate -look-for ml -position 2:16 ./issue949.ml < ./issue949.ml
  {
    "class": "return",
    "value": "Not in environment ''",
    "notifications": []
  }

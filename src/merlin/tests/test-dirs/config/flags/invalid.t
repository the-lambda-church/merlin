  $ echo | $MERLIN single check-configuration -filename invalid_flag.ml -lalala
  {
    "class": "return",
    "value": {
      "dot_merlins": [
        "tests/test-dirs/config/flags/.merlin"
      ],
      "failures": [
        "unknown flag -lalala"
      ]
    },
    "notifications": []
  }

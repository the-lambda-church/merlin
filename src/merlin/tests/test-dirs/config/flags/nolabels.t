classic and labels

  $ $MERLIN single errors -filename labels_ok_1.ml <<EOF \
  > let f ~x = () in f ~x:(); f () \
  > EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ $MERLIN single errors -filename labels_ko_1.ml -nolabels <<EOF \
  > let f ~x = () in f ~x:(); f () \
  > EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ $MERLIN single construct -position 2:25 \
  > -filename e1.ml <<EOF
  > EOF
  {
    "class": "error",
    "value": "Construct only works on holes.",
    "notifications": []
  }

  $ $MERLIN single construct -position 1:15 \
  > -filename e1.ml <<EOF
  > let x : int =
  > EOF
  {
    "class": "error",
    "value": "Construct only works on holes.",
    "notifications": []
  }

  $ $MERLIN single construct -position 1:5 \
  > -filename e1.ml <<EOF
  > let _ = 3
  > EOF
  {
    "class": "error",
    "value": "Construct only works on holes.",
    "notifications": []
  }

  $ $MERLIN single construct -position 2:16 \
  > -filename e1.ml <<EOF
  > module M = N module type S = module type of M
  > module M : S = _
  > EOF
  {
    "class": "error",
    "value": "Module not found: N",
    "notifications": []
  }

  $ $MERLIN single construct -position 1:15 \
  > -filename e1.ml <<EOF
  > module M : S = _
  > EOF
  {
    "class": "error",
    "value": "Could not find a module type to construct from. Check that you used a correct constraint.",
    "notifications": []
  }

  $ $MERLIN single construct -position 1:12 \
  > -filename e1.ml <<EOF
  > module M = _
  > EOF
  {
    "class": "error",
    "value": "Could not find a module type to construct from. Check that you used a correct constraint.",
    "notifications": []
  }

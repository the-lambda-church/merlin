let rec foo x =
  1 + bar x

and bar x =
  if x = 0 then
    x
  else
    foo (x - 1)

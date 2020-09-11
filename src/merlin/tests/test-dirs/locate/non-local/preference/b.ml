let _ = A.value

module Indir = A

let _ = Indir.value

include A

let _ = value

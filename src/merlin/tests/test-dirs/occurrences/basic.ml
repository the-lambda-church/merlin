let simple some =
  some + some

let withTypeAnnot (some : int) =
  some + some

type boxed_int = {value : int}

let withRecordPattern {value;} =
  value + value

let withRecordLiteral num =
  {value = num;}

let withRecordLiteralPunned value =
  {value;}

let withAlias (value as num) =
  num + num

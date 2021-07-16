val in_annots 
  : Types.Uid.t 
  -> Cmt_format.binary_annots 
  -> Warnings.loc option

val in_env 
  : Types.Uid.t 
  -> Env.t 
  -> Warnings.loc option

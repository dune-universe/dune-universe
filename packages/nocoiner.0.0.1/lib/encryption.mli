val encrypt :
  key:Cstruct.t -> iv:Cstruct.t -> message:string -> Cstruct.t * Cstruct.t

val decrypt :
     reason:exn
  -> key:Cstruct.t
  -> iv:Cstruct.t
  -> cipher:Cstruct.t
  -> tag:Cstruct.t
  -> string

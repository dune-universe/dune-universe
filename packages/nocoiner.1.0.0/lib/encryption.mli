val encrypt :
     key:Cstruct.t
  -> iv:Cstruct.t
  -> metadata:Cstruct.t
  -> message:Cstruct.t
  -> Cstruct.t * Cstruct.t

val decrypt :
     reason:exn
  -> key:Cstruct.t
  -> iv:Cstruct.t
  -> metadata:Cstruct.t
  -> cipher:Cstruct.t
  -> tag:Cstruct.t
  -> Cstruct.t

include Stdlib.Sys
let argv = match !Dynload.ref with | None -> Stdlib.Sys.argv | Some array -> array 
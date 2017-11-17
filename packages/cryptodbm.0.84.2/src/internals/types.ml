
(* Phantom types *)
type read
type full

type 'a status = Read | Closed | Full of 'a

include Errors


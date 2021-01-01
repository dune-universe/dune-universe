open GT

@type expr =
| Abs   of string * expr
| App   of expr * expr
| Var   of string
| Binop of string * expr * expr
| Let   of string * expr * expr
| Seq   of expr * expr
| Assn  of string * expr
with eval, show

@type state = (string * int) list with show

let lookup st x   = List.assoc st x
let update st x z = (x, z) :: st

class eval fself =
object
  inherit [state, state] @expr[eval] fself
                                
end

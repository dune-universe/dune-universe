type (* type t => *) t (* <= type t *) = (* Foo => *) Foo (* <= Foo *) | (* Bar => *) Bar (* <= Bar *) of int 

let _ = Foo (* ? Foo *)
let _ = Bar (* ? Bar *) 1

type u = A of v (* ? type v *)
and (* type v => *) v (* <= type v *) = (* B => *) B (* <= B *) of u | (* C => *) C  (* <= C *)

let _ = A C (* ? C *)
let _ = B (* ? B *) (A C)

type (* type x => *) x (* <= type x *) = { (* x.y => *) y (* <= x.y *) : int } 

let (* x => *) x (* <= x *) = { y = 1 } (* ? type x *)

let _ = function Foo (* ? Foo *) -> 1 | Bar (* ? Bar *) n -> n

let _ = 
  match { y = 1 } (* ? type x *) with
  | { y (* ? x.y *) = (* n => *) n (* <= n *) } (* ? type x *) -> n (* ? n *)

let _ = x(* ? x *).y (* ? x.y *) 

let _ = fun ((* fun x => *) x (* <= fun x *) : t (* ? type t *)) -> x (* ? fun x *) 

let _ = (1 : int)

type 'a (* type tt => *) tt (* <= type tt *) = Null | Cons of 'a * 'a tt (* ? type tt *) 


type 'a uu = Foo of 'a vv (* ? type vv *)
and 'a (* type vv => *) vv (* <= type vv *) = Bar of 'a uu 

module M = struct
  type (* type typ => *) typ (* <= type typ *) = (* M.F => *) F (* <= M.F *)
  let (* value typ => *) typ (* <= value typ *) = 1
end

type mt = M.typ (* ? type typ *)

let _ = (M.F (* ? M.F *) : M.typ (* ? type typ *))
let _ = M.typ (* ? value typ *)

(* very simple one *)
let (* foo => *) foo (* <= foo *) = 1
let _ = foo (* ? foo *)

(* from internal module *)

module (* M => *) M (* <= M *) = struct
  let (* M.bar => *) bar (* <= M.bar *) = foo
end

(* to internal module *)
let _ = M.bar (* ? M.bar *)

module (* F => *) F (* <= F *) (S : sig val bar : int end) = struct
  include S
  let (* F.bar2 => *) bar2 (* <= F.bar2 *) = 
    S.bar (* CR jfuruse: functor abstracted module? *)
  let _ = bar2 (* ? F.bar2 *)
  let _ = bar (* CR jfuruse: functor abstracted module? *)
end

module N = F (* ? F *)(M (* ? M *))

let _ = N.bar2 (* ? F.bar2 *)
let _ = N.bar (* ? M.bar *) 

include M (* ? M *)
let _ = bar (* ? M.bar *)

module O = struct
  include F(M)
end

let _ = O.bar (* ? M.bar *)

module P = struct
  type t = int
  let (* P.t => *) t (* <= P.t *) = 3
  module T = struct
    let (* P.T.t => *) t (* <= P.T.t *) = 2
  end
end

let _ = P.t (* ? P.t *)
let _ = P.T.t (* ? P.T.t *)

type tt = int
let tt = 1
let ttt = 1
class type ttt = object val x : float end

external (* ext => *)ext(* <= ext *) : int -> int = "hoge"
let _ = ext (* ? ext *)
 
module KM : sig
  val x : int
end = struct
  let y = 1
  let (* KM.x => *) x (* <= KM.x *) = 2
end

let _ = KM.x (* ? KM.x *)

module Z1 : sig
  val zx : int
end = struct
  let (* Z1.zx => *) zx (* <= Z1.zx *) = 1
  let zy = 2
end

module Z2 = struct
  let zx = 2
  let (* Z2.zy => *) zy (* <= Z2.zy *) = 3
  include Z1
end

let _ = Z2.zx (* ? Z1.zx *) (* fixed bug : did not point to zx in Z1 *)
let _ = Z2.zy (* ? Z2.zy *)

module Z3 = struct
  include Z1
  let (* Z3.zx => *) zx (* <= Z3.zx *) = 2
end

let _ = Z3.zx (* ? Z3.zx *)

let _ = Printf.sprintf

module PR = Printf

let _ = PR.fprintf

let (* function1 => *) function1 (* <= function1 *) (* fun arg x => *) x (* <= fun arg x *) = x (* ? fun arg x *)
let v = function1 (* ? function1 *) 1

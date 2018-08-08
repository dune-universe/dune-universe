exception (* E => *) E (* <= E *) of int

let x = E (* ? E *) 1 

exception (* F => *) F (* <= F *) = E (* CR jfuruse: should point to the original E? *)

let y = F (* ? F *) 1 

type t = ..

type t += (* A => *) A (* <= A *)

module M = struct
  type (* M.t => *) t (* <= M.t *) = ..
  type t += (* MA => *) MA (* <= MA *)
end

type M.t (* ? M.t *) += B

let x = A (* ? A *)
let y = M.MA (* ? MA *)


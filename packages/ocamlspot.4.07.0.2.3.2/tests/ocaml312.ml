module (* M => *) M (* <= M *) = struct
  type (* M.t => *) t (* <= M.t *) = { (* t.foo => *) foo (* <= t.foo *) : int } 
  let _ = fun (* foo1 => *) foo (* <= foo1 *) -> { foo (* ? foo1 *) (* ? M.t *) }
  let _ = fun { (* foo2 => *) foo (* <= foo2 *) (* ? M.t *) } -> foo (* ? foo2 *)
  let (* x => *) x (* <= x *) = 1
end

let _ = fun (* foo3 => *) foo (* <= foo3 *) -> { M.foo (* ? foo3 *) (* ? M.t *) }
let _ = fun { (* M.foo => *) M.foo (* <= M.foo *) (* ? M.t *) } -> foo (* ? M.foo *)
 
let f (type t) (x : t) = 
  let module N = struct
    exception E of t
  end in
  raise (N.E x)
;;

let f : 'a. 'a -> 'a = fun x -> x

(* first class packaged modules are tested in fstclassmodule.ml *)

let _ = 
  let open M (* ? M *) (* damn, no position for M *) in
  x (* ? x *)
;;

let _ = 
  M(* ? M *) . (* damn, no position for M *) (x (* ? x *))
;;

(* untested 
- method! val! inherit!
- <signature> with type t := <typeconstr>
  <signature> with module M := <module-path>
*)

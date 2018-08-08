type (* t => *) t (* <= t *) = Foo 

let _ = (Foo : t (* ? t *))

type (* t1 => *) t1 (* <= t1 *)
type t2 = t1 (* ? t1 *) list

type ('a,'b) (* t3 => *) t3 (* <= t3 *)
type ('a, 'b) t4 = ('a, 'b) t3 (* ? t3 *) list


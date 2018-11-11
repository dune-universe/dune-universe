let _loc = Location.none

let e1 = <:expr<2>>

let t2 = <:type<int -> bool -> (float * bool option) list * (int, string) Hashtbl.t>>

let f1 x = <:expr<1 + $x$ + $x$ >>

let f2 x y t = <:expr<3 * $x$ * (match $y$ with _ -> $y$ $t$ | _ -> $x$) + 2>>

let f i j =
  <:expr< $int:i$ + $int:j$ >>

let f3 i j p e = <:expr< function $int:i$ -> $int:j$ | $p$ -> $e$ >>

let f id = <:expr<$lid: id$>>

let f4 id = <:expr< fun $lid:id^"toto"$ -> $lid:id^"toto"$ >>

let f5 id = <:expr< $longident:id$ >>

let f6 s = <:expr< "blabla" ^ $string:s$>>

let f7 s =
  <:struct<
    let id x = x
    $struct:s$
  >>

let f8 u =
  <:struct<
    open $uid:u$
    let x = $uid:u$.x
  >>


(*let f9 x y t p = <:expr<3 * $x$ * (match $y$ : $t$ with $p$ -> $y$ | _ -> $x$) + 2>>*)

let f10 a x y = <:struct<let $lid:a$ = $x$ and b = $lid:y$>>

(*let f11 x y = <:sig<val $lid:x$ : $y$>>*)

let f12 x y z = <:expr< ($list:x$, $array:y$, $tuple:z$) >>

let f13 x y z x' y' z' = <:expr< fun ($list:x$, $array:y$, $tuple:z$) -> ($list:x'$, $array:y'$, $tuple:z'$) >>

(*

let j a b c d e f g h = <:expr<$bool:a$, $int:b$, $int32:c$, $int64:d$, $natint:e$, $char:f$, $string:g$, $float:h$>>

let k a b c1 c2 cs1 cs2 = <:structure<type ('$ident:a$) $lid:b$ = $uid:c1$ | $uid:c2$ of '$ident:a$ | $cs1$ | $cs2$ >>

let l a b = <:type<[ `$ident:a$ | `$ident:b$ ]>>

let fc x = <:constructors< $x$ >>

let ff x = <:fields< $x$ >>

let fb x = <:bindings< $bindings:x$ >>

let fc x = <:cases< $cases:x$ >>

let fm x = <:module< $x$ >>

let ft x = <:module type< $x$ >>
*)

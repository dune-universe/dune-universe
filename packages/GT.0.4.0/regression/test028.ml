(* The same as test027 but tuple instead of record *)
open GT

@type ('a, 'b) t = (int * string * 'a * 'b) with show, gmap, foldr, foldl, eq, compare

class ['a, 'b, 'self] print fa fb _prereq = object
  inherit [unit, 'a, unit, unit, 'b, unit, unit, 'self, unit] @t
  method c_T _ (x, y, a, b) =
    Printf.printf "%d\n" x;
    Printf.printf "%s\n" y;
    fa () a;
    fb () b;
end

let _ =
  let cs    = function EQ -> "EQ" | GT -> "GT" | LT -> "LT" in  
  let c x y = if x = y then EQ else if x < y then LT else GT in
  let x = (1, "2", "a", `B) in
  let y = (1, "2", "3", `B) in
  Printf.printf "x == x: %b\n" (transform(t) (new @t[eq] (=) (=)) x x);
  Printf.printf "x == y: %b\n" (transform(t) (new @t[eq] (=) (=)) x y);
  Printf.printf "compare (x, x) = %s\n" (cs (transform(t) (new @t[compare] c c) x x));
  Printf.printf "compare (x, y) = %s\n" (cs (transform(t) (new @t[compare] c c) x y));
  Printf.printf "compare (y, x) = %s\n" (cs (transform(t) (new @t[compare] c c) y x));
  Printf.printf "%s\n"
    (transform(t)
       (new @t[show] (fun _ a -> string_of_int a)  (fun _ -> function `B -> "`B") )
       ()
       (transform(t) (new @t[gmap] (fun _ x -> int_of_string x) (fun _ x -> x)) () y)
    );
  transform(t)
    (new print (fun _ a -> Printf.printf "%s\n" a) (fun _ -> function `B -> Printf.printf "`B\n") )
    ()
    x

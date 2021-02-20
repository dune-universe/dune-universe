@type ('a, 'b) t = GT.int * (GT.string * ('a * 'b))
with show, gmap, eq, compare, foldl, foldr

class ['a, 'b] print (fa: unit -> 'a -> unit) (fb: unit -> 'b -> unit) _fself =
  object
    inherit [unit, 'a, unit, unit, 'b, unit, unit, _, unit] @t
    method c_T () (x, (y, (a, b))) =
      Printf.printf "%d\n" x;
      Printf.printf "%s\n" y;
      fa () a;
      fb () b
  end

let printer fa fb subj =
  GT.transform(t) (new print fa fb) subj

let _ =
  let cs    = function GT.EQ -> "EQ" | GT -> "GT" | LT -> "LT" in
  let c x y = if x = y then GT.EQ else if x < y then LT else GT in
  let x = (1, ("2", ("a", `B))) in
  let y = (1, ("2", ("3", `B))) in
  let eq1 a b = GT.transform(t) (new @t[eq] (=) (=)) a b in
  Printf.printf "x == x: %b\n" (eq1 x x);
  Printf.printf "x == y: %b\n" (eq1 x y);
  let cmp1 a b = GT.transform(t) (new @t[compare] c c) a b in
  Printf.printf "compare (x, x) = %s\n" (cs @@ cmp1 x x);
  Printf.printf "compare (x, y) = %s\n" (cs @@ cmp1 x y);
  Printf.printf "compare (y, x) = %s\n" (cs @@ cmp1 y x);
  Printf.printf "%s\n" @@
  GT.show(t) GT.(show int) (function `B -> "`B") @@
  GT.gmap(t) int_of_string GT.id y
  ;
  GT.transform(t)
    (new print
      GT.(lift @@ Printf.printf "%s\n")
      (fun () -> function `B -> Printf.printf "`B\n"))
    ()
    x

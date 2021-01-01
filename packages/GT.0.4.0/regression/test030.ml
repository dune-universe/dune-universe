open GT

@type ('a, 'b) t = A of ('a * 'b) with show, gmap, foldl, foldr, eq, compare

class ['a,'b] print fa fb _fself =
  object
    inherit [unit, 'a, string, unit, 'b, string, unit, ('a,'b) t, unit] @t
    method c_A _ _ (a, b) = Printf.printf "A (%s, %s)\n" (fa () a) (fb () b)
  end

let _ =
  let cs    = function EQ -> "EQ" | GT -> "GT" | LT -> "LT" in
  let c x y = if x = y then EQ else if x < y then LT else GT in
  let x = A (1, "2") in
  let y = A (1, "3") in
  Printf.printf "x == x: %b\n" (GT.eq(t) (=) (=) x x);
  Printf.printf "x == y: %b\n" (GT.eq(t) (=) (=) x y);
  Printf.printf "compare (x, x) = %s\n" (cs (GT.compare(t) c c x x));
  Printf.printf "compare (x, y) = %s\n" (cs (GT.compare(t) c c x y));
  Printf.printf "compare (y, x) = %s\n" (cs (GT.compare(t) c c y x));
  Printf.printf "%s\n"
    (GT.show(t) id            string_of_int @@
     GT.gmap(t) string_of_int int_of_string @@
     A (1, "2")
    );
  transform(t) (new print (GT.lift string_of_int) (GT.lift id)) () (A (1, "2"))

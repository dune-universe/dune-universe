open GT

type ('a, 'b) t = int * (string * ('a * 'b))
[@@deriving gt ~show ~gmap ~eq ~compare]


class ['a, 'b] print _ fa fb =
  object 
    inherit ['a, unit, unit, 'b, unit, unit, unit, unit, _] class_t
    method c_Pair () x (y, (a, b)) =
      Printf.printf "%d\n" x;
      Printf.printf "%s\n" y; 
      fa () a;
      fb () b;
  end

let printer fa fb subj =
  GT.fix0 (fun self -> GT.transform t (new print self fa fb) ()) subj

let _ =
  let cs    = function EQ -> "EQ" | GT -> "GT" | LT -> "LT" in  
  let c x y = if x = y then EQ else if x < y then LT else GT in
  let x = (1, ("2", ("a", `B))) in
  let y = (1, ("2", ("3", `B))) in

  let eq1 fa fb x y =
    GT.fix0 (fun self -> transform(t) (new eq_t self fa fb )) x y
  in
  let cmp1 fa fb x y =
    GT.fix0 (fun self -> transform(t) (new compare_t self fa fb )) x y
  in

  Printf.printf "x == x: %b\n" (eq1 (=) (=) x x);
  Printf.printf "x == y: %b\n" (eq1 (=) (=) x y);
  Printf.printf "compare (x, x) = %s\n" (cs @@ cmp1 c c x x);
  Printf.printf "compare (x, y) = %s\n" (cs @@ cmp1 c c x y);
  Printf.printf "compare (y, x) = %s\n" (cs @@ cmp1 c c y x);
  Printf.printf "%s\n" @@
     GT.show t string_of_int (function `B -> "`B") @@
     GT.gmap t int_of_string (fun x -> x)  y
  ;
  printer
    (fun _ a -> Printf.printf "%s\n" a)
    (fun _ -> function `B -> Printf.printf "`B\n") 
    x

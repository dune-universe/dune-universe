open GT

@type ('a, 'b) t = {x: int; y: string; a: 'a; b: 'b} with show, gmap

class ['a, 'b] print fa fb _ =
  object
    inherit [unit, 'a, unit, unit, 'b, unit, unit, _, unit] @t
    method do_t () {x; y; a; b } =
      Printf.printf "%d\n" x;
      Printf.printf "%s\n" y;
      let () = fa () a in
      let () = fb () b in
      ()
  end

let _ =
  Printf.printf "%s\n"
    (GT.show(t) string_of_int id @@
     GT.gmap(t) int_of_string (fun `B -> "`B") @@
	   {x=1; y="2"; a="3"; b=`B}
    )
  ;
  GT.transform(t) (new print
                    (fun () -> Printf.printf "%s\n")
                    (fun () `B -> Printf.printf "`B\n") )
    ()
    {x=1; y="2"; a="a"; b=`B}

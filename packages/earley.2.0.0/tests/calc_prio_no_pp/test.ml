open Earley_core
open Earley
open Common

let compare s l =
  let s' = Bytes.create (List.length l) in
  let rec fn i = function
      [] -> ()
    | c::l' -> Bytes.set s' i c; fn (i+1) l'
  in
  fn 0 (List.rev l);
  let s' = Bytes.to_string s' in
  Printf.printf "-> %S\n%!" s';
  assert (s' = s)

let test_one ?(must_fail=false) ?(blank=no_blank) parse s =
  Printf.printf "PARSING: %S %!" s;
  if must_fail then
    try compare s (parse_string parse blank s);
    with Parse_error _ -> Printf.printf "-> ERROR\n%!"
       | e -> Printf.printf "-> %s\n%!" (Printexc.to_string e)
  else
    compare s (handle_exception (parse_string parse blank) s)

let test gen parse m =
  for i = 0 to m do
    let l = gen [""] i in
    List.iter (test_one parse) l
  done

let spec = [ ("--debug", Arg.Set_int debug_lvl,
              "set debug lvl");
              ("--quick", Arg.Unit (fun () -> kind := Quick),
	      "quick tests");
	     ("--normal", Arg.Unit (fun () -> kind := Normal),
	      "normal tests");
	     ("--full", Arg.Unit (fun () -> kind := Full),
	      "full tests (very long)"); ]

let _  = Arg.parse spec
		   (fun _ -> raise (Arg.Bad "extra arguments"))
		   "run unit tests on decap combinators"

let _ = Printf.printf "Testing {a|b|c}\n%!"

let a = char 'a' ['a']
let b = char 'b' ['b']
let c = char 'c' ['c']
let d = char 'd' ['d']
let e = char 'e' ['e']
let f = char 'f' ['f']
let a' = apply (fun x l -> x @ l) a

let _ = test_one a "a"
let _ = test_one b "b"
let _ = test_one c "c"

let abc = alternatives [a;b;c]
let abc' = apply (fun x l -> x @ l) abc

let _ = test_one abc "a"
let _ = test_one abc "b"
let _ = test_one abc "c"

let _ = Printf.printf "Testing a*\n%!"

let astar = fixpoint [] a'

let _ = test_one astar ""
let _ = test_one astar "a"
let _ = test_one astar "aa"
let _ = test_one astar "aaa"
let _ = test_one astar "aaaa"
let _ = test_one astar "aaaaa"

let _ = Printf.printf "Testing a+\n%!"

let aplus = fixpoint1 [] a'

let _ = test_one ~must_fail:true aplus ""
let _ = test_one aplus "a"
let _ = test_one aplus "aa"
let _ = test_one aplus "aaa"
let _ = test_one aplus "aaaa"
let _ = test_one aplus "aaaaa"

let _ = Printf.printf "Testing abc*\n%!"

let abcstar = fixpoint [] abc'

let _ = test_one abcstar ""
let _ = test_one abcstar "a"
let _ = test_one abcstar "ac"
let _ = test_one abcstar "aba"
let _ = test_one abcstar "abca"
let _ = test_one abcstar "acbab"
let _ = test_one abcstar "abccba"

let abcplus = fixpoint1 [] abc'

let _ = Printf.printf "Testing abc+\n%!"

let _ = test_one ~must_fail:true abcplus ""
let _ = test_one abcplus "a"
let _ = test_one abcplus "ac"
let _ = test_one abcplus "aba"
let _ = test_one abcplus "abca"
let _ = test_one abcplus "acbab"
let _ = test_one abcplus "abccba"

let _ = Printf.printf "Testing a*(right)\n%!"

let astar2 = declare_grammar "astar2"
let _ = set_grammar astar2 (alternatives [empty []; sequence a astar2 (fun x l -> l @ x)])

let _ = test_one astar2 ""
let _ = test_one astar2 "a"
let _ = test_one astar2 "aa"
let _ = test_one astar2 "aaa"
let _ = test_one astar2 "aaaa"
let _ = test_one astar2 "aaaaa"
(*
let _ = Printf.printf "Testing b(a*)(dep)\n%!"

let astar3 = declare_grammar "astar3"
let _ = set_grammar astar3 (alternatives
			      [apply (fun _ -> ['b']) b;
			       dependent_sequence astar3 (fun x -> apply (fun l -> l @ x) a)])

let _ = test_one astar3 "b"
let _ = test_one astar3 "ba"
let _ = test_one astar3 "baa"
let _ = test_one astar3 "baaa"
let _ = test_one astar3 "baaaa"
let _ = test_one astar3 "baaaaa"
*)
let _ = Printf.printf "Testing b(a*)(right)\n%!"

let astar3 = declare_grammar "astar3"
let _ = set_grammar astar3 (alternatives [empty []; sequence astar3 a (fun x l -> l @ x)])

let _ = test_one astar3 ""
let _ = test_one astar3 "a"
let _ = test_one astar3 "aa"
let _ = test_one astar3 "aaa"
let _ = test_one astar3 "aaaa"
let _ = test_one astar3 "aaaaa"

let _ = Printf.printf "Testing ab?\n%!"

let abo = sequence a (option [] b) (fun x y -> y @ x)

let _ = test_one abo "a"
let _ = test_one abo "ab"
let _ = test_one ~must_fail:true abo "b"

let _ = Printf.printf "Testing ab?c\n%!"

let aboc = sequence abo c (fun x l -> l @ x)

let _ = test_one aboc "ac"
let _ = test_one aboc "abc"
let _ = test_one ~must_fail:true aboc "bc"

let _ = Printf.printf "Testing apply ab?\n%!"

let abo' = apply (fun x -> x) abo

let _ = test_one abo' "a"
let _ = test_one abo' "ab"
let _ = test_one ~must_fail:true abo' "b"

let _ = Printf.printf "Testing apply (ab?)*\n%!"

let abostar = fixpoint [] (apply (fun x l -> x @ l) abo)

let _ = test_one abostar ""
let _ = test_one abostar "a"
let _ = test_one abostar "ab"
let _ = test_one abostar "aab"
let _ = test_one abostar "aba"
let _ = test_one abostar "abab"

let _ = Printf.printf "two mutually recursive grammars test ...%!"

let a = char 'a' 'a'
let b = char 'b' 'b'
let c = char 'c' 'c'
let d = char 'd' 'd'
let e = char 'e' 'e'
let f = char 'f' 'f'

let mutrec2a = declare_grammar "mutrec2a"
let mutrec2b = declare_grammar "mutrec2b"
let snoc la c = c :: la
let _ = set_grammar mutrec2a
		    (alternatives [
			 empty [];
			 sequence mutrec2a a snoc;
			 sequence mutrec2b c snoc])
let _ = set_grammar mutrec2b
		    (alternatives [
			 empty [];
			 sequence mutrec2b b snoc;
			 sequence mutrec2a d snoc])

let (^^) s l = List.map (fun x -> s ^ x) l

let rec genmutrec2a suffix n =
  if n > 0 then
    genmutrec2a ("a"^^suffix) (n-1) @
      genmutrec2b ("c"^^suffix) (n-1)
  else suffix

and genmutrec2b suffix n =
  if n > 0 then
    genmutrec2b ("b"^^suffix) (n-1) @
      genmutrec2a ("d"^^suffix) (n-1)
  else suffix

let _ = test genmutrec2a mutrec2a (test_cases (6, 8, 10))
let _ = Printf.printf "1%!"
let _ = test genmutrec2b mutrec2b (test_cases (6, 8, 10))
let _ = Printf.printf "2 OK\n%!"

let _ = warn_merge := false
let _ = Printf.printf "three mutually recursive grammars test ...%!"

let mutrec3a = declare_grammar "mutrec3a"
let mutrec3b = declare_grammar "mutrec3b"
let mutrec3c = declare_grammar "mutrec3c"

let _ = set_grammar mutrec3a
		    (alternatives [
			 empty [];
			 sequence mutrec3b a snoc;
			 sequence mutrec3c b snoc])
let _ = set_grammar mutrec3b
		   (alternatives [
			 empty [];
			 sequence mutrec3a c snoc;
			 sequence mutrec3c d snoc])
let _ = set_grammar mutrec3c
		   (alternatives [
			 empty [];
			 sequence mutrec3a e snoc;
			 sequence mutrec3b f snoc])

let rec genmutrec3a suffix n =
  if n > 0 then
    genmutrec3b ("a"^^suffix) (n-1) @
      genmutrec3c ("b"^^suffix) (n-1)
  else suffix

and genmutrec3b suffix n =
  if n > 0 then
    genmutrec3a ("c"^^suffix) (n-1) @
      genmutrec3c ("d"^^suffix) (n-1)
  else suffix

and genmutrec3c suffix n =
  if n > 0 then
    genmutrec3a ("e"^^suffix) (n-1) @
      genmutrec3b ("f"^^suffix) (n-1)
  else suffix


let _ = test genmutrec3a mutrec3a (test_cases (5, 7, 9))
let _ = Printf.printf "1%!"
let _ = test genmutrec3b mutrec3b (test_cases (5, 7, 9))
let _ = Printf.printf "2%!"
let _ = test genmutrec3c mutrec3c (test_cases (5, 7, 9))
let _ = Printf.printf "3 OK\n%!"

let _ = Printf.printf "three mutually bi-recursive grammars test ...%!"

let mutbirec3a = declare_grammar "mutbirec3a"
let mutbirec3b = declare_grammar "mutbirec3b"
let mutbirec3c = declare_grammar "mutbirec3c"

let (@@) l1 l2 = l2 @ l1

let _ = set_grammar mutbirec3a
		    (alternatives [
			 empty [];
			 sequence mutbirec3b (sequence mutbirec3c a snoc) (@@);
			 sequence mutbirec3c (sequence mutbirec3b b snoc) (@@)])
let _ = set_grammar mutbirec3b
		    (alternatives [
			 empty [];
			 sequence mutbirec3a (sequence mutbirec3c c snoc) (@@);
			 sequence mutbirec3c (sequence mutbirec3a d snoc) (@@)])
let _ = set_grammar mutbirec3c
		    (alternatives [
			 empty [];
			 sequence mutbirec3a (sequence mutbirec3b e snoc) (@@);
			 sequence mutbirec3b (sequence mutbirec3a f snoc) (@@)])

let rec genmutbirec3a suffix n =
  if n > 0 then
    let res = ref [] in
    for i = 0 to n - 1 do
      let j = n - 1 - i in
      res := genmutbirec3b (genmutbirec3c ("a"^^suffix) i) j @
	       genmutbirec3c (genmutbirec3b ("b"^^suffix) i) j @ !res
    done;
    !res
  else suffix

and genmutbirec3b suffix n =
  if n > 0 then
    let res = ref [] in
    for i = 0 to n - 1 do
      let j = n - 1 - i in
      res := genmutbirec3a (genmutbirec3c ("c"^^suffix) i) j @
	       genmutbirec3c (genmutbirec3a ("d"^^suffix) i) j @ !res
    done;
    !res
  else suffix

and genmutbirec3c suffix n =
  if n > 0 then
    let res = ref [] in
    for i = 0 to n - 1 do
      let j = n - 1 - i in
      res := genmutbirec3a (genmutbirec3b ("e"^^suffix) i) j @
	       genmutbirec3b (genmutbirec3a ("f"^^suffix) i) j @ !res
    done;
    !res
  else suffix

let _ = test genmutbirec3a mutbirec3a (test_cases (3, 4, 5))
let _ = Printf.printf "1%!"
let _ = test genmutbirec3b mutbirec3b (test_cases (3, 4, 5))
let _ = Printf.printf "2%!"
let _ = test genmutbirec3c mutbirec3c (test_cases (3, 4, 5))
let _ = Printf.printf "3 OK\n%!"

let gA = declare_grammar "gA" and gB = declare_grammar "gB"

let _ = set_grammar gA
  (alternatives [char 'x' ['x'];
		 sequence gB (sequence (char 'a' ()) gB (fun _ x -> x))
		   (fun x y -> y @ 'a' :: x )])

let _ = set_grammar gB
  (alternatives [gA;
		 sequence gA (sequence (char 'b' ()) gA (fun _ x -> x))
     (fun x y -> y @ 'b' :: x)])

let rec gengA suffix n =
  if n = 1 then "x" ^^ suffix else if n <= 0 then [] else
    let res = ref [] in
    for i = 0 to n - 1 do
      let j = n - 1 - i in
      res := gengB ("a"^^(gengB suffix i)) j @ !res
    done;
    !res

and gengB suffix n =
    let res = ref (gengA suffix n) in
    if n > 0 then
      for i = 0 to n - 1 do
	let j = n - 1 - i in
	res := gengA ("b"^^(gengA suffix i)) j @ !res
      done;
    !res
let _ = Printf.printf "two mutually recursive grammars that revealed bugs ...%!"

let _ = test gengA gA (test_cases (7, 11, 13))
let _ = Printf.printf "gA%!"
let _ = test gengB gB (test_cases (7, 11, 13))
let _ = Printf.printf "gB%!"
let _ = Printf.printf " OK%!\n"

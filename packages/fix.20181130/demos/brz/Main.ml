open Printf
let iter = List.iter
let map = List.map

(* -------------------------------------------------------------------------- *)

(* Instantiate Brzozowski with an alphabet restricted to 'a'-'e' and '$'. *)

module B = Brzozowski.Make(struct
  include Char
  let hash = Hashtbl.hash
  let foreach f =
    for i = Char.code 'a' to Char.code 'e' do f (Char.chr i) done;
    List.iter f [ '$' ]
  let print c =
    Printf.sprintf "%c" c
end)

open B

let a   = char 'a'
let b   = char 'b'
let c   = char 'c'
let eof = char '$'

let rec word (cs : char Seq.t) : regexp =
  match cs() with
  | Seq.Nil ->
      epsilon
  | Seq.Cons (c, cs) ->
      char c @@ word cs

let word (w : string) =
  word (String.to_seq w)

(* -------------------------------------------------------------------------- *)

(* Test routines. *)

type test =
  {
    basename: string;
    regexp: regexp;
    accepted: string list;
    rejected: string list;
  }

let exec test =
  printf "Regular expression: %s\n%!" (print test.regexp);
  printf "Automaton:\n%!";
  let automaton = dfa test.regexp in
  printf "%d states.\n%!" (size automaton);
  let filename = test.basename ^ ".dot" in
  let f = open_out filename in
  dump f automaton;
  close_out f;
  printf "Written to %s.\n%!" filename;
  test.accepted |> iter (fun input ->
    if exec automaton (String.to_seq input) = None then begin
      printf "Error: the input \"%s\" should be accepted, but is rejected.\n"
        input
    end
  );
  test.rejected |> iter (fun input ->
    if exec automaton (String.to_seq input) <> None then begin
      printf "Error: the input \"%s\" should be rejected, but is accepted.\n"
        input
    end
  )

(* -------------------------------------------------------------------------- *)

(* Examples. *)

let tests =
  ref []

let register test =
  tests := test :: !tests;
  test

(* [a(a|b)*] *)

let r =
  a @@ star (a ||| b)

let test_r =
  register {
    basename = "r";
    regexp = r;
    accepted = [ "a"; "aa"; "abb"; "aaababababa"; "ac"; ];
    rejected = [ ""; "b"; "bab"; "ca"; ];
  }

(* [a(a|b)*$] *)

let rd =
  r @@ eof

let test_rd =
  register {
    basename = "rd";
    regexp = rd;
    accepted = [ "a$"; "aa$"; "abb$"; "aaababababa$"; ];
    rejected = [ "$"; "b$"; "bab$"; "ca$"; "ac$"; ];
  }

(* [a(a|b)*(bc)*] *)

let s =
  r @@ star (b @@ c)

let _ =
  register {
    basename = "s";
    regexp = s;
    accepted = test_r.accepted @
               [ "abc"; "abcb"; "abcbc"; "aabbcbcbc"; "aaabbbcbcbc"; "ac"; ];
    rejected = [ "ca" ];
  }

(* [a(a|b)*(bc)*$] *)

let sd =
  s @@ eof

let _ =
  register {
    basename = "sd";
    regexp = sd;
    accepted = test_rd.accepted @
               [ "abc$"; "abcbc$"; "aabbcbcbc$"; "aaabbbcbcbc$"; ];
    rejected = [ "ca$"; "ac$"; "abcb$"; ];
  }

(* An automaton that tests whether a word belongs in a dictionary. *)

let keywords =
  [ "cab"; "bed"; "ace"; "add"; "dead"; "dad"; ]

let dict =
  keywords |> map word |> disjunction

let _ =
  register {
    basename = "dict";
    regexp = dict;
    accepted = [ "cab"; "added"; "dead"; ];
    rejected = [ "deed"; ];
  }

(* An automaton that searches for a word that belongs in a dictionary. *)

let k =
  one @@ (keywords |> map word |> disjunction)

let _ =
  register {
    basename = "k";
    regexp = k;
    accepted = [ "abed"; "bace"; ];
    rejected = [ "beca"; ];
  }

(* Main. *)

let () =
  iter exec (List.rev !tests)

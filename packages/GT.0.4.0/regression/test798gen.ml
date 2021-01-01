type ('a,'b) list_like = Nil | Cons of 'a * 'b
[@@deriving gt ~options:{show}]

let () =
  let rec show fa xs = GT.show list_like fa (show fa) xs
    (* glist_gcata (GT.lift fa) (GT.lift @@ show fa) (new show_glist) () xs *)
  in
  Printf.printf "%s\n%!" (show string_of_int (Nil));
  Printf.printf "%s\n%!" (show string_of_int (Cons (2, Nil)));
  Printf.printf "%s\n%!" (show string_of_int (Cons (2, Cons (2, Nil))));
()

type 'a list = ('a, 'a list) list_like
[@@deriving gt ~options:{show}]

let () =
  let rec show fa xs =
    GT.transform list  (new show_list_t (GT.lift fa)) () xs
  in
  Printf.printf "%s\n%!" (show string_of_int (Nil));
  Printf.printf "%s\n%!" (show (fun x -> x) (Cons ("WTF", Nil)));
  Printf.printf "%s\n%!" (show string_of_int (Cons (3, Cons (4, Nil))));
  ()

type intlist = GT.int list
[@@deriving gt ~options:{show}]

let () =
  let rec show xs = GT.transform intlist (new show_intlist_t) () xs in
  Printf.printf "%s\n%!" (show  Nil);
  Printf.printf "%s\n%!" (show  (Cons (6, Nil)));
  Printf.printf "%s\n%!" (show  (Cons (7, Cons (8, Nil))));
  ()

let id x = x

let show_typed_string = Printf.sprintf "\"%s\""

module AL : sig
  type ('a,'b) alist = Nil | Cons of 'a * 'b
  [@@deriving gt ~show ~show_typed ~gmap ]
end = struct
  type ('a,'b) alist  = Nil | Cons of 'a * 'b
  [@@deriving gt ~show ~show_typed ~gmap ]
end


let () =
  let open AL in
  let sh xs = show_typed_alist
      "string" show_typed_string
      "string" show_typed_string xs
  in
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", "bbb"));
  ()

module L : sig
  type 'a list = ('a, 'a list) AL.alist
  [@@deriving gt ~show_typed ]
end = struct
  type 'a list = ('a, 'a list) AL.alist
  [@@deriving gt ~show_typed ]
end


let () =
  let open L in
  let sh x = show_typed_list "string" show_typed_string x in
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", Cons ("bbb", Nil)))


module GT = struct
  include GT
  let int =
    {gcata = int.gcata;
     plugins = object
       method show_typed = int.plugins#show
       method show       = int.plugins#show
       method gmap       = int.plugins#gmap
     end
    }
end

module Lo : sig
  type 'a logic = Var of GT.int | Value of 'a
  [@@deriving gt ~show_typed ]
end = struct
  type 'a logic = Var of GT.int | Value of 'a
  [@@deriving gt ~show_typed ]
end

(* enhancing a class to print a type for constructor Var *)
class ['a,'extra] show_typed_logic fself  typ_a  fa = object
  inherit  ['a, 'extra] Lo.show_typed_logic fself typ_a fa
  method c_Var () _a =
    Format.sprintf "Var(%s : %s)"
      ((GT.int.GT.plugins)#show_typed  _a)
      typ_a
end

let rec custom_show_typed_logic typ_a fa subj =
  GT.fix0
    (fun self -> Lo.gcata_logic ((new show_typed_logic) self typ_a fa) ())
    subj

let () =
  let open Lo in
  let sh x = custom_show_typed_logic "string" show_typed_string x in
  Printf.printf "%s\t%s\n%!" (sh @@ Var 5) (sh @@ Value "asdf")

module LList : sig
  type 'a llist = ('a, 'a llist) AL.alist Lo.logic
  [@@deriving gt ~show_typed ]
end = struct
  type 'a llist = ('a, 'a llist) AL.alist Lo.logic
  [@@deriving gt ~show_typed ]
end

let () =
  let sh x = LList.show_typed_llist "string" show_typed_string x in
  Printf.printf "%s\n%!" (sh @@ Value (Cons ("aaa", Value (Cons ("bbb", Var 15)))) )


(* Now let's try show_typed for mutal recursion *)

module Mutal = struct
  type 'a foo = F1 of 'a | F2 of 'a boo
  and  'b boo = B1 of 'b | B2 of 'b foo
  [@@deriving gt ~show_typed ]

let () =
  let sh1 x = show_typed_foo "string" show_typed_string x in
  let sh2 x = show_typed_boo "string" show_typed_string x in

  Printf.printf "%s\n%!" (sh1 @@ F2 (B2 (F1 "asdf")));
  Printf.printf "%s\n%!" (sh1 @@ F2 (B2 (F2 (B1 "z"))));
  Printf.printf "%s\n%!" (sh2 @@ B2 (F2 (B2 (F1 "asdf"))) );
  Printf.printf "%s\n%!" (sh2 @@ B2 (F2 (B2 (F2 (B1 "z")))) );
  ()
end

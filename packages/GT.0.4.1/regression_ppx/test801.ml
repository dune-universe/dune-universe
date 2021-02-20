let id x = x

module AL : sig
  type ('a,'b) alist = Nil | Cons of 'a * 'b
  [@@deriving gt ~gmap ~show ~foldl ]
end = struct
  type ('a,'b) alist  = Nil | Cons of 'a * 'b
  [@@deriving gt ~gmap ~show ~foldl ]
end



let () =
  let open AL in
  let sh xs = show_alist id id xs in
  (* let fo xs = foldl_alist (fun () -> id) (fun () -> id) "" xs in *)
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", "bbb"));
  (* Printf.printf "%s\n%!" (fo @@ Cons ("aaa", "bbb")); *)
  ()

module L : sig
  type 'a list = ('a, 'a list) AL.alist
  [@@deriving gt ~gmap ~show ~foldl ]

end = struct
  type 'a list = ('a, 'a list) AL.alist
  [@@deriving gt ~gmap ~show ~foldl ]
end

let () =
  let open L in
  let sh x = show_list id x in
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", Cons ("bbb", Nil)))

module Lo : sig
  type 'a logic = Var of GT.int | Value of 'a
  [@@deriving gt ~gmap ~show ~foldl ]
end = struct
  type 'a logic = Var of GT.int | Value of 'a
  [@@deriving gt ~gmap ~show ~foldl ]
end

let () =
  let open Lo in
  let sh x = show_logic id x in
  Printf.printf "%s\t%s\n%!" (sh @@ Var 5) (sh @@ Value "asdf")

module LList : sig
  type 'a llist = ('a, 'a llist) AL.alist Lo.logic
  [@@deriving gt ~gmap ~show ~foldl ]
end = struct
  type 'a llist = ('a, 'a llist) AL.alist Lo.logic
  [@@deriving gt ~gmap ~show ~foldl ]
end

let () =
  let sh x = LList.show_llist id x in
  Printf.printf "%s\n%!" (sh @@ Value (Cons ("aaa", Value (Cons ("bbb", Var 15)))) )

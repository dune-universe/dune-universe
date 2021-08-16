module AL : sig
  @type ('a,'b) alist = [] [@name "nil"] | (::) of 'a * 'b [@name "cons"]
  with show,foldl,gmap
end  = struct
  @type ('a,'b) alist  = [] [@name "nil"] | (::) of 'a * 'b [@name "cons"]
  with show,foldl,gmap
end

let () =
  let open AL in
  let sh xs = GT.show alist Fun.id Fun.id xs in
  (* let fo xs = foldl_alist (fun () -> id) (fun () -> id) "" xs in *)
  Printf.printf "%s\n%!" (sh @@ "aaa" :: "bbb");
  (* Printf.printf "%s\n%!" (fo @@ Cons ("aaa", "bbb")); *)
  ()

module L : sig
  @type 'a list = ('a, 'a list) AL.alist with show,gmap,foldl
end = struct
  @type 'a list = ('a, 'a list) AL.alist with show,gmap,foldl
end

let () =
  let open L in
  let sh x = GT.show list Fun.id x in
  Printf.printf "%s\n%!" (sh @@ "aaa" :: "bbb" :: [])

module Lo : sig
  @type 'a logic = Var of GT.int | Value of 'a with show,gmap,foldl
end = struct
  @type 'a logic = Var of GT.int | Value of 'a with show,gmap,foldl
end

let () =
  let open Lo in
  let sh x = GT.show logic Fun.id x in
  Printf.printf "%s\t%s\n%!" (sh @@ Var 5) (sh @@ Value "asdf")


module LList : sig
  @type 'a llist = ('a, 'a llist) AL.alist Lo.logic with show,gmap,foldl
end = struct
  @type 'a llist = ('a, 'a llist) AL.alist Lo.logic with show,gmap,foldl
end

let () =
  let sh x = GT.show LList.llist Fun.id x in
  Printf.printf "%s\n%!" (sh @@ Value ("aaa" :: Value ("bbb" :: Var 15)) )


module Lo2 = struct
  include Lo

  class ['a, 'self] my_show fa fself = object
    inherit ['a, 'self] Lo.show_logic_t fa fself
    method c_Value () _ x = fa () x
  end

  let logic =
    { Lo.logic with
      GT.plugins = object
        method show fa xs =
          GT.transform (Lo.logic) (new my_show (GT.lift fa)) () xs
        method gmap x = Lo.logic.GT.plugins#gmap x
        method foldl x = Lo.logic.GT.plugins#foldl x
      end }
end

let () =
  let open Lo2 in
  let sh x = GT.show logic Fun.id x in
  Printf.printf "Modified implementation:\n%!";
  Printf.printf "\t%s\n%!" (sh @@ Var 5);
  Printf.printf "\t%s\n%!" (sh @@ Value "asdf");
  ()

module ReworkedLList : sig
  @type 'a llist = ('a, 'a llist) AL.alist Lo2.logic with show,gmap,foldl
end = struct
  @type 'a llist = ('a, 'a llist) AL.alist Lo2.logic with show,gmap,foldl
end

let () =
  let sh x = GT.show ReworkedLList.llist Fun.id x in
  Printf.printf "Printing of modified logic list\n";
  Printf.printf "%s\n%!" (sh @@ Value ("aaa" :: (Value ("bbb" :: (Var 15)))) )

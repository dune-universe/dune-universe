type ('a,'b) list_like = [] [@name "nil"] | (::) of 'a * 'b [@name "cons"]
[@@deriving gt ~options:{show}]


let () =
  let rec show fa xs = GT.show list_like fa (show fa) xs in
  Printf.printf "%s\n%!" (show string_of_int []);
  Printf.printf "%s\n%!" (show string_of_int [2]);
  Printf.printf "%s\n%!" (show string_of_int [2;2]);
  ()



type 'a list = ('a, 'a list) list_like
[@@deriving gt ~options:{show}]

let () =
  let rec show fa xs =
    GT.transform list  (new show_list_t (GT.lift fa)) () xs
  in
  Printf.printf "%s\n%!" (show string_of_int []);
  Printf.printf "%s\n%!" (show (fun x -> x)  ["WTF"]);
  Printf.printf "%s\n%!" (show string_of_int [3;4]);
  ()

type intlist = GT.int list
[@@deriving gt ~options:{show}]

let () =
  let rec show xs = GT.transform intlist (new show_intlist_t) () xs in
  Printf.printf "%s\n%!" (show []);
  Printf.printf "%s\n%!" (show [6]);
  Printf.printf "%s\n%!" (show [7;8]);
  ()


module Lo = struct
  type 'a t = Var of GT.int | Value of 'a [@@deriving gt ~options:{show}]
end

let () =
  let show xs = GT.show Lo.t (GT.show GT.int) xs in
  Printf.printf "Default logic values\n%!";
  Printf.printf "\t%s\n%!" (show  (Var 5));
  Printf.printf "\t%s\n%!" (show  (Value 6));
  ()

module Lo2 = struct
  type 'a t = 'a Lo.t = Var of GT.int | Value of 'a [@@deriving gt ~options:{show}]

  class ['a, 'self] my_show fa fself = object
    inherit ['a, 'self] Lo.show_t_t fa fself
    method c_Value () _ x = fa () x
  end

  let t =
    { Lo.t with
      GT.plugins = object
        method show fa xs =
          GT.transform (Lo.t) (new my_show (GT.lift fa)) () xs
      end }
end

let () =
  let show xs = GT.show Lo2.t (GT.show GT.int) xs in
  Printf.printf "Modified logic values\n%!";
  Printf.printf "\t%s\n%!" (show  (Var 5));
  Printf.printf "\t%s\n%!" (show  (Value 6));
  ()


module LList2 = struct
  type 'a t = ('a, 'a t) list_like Lo2.t [@@deriving gt ~options:{show}]
end

let () =
  let show xs = GT.show LList2.t (GT.show GT.int) xs in
  Printf.printf "Modified logic list values\n%!";
  Printf.printf "\t%s\n%!" (show  @@ Value []);
  Printf.printf "\t%s\n%!" (show  @@ Value (6 :: Value []));
  ()

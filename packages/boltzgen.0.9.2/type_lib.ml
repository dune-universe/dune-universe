open Type
open Sum_type

let fill_type_lib () = ()

let get_param1 = function
  | Type.Name (_, [ param ]) -> param
  | _ -> assert false

(* Unit *)
let type_unit =
  {
    identifier = "unit";
    arguments = 0;
    gen_fun = (fun _ _ _ _ bt _ -> (hide 0 bt, 1));
    to_string = (fun _ _ _ -> "()");
    string_of_named = (fun _ _ _ -> "(fun ()->\"()\")");
    boltzman_fun = (fun _ _ x -> (x, 1.0));
  }

(* Int *)
let type_int =
  {
    identifier = "int";
    arguments = 0;
    gen_fun =
      (fun rs m _ _ bt _ ->
        let rv = Random.State.int rs (2 * m) - m in
        (hide rv bt, 1));
    to_string =
      (fun _ bt j ->
        let i = reveal j bt in
        if i < 0 then Printf.sprintf "(%i)" i else string_of_int i);
    string_of_named = (fun _ _ _ -> "string_of_int");
    boltzman_fun = (fun _ _ x -> (*1.0+.*) (x, 1.0));
  }

let _ =
  add_type_to_lib type_unit;
  add_type_to_lib type_int;

  (* Nat = int >=0 *)
  add_type_to_lib ~rename:"nat"
    {
      type_int with
      gen_fun = (fun rs m _ _ bt _ -> (hide (Random.State.int rs m) bt, 1));
      to_string = (fun _ bt o -> string_of_int (reveal o bt));
    };
  (* Natp = int>0 *)
  add_type_to_lib ~rename:"natp"
    {
      type_int with
      gen_fun =
        (fun rs m _ _ bt _ -> (hide (1 + Random.State.int rs (m - 1)) bt, 1));
      to_string = (fun _ bt o -> string_of_int (reveal o bt));
    };
  add_type_to_lib ~rename:"small_nat"
    {
      type_int with
      gen_fun = (fun rs _ _ _ bt _ -> (hide (1 + Random.State.int rs 10) bt, 1));
      to_string = (fun _ bt o -> string_of_int (reveal o bt));
    };

  (* Float *)
  add_type_to_lib
    {
      identifier = "float";
      arguments = 0;
      gen_fun =
        (fun rs m _ _ bt _ ->
          (hide (Random.State.float rs (float (2 * m)) -. float m) bt, 1));
      to_string =
        (fun _ bt o ->
          let f = reveal o bt in
          if f >= 0.0 then Printf.sprintf "%g" f else Printf.sprintf "(%g)" f);
      string_of_named = (fun _ _ _ -> "string_of_float");
      (*boltzman_fun = (fun _ _ x -> x ** 4.0 ,4.0 *. ( x ** 3.0) )*)
      boltzman_fun = (fun _ _ x -> (x, 1.0));
    };

  (* Bool *)
  add_type_to_lib
    {
      identifier = "bool";
      arguments = 0;
      gen_fun = (fun rs _ _ _ bt _ -> (hide (Random.State.bool rs) bt, 1));
      to_string = (fun _ bt o -> string_of_bool (reveal o bt));
      string_of_named = (fun _ _ _ -> "string_of_bool");
      boltzman_fun = (fun _ _ x -> (x *. x, 2.0 *. x));
    }

(* Char *)
let gen_char ?(low = ' ') ?(high = '~') rs =
  let start = int_of_char low in
  let range = int_of_char high - start in
  let c = char_of_int (start + Random.State.int rs range) in
  if c = '"' || c = '\\' then ' ' else c

let gen_char_simple ?(use_space = false) rs =
  match Random.State.int rs 6 with
  | 0 -> gen_char ~low:'0' ~high:'9' rs
  | 1 | 2 -> gen_char ~low:'A' ~high:'Z' rs
  | 3 when use_space -> ' '
  | _ -> gen_char ~low:'a' ~high:'z' rs

let type_char =
  {
    identifier = "char";
    arguments = 0;
    gen_fun = (fun rs _ _ _ bt _ -> (hide (gen_char rs) bt, 1));
    to_string = (fun _ bt o -> "\'" ^ Char.escaped (reveal o bt) ^ "\'");
    string_of_named = (fun _ _ _ -> "Char.escaped");
    boltzman_fun = (fun _ _ x -> (x, 1.0));
  }

let _ = add_type_to_lib type_char

(*String*)
let type_string =
  {
    identifier = "string";
    arguments = 0;
    gen_fun =
      (fun rs m _ boltz bt z ->
        let cC, _ = boltz bt z in
        let n =
          if cC > 0.0 then
            min m (3 * (geom_law (1.0 /. cC)) (Random.State.float rs 1.0))
          else Random.State.int rs m
        in
        (*let n = Random.State.int rs m in*)
        let s = Bytes.create n in
        for i = 0 to n - 1 do
          Bytes.set s i (gen_char rs)
        done;
        (hide (Bytes.to_string s) bt, n));
    to_string = (fun _ bt o -> "\"" ^ reveal o bt ^ "\"");
    string_of_named = (fun _ _ _ -> "(fun s ->\"\\\"\"^s^\"\\\"\")");
    boltzman_fun =
      (fun _ _ x -> (1.0 /. (1.0 -. x), 1.0 /. ((1.0 -. x) ** 2.0)))
      (*boltzman_fun = (fun _ _ x -> x , 1.0 )*);
  }

let _ =
  add_type_to_lib type_string;
  add_type_to_lib ~rename:"simple_string"
    {
      type_string with
      gen_fun =
        (fun rs m _ boltz bt z ->
          let cC, _ = boltz bt z in
          let n =
            if cC > 0.0 then
              min m (3 * (geom_law (1.0 /. cC)) (Random.State.float rs 1.0))
            else Random.State.int rs m
          in
          let s = Bytes.create n in
          for i = 0 to n - 1 do
            Bytes.set s i (gen_char ~low:'A' ~high:'z' rs)
          done;
          (hide (Bytes.to_string s) bt, n));
    }

let _ =
  add_type_to_lib ~rename:"simple_spaced_string"
    {
      type_string with
      gen_fun =
        (fun rs m _ boltz bt z ->
          let _, _ = boltz bt z in
          (*let n = min m (3*(geom_law (1.0/.cC)) (Random.State.float rs 1.0)) in*)
          let n = Random.State.int rs m in
          let s = Bytes.create n in
          for i = 0 to n - 1 do
            Bytes.set s i (gen_char_simple ~use_space:true rs)
          done;
          (hide (Bytes.to_string s) bt, n));
    };

  add_type_to_lib ~rename:"id_string"
    {
      type_string with
      gen_fun =
        (fun rs m _ boltz bt z ->
          let cC, _ = boltz bt z in
          let n =
            1 + min m (3 * (geom_law (1.0 /. cC)) (Random.State.float rs 1.0))
          in

          (*let n = Random.State.int rs m in*)
          let s = Bytes.create n in
          Bytes.set s 0 (gen_char ~low:'a' ~high:'z' rs);
          for i = 1 to n - 1 do
            Bytes.set s i (gen_char_simple rs)
          done;
          (hide (Bytes.to_string s) bt, n));
    };

  (* 'a List *)
  (* First an implementatioon of list*)
  let malist =
    Recursive_type_gen.named_of_string
      "type 'a m__list = Null | Node of 'a *'a m__list"
  in
  add_type_to_lib malist;
  add_type_to_lib ~rename:"qlist" malist;

  (* a wrapper of the list of caml to the custom impl*)
  let print_list f et o =
    "["
    ^ List.fold_left
        (fun a b ->
          let c = f et b in
          if a <> "" then a ^ "; " ^ c else c)
        "" o
    ^ "]"
  in

  let rename_list ?arg2 name =
    let get_arg bt =
      match (bt, arg2) with
      | Name (_, [ arg ]), None | Name (_, []), Some arg -> arg
      | _ -> assert false
    in
    let mylist_of_list bt =
      let arg = get_arg bt in
      Name ("m__list", [ arg ])
    in
    let getp bt = match arg2 with None -> get_param1 bt | Some arg -> arg in
    {
      identifier = name;
      arguments = (if arg2 = None then 1 else 0);
      gen_fun =
        (fun rs m f boltz bt z ->
          let arg = get_arg bt in
          let newtype = Name ("m__list", [ arg ]) in
          let mlist, size = malist.gen_fun rs m f boltz newtype z in
          let rec conv l =
            match l with
            | "Node", Some x ->
                let tab = reveal x (Prod [ arg; newtype ]) in
                let t, q = tab in
                hide t arg :: conv q
            | _ -> []
          in
          (hide (conv (reveal mlist newtype)) bt, size));
      to_string = (fun f bt o -> print_list f (getp bt) (reveal o bt));
      string_of_named =
        (fun meml f bt ->
          "(fun s ->(List.fold_left (fun a b -> (if a <> \"[\" then a^\"; \" \
           else \"[\")^("
          ^ f meml (getp bt)
          ^ " b) ) \"[\" s)^\"]\")");
      boltzman_fun =
        (fun f bt z ->
          let newtype = mylist_of_list bt in
          malist.boltzman_fun f newtype z);
    }
  in
  add_type_to_lib (rename_list "list");
  add_type_to_lib (rename_list ~arg2:(Name ("int", [])) "qlist")

(* 'a option *)
let _ =
  let option_type =
    Recursive_type_gen.named_of_string "type 'a option = None | Some of 'a "
  in
  add_type_to_lib option_type

(* ('a, 'e ) result *)
let _ =
  let result_type =
    Recursive_type_gen.named_of_string
      "type ('a, 'e) result = Ok of 'a | Error of 'e "
  in
  add_type_to_lib result_type

let _ =
  add_type_to_lib
  @@ Recursive_type_gen.named_of_string "type qcm = A | B | C | D"

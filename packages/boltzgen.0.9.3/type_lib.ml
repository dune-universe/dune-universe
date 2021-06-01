open Type

let fill_type_lib () = ()

let get_param1 = function
  | Type.Name (_, [ param ]) -> param
  | _ -> assert false

let simple_type =
  {
    identifier = "";
    boltz_identifier = "";
    is_simple = true;
    get_equ = (fun _ acc _ -> acc);
    arguments = 0;
    gen_fun = (fun _ _ _ _ _ _ -> failwith "ToInstantiate");
    print = (fun _ _ _ _ -> failwith "ToInstantiate");
    string_of_named = (fun _ _ _ -> failwith "ToInstantiate");
    boltzman_fun = (fun _ _ _ -> failwith "ToInstantiate");
  }

(* Unit *)
let type_unit =
  {
    simple_type with
    identifier = "unit";
    boltz_identifier = "unit";
    gen_fun = (fun _ _ _ _ bt _ -> (hide 0 bt, 1));
    print = (fun _ _ f _ -> Format.pp_print_string f "()");
    string_of_named = (fun _ _ _ -> "(fun ()->\"()\")");
    boltzman_fun = (fun _ _ x -> (x, 1.0));
  }

(* Int *)
let type_int =
  {
    simple_type with
    identifier = "int";
    boltz_identifier = "int";
    gen_fun =
      (fun rs m _ _ bt _ ->
        let rv = Random.State.int rs (2 * m) - m in
        (hide rv bt, 1));
    print =
      (fun _ bt f j ->
        let i = reveal j bt in
        if i < 0 then Format.fprintf f "(%i)" i else Format.fprintf f "%i" i);
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
    };
  (* Natp = int>0 *)
  add_type_to_lib ~rename:"natp"
    {
      type_int with
      gen_fun =
        (fun rs m _ _ bt _ -> (hide (1 + Random.State.int rs (m - 1)) bt, 1));
    };
  add_type_to_lib ~rename:"small_nat"
    {
      type_int with
      gen_fun = (fun rs _ _ _ bt _ -> (hide (1 + Random.State.int rs 10) bt, 1));
    };

  (* Float *)
  add_type_to_lib
    {
      simple_type with
      identifier = "float";
      boltz_identifier = "float";
      gen_fun =
        (fun rs m _ _ bt _ ->
          (hide (Random.State.float rs (float (2 * m)) -. float m) bt, 1));
      print =
        (fun _ bt form o ->
          let f = reveal o bt in
          if f >= 0.0 then Format.fprintf form "%g" f
          else Format.fprintf form "(%g)" f);
      string_of_named = (fun _ _ _ -> "string_of_float");
      (*boltzman_fun = (fun _ _ x -> x ** 4.0 ,4.0 *. ( x ** 3.0) )*)
      boltzman_fun = (fun _ _ x -> (x, 1.0));
    };

  (* Bool *)
  add_type_to_lib
    {
      simple_type with
      identifier = "bool";
      boltz_identifier = "bool";
      gen_fun = (fun rs _ _ _ bt _ -> (hide (Random.State.bool rs) bt, 1));
      print = (fun _ bt f o -> Format.pp_print_bool f (reveal o bt));
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
    simple_type with
    identifier = "char";
    boltz_identifier = "char";
    gen_fun = (fun rs _ _ _ bt _ -> (hide (gen_char rs) bt, 1));
    print =
      (fun _ bt form o ->
        Format.fprintf form "\'%s\'" (Char.escaped (reveal o bt)));
    string_of_named = (fun _ _ _ -> "(fun c -> \"'\"^(Char.escaped c)^\"'\")");
    boltzman_fun = (fun _ _ x -> (x, 1.0));
  }

let _ = add_type_to_lib type_char

(*String*)
let type_string =
  {
    simple_type with
    identifier = "string";
    boltz_identifier = "string";
    gen_fun =
      (fun rs m _ boltz bt z ->
        let cC, _ = boltz (Name ("char", [])) z in
        let n = min m (Math.geom_law (1.0 -. cC) (Random.State.float rs 1.0)) in
        (*let cC, _ = boltz bt z in
          let n =
            if cC > 0.0 then
              min m (3 * (geom_law (1.0 /. cC)) (Random.State.float rs 1.0))
            else Random.State.int rs m
          in*)
        (*let n = Random.State.int rs m in*)
        let s = Bytes.create n in
        for i = 0 to n - 1 do
          Bytes.set s i (gen_char rs)
        done;
        (hide (Bytes.to_string s) bt, n));
    print = (fun _ bt f o -> Format.fprintf f "\"%s\"" (reveal o bt));
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
          let cC, _ = boltz (Name ("char", [])) z in
          let n =
            min m (Math.geom_law (1.0 -. cC) (Random.State.float rs 1.0))
          in
          (*let cC, _ = boltz bt z in
            let n =
              if cC > 0.0 then
                min m (3 * (geom_law (1.0 /. cC)) (Random.State.float rs 1.0))
              else Random.State.int rs m
            in*)
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
          let cC, _ = boltz (Name ("char", [])) z in
          let n =
            min m (Math.geom_law (1.0 -. cC) (Random.State.float rs 1.0))
          in
          (*let _, _ = boltz bt z in
            (*let n = min m (3*(geom_law (1.0/.cC)) (Random.State.float rs 1.0)) in*)
            let n = Random.State.int rs m in*)
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
          let cC, _ = boltz (Name ("char", [])) z in
          let n =
            max 1
              (min m (Math.geom_law (1.0 -. cC) (Random.State.float rs 1.0)))
          in
          (*let n = Random.State.int rs m in*)
          let s = Bytes.create n in
          if n > 0 then Bytes.set s 0 (gen_char ~low:'a' ~high:'z' rs);
          for i = 1 to n - 1 do
            Bytes.set s i (gen_char_simple rs)
          done;
          (hide (Bytes.to_string s) bt, n));
    };

  (* 'a array*)
  add_type_to_lib
    {
      identifier = "array";
      boltz_identifier = "array";
      arguments = 1;
      is_simple = false;
      get_equ = (fun _ acc _ -> acc);
      gen_fun =
        (fun rs m f boltz bt z ->
          let tt = get_param1 bt in
          let cC, _ = boltz tt z in
          let n = Math.geom_law (1.0 -. cC) (Random.State.float rs 1.0) in

          let vc, s = f rs m tt z in
          (*Printf.printf "size : %i, cc:%f\n" n cC;*)
          let a = Array.make n (reveal vc tt) in
          let sr = ref s in
          for i = 1 to n - 1 do
            let vci, si = f rs m tt z in
            sr := !sr + si;
            a.(i) <- reveal vci tt
          done;
          (hide a bt, !sr));
      print =
        (fun f bt form o ->
          let vtable = reveal o bt in
          let tt = get_param1 bt in
          Format.pp_print_string form "[|";
          Array.iteri
            (fun i v ->
              if i > 0 then Format.pp_print_string form ";";
              f tt form (hide v tt))
            vtable;
          Format.pp_print_string form "|]");
      string_of_named =
        (fun meml f bt ->
          let tt = get_param1 bt in
          "(fun a -> (Array.fold_left (fun a b -> (if a<> \"[|\" then a^\"; \" \
           else \"[|\")^(" ^ f meml tt ^ " b) ) \"[|\" a)^\"|]\")");
      boltzman_fun =
        (fun f bt z ->
          let tt = get_param1 bt in
          let v, dv = f tt z in
          let vm1 = 1.0 -. v in
          (1.0 /. vm1, dv /. (vm1 *. vm1)));
    };

  (* 'a List *)
  (* First an implementatioon of list*)
  let malist =
    Recursive_type_gen.named_of_string
      "type 'a m__list = Null | Node of 'a *'a m__list"
  in
  add_type_to_lib malist;

  (*add_type_to_lib ~rename:"qlist" malist;*)

  (* a wrapper of the list of caml to the custom impl*)
  let print_list f et form o =
    Format.fprintf form "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun _ _ -> Format.fprintf form "; ")
         (f et))
      o
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
      boltz_identifier = name;
      arguments = (if arg2 = None then 1 else 0);
      is_simple = false;
      get_equ =
        (fun f acc bt ->
          let arg = get_arg bt in
          let newtype = Name ("m__list", [ arg ]) in
          malist.get_equ f acc newtype);
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
      print = (fun f bt form o -> print_list f (getp bt) form (reveal o bt));
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
  add_type_to_lib (rename_list "list")

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

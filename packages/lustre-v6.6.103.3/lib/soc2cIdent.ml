(* Time-stamp: <modified the 12/03/2020 (at 09:30) by Erwan Jahier> *)

let colcol = Str.regexp "::"
let prime = Str.regexp "'"
let id2s id = (* XXX Refuser les noms de module à la con plutot *)
  let str =
	 match Str.split colcol id with
	   | ["Lustre";"true"] -> "_true" (* a good idea to do that here ? *)  
	   | ["Lustre";"false"] -> "_false"
	   | ["true"] -> "_true" (* a good idea to do that here ? *)  
	   | ["false"] -> "_false"
	   | [s] -> s
	   | [m;s] -> if Lv6MainArgs.global_opt.Lv6MainArgs.no_prefix then s else m^"_"^s 
	   | _ -> id
  in
  let str = Str.global_replace colcol "_" str in
  let str = Str.global_replace prime "_prime" str in
  str

let long2s l = id2s (Lv6Id.string_of_long false l)

let rec (type_to_short_string : Data.t -> string) = 
  fun v -> 
    let str =
      match v with
        | Data.String -> "s"
        | Data.Bool -> "b"
        | Data.Int -> "i"
        | Data.Real-> "r"
        | Data.Extern s -> id2s s 
        | Data.Enum  (_s, _sl) -> "e" (* s *) 
        | Data.Struct (sid,_) -> id2s sid
        | Data.Array (ty, sz) -> Printf.sprintf "%sp%d" (type_to_short_string ty) sz 
        | Data.Alias(n,_) -> n
        | Data.Alpha nb ->
        (* On génère des "types" à la Caml : 'a, 'b, 'c, etc. *)
          let a_value = Char.code('a') in
          let z_value = Char.code('z') in
          let str =
            if (nb >= 0 && nb <= (z_value - a_value)) then
              ((Char.escaped (Char.chr(a_value + nb))))
            else
              ("a" ^ (string_of_int nb))
          in
          str
    in
    str

(* Associate a unique string to each key, while still returning id
   for the key (id,l,opt) for the first time a key with "id" is used.

   To do that, I maintain a table of tables, associating a table to
   each id. The inner table associates a unique string to each pair
   "Data.t list * key_opt"
*)

let (key_table: (Soc.ident, (Data.t list * Soc.key_opt, int) Hashtbl.t) Hashtbl.t) = 
  Hashtbl.create 10

let (get_base_name : Soc.key -> string) =
  fun (id,l,opt) -> 
    let id = id2s id in
    let suff = 
      try 
        let inner_tbl = Hashtbl.find key_table id in
        (try 
          let cpt = Hashtbl.find inner_tbl (l,opt) in
          let str = if cpt = 1 then "" else ("_"^(string_of_int cpt)) in
          str
         with Not_found ->
           (* The first time (l,opt) is encountered for id *)
           let cpt = 1+Hashtbl.length inner_tbl in 
           let str = "_"^(string_of_int cpt) in
           Hashtbl.add inner_tbl (l,opt) cpt;
           str
        )
      with Not_found ->
        (* The first time id is encountered *)
        let inner_tbl = Hashtbl.create 1 in
        Hashtbl.add inner_tbl (l,opt) 1;
        Hashtbl.add key_table id inner_tbl;
        ""
    in
    id ^ suff

(* unit tests *)
let _ = 
  assert (get_base_name ("x", [Data.Int],  Soc.Nomore) = "x");
  assert (get_base_name ("x", [Data.Int],  Soc.Nomore) = "x");
  assert (get_base_name ("x", [Data.Bool], Soc.Nomore) = "x_2");
  assert (get_base_name ("x", [Data.Int],  Soc.Nomore) = "x");
  assert (get_base_name ("x", [Data.Real], Soc.Nomore) = "x_3");
  assert (get_base_name ("x", [Data.Bool], Soc.Nomore) = "x_2");
  Hashtbl.clear key_table; 
  ()

let (get_ctx_name : Soc.key -> string) =
  fun sk -> (get_base_name sk) ^ "_ctx"

(* This function is injective *)
let (get_soc_name : Soc.key -> string) =
  fun sk -> (get_base_name sk)

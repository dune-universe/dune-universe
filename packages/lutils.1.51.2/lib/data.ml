(* Time-stamp: <modified the 12/03/2020 (at 15:40) by Erwan Jahier> *)

type ident = string
type v = I of int | F of float | B of bool
       | E of ident * int
       | A of v array | S of (ident * v) list | U
       | Str of string

type t = 
  | Bool | Int | Real
  | Extern of ident
  | Enum   of (ident * ident list)
  | Struct of ident * (ident * t) list
  | Array  of (t * int)
  | Alpha of int 
  | Alias of (string * t)
  | String
    
let (val_to_string_type : v -> string) =
  function
    | I _ -> "int"
    | F _  -> "real"
    | B _ -> "bool"
    | E (e,_) -> e
    | S _ -> "struct"
    | A _ -> "array"
    | U -> "nil"
    | Str _ -> "string"
      
let rec (val_to_string : (float -> string) -> v -> string) =
  fun s2f -> 
  function
    | I i  -> (try string_of_int i with _ -> assert false)
    | F f  -> s2f f
    | B true -> "t"
    | B false -> "f"
    | E (e,_) -> e
    | S fl -> "{"^(String.concat ";" 
                     (List.map (fun (fn,fv) -> fn^"="^(val_to_string s2f fv)) fl))^"}"
    | A a -> 
      let str = ref "[" in
      let f i a = str := !str ^ (if i = 0 then "" else ",") ^ (val_to_string s2f a) in
      Array.iteri f a;
      (!str^"]")
    | U -> "nil"
    | Str str -> str

let (val_to_rif_string : (float -> string) -> v -> string) =
  fun s2f -> 
  function
    | I i  -> (try string_of_int i with _ -> assert false)
    | F f  -> s2f f
    | B true -> "t"
    | B false -> "f"
    | E (e,_) -> e
    | S fl -> ""^(String.concat ";" 
                     (List.map (fun (_fn,fv) -> " "^(val_to_string s2f fv)) fl))^""
    | A a -> 
      let str = ref "" in
      let f i a = str := !str ^ (if i = 0 then "" else " ") ^ (val_to_string s2f a) in
      Array.iteri f a;
      (!str)
    | U -> "nil"
    | Str str -> str

 
let rec (type_to_string_gen : bool -> t -> string) = 
  fun alias v -> 
    let str =
      match v with
        | String -> "string"
        | Bool -> "bool"
        | Int  -> "int"
        | Real -> "real"
        | Extern _s -> "string" (* what else should be done? *)
        (*  | Enum  (s, sl) -> "enum " ^ s ^ " {" ^ (String.concat ", " sl) ^ "}" *)
        | Enum  (s, _sl) -> s
        | Struct (sid,_) -> sid
        | Array (ty, sz) -> Printf.sprintf "%s^%d" (type_to_string_gen alias ty) sz 
        | Alpha nb ->
        (* On génère des "types" à la Caml : 'a, 'b, 'c, etc. *)
          let a_value = Char.code('a') in
          let z_value = Char.code('z') in
          let str =
            if (nb >= 0 && nb <= (z_value - a_value)) then
              ("'" ^ (Char.escaped (Char.chr(a_value + nb))))
            else
              ("'a" ^ (string_of_int nb))
          in
          str

        | Alias(n,t) -> if alias then n else type_to_string_gen alias t
    in
    str
let (type_to_string : t -> string) = type_to_string_gen false
let (type_to_string_alias : t -> string) = type_to_string_gen true




let (type_of_string : string -> t) = 
  function
    | "bool"  ->  Bool
    | "real"  -> Real
    | "float" -> Real
    | "int"   -> Int
    | "string" -> String
    | s -> failwith (s ^ ": unsupported type.\n")


type vntl = (string * t) list
type subst = (string * v) 

type access = Idx of int | Fld of ident | Sle of int * int * int * int

(* exported *)
let rec (update_val : v -> v -> access list -> v) =
  fun pre_v v access -> 
    match pre_v,access with
      | _,[] -> v
      | A a, (Sle(f,l,s,w))::access -> (
        let a = Array.copy a in
        let j = ref 0 in
        let sub_array = Array.make w U in
        for i = f to l do
          if (i - f) mod s = 0 then (
            sub_array.(!j) <- a.(i);
            incr j
          );
        done;
        let sub_array = match update_val (A sub_array) v access  with
            A sub_array -> sub_array | _ -> assert false
        in
        j := 0;
        for i = f to l do
          if (i - f) mod s = 0 then (
            a.(i) <- sub_array.(!j);
            incr j
          );
        done;
        A a
      )
      | A a, (Idx i)::access ->
        let a = Array.copy a 
        (* necessary for arrays of arrays. It would probably more
           clever to only copy a_i though. *)
        in
        let a_i = update_val a.(i) v access in
        a.(i) <- a_i;
        A a
      | S(fl), (Fld fn)::access ->
        S (List.map
             (fun (fn2,v2) -> if fn=fn2 then fn,update_val v2 v access else (fn2,v2)) 
             fl)
      | U,_ -> assert false (* create_val v access *)
      | _,_ -> assert false (* finish me *)


(* exported *)
let rec (create_u_val : t -> v) =
  fun vt -> 
    match vt with
      | Array(vt,size) -> 
        let a = Array.make size U in
        for i=0 to size-1 do
          a.(i) <- create_u_val vt
        done;
        A a
      | Struct(_sn,fl) -> S(List.map (fun (fn,ft) -> fn, create_u_val ft) fl)
      | _ -> U

(* seems slower (??) *)
let (create_val : t -> v -> access list -> v) =
  fun vt v access ->
    let u_val = create_u_val vt in
    update_val u_val v access

let (_create_val_alt : t -> v -> access list -> v) =
  fun vt v access ->
    match vt,access with
      | _,[] -> v
      | Array(vt,size), (Sle(f,l,s,w))::access -> (
        let j = ref 0 in
        let a = Array.make size U in
        let vt = Array(vt,w) in
        let sub_array = match create_val vt v access 
          with A sa -> sa | _ -> assert false 
        in
        for i = f to l do
          if (i - f) mod s = 0 then (
            a.(i) <- sub_array.(!j);
            incr j
          );
        done;
        A a
      )
      | Array(vt,size), (Idx i)::access -> 
        let a = Array.make size U in
        let a_i = create_val vt v access in
        a.(i) <- a_i;
        A a
      | Struct(_sn,fl), (Fld fn)::access -> 
        S(List.map
            (fun (fn2,vt2) -> if fn=fn2 then fn,create_val vt2 v access else fn2,U) fl)
      | _,_ -> assert false



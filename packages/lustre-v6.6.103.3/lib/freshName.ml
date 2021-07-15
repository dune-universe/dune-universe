(* Time-stamp: <modified the 29/08/2019 (at 15:05) by Erwan Jahier> *)


(* maps node_key to a string that won't clash *)
let node_key_tbl = Hashtbl.create 0

(* maps node name (string) to a counter *)
let node_name_tbl = Hashtbl.create 0

(* exported *)
let (node_key: Lic.node_key -> string -> string) =
  fun nk name -> 
    let (long, _sargs) = nk in
    try Hashtbl.find node_key_tbl nk
    (* Note that we ignore the "name" in argument in this case *)
    with Not_found -> 
      (* let's build an ident that won't clash *)
      (* all new name should not begins with a "_" ; hence we prefix by "n_" *)
      let name = if name = "" then "n_" ^ (Lv6Id.no_pack_string_of_long long) else name in
      if not (Hashtbl.mem node_name_tbl name) then
        (
          (* that name won't clash, but let's tabulate it *)
          Hashtbl.add node_name_tbl name 2;
          Hashtbl.add node_key_tbl nk name;
          name
        )
      else
        (* That name have already been given, there is a possible clash! *)
        let cpt = Hashtbl.find node_name_tbl name in
        let fresh_name = 
          Hashtbl.replace node_name_tbl name (cpt+1);
          name ^ "_" ^ (string_of_int cpt)
        in
        Hashtbl.add node_key_tbl nk fresh_name;
        fresh_name


(********************************************************************************)
(* Dealing with fresh local (to the node) variable idents *)
let local_var_tbl = Hashtbl.create 0

(********************************************************************************)
(* The idea is to prefix fresh var name by "_", except if at least
   one user ident begins by "_". In that case, we try to prefix them
   by "_0", and then "_1", and so on so forth. We take the first
   possible one.

   nb : this won't work if the user have defined all idents from "_1" to
   "_1073741823" (on 32-bits machine), but I bet that this compiler
   would die before anyway...

   nb : We stored in Lv6parserUtils.name_table the set of idents that begins by "_".
*)
let fresh_var_prefix = ref "_"

let char_is_int = function
  |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' -> true
  | _  -> false

(* Returns None if str.[1] is not an int, and Some i otherwhise,
   where i is the biggest possible int in str after the "_" (e.g.,
   "_23toto" returns "Some 23" *)
let (get_int : string -> int option) =
  fun str -> 
    let _ = assert (str<>"" && str.[0]='_') in
    let s = String.length str in
      if s>1 && char_is_int str.[1] then
        let j = ref 2 in
          while  !j<s && char_is_int str.[!j]  do incr j done;
          Some (int_of_string (String.sub str 1 (!j-1)))
      else
        None

let _ = (* A few unit tests *)
  assert (get_int "_" = None);
  assert (get_int "_toto" = None);
  assert (get_int "_1" = Some 1);
  assert (get_int "_1234" = Some 1234);
  assert (get_int "_1234toto" = Some 1234)
  
module IntSet = 
  Set.Make(struct
             type t = int
             let compare = compare
           end)

(* Make sure that update_fresh_var_prefix has been called *)
let fresh_var_prefix_updated = ref false

(* exported *)
let (update_fresh_var_prefix : unit -> unit) =
  fun _ ->
    let used_ints = (* the set of ints apprearing after a "_" in program idents *)
      Hashtbl.fold 
        (fun name _ acc -> 
           match get_int name with
               None -> IntSet.add (-1) acc
             | Some i -> IntSet.add i acc
        )
        Lv6parserUtils.name_table
        IntSet.empty
    in
    let used_ints = IntSet.elements used_ints in
    let rec find_int l =
      match l with 
        | [] -> -1
        | [i] -> if i > 0 then 0 else i+1
        | i::j::tail -> if j=i+1 then find_int (j::tail) else i+1
    in 
    let index = find_int used_ints in
      if index = (-1) then 
        ()
          (* no var begins by "_", so "_" is a good prefix.*)
      else (
        let new_prefix =  ("_" ^ (string_of_int index)) in
          fresh_var_prefix := new_prefix ; 
          Lv6Verbose.exe ~level:1 (
            fun () ->
            prerr_string ("I use " ^ new_prefix ^ " as prefix for fresh var names.\n");
            flush stderr
          )
      );
    fresh_var_prefix_updated := true


(********************************************************************************)
(* exported *)

let (local_var : string -> string) =
  fun prefix ->
    try 
      let cpt = Hashtbl.find local_var_tbl prefix in
      assert (!fresh_var_prefix_updated);
      Hashtbl.replace local_var_tbl prefix (cpt+1);
      !fresh_var_prefix ^ prefix ^"_"^ (string_of_int cpt)
    with
        Not_found -> 
          Hashtbl.add local_var_tbl prefix 2;
          !fresh_var_prefix ^ prefix ^ "_1"


(********************************************************************************)
(* exported *)
open Lic
let (var_info : string -> Lic.type_ -> Lic.id_clock -> Lic.var_info) =
  fun str type_eff clock_eff ->
    let id = Lv6Id.of_string (local_var str) in
    let var =
      { 
        var_name_eff   = id;
        var_nature_eff = AstCore.VarLocal;
        var_number_eff = -1; (* this field is used only for i/o. 
                                Should i rather put something sensible there ? *)
        var_type_eff   = type_eff;
        var_clock_eff  = clock_eff;
      }
    in
    var


(********************************************************************************)

let (array_type : Lic.type_ -> string -> string) =
  fun _t name -> 
    "A_" ^ name

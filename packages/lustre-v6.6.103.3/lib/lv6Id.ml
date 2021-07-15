(* Time-stamp: <modified the 29/08/2019 (at 14:41) by Erwan Jahier> *)

(* J'ai appele ca symbol (mais ca remplace le ident) :
c'est juste une couche qui garantit l'unicite en memoire
des strings ...
C'est tout petit, non ???

(* debut symbol.mli *)
type t
val to_string : t -> string
val of_string : string -> t

(* fin symbol.mli *)

------------------

(* debut symbol.ml *)
type t = string

module WeakStringTab = struct
  include Weak.Make(
     struct
        type t = string
        let equal = (=)
        let hash = Hashtbl.hash
     end
  )
end

let zetab = WeakStringTab.create 100 
let (to_string : t -> string) =
 fun x -> x

let (of_string : string -> t) =
 fun x -> (
     WeakStringTab.merge zetab x
 )
(* fin symbol.ml *)

 *)

(*cf  ~/dd/ocaml-3.10.0/typing/ident.ml *)

type t = string
type pack_name = t
type long = pack_name * t

let (pack_of_long : long -> pack_name) =
  fun l  -> fst l

let (of_long : long -> t) =
  fun l -> snd l

let (to_string : t -> string) =
  fun x -> x

let (of_string : string -> t) =
  fun x -> x

let (pack_name_of_string : string -> pack_name) =
  fun x -> x

let (pack_name_to_string : pack_name -> string) =
  fun x -> x

let (string_of_long: bool -> long -> string) =
  fun forprint (pn, id) ->
    if forprint then
      let sep =
        if Lv6MainArgs.global_opt.Lv6MainArgs.ec || Lv6MainArgs.global_opt.Lv6MainArgs.lv4 
        then "__" else "::"
      in
      match pn,id with
      | "",id -> id
      | "Lustre","true" -> "true"
      | "Lustre","false" -> "false"
      | _,_ -> 
        (* if Lv6MainArgs.global_opt.Lv6MainArgs.no_prefix then id else   *)
        Printf.sprintf "%s%s%s" pn sep id
    else if pn = "" then id else
      Printf.sprintf "%s::%s" pn id

                          
let (no_pack_string_of_long : long -> string) =
  fun (_pn, id) -> 
    id

let (string_of_long_bis : bool -> long -> string) =
  fun forprint x -> 
  if Lv6MainArgs.global_opt.Lv6MainArgs.kcg then
     no_pack_string_of_long x
  else    
    if Lv6MainArgs.global_opt.Lv6MainArgs.no_prefix then
      no_pack_string_of_long x
    else 
      string_of_long forprint x
           

let (make_long : pack_name -> t -> long) =
  fun pn id -> (pn,id)

let dft_pack_name = ref "DftPack" (* this dft value ougth to be reset before being used *)

let (set_dft_pack_name : pack_name -> unit) =
  fun pn -> 
(*     print_string ("Change the dft pack name to "^ pn^"\n");flush stdout; *)
    dft_pack_name := pn



(*  -> syntaxeTree.ml ? *)

type idref = 
    {
      id_pack : pack_name option;
      id_id  : t
    }

let (pack_of_idref : idref -> pack_name option) =
  fun ir -> ir.id_pack

let (name_of_idref : idref -> t) =
  fun ir -> ir.id_id


(* utilitaires idref *)
let idref_of_string s = (
  match (Str.split (Str.regexp "::") s) with
      [i] -> { id_pack = None; id_id = i}
    | [p;i]-> { id_pack = Some p; id_id = i}
    | _ -> raise (Failure ("idref_of_string: \""^s^"\" not a proper ident")) 
)

let out_of_pack s = ("", s)

let (long_of_string : string -> long) =
  fun s -> 
    match (Str.split (Str.regexp "::") s) with
        [i] -> !dft_pack_name, i
      | [p;i]-> p, i
      | _ -> raise (Failure ("idref_of_string: \""^s^"\" not a proper ident")) 

let string_of_idref forprint i = (
  match i.id_pack with
    Some p ->
    if not forprint then  (p^"::"^i.id_id) else 
      if Lv6MainArgs.global_opt.Lv6MainArgs.no_prefix then i.id_id else
        if Lv6MainArgs.global_opt.Lv6MainArgs.ec then p^"__"^i.id_id else
          if Lv6MainArgs.global_opt.Lv6MainArgs.lv4 then  (p^"__"^i.id_id) else
          (p^"::"^i.id_id)
    | None -> i.id_id
)
let raw_string_of_idref i = (
   let p = match i.id_pack with
   | Some p -> "Some \""^p^"\""
   | None -> "None"
   in
   Printf.sprintf "(%s, \"%s\")" p i.id_id
)


let (wrap_idref : idref -> string -> string -> idref) =
  fun { id_pack = p ; id_id = id } pref suff -> 
     { id_pack = p ; id_id = of_string (pref ^ (to_string id)^suff) }

let (of_idref : bool -> idref -> t) = 
  fun forprint idref -> 
    of_string (string_of_idref forprint idref)

let (to_idref : t -> idref) = 
  fun id -> idref_of_string (to_string id)

let (long_of_idref : idref -> long) =
  fun idr -> 
    match pack_of_idref idr with
        Some p -> (p, name_of_idref idr)
      | None   -> (!dft_pack_name, name_of_idref idr)

let (idref_of_long : long -> idref) =
  fun (pn,id) -> 
    { id_pack = Some pn ; id_id = id } 

let (idref_of_id : t -> idref) =
  fun id -> 
    { id_pack = None ; id_id = id } 

let (make_idref : pack_name -> t -> idref) =
  fun pn id -> 
    { id_pack = Some pn ; id_id = id } 



type clk = long * t

let (string_of_clk :clk -> string) = 
  fun (cc,cv) ->
      (string_of_long false cc) ^ "(" ^ (to_string cv) ^ ")" 

(*************************************************************************)

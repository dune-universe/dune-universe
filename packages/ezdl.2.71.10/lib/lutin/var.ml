(*-----------------------------------------------------------------------
* Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: var.mli
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)

type name = string
type mode = Input | Output | Local | Pre

(*
module NameMap  = struct
  include Map.Make(
	struct
		type t = name
		let compare = compare
	end
	)
end

module Name2Val = struct
	type t = Value.t NameMap.t
	let empty:t = NameMap.empty
	let get (n2v:t) (n:name) = NameMap.find n n2v
	let add (n2v:t) ((n,v):name * Value.t) = NameMap.add n v n2v
	let add_list (n2v:t) (l:(name * Value.t) list) = List.fold_left add n2v l
	let from_list (l:(name * Value.t) list) = List.fold_left add empty l
	let union (x1:t) (x2:t) = NameMap.fold (fun n v x -> add x (n,v)) x1 x2 
	let support (x:t) = NameMap.fold (fun n v acc -> n::acc) x []
	let partition f (x:t) = NameMap.fold
		(fun n v (yes, no) -> if f (n,v) then (add yes (n,v), no) else (yes, add no (n,v))) x (empty,empty)
	let content (x:t) = (
		List.fast_sort (fun (vn1, _) (vn2, _) -> compare vn1 vn2)
			(NameMap.fold (fun n v acc -> (n,v)::acc) x [])
	)
	let to_string (pfx:string) (x:t) = (
		if x = empty then pfx^"empty\n"
		else (
			let nv2s (n,v) = pfx ^ "\t" ^ (Prevar.format n) ^ " = " ^ (Value.to_string v) ^ "\n" in
			let str_l = List.map nv2s (content x) in
			String.concat "" str_l
		)
	)
	let print (x:t) (oc:out_channel) = output_string oc (to_string "" x)
	let mapi = NameMap.mapi
	let iter = NameMap.iter
end
*)

type vnt = name * Type.t

type subst = (name * Value.t)
type num_subst = (name * Value.num)


(* type env_in  = (name, Value.t) Hashtbl.t *)
type env     = Value.OfIdent.t
type env_in  = Value.OfIdent.t
type env_out = Value.OfIdent.t
type env_loc = Value.OfIdent.t

let (sort_list_string_pair:  (string * 'a) list -> (string * 'a) list) =
  fun var_list ->
    List.sort (fun (vn1, _t1) (vn2, _t2) -> compare vn1 vn2) var_list


let (subst_list_to_string : string -> subst list -> string) =
  fun prefix sl -> 
    let str_l = List.map
      (fun (vn, e) -> prefix ^ "\t" ^ (Prevar.format vn) ^ " = " ^ (Value.to_string e) ^ "\n")
      (sort_list_string_pair sl)
    in
      String.concat "" str_l

let (print_subst_list : subst list -> out_channel -> unit) =
  fun sl oc ->
    output_string oc (subst_list_to_string "" sl)




(* let (print_env_out : env_out -> out_channel -> unit) = print_subst_list *)
(* let (print_env_loc : env_loc -> out_channel -> unit) = print_subst_list *)
let (print_env_out : env_out -> out_channel -> unit) = Value.OfIdent.print
let (print_env_loc : env_out -> out_channel -> unit) = Value.OfIdent.print
let (print_env_in : env_in -> out_channel -> unit) = Value.OfIdent.print

(* OBSOLETE 
let (print_env_in : env_in -> out_channel -> unit) =
  fun tbl oc ->
    Hashtbl.iter
    (fun vn e ->
       output_string oc (Prevar.format vn) ;
       output_string oc " = ";
       Value.print oc e;
       output_string oc "\n\t"
    )
    tbl
*)

let (get_val_env_in : env_in -> name -> Value.t) = 
  fun env n -> 
    (* try Hashtbl.find env n  *)
    try Value.OfIdent.get env n 
    with Not_found -> (* I should rather raise a specific exception *)
      print_string (
          "Error: a (Lutin program) input is missing: " ^ n  ^ "\n" ^
            "E: Maybe this program is not bootable (able to start without input)\n"^
              "E: and used as an environment of Lurette or rdbg?\n");
      flush stdout;
      exit 2


(* OBSOLETE ?
let (inputs_to_list : env_in -> subst list) =
  fun inputs ->
    Hashtbl.fold
    (fun name value acc -> (name, value)::acc)
       inputs
       []
*)

(* let (get_val_env_out : env_out -> name -> Value.t) = fun l n -> List.assoc n l *)
let (get_val_env_out : env_out -> name -> Value.t) = Value.OfIdent.get

(* let (get_val_env_loc : env_loc -> name -> Value.t) = fun l n -> List.assoc n l *)
let (get_val_env_loc : env_loc -> name -> Value.t) = Value.OfIdent.get



(* let (init_env_out : unit -> env_out) = fun _  -> [] *)
(* let (init_env_loc : unit -> env_loc) = fun _  -> [] *)
let (init_env_out : unit -> env_out) = fun _  -> Value.OfIdent.empty
let (init_env_loc : unit -> env_loc) = fun _  -> Value.OfIdent.empty
let (init_env_in : unit -> env_in) = fun _  -> Value.OfIdent.empty

type 'a t = {
  index   : int;
  n       : name;
  t       : Type.t;
  mode    : mode;
  alias   : 'a option;
  min     : 'a option;
  max     : 'a option;
  default : 'a option;
  init    : 'a option
}

let (name : 'a t -> name) =
  fun var -> var.n

let (typ : 'a t -> Type.t) =
  fun var -> var.t

let (mode : 'a t -> mode) =
  fun var -> var.mode

let (min : 'a t -> 'a option) =
  fun var -> var.min

let (max : 'a t -> 'a option) =
  fun var -> var.max

let (alias : 'a t -> 'a option) =
  fun var -> var.alias

let (default : 'a t -> 'a option) =
  fun var -> var.default

let (init : 'a t -> 'a option) =
  fun var -> var.init

let (index : 'a t -> int) =
  fun var -> var.index


(* global counter that is incremented each time a variable is created *)
let var_cpt = ref 0


(* exported *)
let (make : string -> string -> Type.t -> mode -> 'a t) =
  fun lv_pref n t m ->
(*     let _ = print_string (n^"\n") ; flush stdout in *)
    let n' =
      if 
	m <> Local 
      then 
	n
      else
	(* Rename non-local vars to avoid clashes *)
	let l  = String.length lv_pref 
	and ln = String.length n 
	in
	  if ln < l || (String.sub n 0 l) <> lv_pref then
	    (lv_pref ^ n)
	  else
	    n
    in
    let idx = !var_cpt in
(*       print_string ("variable " ^ n' ^ "  -> " ^ (string_of_int idx) ^ "\n"); *)
(*       flush stdout; *)
      incr var_cpt;
      {n=n'; t=t; mode=m;alias=None; min=None; max=None; 
       default=None; init=None ; index = idx}


let (change_type : 'a t -> Type.t -> 'a t) =
  fun var t ->
    {
      n=var.n;
      t = t;
      mode=var.mode;
      alias=var.alias;
      min = var.min;
      max=var.max;
      default=var.default;
      init=var.init;
      index =var.index
    }


let (set_min : 'a t -> 'a  -> 'a t) =
  fun var min ->
    {
      n=var.n;
      t=var.t;
      mode=var.mode;
      alias=var.alias;
      min = Some min;
      max=var.max;
      default=var.default;
      init=var.init;
      index =var.index
    }

let (set_max : 'a t -> 'a -> 'a t) =
  fun var max ->
    {
      n=var.n;
      t=var.t;
      mode=var.mode;
      alias=var.alias;
      min=var.min;
      max = Some max;
      default=var.default;
      init=var.init;
      index =var.index
    }

let (set_alias : 'a t -> 'a -> 'a t) =
  fun var alias ->
    {
      n=var.n;
      t=var.t;
      mode=var.mode;
      alias = Some alias;
      min=var.min;
      max=var.max;
      default=var.default;
      init=var.init;
      index =var.index
    }

let (set_default : 'a t -> 'a -> 'a t) =
  fun var default ->
    {
      n=var.n;
      t=var.t;
      mode=var.mode;
      alias=var.alias;
      min=var.min;
      max=var.max;
      default = Some default;
      init=var.init;
      index =var.index
    }

let (set_init : 'a t -> 'a -> 'a t) =
  fun var init ->
    {
      n=var.n;
      t=var.t;
      mode=var.mode;
      alias=var.alias;
      min=var.min;
      max=var.max;
      default=var.default;
      init = Some init;
      index =var.index
    }


let (make_pre : 'a t -> 'a t) =
  fun var -> 
    let pre_str = Prevar.give_pre_var_name var.n in
    let pv = make ""  pre_str var.t Pre in
      match var.init with
           None  -> pv 
        | Some i ->  set_init pv i

let (mode_of_string : string -> mode) =
  fun str ->
    match str with
	"inputs" -> Input
      | "outputs" -> Output
      | "locals" -> Local
      | _ -> assert false

let (mode_to_string : mode -> string) =
  fun m ->
    match m with
	Input -> "input"
      | Output -> "output"
      | Local -> "local"
      | Pre -> "pre"


let (_print_format : 'a t -> unit) =
  fun var ->
    Format.print_string (
      var.n ^ ":" ^
      (Type.to_string var.t) ^ ":" ^
      (mode_to_string var.mode) ^ " min=" ^
      (if var.min = None then "None" else "Some ...") ^ " max=" ^
      (if var.max = None then  "None" else "Some ...") ^
(*       " \n\talias=" ^ *)
(*       (if var.alias = None then  "None" else "Some ...") ^ " \n\tdefault=" ^ *)
(*       (if var.default = None then  "None" else "Some ...") ^ " \n\tinit=" ^ *)
(*       (if var.init = None then  "None" else "Some ...")  *)
      "\n")

let (to_string : 'a t -> string) =
  fun var ->
    (
      var.n ^ ":" ^
      (Type.to_string var.t) ^ ":" ^
      (mode_to_string var.mode) ^ 
      " min=" ^ (if var.min = None then "None" else "Some ...") ^ 
      " max=" ^ (if var.max = None then  "None" else "Some ...") ^
      " alias=" ^ (if var.alias = None then  "None" else "Some ...") ^
      " default=" ^ (if var.default = None then  "None" else "Some ...") ^
      " init=" ^ (if var.init = None then  "None" else "Some ...") ^
      " index=" ^ (string_of_int var.index) ^
      "\n")

let (to_string_verbose : ('a -> string) -> 'a t -> string) =
  fun convert var ->
    (
      var.n ^ ":" ^
      (Type.to_string var.t) ^ ":" ^
      (mode_to_string var.mode) ^ 
      " min=" ^ (match var.min with None -> "None" |Some x  -> ( convert x)) ^ 
      " max=" ^ (match var.max with None -> "None" |Some x  -> ( convert x)) ^ 
      " alias=" ^ (match var.alias with None -> "None" |Some x  -> ( convert x)) ^ 
      " default=" ^ (match var.default with None -> "None" |Some x  -> ( convert x)) ^ 
      " init=" ^ (match var.init with None -> "None" |Some x  -> ( convert x)) ^ 
      " index=" ^ (string_of_int var.index)
    )
      (* "\n") *)




let (print : 'a t -> unit) =
  fun var ->
    print_string (to_string var)
    
let (is_newer : 'a t -> 'a t -> int) =
  fun var1 var2 -> 
    var1.index - var2.index

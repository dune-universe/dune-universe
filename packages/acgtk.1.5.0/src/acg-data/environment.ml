(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008-2018 INRIA                             *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

open UtilsLib
open Logic
open Abstract_syntax
open Interface



module type Environment_sig =
sig
  exception Signature_not_found of string
  exception Lexicon_not_found of string
  exception Entry_not_found of string

  module Signature1:Signature_sig with type term=Lambda.Lambda.term
  module Lexicon:Interface.Lexicon_sig with type Signature.t=Signature1.t and type Signature.term=Signature1.term and type Signature.stype=Signature1.stype


  type t
  type entry = 
    | Signature of Signature1.t
    | Lexicon of Lexicon.t
  val empty : t
  val insert : ?overwrite:bool -> entry -> to_be_dumped:bool -> t -> t
  val get_signature : string -> t -> Signature1.t
  val get_lexicon : string -> t -> Lexicon.t
  val get : string -> t -> entry 
  val append : ?overwrite:bool -> t -> t -> t
  val iter : (entry -> unit) -> t -> unit
  val fold : (entry -> 'a -> 'a) -> 'a -> t -> 'a
  val sig_number : t -> int
  val lex_number : t -> int
  val choose_signature : t -> Signature1.t option

  val compatible_version : t -> bool
  val read : string -> string list -> t option
  val write : string -> t -> unit



  val select : string -> t -> t

  val unselect : t -> t

  val focus : t -> entry option
end

(*  
module Make (Lex:Interface.Lexicon_sig) =
*)
module Lex=Acg_lexicon.Data_Lexicon
  
module Environment =
struct

  module Lexicon=Lex
  module Sg=Lex.Signature
  module Signature1=Sg

  exception Signature_not_found of string
  exception Lexicon_not_found of string
  exception Entry_not_found of string
  module Env = Utils.StringMap

  type to_be_dumped = bool
    
  type entry = 
    | Signature of Sg.t
    | Lexicon of Lex.t

  module Dep = 
    DependencyManager.Make (
	struct
	  type t = entry
	  let to_string = function
	    | Signature s -> fst (Sg.name s)
	    | Lexicon l -> fst (Lex.name l)
	  let compare e1 e2 =
	    String.compare (to_string e1) (to_string e2)
	end)

  type t = {map:(entry*to_be_dumped) Env.t;
	    sig_number:int;
	    lex_number:int;
	    focus:entry option;
	    version:string;
	    dependencies:Dep.t}

  let empty = {map=Env.empty;
	       sig_number=0;
	       lex_number=0;
	       focus=None;
	       version=Version.version;
	       dependencies=Dep.empty}

  let check_version e =
    let v=e.version in
    if (v <> Version.version) then
      raise (Error.Error (Error.Version_error (Error.Outdated_version (v,Version.version))))
    else
      ()

  let append ?(overwrite=false) e1 e2 =
    let () = check_version e1 in
    let () = check_version e2 in
    let erased_sig = ref 0 in
    let erased_lex = ref 0 in
    let new_map =
      Env.merge
	(fun k v1 v2 ->
	  match v1,v2,overwrite with
	  | None,None,_ -> None
	  | None,Some v,_ -> Some v
	  | Some v,None,_ -> Some v
	  | Some (Lexicon _,_),Some v2,true -> 
	    let () = erased_lex:=!erased_lex+1 in
	    Some v2
	  | Some (Signature _,_),Some v2,true -> 
	    let () = erased_sig:=!erased_sig+1 in
	    Some v2
	  | Some (v1,_),Some (v2,_),false ->
	    match v2 with
	    | Signature sg ->
	      let _,pos=Sg.name sg in
	      raise (Error.Error (Error.Env_error (Error.Duplicated_entry k,pos)))
	    | Lexicon lex ->
	      let _,pos=Lex.name lex in
	      raise (Error.Error (Error.Env_error (Error.Duplicated_entry k,pos))))
	e1.map
	e2.map in
    {map=new_map;
     sig_number=e1.sig_number + e2.sig_number - !erased_sig;
     lex_number=e1.lex_number + e2.lex_number - !erased_lex;
     focus = (match e2.focus with
     | Some e -> Some e
     | None -> e1.focus);
    version=Version.version;
    dependencies=Dep.merge e1.dependencies e2.dependencies}
    

  let update_dependencies lex m =
    match Lex.get_dependencies lex with
    | Lex.Signatures (s1,s2) -> 
       Dep.add_dependency
	 (Lexicon lex)
	 (Signature s1) 
	 (Dep.add_dependency (Lexicon lex) (Signature s2) m)
    | Lex.Lexicons (l1,l2) -> 
       Dep.add_dependency
	 (Lexicon lex)
	 (Lexicon l1) 
	 (Dep.add_dependency (Lexicon lex) (Lexicon l2) m)

  let insert ?(overwrite=false) d ~to_be_dumped:dump e =
    match d with
    | Signature s ->
       let name,(p1,p2) = Sg.name s in
       if (not (Env.mem name e.map))||overwrite
       then
	 {e with
	   map=Env.add name (d,dump) e.map ;
	   sig_number=e.sig_number+1}
       else
	 raise (Error.Error (Error.Env_error (Error.Duplicated_signature name,(p1,p2))))
    | Lexicon l ->
       let name,(p1,p2) = Lex.name l in
       if not (Env.mem name e.map)||overwrite
       then
	 {e with
	   map=Env.add name (d,dump) e.map ;
	   lex_number=e.lex_number+1;
	   dependencies = update_dependencies l e.dependencies}
       else
	 raise (Error.Error (Error.Env_error (Error.Duplicated_lexicon name,(p1,p2))))
	       
  let iter f {map=e} =  Env.iter (fun _ (d,_) -> f d) e
				 
  let fold f a {map=e} = Env.fold (fun _ (d,_) acc -> f d acc) e a

  let sig_number {sig_number=n} = n
  let lex_number {lex_number=n} = n

  let get_signature s {map=e} =
    match Env.find s e with
    | Signature sg,_ -> sg
    | Lexicon _,_ -> raise (Signature_not_found s)
    | exception Not_found -> raise (Signature_not_found s)
                                   
  let get_lexicon s {map=e} =
    match Env.find s e with
    | Signature _,_ -> raise (Lexicon_not_found s)
    | Lexicon lex,_ -> lex
    | exception Not_found -> raise (Lexicon_not_found s)


  let get s {map=e} =
    try
      let data,_ = Env.find s e in
      data
    with
      | Not_found -> raise (Entry_not_found s)


  let compatible_version {version} = version = Version.version

  let stamp v = Printf.sprintf "acg object file version %s" v

  let read filename dirs =
    try
      let file =(Utils.find_file filename dirs) in
      let in_ch = open_in file in
      let first_line = input_line in_ch in
      if first_line = (stamp Version.version) then
	let () = Printf.printf "Loading object file \"%s\"...\n%!" file in
	let new_env = input_value in_ch in
	let () = Printf.printf "Done.\n%!" in
	let () = close_in in_ch in
	Some new_env
      else
	let object_version = Scanf.sscanf first_line "acg object file version %s" (fun s -> s) in
	let err = Error.Version_error (Error.Outdated_version (object_version,Version.version)) in
	let () = Printf.fprintf stderr "Error: %s\n%!" (Error.error_msg err filename) in
	None
    with
    | Scanf.Scan_failure _ ->
       let err = Error.System_error (Printf.sprintf "\"%s\" is not recognized as an acg object file" filename )in
       let () = Printf.fprintf stderr "Error: %s\n%!" (Error.error_msg err filename) in
       None
    | Utils.No_file(f,msg) -> 
       let err = Error.System_error (Printf.sprintf "No such file \"%s\" in %s" filename msg) in
       let () = Printf.fprintf stderr "Error: %s\n%!" (Error.error_msg err filename) in
       None

let write filename env =
  let () = Logs.debug (fun m -> m "The environment currently has %d signature(s) and %d lexicon(s)."  (sig_number env) (lex_number env)) in
  let new_env =
    Env.fold
      (fun k (d,dump) acc ->
	if dump then
	  (* all data are inserted with the [false] value as [to_be_dumped] *)
	  insert d ~to_be_dumped:false acc
	else
	  acc)
      env.map
      empty in
  let out_ch=open_out filename in
  let () = Printf.fprintf out_ch "%s\n" (stamp Version.version) in
  let () = output_value out_ch {new_env with dependencies=env.dependencies} in
  close_out out_ch


  let select name e =
    {e with focus=Some (get name e)}

  let unselect e = {e with focus=None}

  let focus {focus=f} = f



  exception Sig of Sg.t    

  let choose_signature {map=e} =
    try
      let () = Env.fold
	(fun _ c a -> 
	   match c with
	     | Signature s,_ -> raise (Sig s)
	     | Lexicon _,_ -> a )
	e
	() in
	None
    with
      | Sig s -> Some s
    


end

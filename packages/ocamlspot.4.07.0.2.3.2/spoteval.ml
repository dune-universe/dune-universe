(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

open Format
open Utils
open Ext

(* To avoid name collisions *)
module OCaml = struct
  module Format = Format
end

open Spot

module PIdent = struct
  type t = {
    path : string; (* "" means predefined *)
    ident : Ident.t option; (* None means the top module *)
  }

  let format ppf id =
    fprintf ppf "%s%s" 
      (match id.path with
      | "" -> ""
      | p -> 
          (let len = String.length p in
           if len > 20 then
             "..." ^ String.sub p (len - 20) 20 
           else p) ^ ":")
      (match id.ident with
      | Some id -> Ident.name id
      | None -> "TOP")
end

module Value : sig

  type t = 
    | Ident of PIdent.t
    | Structure of PIdent.t * structure * structure option (* sig part *)
    | Closure of PIdent.t * env * Ident.t * Types.module_type option * Abstraction.module_expr
    | Parameter of PIdent.t
    | Error of exn 

  and structure = structure_item list

  and structure_item = Ident.t * (Kind.t * z)

  and z = t Lazy.t

  and env = {
    path : string;
    cwd : string;
    load_paths : string list;
    binding : binding;
  } 

  and binding
      
  module Binding : sig
    type t = binding
    val domain : t -> Ident.t list
    val find : t -> Ident.t -> (Kind.t * z) option
(*    val find_first_name : t -> Kind.t -> string -> (Kind.t * z) option *)
    val override : t -> structure_item -> t
    val overrides : t -> structure -> t
    val set : t -> structure -> unit
    val predef : t
    val empty : t
    val invalid : t
  end

  module Enforcer(A : sig 
  end) : sig
    val t : t -> unit
    val env : env -> unit
    val binding : binding -> unit
    val structure : structure -> unit
    val structure_item : structure_item -> unit
    val z : z -> unit
  end

  module Format : sig
    (* include module type of Format 3.12 *)
    val t : formatter -> t -> unit
    val env : formatter -> env -> unit
    val binding : formatter -> binding -> unit
    val structure : formatter -> structure -> unit
    val z : formatter -> z -> unit
  end

end = struct

  type t = 
    | Ident     of PIdent.t
    | Structure of PIdent.t * structure * structure option (* sig part *)
    | Closure   of PIdent.t * env * Ident.t * Types.module_type option * Abstraction.module_expr
    | Parameter of PIdent.t
    | Error     of exn 

  and structure = structure_item list

  and structure_item = Ident.t * (Kind.t * z)

  and z = t Lazy.t 

  and env = {
    path : string;
    cwd : string;
    load_paths : string list;
    binding : binding;
  }

  (* dirty hack for flat recursion *)
  and binding = structure option ref
      
  module Binding = struct
    type t = binding
    let error () = failwith "Binding: premature"
    let with_check f t = match !t with
      | None -> error ()
      | Some str -> f str
    let domain = with_check (List.map fst) 
    let find t id = try Some (with_check (List.assoc id) t) with Not_found -> None
(*
    let find_first_name t k name = 
      let assoc name xs = 
        match 
          List.find_map_opt (fun (id, (k', _ as v)) ->
            if Ident0.name id = name && k = k' then Some v
            else None) xs
        with
        | None -> raise Not_found
        | Some v -> v
      in
      try Some (with_check (assoc name) t) with Not_found -> None
*)
    let override t v = ref (Some (with_check (fun t -> v :: t) t))
    let overrides t vs = ref (Some (with_check (fun t -> vs @ t) t))
    let invalid = ref None 
    let empty = ref (Some [])
    let predef = 
      let items = ref [] in
      let add_predefined kind id = 
        items := 
          (id, 
           (kind, eager (Ident { PIdent.path = "";
                                 ident = Some id })))
          :: !items
      in
      let (), () = Predef.build_initial_env 
        (fun id decl _ -> 
          add_predefined Kind.Type id;
          match decl.Types.type_kind with
          | Types.Type_abstract -> ()
          | Types.Type_record (l, _) -> List.iter (fun {Types.ld_id=id} -> add_predefined Kind.Type id) l
          | Types.Type_variant l     -> List.iter (fun {Types.cd_id=id} -> add_predefined Kind.Type id) l
          | Types.Type_open -> () (* CR jfuruse: not sure *)
        )
        (fun id _ _ -> add_predefined Kind.Exception id) 
        ();
      in
      List.iter (fun (_, id) -> add_predefined Kind.Value id) Predef.builtin_values;
      ref (Some !items)
    let set b str = b := Some str
  end

  module Enforcer(A : sig 
  end) = struct
    (* prevent looping forever *)
    let cache = ref []
    let rec t = function
      | Structure (_, str, str_opt) -> 
          structure str;
          Option.iter str_opt ~f:structure
      | Closure (_, e, _, _, _) -> env e
      | Ident _ | Error _ | Parameter _ -> ()
    and env e = binding e.binding
    and binding b =
      match !b with
      | None -> failwith "Enforcer.binding: binding is premature"
      | Some str -> structure str
    and structure str = List.iter structure_item str
    and structure_item (_, (_, zt)) = z zt
    and z zt =
      if List.memq zt !cache then ()
      else begin
        cache := zt :: !cache;
        t !!zt
      end
  end

  module Format = struct

    include Format

    let rec t ppf = function
      | Ident id -> fprintf ppf "Ident(%a)" PIdent.format id
      | Parameter id -> fprintf ppf "Parameter(%a)" PIdent.format id
      | Structure (pid, str, None) -> 
            fprintf ppf "@[<v2>Module(%a)@ %a None@]"
              PIdent.format pid
            structure str
      | Structure (pid, str, Some str') -> 
            fprintf ppf "@[<v2>Module(%a)@ %a (Some %a)@]"
              PIdent.format pid
            structure str
            structure str'
      | Closure (pid, _, id, _mty, module_expr) ->
            fprintf ppf "(@[<2>(%a =)fun %s ->@ @[%a@]@])" 
              PIdent.format pid
              (Ident.name id)
              Abstraction.format_module_expr module_expr
      | Error (Failure s) -> fprintf ppf "ERROR(%s)" s
      | Error exn -> fprintf ppf "ERROR(%s)" (Printexc.to_string exn)
            
    and env ppf env = 
      fprintf ppf "{ @[path=%s;@,@[<2>load_paths=@,[@[%a@]];@]@,@[<2>structure=@,@[%a@]@]@] }"
        env.path
        (Format.list "; " (fun ppf s -> fprintf ppf "%S" s)) env.load_paths
        binding env.binding
        
    and binding ppf b = 
      match !b with
      | None -> fprintf ppf "PREM"
      | Some str -> structure ppf str

    and structure ppf =
      fprintf ppf "{ @[<v>%a@] }"
        (Format.list ";@ " (fun ppf (id, (kind, t)) ->
            fprintf ppf "@[<2>%s %s =@ %a@]" 
              (String.capitalize_ascii (Kind.to_string kind))
            (Ident.name id) z t))
        
    and z ppf = Format.lazy_ t ppf
  end
end

module Binding = Value.Binding

module Env = struct
  open Value
  type t = env = {
    path : string;
    cwd : string;
    load_paths : string list;
    binding : Binding.t;
  } 
  let format = Value.Format.env
  let domain t = Binding.domain t.binding
  let find t id = Binding.find t.binding id
  (* let find_first_name t name = Binding.find_first_name t.binding name *)
  let override t v = { t with binding = Binding.override t.binding v }
  let overrides t vs = { t with binding = Binding.overrides t.binding vs }
  let predef = {
    path = "";
    cwd = "";
    load_paths = [];
    binding = Binding.predef;
  }
end

module Eval = struct

  open Abstraction
  open Value
  module Format = OCaml.Format

  let str_of_global_ident = ref (fun ~cwd:_ ~load_paths:_ _ -> assert false : cwd: string -> load_paths: string list -> Ident.t -> string * Value.structure)
  let packed = ref (fun _ _ -> assert false : Env.t -> string -> Value.t)

  let rec find_path env (kind, p) : Value.z = 
    match kind, p with
    | _, Path.Papply (p1, p2) ->
	let v1 = find_path env (Kind.Module, p1) in
	let v2 = find_path env (Kind.Module, p2) in
	apply v1 v2
    | k, Path.Pident id ->
        (* predef check first (testing) *)
        begin match Env.find Env.predef id with
        | Some (_, v) -> v
        | None when Ident.global id -> 
            (* This must be a module *)
            assert (k = Kind.Module);
            lazy begin try
              let path, str = !str_of_global_ident ~cwd:env.cwd ~load_paths:env.load_paths id in
              let str = Structure ( { PIdent.path = path; ident = None }, 
                                    str,
                                    None (* CR jfuruse: todo (read .mli) *) )
              in
              Debug.format "@[<2>LOAD SUCCESS %s =@ %a@]@."
                (Ident.name id)
                Value.Format.t str;
              str
              with
              | e -> 
                  eprintf "LOAD FAILURE %s: %s@." (Ident.name id) (Printexc.to_string e);
                  Error e
            end
        | None ->
            lazy begin
              Debug.format "find_path %s:%s in { %s }@." 
                (Kind.name kind)
                (Path.name p)
                (String.concat "; " 
                   (List.map Ident.name (Env.domain env)));
              match Env.find env id with
              | Some (_, lazy v) -> v
              | None -> 
    (*
                  (* it may be a predefed thing *)
                  try !!(snd (Env.find Env.predef id)) with Not_found ->
    *)
                  (* If it is a non-value object, it might be included with stamp = -1 *)
                  let error id = 
                    Value.Error (Failure (Printf.sprintf "%s:%s not found in { %s }" 
                                      (Kind.name kind)
                                      (Ident.name id)
                                      (String.concat "; " 
                                         (List.map Ident.name (Env.domain env)))))
                  in
                  match kind with
                  | Kind.Value | Kind.Module | Kind.Class | Kind.Exception -> error id
                  | Constructor | Field -> assert false
                  | _ ->
                      (* CR jfuruse: is it really required? *)
                      (* OCaml 4.07.0 cannot allow this *)
(*
                        let gid = { id with stamp = -1 } in
                      match Env.find env gid with
                      | Some (_, lazy v) -> v
                      | None -> error id
*)
                      error id
            end
        end
    | (Kind.Constructor | Field ), Path.Pdot (p, name, pos) ->
        assert (pos = -1);
        lazy begin
          match !!(find_path env (Kind.Type, p)) with
          | Structure (pid, str, _ (* CR jfuruse *)) -> 
              begin Debug.format "Type %s found (%a)@." (Path.name p) PIdent.format pid;
              try
                !!(find_name str (kind, name))
              with
              | Not_found -> Error (Failure (Printf.sprintf "Not_found %s:any" name))
              end
          | _ -> assert false
        end

    | _, Path.Pdot (p, name, pos) ->
        lazy begin
          match !!(find_path env (Kind.Module, p)) with
          | Ident _ -> (try assert false with e -> Error e)
          | Parameter pid -> Parameter pid
          | Closure _ -> (try assert false with e -> Error e)
          | Error exn -> Error exn
          | Structure (pid, str, _ (* CR jfuruse *)) -> 
              Debug.format "Module %s found (%a)@." (Path.name p) PIdent.format pid;
              try
                !!(find_ident str (kind, name, pos))
              with
              | Not_found -> Error (Failure (Printf.sprintf "Not_found %s:%d" name pos))
        end

  and find_ident (str : Value.structure) (kind, name, pos) : Value.z =
    let name_filter = fun (id, (k,_)) -> 
(*
      Debug.format "DEBUG: %s %s ? %s %s@."
        (Kind.to_string kind)
        name 
        (Kind.to_string k)
        (Ident0.name id);
*)
      k = kind && Ident0.name id = name in
    (* CR jfuruse: double check by pos! *)
    (* CR jfuruse: yes it can cause a bug if two x with the same kind exist in a stucture *)
    lazy begin
      try
        !!(snd (snd (List.find (fun id_value ->
          (* pos_filter id_value && *) name_filter id_value) str)))
      with
      | Not_found ->
          Debug.format "Error: Not found %s %s in { @[%a@] }@."
            (String.capitalize_ascii (Kind.to_string kind))
            name
            Value.Format.structure str;
          Error (Failure (Printf.sprintf "Not found: %s__%d" name pos))
    end

  (* Used for finding constructor/field *)      
  and find_name (str : Value.structure) (kind, name) : Value.z =
    let name_filter = fun (id, (k,_)) -> k = kind && Ident0.name id = name in
    lazy begin
      try
        !!(snd (snd (List.find (fun id_value ->
          (* pos_filter id_value && *) name_filter id_value) str)))
      with
      | Not_found ->
          Debug.format "Error: Not found %s %s in { @[%a@] }@."
            (String.capitalize_ascii (Kind.to_string kind))
            name
            Value.Format.structure str;
          Error (Failure (Printf.sprintf "Not found: %s__any" name))
    end

  and module_expr env idopt : module_expr -> Value.z = function
    | AMod_functor_parameter -> 
        eager (Parameter { PIdent.path= env.path; ident = idopt })
    | AMod_abstract -> eager (Value.Error (Failure "abstract"))
    | AMod_ident p -> find_path env (Kind.Module, p)
    | AMod_packed s -> lazy (!packed env s)
    | AMod_structure str -> 
        lazy begin
          let str = structure env str in
          Structure ({ PIdent.path= env.path; ident = idopt }, str, None)
        end
    | AMod_functor (id, mtyo, mexp) -> 
        Debug.format "creating a closure of a functor (fun %s -> ...) under %s@."
          (Ident.name id)
          (String.concat "; " (List.map Ident.name (Env.domain env)));
        eager (Closure ({ PIdent.path = env.path; ident = idopt }, 
                        env, id, mtyo, mexp))
    | AMod_constraint (mexp, _mty) -> 
        (* [mty] may not be a simple signature but an ident which is
           hard to get its definition at this point. 
           Therefore we do not constrain our result here. 
           Only the sensitive case is when a constrained module is
           included, but we can handle this case using included
           value list. 

           Types never override themselves so the including module's
           type wins against the type of the same name in the included one:

           type t (* WINS! *)
           include (struct
           type t (* ocamlspot does not hide it *)
           end : sig
        (* type system hide t *)
           end)
        *)
        module_expr env idopt (*?*) mexp
    | AMod_apply (mexp1, mexp2) ->
        let v1 = module_expr env None mexp1 in
        let v2 = module_expr env None mexp2 in
	apply v1 v2
    | AMod_unpack mty -> module_expr env None mty

  (* expand internal Include and get alist by Ident.t *)
  (* the list order is REVERSED and is last-defined-first, 
     but it is REQUIRED for environment query *)
  and structure env0 sitems : Value.structure =

    List.fold_left (fun str sitem ->
      match sitem with
      | AStr_value       id 
      | AStr_constructor id 
      | AStr_field       id 
      | AStr_exception   id
      | AStr_class       id
      | AStr_class_type  id ->
          (* CR jfuruse: not sure *)
          let pident = { PIdent.path = env0.Env.path; ident = Some id } in
          let v = Ident pident in
          (* CR jfuruse: use ident_of_structure_item *)
          let kind = match sitem with
            | AStr_value      _ -> Kind.Value
            | AStr_type       _ -> assert false
            | AStr_constructor _ -> Kind.Constructor
            | AStr_field      _ -> Kind.Field
            | AStr_exception  _ -> Kind.Exception
            | AStr_modtype    _ -> Kind.Module_type
            | AStr_class      _ -> Kind.Class
            | AStr_class_type _ -> Kind.Class_type
            | AStr_included (_, _, kind, _) -> kind
            | AStr_module _ -> assert false
          in
          (id, (kind, eager v)) :: str

      (* CR: very ad-hoc rule for functor parameter *)      
      | AStr_module (id, Some (AMod_ident (Path.Pdot (Path.Pident _id, 
                                                      "parameter", 
                                                      -2)))) ->
          (* id = id_ *)
          let pident = { PIdent.path = env0.Env.path; ident = Some id } in
          (id, (Kind.Module, eager (Parameter pident))) :: str
          
      | AStr_module (id, Some (mexp)) ->
          let v = lazy begin
            try
              (* create it lazily for recursiveness of flat *)
              let env = Env.overrides env0 str in
              !!(module_expr env (Some id) mexp)
            with
            | exn -> Error exn
          end
          in
          (id, (Kind.Module, v)) :: str

      | AStr_type (id, td) ->
          let v = lazy begin
            let pident = { PIdent.path = env0.Env.path; ident = Some id } in
            try
              Structure (pident, structure env0 td, None)
            with
            | exn -> Error exn
          end
          in
          (id, (Kind.Type, v)) :: str

      | AStr_modtype (id, Some mexp) ->
          (* CR jfuruse: dup code *)
          let v = lazy begin
            try
              (* create it lazily for recursiveness of flat *)
              let env = Env.overrides env0 str in
              !!(module_expr env (Some id) mexp) (* ??? *)
            with
            | exn -> Error exn
          end
          in
          (id, (Kind.Module_type, v)) :: str

      | AStr_module (_, None) 
      | AStr_modtype (_, None) -> assert false (* not yet *)

      | AStr_included (id', mexp, k, id) ->
          (* shared include should share the result of mexp *)
          (* be careful: everything must be done lazily *)
          let v = lazy begin
            (* createate it lazily for recursiveness of flat *)
            let env = Env.overrides env0 str in
            !!(module_expr env None(*?*) mexp)
          end in
          let kid_ztbl = 
            lazy begin match !!v with
            | Structure (_, str, _ (* CR jfuruse *) ) -> 
                List.map (fun (id, (k, v)) -> (k, id), v) str
            | Parameter pid -> [ (k, id), eager (Parameter pid) ]
            | Ident _ -> assert false
            | Closure _ -> assert false
            | Error _ -> [] (* error *)
            end
          in
          let v = lazy begin
            let kid_tbl = !!kid_ztbl in
              (* include does not preserve id stamp, so we must ignore them *)
            match 
              List.find_map_opt (fun ((k', id'), v) -> 
                if k = k' && Ident0.name id = Ident0.name id' then Some v else None) kid_tbl
            with
            | Some vz -> !!vz
            | None -> 
                Format.eprintf "INCLUDE ERROR: %s %a in @[%a@]@."
                  (Kind.name k)
                  Ident.format id
                  (Format.list ";@ " (fun ppf ((k,id), _) -> 
                    Format.fprintf ppf "%s %a" (Kind.name k) Ident.format id))
                  kid_tbl;
                Error (Failure "not found in include")
          end in
          (id', (k, v)) :: str
          ) [] sitems

  and apply v1 v2 =
    lazy begin match !!v1 with
    | Ident _ -> assert false
    | Parameter pid -> Parameter pid (* CR jfuruse: ??? *)
    | Structure _ -> assert false
    | Error exn -> Error exn
    | Closure (_, env, id, _mty, mexp) -> 
        let v = 
          !!(module_expr (Env.override env (id, (Kind.Module, v2)))
               None(*?*) mexp)
        in
        Debug.format "closure app: %a@." Value.Format.t v;
        v
    end
end

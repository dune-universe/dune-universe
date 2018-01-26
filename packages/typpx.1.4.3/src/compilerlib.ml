(* Extension of compilier_libs modules *)

open Ppxx.Utils
open List

module Ident = struct
  include Ident

  open Format
    
  let format ppf id = pp_print_string ppf id.name
  let format_verbose ppf id = fprintf ppf "%s/%d" id.name id.stamp
end

module Path = struct
  include Path

  open Format

  let rec format ppf = function
    | Pident id -> Ident.format ppf id
    | Pdot (p, name, _n) -> fprintf ppf "%a.%s" format p name
    | Papply (p1, p2) -> fprintf ppf "%a(%a)" format p1 format p2

  let rec format_verbose ppf = function
    | Pident id -> Ident.format_verbose ppf id
    | Pdot (p, name, n) -> fprintf ppf "%a.%s__%d" format_verbose p name n
    | Papply (p1, p2) -> fprintf ppf "%a(%a)" format_verbose p1 format_verbose p2

  let to_string l = ksprintf (fun x -> x) "%a" format l
end
  
module Ctype = struct
(*
  ctype.ml says:

   Type manipulation after type inference
   ======================================
   If one wants to manipulate a type after type inference (for
   instance, during code generation or in the debugger), one must
   first make sure that the type levels are correct, using the
   function [correct_levels]. Then, this type can be correctely
   manipulated by [apply], [expand_head] and [moregeneral].

  Therefore we simply wrap these functions by correct_levels.
  They may be slower but I do not want to be bothered by strange
  type level bugs.
*)
  include Ctype
    
  let expand_head env ty = expand_head env & correct_levels ty
  let apply env tys ty tys2 =
    apply env (map correct_levels tys) (correct_levels ty) (map correct_levels tys2)
  let moregeneral env b ty1 ty2 = moregeneral env b (correct_levels ty1) (correct_levels ty2)
end

module XTypes = struct
  open Types
  open Btype
  open Ctype
  let repr_desc ty = (repr ty).desc

  let expand_repr_desc env ty = (repr & expand_head env ty).desc

  let with_snapshot f =
    let snapshot = snapshot () in
    let res = protect f in
    backtrack snapshot;
    unprotect res

  let is_constr env ty = match expand_repr_desc env ty with
    | Tconstr (p, tys, _) -> Some (p, tys)
    | _ -> None
  
  let is_option_type env ty = match is_constr env ty with
    | Some (po, [ty]) when po = Predef.path_option -> Some ty
    | _ -> None

  let gen_vars ty =
    flip filter (Ctype.free_variables ty) & fun ty ->
      ty.level = Btype.generic_level

  (* Create a type which can be unified only with itself *)
  let create_uniq_type =
    let cntr = ref 0 in
    fun () -> 
      incr cntr;
      (* Ident.create is not good. Unifying this data type ident with
         a tvar may cause "escaping the scope" errors
      *)
      Ctype.newty ( Tconstr ( Pident (Ident.create_persistent & "*uniq*" ^ string_of_int !cntr), [], ref Mnil ) )

  let close_gen_vars ty = flip iter (gen_vars ty) & fun gv ->
    match repr_desc gv with
    | Tvar _ ->
        Ctype.unify Env.empty gv (create_uniq_type ());
       (* eprintf "Closing %a@." Printtyp.type_expr gv *)
    | Tunivar _ -> ()
    | _ -> assert false
end

module Types = struct
  include Types
  include XTypes
end


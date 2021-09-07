(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

type ty = Ty
type tm = Tm

module Sort = struct
  type 'sort t =
    | Term : tm t
    | Type : ty t

  let equal
    : type s1 s2 any.
      s1 t -> s2 t
      -> ((s1, s2) Tyabt.eq, (s1, s2) Tyabt.eq -> any) Either.t =
    fun s1 s2 -> match s1, s2 with
      | Term, Term -> Left Refl
      | Term, Type -> Right (function _ -> .)
      | Type, Type -> Left Refl
      | Type, Term -> Right (function _ -> .)
end

module Operator = struct
  type ('arity, 'sort) t =
    | Unit : (ty Tyabt.ar, ty) t
    | Arrow : (ty Tyabt.va -> ty Tyabt.va -> ty Tyabt.ar, ty) t
    | Ax : (tm Tyabt.ar, tm) t
    | App : (tm Tyabt.va -> tm Tyabt.va -> tm Tyabt.ar, tm) t
    | Lam : (ty Tyabt.va -> (tm -> tm Tyabt.va) -> tm Tyabt.ar, tm) t

  let equal
    : type a1 a2 s. (a1, s) t -> (a2, s) t -> (a1, a2) Tyabt.eq option =
    fun op1 op2 -> match op1, op2 with
      | App, App -> Some Refl
      | Arrow, Arrow -> Some Refl
      | Ax, Ax -> Some Refl
      | Lam, Lam -> Some Refl
      | Unit, Unit -> Some Refl
      | _, _ -> None

  let pp_print : type a s. Format.formatter -> (a, s) t -> unit =
    fun fmt op ->
    Format.pp_print_string fmt
      (match op with
       | Unit -> "unit"
       | Arrow -> "arrow"
       | Ax -> "ax"
       | App -> "app"
       | Lam -> "lam")
end

module Syn = Tyabt.Make(Sort)(Operator)

open Operator

let ( let+ ) res f = Result.map f res

let ( and+ ) res1 res2 = match res1, res2 with
  | Ok x, Ok y -> Ok (x, y)
  | Ok _, Error e -> Error e
  | Error e, Ok _ -> Error e
  | Error e, Error _ -> Error e

let ( and* ) = ( and+ )

let ( let* ) = Result.bind

let to_string term =
  let buf = Buffer.create 32 in
  let ppf = Format.formatter_of_buffer buf in
  Syn.pp_print ppf term;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let rec infer
    (gamma : (tm Syn.Variable.t * ty Tyabt.va Syn.t) list)
    (term : tm Tyabt.va Syn.t)
  : (ty Tyabt.va Syn.t, string) result =
  match Syn.out term with
  | Op(Ax, Syn.[]) -> Ok (Syn.op Unit Syn.[])
  | Op(Lam, Syn.[in_ty; abstr]) ->
    let Abs(var, body) = Syn.out abstr in
    let+ out_ty = infer ((var, in_ty) :: gamma) body in
    Syn.op Arrow Syn.[in_ty; out_ty]
  | Op(App, Syn.[f; arg]) ->
    let* f_ty = infer gamma f
    and* arg_ty = infer gamma arg in
    begin match Syn.out f_ty with
      | Op(Arrow, Syn.[in_ty; out_ty]) ->
        if Syn.aequiv in_ty arg_ty then
          Ok out_ty
        else
          Error ("Expected argument of type " ^ to_string in_ty ^
                 ", got argument of type " ^ to_string arg_ty ^ "!")
      | _ ->
        Error ("Expected function, got term of type " ^ to_string f_ty ^ "!")
    end
  | Var v -> Ok (List.assoc v gamma)

type progress = Step of tm Tyabt.va Syn.t | Val | Err

let rec cbv (term : tm Tyabt.va Syn.t) =
  match Syn.out term with
  | Op(Ax, Syn.[]) -> Val
  | Op(Lam, Syn.[_; _]) -> Val
  | Op(App, Syn.[f; arg]) ->
    begin match cbv f with
      | Step next -> Step (Syn.op App Syn.[next; arg])
      | Val ->
        begin match cbv arg with
          | Step next -> Step (Syn.op App Syn.[f; next])
          | Val ->
            begin match Syn.out f with
              | Op(Lam, Syn.[_; abstr]) ->
                let Abs(var, body) = Syn.out abstr in
                Step (body |> Syn.subst Term begin fun var' ->
                    match Syn.Variable.equal var var' with
                    | Some Refl -> Some arg
                    | None -> None
                  end)
              | _ -> Err
            end
          | Err -> Err
        end
      | Err -> Err
    end
  | Var _ -> Err

let has_ty (term : tm Tyabt.va Syn.t) (ty : ty Tyabt.va Syn.t) =
  match infer [] term with
  | Ok ty' -> Syn.aequiv ty ty'
  | Error _ -> false

let () =
  let unit_type = Syn.op Unit Syn.[] in
  let unit_arr_unit = Syn.op Arrow Syn.[unit_type; unit_type] in
  let ax = Syn.op Ax Syn.[] in
  let id_unit =
    Syn.op Lam Syn.[ Syn.op Unit Syn.[]
                   ; let x = Syn.Variable.fresh Term "x" in
                     Syn.abs x (Syn.var x) ]
  in
  let ret_id_unit =
    Syn.op Lam Syn.[ Syn.op Unit Syn.[]
                   ; let y = Syn.Variable.fresh Term "y" in
                     Syn.abs y id_unit ]
  in
  let const =
    Syn.op Lam Syn.[ unit_arr_unit
                   ; let x = Syn.Variable.fresh Term "x" in
                     Syn.abs x
                       (Syn.op Lam Syn.[ unit_type
                                       ; let y = Syn.Variable.fresh Term "y" in
                                         Syn.abs y (Syn.var x) ]) ]
  in
  assert (has_ty ax unit_type);
  assert (has_ty id_unit unit_arr_unit);
  assert (cbv ax = Val);
  assert (cbv (Syn.op App Syn.[id_unit; ax]) = Step ax);
  assert (cbv (Syn.op App Syn.[ret_id_unit; ax]) = Step id_unit);
  assert (cbv (Syn.op App Syn.[const; id_unit])
               = Step (Syn.op Lam Syn.[ unit_type
                                      ; let y = Syn.Variable.fresh Term "y" in
                                        Syn.abs y id_unit ]));
  assert (to_string unit_arr_unit = "arrow(unit();unit())")

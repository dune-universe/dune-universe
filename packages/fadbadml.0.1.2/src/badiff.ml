(**************************************************************************)
(*                                                                        *)
(*                                FADBADml                                *)
(*                                                                        *)
(*           OCaml port by François Bidet and Ismail Bennani              *)
(*     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      *)
(*                                                                        *)
(*                          Copyright 2019-2020                           *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C license.    *)
(*                                                                        *)
(**************************************************************************)

open Fadbad_utils

module Derivatives (T : Types.OpS) =
struct
  type t = T.t array ref

  let create () = ref [||]
  let make n e = Array.init n (fun _ -> T.copy e)
  let map f this = Array.map f !this
  let copy this = map T.copy this
  let deepcopy this = map T.deepcopy this
  let mapi f this = Array.mapi f !this
  let iter f this = Array.iter f !this
  let iteri f this = Array.iteri f !this
  let length this = Array.length !this
  let to_string this =
    Printf.sprintf "[%s]"
      (String.concat ", "
         (Array.to_list (Array.map T.to_string !this)))

   let fprint_t_list ff t_l =
     let rec aux ff t_l =
       match t_l with
       | [] -> ()
       | x :: q -> Format.fprintf ff ";@,%s%a"
                     T.(string_of_elt !!x) aux q
     in
     match t_l with
     | [] -> ()
     | x :: q -> Format.fprintf ff "@[<2>%s%a@]"
                   T.(string_of_elt !!x) aux q

  let fprint ff this =
    Format.fprintf ff "@[<2>[%a]@]" fprint_t_list (Array.to_list !this)

  let has_values this = length this > 0
  let check_has_values this =
    user_assert (has_values this)
      "Derivatives.check_has_values: Propagating node with no derivatives"

  let check_bounds this i =
    user_assert (i < length this && i >= 0)
      ("Derivatives.check_bounds: Index " ^ (string_of_int i) ^
       " out of range [0," ^ (string_of_int (length this - 1)) ^ "]")

  let get this i =
    if has_values this then begin
      check_bounds this i;
      !this.(i)
    end else T.zero ()

  let diff this i n =
    user_assert (i < n && i >= 0)
      ("Derivatives.diff: Index " ^ (string_of_int i) ^
       " out of range [0," ^ (string_of_int (n - 1)) ^ "]");
    let res = if has_values this then this else ref (make n (T.zero ())) in
    !res.(i) <- T.one ();
    this := !res

  let cAdd v v' =
    check_has_values v';
    if has_values v then begin
      user_assert (length v = length v')
        ("Derivatives.cAdd: Size mismatch " ^
         (string_of_int (length v)) ^ "<>" ^ (string_of_int (length v')));
      iteri (fun i _ -> ignore T.(!v.(i) += !v'.(i))) v;
    end else v := copy v'

  let cSub v v' =
    check_has_values v';
    if has_values v then begin
      user_assert (length v = length v')
        ("Derivatives.cSub: Size mismatch " ^
         (string_of_int (length v)) ^ "<>" ^ (string_of_int (length v')));
      iteri (fun i _ -> ignore T.(!v.(i) -= !v'.(i))) v;
    end else v := map T.(~-) v'

  (** multiply-accumulate operation *)
  let cMac v a v' =
    check_has_values v';
    if has_values v then begin
      user_assert (length v = length v')
        ("Derivatives.cMac: Size mismatch " ^
         (string_of_int (length v)) ^ "<>" ^ (string_of_int (length v')));
      iteri (fun i _ -> ignore T.(!v.(i) += a * !v'.(i))) v;
    end else v := map (fun v' -> T.(a * v')) v'

  (** substractive multiply-accumulate operation *)
  let cSmac v a v' =
    check_has_values v';
    if has_values v then begin
      user_assert (length v = length v')
        ("Derivatives.cSmac: Size mismatch " ^
         (string_of_int (length v)) ^ "<>" ^ (string_of_int (length v')));
      iteri (fun i _ -> ignore T.(!v.(i) -= a * !v'.(i))) v;
    end else v := map (fun v' -> T.(- a * v')) v'

end

module BTypeName (T : Types.OpS) =
struct
  module D = Derivatives(T)

  type elt = T.elt
  type scalar = T.scalar

  type op = ..
  type op +=
    | CONST | SCALE of scalar | TRANS of scalar
    | ADD | SUB | MUL | DIV | POW
    | POS | NEG | INV | SQR | SQRT | EXP | LOG | SIN | COS | TAN
    | ASIN | ACOS | ATAN

  type t = {
    mutable operator : op;
    mutable operands : t array;
    mutable rc : int;
    mutable value : T.t;
    derivatives : D.t;
  }

  let string_of_op = function
    | CONST -> "CONST"
    | SCALE f -> Printf.sprintf "SCALE %s" (T.string_of_scalar f)
    | TRANS f -> Printf.sprintf "TRANS %s" (T.string_of_scalar f)
    | ADD -> "ADD" | SUB -> "SUB" | MUL -> "MUL" | DIV -> "DIV" | POW -> "POW"
    | POS -> "POS" | NEG -> "NEG" | INV -> "INV" | SQR -> "SQR" | SQRT -> "SQRT"
    | EXP -> "EXP" | LOG -> "LOG" | SIN -> "SIN" | COS -> "COS" | TAN -> "TAN"
    | ASIN -> "ASIN" | ACOS -> "ACOS" | ATAN -> "ATAN"
    | _ -> failwith "Unknown operator"

  let to_short_string this = string_of_op this.operator

  let rec fprint_t_list ff t_l =
    let rec aux ff t_l =
      match t_l with
      | [] -> ()
      | x :: q -> Format.fprintf ff ";@,%a%a" fprint_t x aux q
    in
    match t_l with
    | [] -> ()
    | x :: q -> Format.fprintf ff "@[<2>%a%a@]" fprint_t x aux q

  and fprint_t ff this =
    let fprint_value ff value =
      Format.fprintf ff "@[<2>value@ =@ %s@]"
        T.(string_of_elt !!value)
    in
    let fprint_operator ff op =
      Format.fprintf ff "@[<2>operator@ =@ %s@]" (string_of_op op)
    in
    let fprint_operands ff operands =
      Format.fprintf ff "@[<2>operands@ =@ [%a]@]"
        fprint_t_list (Array.to_list operands)
    in
    let fprint_rc ff rc =
      Format.fprintf ff "@[<2>rc@ =@ %d@]" rc
    in
    let fprint_derivatives ff derivatives =
      Format.fprintf ff "@[<2>derivatives@ =@ %a@]"
        D.fprint derivatives
    in
    Format.fprintf ff "@[<2>{@;%a;@;%a;@;%a;@;%a;@;%a;@;}@]"
      fprint_value this.value
      fprint_operator this.operator
      fprint_operands this.operands
      fprint_rc this.rc
      fprint_derivatives this.derivatives

  let fprint ff this = Format.fprintf ff "%a" fprint_t this

  let to_string this =
    (Printf.sprintf "{\n\toperator = %s\n\toperands =\n\t\t[%s]\n\t"
      (string_of_op this.operator)
      (String.concat ", " (Array.to_list
        (Array.map to_short_string this.operands))))
    ^
    (Printf.sprintf "rc = %d\n\tvalue = %s\n\tderivatives = %s\n}"
      this.rc (T.to_string this.value) (D.to_string this.derivatives))

  let string_of_scalar = T.string_of_scalar
  let string_of_elt = T.string_of_elt

  let add_der this d = D.cAdd this.derivatives d.derivatives
  let sub_der this d = D.cSub this.derivatives d.derivatives
  let mac_der this a d = D.cMac this.derivatives a d.derivatives
  let smac_der this a d = D.cSmac this.derivatives a d.derivatives

  let get_operands this i =
    user_assert (i < Array.length this.operands && i >= 0)
      ("BTypeName.get_operands: Index " ^ (string_of_int i) ^
       " out of range [0," ^ (string_of_int (Array.length this.operands - 1)) ^
       "]");
    this.operands.(i)

  let create () = let v = T.create () in {
      operator = CONST;
      operands = [||];
      rc = 0;
      value = v;
      derivatives = D.create ();
    }

  let lift v = {
    operator = CONST;
    operands = [||];
    rc = 0;
    value = v;
    derivatives = D.create ();
  }

  let integer i = lift (T.integer i)
  let make n = lift (T.make n)
  let zero () = lift (T.zero ())
  let one () = lift (T.one ())
  let two () = lift (T.two ())

  let copy this =
    {
      operator = this.operator;
      operands = Array.copy this.operands;
      rc = this.rc;
      value = T.copy this.value;
      derivatives = ref (D.copy this.derivatives);
    }

  let rec deepcopy this =
    {
      operator = this.operator;
      operands = Array.map deepcopy this.operands;
      rc = this.rc;
      value = T.deepcopy this.value;
      derivatives = ref (D.deepcopy this.derivatives);
    }

  let value this = this.value

  let get this = T.get this.value
  let ( !! ) = get

  let deriv this i = D.get this.derivatives i
  let d this i = T.get (deriv this i)

  let propagate this =
    match this.operator with
    | CONST -> ()
    | SCALE f ->
      let t = get_operands this 0 in
      D.cAdd t.derivatives
        (ref (Array.map (fun x -> T.scale x f) !(this.derivatives)))
    | ADD ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      add_der t1 this;
      add_der t2 this
    | SUB ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      add_der t1 this;
      sub_der t2 this
    | MUL ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      mac_der t1 (value t2) this;
      mac_der t2 (value t1) this
    | DIV ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let inv_t2 = T.inv (value t2) in
      mac_der t1 inv_t2 this;
      smac_der t2 T.(inv_t2 * (value this)) this
    | POW ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let t1_val = value t1 in
      let t2_val = value t2 in
      let tmp1 = T.(t2_val * (t1_val ** (t2_val - (one ())))) in
      let tmp2 = T.((value this) * (log t1_val)) in
      mac_der t1 tmp1 this;
      mac_der t2 tmp2 this
    | TRANS _
    | POS ->
      let t = get_operands this 0 in
      add_der t this
    | NEG ->
      let t = get_operands this 0 in
      sub_der t this
    | INV ->
      let t = get_operands this 0 in
      smac_der t T.(sqr (value this)) this
    | SQR ->
      let t = get_operands this 0 in
      let tmp = T.((two ()) * (value t)) in
      mac_der t tmp this
    | SQRT ->
      let t = get_operands this 0 in
      let tmp = T.(inv ((value this) * (two ()))) in
      mac_der t tmp this
    | EXP ->
      let t = get_operands this 0 in
      mac_der t (value this) this
    | LOG ->
      let t = get_operands this 0 in
      mac_der t T.(inv (value t)) this
    | SIN ->
      let t = get_operands this 0 in
      let tmp = T.cos (value t) in
      mac_der t tmp this
    | COS ->
      let t = get_operands this 0 in
      let tmp = T.sin (value t) in
      smac_der t tmp this
    | TAN ->
      let t = get_operands this 0 in
      let tmp = T.((sqr (value this)) + (one ())) in
      mac_der t tmp this
    | ASIN ->
      let t = get_operands this 0 in
      let tmp = T.(inv (sqrt ((one ()) - (sqr (value t))))) in
      mac_der t tmp this
    | ACOS ->
      let t = get_operands this 0 in
      let tmp = T.(inv (sqrt ((one ()) - (sqr (value t))))) in
      smac_der t tmp this
    | ATAN ->
      let t = get_operands this 0 in
      let tmp = T.(inv ((sqr (value t)) + (one ()))) in
      mac_der t tmp this
    | _ -> failwith "Unknown operator"

  let rec propagateChildren this =
    Array.iter decRef this.operands;
    this.operands <- [||]

  and decRef this =
    user_assert (this.rc > 0) "BTypeName.decRef: Ressource counter negative";
    this.rc <- this.rc - 1;
    if this.rc = 0 then
      if D.has_values this.derivatives then begin
        propagate this;
        propagateChildren this;
      end

  let incRef this = this.rc <- this.rc + 1

  let rec incRef_subtree this =
    incRef this;
    if this.rc = 1 then
      Array.iter incRef_subtree this.operands

  let compute_list t_l =
    List.iter incRef_subtree t_l;
    List.iter decRef t_l

  let compute this = compute_list [this]

  let diff this idx n =
    D.diff this.derivatives idx n

  let un_op operator operation t =
    {
      operator;
      operands = [|t|];
      rc = 0;
      value = operation (value t);
      derivatives = D.create ();
    }

  let bin_op operator operation t1 t2 =
    {
      operator;
      operands = [|t1; t2|];
      rc = 0;
      value = operation (value t1) (value t2);
      derivatives = D.create ();
    }

  let bin_cOp operator operation t1 t2 =
    let copy_t1 = copy t1 in
    t1.operator <- operator;
    t1.operands <- [|copy_t1; t2|];
    t1.rc <- 0;
    t1.value <- operation (value t1)  (value t2);
    t1.derivatives := [||];
    t1

  let scale t f = un_op (SCALE f) (fun x -> T.scale x f) t
  let translate t f = un_op (TRANS f) (fun x -> T.translate x f) t

  let ( ~+ ) = un_op POS T.(~+)
  let ( ~- ) = un_op NEG T.(~-)

  let ( + ) = bin_op ADD T.( + )
  let ( += ) = bin_cOp ADD T.( + )

  let ( - ) = bin_op SUB T.( - )
  let ( -= ) = bin_cOp SUB T.( - )

  let ( * ) = bin_op MUL T.( * )
  let ( *= ) = bin_cOp MUL T.( * )

  let ( / ) = bin_op DIV T.( / )
  let ( /= ) = bin_cOp DIV T.( / )

  let ( ** ) = bin_op POW T.( ** )

  let inv = un_op INV T.inv
  let sqr = un_op SQR T.sqr
  let sqrt = un_op SQRT T.sqrt
  let log = un_op LOG T.log
  let exp = un_op EXP T.exp
  let sin = un_op SIN T.sin
  let cos = un_op COS T.cos
  let tan = un_op TAN T.tan
  let asin = un_op ASIN T.asin
  let acos = un_op ACOS T.acos
  let atan = un_op ATAN T.atan

  let ( = ) t1 t2 = T.((value t1) = (value t2))
  let ( <> ) t1 t2 = T.((value t1) <> (value t2))

end

module OrderedBTypeName (T : Types.OrderedOpS) =
struct
  include BTypeName(T)

  let ( < ) a b = T.(value a < value b)
  let ( <= ) a b = T.(value a <= value b)
  let ( > ) a b = T.(value a > value b)
  let ( >= ) a b = T.(value a >= value b)

  let min a b = if a < b then a else b
  let max a b = if a > b then a else b
end

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

module TValues(T : Types.OpS) =
struct
  type t = {
    mutable n : int;
    values : T.t array;
  }

  let max_length = 40

  let get_values this = Array.sub this.values 0 this.n

  let to_derivatives values =
    let rec mult_by_fact last_fact i arr =
      if i >= Array.length arr then arr
      else if i = 0 then mult_by_fact 1 1 arr
      else begin
        let new_fact = last_fact * i in
        arr.(i) <- T.((integer new_fact) * arr.(i));
        mult_by_fact new_fact (i+1) arr
      end
    in mult_by_fact 1 0 (Array.copy values)

  let get_derivatives this = to_derivatives (get_values this)

  let string_of_arr arr =
    Printf.sprintf "[%s]" (String.concat ", " (Array.to_list
      (Array.map T.to_string arr)))
  let string_of_values this = string_of_arr (get_values this)
  let string_of_derivatives this = string_of_arr (get_derivatives this)


  let copy this = {
    n = this.n;
    values = Array.map T.copy this.values
  }

  let deepcopy this = {
    n = this.n;
    values = Array.map T.deepcopy this.values
  }

  let create length = {
    n = 0;
    values = Array.init length (fun _ -> T.zero ());
  }

  let lift f =
    let res = create max_length in
    res.n <- 1;
    res.values.(0) <- f;
    res

  let lift_sized f length =
    let res = create length in
    res.n <- 1;
    res.values.(0) <- f;
    res

  let make_sized f length = lift_sized (T.make f) length
  let make f = make_sized f max_length

  let check_bounds this i =
    user_assert (i >= 0 && i < (Array.length this.values))
      ("TValues.get: Index " ^ (string_of_int i)^
        " out of bounds [0," ^ (string_of_int (Array.length this.values)) ^ "]")

  let length this = this.n
  let set_length this n = check_bounds this n; this.n <- n
  let reset this = set_length this 0
  let size this = Array.length this.values

  let fill_from this i e =
    Array.fill this.values i (size this - i) e

  let get this i =
    check_bounds this i;
    this.values.(i)

  let set this i v =
    check_bounds this i;
    this.values.(i) <- v
end

module TTypeName(T : Types.OpS) =
struct
  module OpTValues = TValues(T)

  type elt = T.elt
  type scalar = T.scalar

  type op = ..
  type op +=
    | CONST | SCALE of scalar | TRANS of scalar
    (* while computing the derivatives of SIN or COS, we also compute
       the derivatives of COS or SIN. the argument of the constructor is
       used to store those values (to avoid computing at each call of eval) *)
    | SIN of T.t array | COS of T.t array
    | ADD | SUB | MUL | DIV | POW
    | POS | NEG | INV | SQR | SQRT | EXP | LOG | TAN
    | ASIN | ACOS | ATAN

  type t = {
    mutable operator : op;
    mutable operands : t array;
    mutable tvalues : OpTValues.t;
  }

  let operator this = this.operator
  let order this = OpTValues.length this.tvalues

  let tvalues this = OpTValues.get_values this.tvalues
  let derivatives this = OpTValues.get_derivatives this.tvalues

  let get_tvalues this = Array.map T.(!!) (tvalues this)
  let get_derivatives this = Array.map T.(!!) (derivatives this)

  let string_of_op = function
    | CONST -> "CONST"
    | SCALE f -> Printf.sprintf "SCALE %s" (T.string_of_scalar f)
    | TRANS f -> Printf.sprintf "TRANS %s" (T.string_of_scalar f)
    | ADD -> "ADD" | SUB -> "SUB" | MUL -> "MUL" | DIV -> "DIV" | POW -> "POW"
    | POS -> "POS" | NEG -> "NEG" | INV -> "INV" | SQR -> "SQR" | SQRT -> "SQRT"
    | EXP -> "EXP" | LOG -> "LOG" | SIN _ -> "SIN" | COS _ -> "COS" | TAN -> "TAN"
    | ASIN -> "ASIN" | ACOS -> "ACOS" | ATAN -> "ATAN"
    | _ -> failwith "Unknown operator"
  let to_short_string this = string_of_op this.operator
  let to_string this =
    (Printf.sprintf "{\n\toperator = %s\n\toperands =\n\t\t[%s]\n\t"
      (string_of_op this.operator)
      (String.concat ", " (Array.to_list
        (Array.map to_short_string this.operands))))
    ^
    (Printf.sprintf "tvalues = %s\n\tderivatives = %s\n}"
      (OpTValues.string_of_values this.tvalues)
      (OpTValues.string_of_derivatives this.tvalues))
  let string_of_scalar = T.string_of_scalar
  let string_of_elt = T.string_of_elt

  let get_operands this i =
    user_assert (i < Array.length this.operands && i >= 0)
      ("BTypeName.get_operands: Index " ^ (string_of_int i) ^
       " out of range [0," ^ (string_of_int (Array.length this.operands)) ^
       "]");
    this.operands.(i)

  let create () = {
    operator = CONST;
    operands = [||];
    tvalues = OpTValues.create OpTValues.max_length
  }

  let create_sized size = {
    operator = CONST;
    operands = [||];
    tvalues = OpTValues.create size
  }

  let lift v = {
    operator = CONST;
    operands = [||];
    tvalues = OpTValues.lift v
  }

  let make f = {
    operator = CONST;
    operands = [||];
    tvalues = OpTValues.make f
  }

  let make_sized f size = {
    operator = CONST;
    operands = [||];
    tvalues = OpTValues.make_sized f size
  }

  let integer i = lift (T.integer i)
  let zero () = lift (T.zero ())
  let one () = lift (T.one ())
  let two () = lift (T.two ())

  let copy this = {
    operator = this.operator;
    operands = Array.copy this.operands;
    tvalues = OpTValues.copy this.tvalues
  }

  let rec deepcopy this = {
    operator = this.operator;
    operands = Array.map deepcopy this.operands;
    tvalues = OpTValues.deepcopy this.tvalues
  }

  let rec reset this =
    match this.operator with
    | CONST ->
      OpTValues.set_length this.tvalues 1;
      OpTValues.fill_from this.tvalues 1 (T.zero ())
    | _ -> OpTValues.reset this.tvalues; Array.iter reset this.operands

  let value this = OpTValues.get this.tvalues 0
  let get this = T.(!!(value this))
  let ( !! ) = get

  let deriv this i = OpTValues.get this.tvalues i
  let d this i = T.(!!(deriv this i))

  let length this = OpTValues.length this.tvalues

  let set this i v =
    OpTValues.set this.tvalues i v;
    OpTValues.set_length this.tvalues (i+1)

  let rec eval this k =
    match this.operator with
    | CONST -> OpTValues.set_length this.tvalues (k+1); k+1
    | SCALE f ->
      let t = get_operands this 0 in
      let l = eval t k in
      for i = order this to l-1 do
        OpTValues.set this.tvalues i T.(scale (deriv t i) f)
      done;
      OpTValues.set_length this.tvalues l;
      l
    | TRANS f ->
      let t = get_operands this 0 in
      let l = eval t k in
      if (order this) = 0 then set this 0 T.(translate (value t) f);
      for i = order this to l-1 do
        OpTValues.set this.tvalues i (deriv t i)
      done;
      OpTValues.set_length this.tvalues l;
      l
    | ADD ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      for i = order this to l-1 do
        OpTValues.set this.tvalues i T.((deriv t1 i) + (deriv t2 i))
      done;
      OpTValues.set_length this.tvalues l;
      l
    | SUB ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      for i = order this to l-1 do
        OpTValues.set this.tvalues i T.((deriv t1 i) - (deriv t2 i))
      done;
      OpTValues.set_length this.tvalues l;
      l
    | MUL ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      let rec aux acc i j =
        if i >= l then ()
        else if j > i then begin
          OpTValues.set this.tvalues i acc;
          aux (T.zero ()) (i+1) 0
        end else aux T.(acc + (deriv t1 j) * (deriv t2 Stdlib.(i-j))) i (j+1)
      in
      aux (T.zero ()) (order this) 0;
      OpTValues.set_length this.tvalues l;
      l
    | DIV ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      let rec aux acc i j =
        if i >= l then ()
        else if j > i then begin
          OpTValues.set this.tvalues i T.(acc / (value t2));
          aux (deriv t1 (i+1)) (i+1) 1
        end else aux T.(acc - (deriv t2 j) * (deriv this Stdlib.(i-j))) i (j+1)
      in
      aux (deriv t1 (order this)) (order this) 1;
      OpTValues.set_length this.tvalues l;
      l
    | POW -> assert false
    | POS ->
      let t = get_operands this 0 in
      let l = eval t k in
      for i = order this to l-1 do
        OpTValues.set this.tvalues i T.(+ (deriv t i))
      done;
      OpTValues.set_length this.tvalues l;
      l
    | NEG ->
      let t = get_operands this 0 in
      let l = eval t k in
      for i = order this to l-1 do
        OpTValues.set this.tvalues i T.(- (deriv t i))
      done;
      OpTValues.set_length this.tvalues l;
      l
    | INV ->
      let t = get_operands this 0 in
      let l = eval t k in
      let rec aux acc i j =
        if i >= l then ()
        else if j > i then begin
          OpTValues.set this.tvalues i T.(acc / (value t));
          aux (T.zero ()) (i+1) 1
        end else aux T.(acc - (deriv t j) * (deriv this Stdlib.(i-j))) i (j+1)
      in
      if (order this) = 0 then set this 0 T.(inv (value t));
      aux (T.zero ()) (order this) 1;
      OpTValues.set_length this.tvalues l;
      l
    | SQR ->
      let t = get_operands this 0 in
      let l = eval t k in
      let rec aux acc i j =
        let m = (i + 1) / 2 in
        if i >= l then ()
        else if j >= m then begin
          let new_val = T.((two ()) * acc) in
          let new_val =
            if i mod 2 = 0 then T.(new_val + (sqr (deriv t m))) else new_val
          in
          OpTValues.set this.tvalues i new_val;
          aux (T.zero ()) (i+1) 0
        end else aux T.(acc + (deriv t j) * (deriv t Stdlib.(i-j))) i (j+1)
      in
      if (order this) = 0 then set this 0 T.(sqr (value t));
      aux (T.zero ()) (order this) 0;
      OpTValues.set_length this.tvalues l;
      l
    | SQRT ->
      let t = get_operands this 0 in
      let l = eval t k in
      let rec aux acc i j =
        let m = (i + 1) / 2 in
        if i >= l then ()
        else if j >= m then begin
          let new_val = T.((two ()) * acc) in
          let new_val =
            if i mod 2 = 0 then T.(new_val + (sqr (deriv this m))) else new_val
          in
          let new_val =
            T.(((deriv t i) - new_val) / ((two ()) * (value this)))
          in
          OpTValues.set this.tvalues i new_val;
          aux (T.zero ()) (i+1) 1
        end else aux T.(acc + (deriv this j) * (deriv this Stdlib.(i-j))) i (j+1)
      in
      if (order this) = 0 then set this 0 T.(sqrt (value t));
      aux (T.zero ()) (order this) 1;
      OpTValues.set_length this.tvalues l;
      l
    | EXP ->
      let t = get_operands this 0 in
      let l = eval t k in
      let rec aux acc i j =
        if i >= l then ()
        else if j >= i then begin
          OpTValues.set this.tvalues i acc;
          aux (T.zero ()) (i+1) 0
        end else
          aux
            T.(acc +
              (((one ()) -
               ((integer j) / (integer i))) *
               (deriv t Stdlib.(i-j)) *
               (deriv this j)))
            i (j+1)
      in
      if (order this) = 0 then set this 0 T.(exp (value t));
      aux (T.zero ()) (order this) 0;
      OpTValues.set_length this.tvalues l;
      l
    | LOG ->
      let t = get_operands this 0 in
      let l = eval t k in
      let rec aux acc i j =
        if i >= l then ()
        else if j >= i then begin
          OpTValues.set this.tvalues i T.(acc / (value t));
          aux (deriv t (i+1)) (i+1) 1
        end else
          aux
            T.(acc -
              (((one ()) -
               ((integer j) / (integer i))) *
               (deriv t j) *
               (deriv this Stdlib.(i-j))))
            i (j+1)
      in
      if (order this) = 0 then set this 0 T.(log (value t));
      let i = order this in aux (deriv t i) i 1;
      OpTValues.set_length this.tvalues l;
      l
    | SIN tcoeff_cos ->
      let t = get_operands this 0 in
      let l = eval t k in
      let rec aux acc_this acc_cos i j =
        if i >= l then ()
        else if j >= i then begin
          OpTValues.set this.tvalues i T.(acc_this / (integer i));
          tcoeff_cos.(i) <- T.(acc_cos / (integer i));
          aux (T.zero ()) (T.zero ()) (i+1) 0
        end else
          aux
            T.(acc_this + (integer Stdlib.(j+1)) * tcoeff_cos.(Stdlib.(i-1-j))
                  * (deriv t Stdlib.(j+1)))
            T.(acc_cos - (integer Stdlib.(j+1)) * (deriv this Stdlib.(i-1-j))
                  * (deriv t Stdlib.(j+1)))
            i (j+1)
      in
      if (order this) = 0 then begin
        set this 0 T.(sin (value t));
        tcoeff_cos.(0) <- T.(cos (value t));
      end;
      aux (T.zero ()) (T.zero ()) (order this) 0;
      OpTValues.set_length this.tvalues l;
      l
    | COS tcoeff_sin ->
      let t = get_operands this 0 in
      let l = eval t k in
      let rec aux acc_this acc_sin i j =
        if i >= l then ()
        else if j >= i then begin
          OpTValues.set this.tvalues i T.(acc_this / (integer i));
          tcoeff_sin.(i) <- T.(acc_sin / (integer i));
          aux (T.zero ()) (T.zero ()) (i+1) 0
        end else
          aux
            T.(acc_this - (integer Stdlib.(j+1)) * tcoeff_sin.(Stdlib.(i-1-j))
                  * (deriv t Stdlib.(j+1)))
            T.(acc_sin + (integer Stdlib.(j+1)) * (deriv this Stdlib.(i-1-j))
                  * (deriv t Stdlib.(j+1)))
            i (j+1)
      in
      if (order this) = 0 then begin
        set this 0 T.(cos (value t));
        tcoeff_sin.(0) <- T.(sin (value t));
      end;
      aux (T.zero ()) (T.zero ()) (order this) 0;
      OpTValues.set_length this.tvalues l;
      l
    | TAN ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      let rec aux acc i j =
        if i >= l then ()
        else if j >= i then begin
          OpTValues.set this.tvalues i
            T.(((deriv t1 i) - acc / (integer i)) / (value t2));
          aux (T.zero ()) (i+1) 1
        end else
          aux T.(acc + (integer j) * (deriv this j)
            * (deriv t2 Stdlib.(i-j))) i (j+1)
      in
      if (order this) = 0 then set this 0 T.(tan (value t1));
      aux (T.zero ()) (order this) 1;
      OpTValues.set_length this.tvalues l;
      l
    | ASIN ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      let rec aux acc i j =
        if i >= l then ()
        else if j >= i then begin
          OpTValues.set this.tvalues i
            T.(((deriv t1 i) - acc / (integer i)) / (value t2));
          aux (T.zero ()) (i+1) 1
        end else
          aux T.(acc + (integer j) * (deriv this j)
            * (deriv t2 Stdlib.(i-j))) i (j+1)
      in
      if (order this) = 0 then set this 0 T.(asin (value t1));
      aux (T.zero ()) (order this) 1;
      OpTValues.set_length this.tvalues l;
      l
    | ACOS ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      let rec aux acc i j =
        if i >= l then ()
        else if j >= i then begin
          OpTValues.set this.tvalues i
            T.(- ((deriv t1 i) + acc / (integer i)) / (value t2));
          aux (T.zero ()) (i+1) 1
        end else
          aux T.(acc + (integer j) * (deriv this j)
            * (deriv t2 Stdlib.(i-j))) i (j+1)
      in
      if (order this) = 0 then set this 0 T.(acos (value t1));
      aux (T.zero ()) (order this) 1;
      OpTValues.set_length this.tvalues l;
      l
    | ATAN ->
      let t1 = get_operands this 0 in
      let t2 = get_operands this 1 in
      let l = min (eval t1 k) (eval t2 k) in
      let rec aux acc i j =
        if i >= l then ()
        else if j >= i then begin
          OpTValues.set this.tvalues i
            T.(((deriv t1 i) - acc / (integer i)) / (value t2));
          aux (T.zero ()) (i+1) 1
        end else
          aux T.(acc + (integer j) * (deriv this j)
            * (deriv t2 Stdlib.(i-j))) i (j+1)
      in
      if (order this) = 0 then set this 0 T.(atan (value t1));
      aux (T.zero ()) (order this) 1;
      OpTValues.set_length this.tvalues l;
      l
    | _ -> failwith "Unknown operator"

  let un_op operator t = {
    operator;
    operands = [|t|];
    tvalues = OpTValues.create (OpTValues.size t.tvalues)
  }

  let bin_op operator t1 t2 =
    let size = min (OpTValues.size t1.tvalues) (OpTValues.size t2.tvalues) in
    {
      operator;
      operands = [|t1; t2|];
      tvalues = OpTValues.create size
    }

  let bin_cOp operator t1 t2 =
    let size = min (OpTValues.size t1.tvalues) (OpTValues.size t2.tvalues) in
    let copy_t1 = copy t1 in
    t1.operator <- operator;
    t1.operands <- [|copy_t1; t2|];
    t1.tvalues <- OpTValues.create size;
    t1

  let scale t f = un_op (SCALE f) t
  let translate t f = un_op (TRANS f) t

  let ( ~+ ) = un_op POS
  let ( ~- ) = un_op NEG

  let ( + ) = bin_op ADD
  let ( += ) = bin_cOp ADD

  let ( - ) = bin_op SUB
  let ( -= ) = bin_cOp SUB

  let ( * ) = bin_op MUL
  let ( *= ) = bin_cOp MUL

  let ( / ) = bin_op DIV
  let ( /= ) = bin_cOp DIV

  let inv = un_op INV
  let sqr = un_op SQR
  let sqrt = un_op SQRT
  let log = un_op LOG
  let exp = un_op EXP
  let sin t =
    un_op (SIN (Array.init (OpTValues.size t.tvalues) (fun _ -> T.zero ()))) t
  let cos t =
    un_op (COS (Array.init (OpTValues.size t.tvalues) (fun _ -> T.zero ()))) t
  (*
     cf. the implementation in FADBAD++ (tadiff.h class TTypeNameTAN and function tan)
     the TAN operator creates a node `sqr (cos x)` in order to use its derivatives
     to compute the n-th derivative of (tan x) with the formula tan x = 1/cos²(x)
  *)
  let tan t = bin_op TAN t (sqr (cos t))
  (* same than TAN *)
  let asin t = bin_op ASIN t (sqrt ((one ()) - (sqr t)))
  (* same than TAN *)
  let acos t = bin_op ACOS t (sqrt ((one ()) - (sqr t)))
  (* same than TAN *)
  let atan t = bin_op ATAN t ((one ()) + (sqr t))

  let ( ** ) t1 t2 = exp (t2 * (log t1))

  let ( = ) t1 t2 = T.((value t1) = (value t2))
  let ( <> ) t1 t2 = T.((value t1) <> (value t2))

end

module OrderedTTypeName(T : Types.OrderedOpS) =
struct
  module OpTTypeName = TTypeName(T)
  include OpTTypeName
  type op += MIN | MAX

  let string_of_op = function
    | MIN -> "min"
    | MAX -> "max"
    | op -> string_of_op op

  let eval this =
    match operator this with
    | MIN -> failwith "not implemented"
    | MAX -> failwith "not implemented"
    | _ -> eval this


  let ( < ) a b = T.(value a < value b)
  let ( <= ) a b = T.(value a <= value b)
  let ( > ) a b = T.(value a > value b)
  let ( >= ) a b = T.(value a >= value b)

  let min = bin_op MIN
  let max = bin_op MAX

end

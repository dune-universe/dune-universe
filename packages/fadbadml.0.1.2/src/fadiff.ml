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

(** Forward Automatic Differentiation (FAD) *)

open Fadbad_utils

module FTypeName (T : Types.OpS) =
struct

  type t = {
    m_val : T.t;
    mutable m_diff : T.t array;
  }

  type elt = T.elt
  type scalar = T.scalar

let string_of_scalar = T.string_of_scalar
  let string_of_elt = T.string_of_elt

  let to_string this =
    Printf.sprintf "{%s | [%s]}" (T.to_string this.m_val)
      (String.concat ", " (Array.to_list (Array.map T.to_string this.m_diff)))

  let create () = {
    m_val = T.create ();
    m_diff = Array.make 0 (T.zero ());
  }

  let reset_diff this =
    for i = 0 to Array.length this.m_diff - 1 do
      this.m_diff.(i) <- T.zero ()
    done

  let get v = T.get v.m_val
  let ( !! ) = get

  let lift v = { (create ()) with m_val = v; }

  let make v = lift (T.make v)
  let integer i = lift (T.integer i)
  let zero () = lift (T.zero ())
  let one () = lift (T.one ())
  let two () = lift (T.two ())

  let scale x a = {
    m_val = T.scale x.m_val a;
    m_diff = Array.map (fun x -> T.scale x a) x.m_diff;
  }

  let translate x a = {
    m_val = T.translate x.m_val a;
    m_diff = Array.map T.copy x.m_diff;
  }

  let copy v = {
    m_val = T.copy v.m_val;
    m_diff = Array.map T.copy v.m_diff;
  }

  let deepcopy v = {
    m_val = T.deepcopy v.m_val;
    m_diff = Array.map T.deepcopy v.m_diff;
  }

  let dim v = Array.length v.m_diff
  let value v = v.m_val

  let set_deriv v i x =
    if i < (dim v) then v.m_diff.(i) <- x

  let deriv v i =
    if i < (dim v) then v.m_diff.(i)
    else T.zero ()

  let d v i = T.get (deriv v i)

  let diff v idx n =
    user_assert (idx < n && idx >= 0)
      ("Index " ^ (string_of_int idx) ^
       " out of bounds [0," ^ (string_of_int n) ^ "]");

    if (dim v) = 0 then begin
      v.m_diff <- Array.init n (fun _ -> T.zero ());
    end else
      user_assert ((dim v) = n) "derivative vectors not of same dim";

    Array.fill v.m_diff 0 (dim v) (T.zero ());
    v.m_diff.(idx) <- T.one ()

  let depend v = (dim v) <> 0

  let setDepend v v' =
    internal_assert ((dim v') > 0) "input is not a dependent variable";
    if ((dim v) = 0) then begin
      v.m_diff <- Array.init (dim v') (fun _ -> T.zero ());
    end else
      user_assert ((dim v) = (dim v'))
        ("derivative vectors not of the same dim "
         ^ (string_of_int (dim v)) ^ "," ^ (string_of_int (dim v')))

  let setDepend2 v v1 v2 =
    internal_assert ((dim v1) = (dim v2))
      ("derivative vectors not of same dim "
       ^ (string_of_int (dim v1)) ^ "," ^ (string_of_int (dim v2)));
    internal_assert ((dim v1) > 0) "lhs-input is not a dependent variable";
    internal_assert ((dim v2) > 0) "rhs-input is not a dependent variable";
    if ((dim v) = 0) then begin
      v.m_diff <- Array.init (dim v1) (fun _ -> T.zero ());
    end else
      user_assert ((dim v) = (dim v1))
        ("derivative vectors not of the same dim "
         ^ (string_of_int (dim v)) ^ "," ^ (string_of_int (dim v1)))

  (* ------------------------------ *)
  (* COMPARISON OPERATORS *)
  (* ------------------------------ *)

  let ( = ) v v' = T.( v.m_val = v'.m_val )
  let ( <> ) v v' = T.( v.m_val <> v'.m_val )

  (* ------------------------------ *)
  (* ARITHMETIC OPERATORS *)
  (* ------------------------------ *)

  (* ADD *)

  let addV (v : t) (v' : T.t) : t =
    let res = lift T.(v.m_val + v') in
    setDepend res v;
    Array.blit v.m_diff 0 res.m_diff 0 (dim v);
    res

  let vAdd v v' =
    let res = lift T.(v + v'.m_val) in
    setDepend res v';
    Array.blit v'.m_diff 0 res.m_diff 0 (dim v');
    res

  let add v v' =
    let res = lift T.(v.m_val + v'.m_val) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- T.(v.m_diff.(i) + v'.m_diff.(i)))
      res.m_diff;
    res

  let ( + ) v v' =
    match depend v, depend v' with
    | false, false -> lift T.(v.m_val + v'.m_val)
    | true, false -> addV v v'.m_val
    | false, true -> vAdd v.m_val v'
    | true, true -> add v v'

  let ( += ) v v' =
    ignore T.(v.m_val += v'.m_val);
    if not (depend v') then v
    else begin
      if depend v then
        Array.iteri (fun i vi -> ignore T.(vi += v'.m_diff.(i))) v.m_diff
      else begin
        setDepend v v';
        Array.blit v'.m_diff 0 v.m_diff 0 (dim v)
      end;
      v
    end

  (* SUB *)

  let subV v v' =
    let res = lift T.(v.m_val - v') in
    setDepend res v;
    Array.blit v.m_diff 0 res.m_diff 0 (dim v);
    res

  let vSub v v' =
    let res = lift T.(v - v'.m_val) in
    setDepend res v';
    Array.iteri (fun i _ -> res.m_diff.(i) <- T.( - v'.m_diff.(i) ))
      res.m_diff;
    res

  let sub v v' =
    let res = lift T.(v.m_val - v'.m_val) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- T.( v.m_diff.(i) - v'.m_diff.(i) ))
      res.m_diff;
    res

  let ( - ) v v' =
    match depend v, depend v' with
    | false, false -> lift T.(v.m_val - v'.m_val)
    | true, false -> subV v v'.m_val
    | false, true -> vSub v.m_val v'
    | true, true -> sub v v'

  let ( -= ) v v' =
    ignore T.(v.m_val -= v'.m_val);
    if depend v' then begin
      if depend v then
        Array.iteri (fun i vi -> ignore T.(vi -= v'.m_diff.(i))) v.m_diff
      else begin
        setDepend v v';
        Array.iteri (fun i _ -> v.m_diff.(i) <- T.(-v'.m_diff.(i))) v.m_diff
      end;
    end;
    v

  (* MUL *)

  let mulV v v' =
    let res = lift T.(v.m_val * v') in
    setDepend res v;
    Array.iteri (fun i _ -> res.m_diff.(i) <- T.(v.m_diff.(i) * v'))
      res.m_diff;
    res

  let vMul v v' =
    let res = lift T.(v * v'.m_val) in
    setDepend res v';
    Array.iteri (fun i _ -> res.m_diff.(i) <- T.(v'.m_diff.(i) * v))
      res.m_diff;
    res

  let mul v v' =
    let res = lift T.(v.m_val * v'.m_val) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <-
          T.((v.m_diff.(i) * v'.m_val) + (v'.m_diff.(i) * v.m_val))
      )
      res.m_diff;
    res

  let ( * ) v v' =
    match depend v, depend v' with
    | false, false -> lift T.(v.m_val * v'.m_val)
    | true, false -> mulV v v'.m_val
    | false, true -> vMul v.m_val v'
    | true, true -> mul v v'

  let ( *= ) v v' =
    begin match depend v, depend v' with
    | true, true ->
      Array.iteri (fun i _ -> v.m_diff.(i) <-
                      T.((v.m_diff.(i) * v'.m_val) + (v'.m_diff.(i) * v.m_val))
                  )
        v.m_diff
    | true, _ -> Array.iter (fun vi -> ignore T.(vi *= v'.m_val))
                   v.m_diff
    | _ -> (* _, true *)
      setDepend v v';
      Array.iteri (fun i _ -> v.m_diff.(i) <- T.( v'.m_diff.(i) * v.m_val ))
        v.m_diff;
    end;
    ignore T.(v.m_val *= v'.m_val);
    v

  (* DIV *)

  let divV v v' =
    let cval = T.(v.m_val / v') in
    let res = lift cval in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- T.(v.m_diff.(i) / v'))
        res.m_diff
    end;
    res

  let vDiv v v' =
    let cval = T.(v / v'.m_val) in
    let res = lift cval in
    if depend v' then begin
      let tmp = T.(- res.m_val / v'.m_val) in
      setDepend res v';
      Array.iteri (fun i _ -> res.m_diff.(i) <- T.(tmp * v'.m_diff.(i)))
        res.m_diff
    end;
    res

  let div v v' =
    let cval = T.(v.m_val / v'.m_val) in
    let res = lift cval in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <-
          T.((v.m_diff.(i) - (cval * v'.m_diff.(i))) / v'.m_val)
      ) res.m_diff;
    res

  let ( / ) v v' =
    match depend v, depend v' with
    | false, false -> lift T.(v.m_val / v'.m_val)
    | true, false -> divV v v'.m_val
    | false, true -> vDiv v.m_val v'
    | true, true -> div v v'

  let ( /= ) v v' =
    ignore T.(v.m_val /= v'.m_val);
    begin match depend v, depend v' with
    | true, true ->
      Array.iteri (fun i _ ->
          v.m_diff.(i) <-
            T.((v.m_diff.(i) - v.m_val * v'.m_diff.(i)) / v'.m_val)
        ) v.m_diff
    | true, _ -> Array.iteri (fun i _ -> ignore T.(v.m_diff.(i) /= v'.m_val))
                   v.m_diff
    | _ -> (* _, true *)
      setDepend v v';
      Array.iteri (fun i _ ->
          v.m_diff.(i) <-
            T.(- (v.m_val * v'.m_diff.(i)) / v'.m_val)
        ) v.m_diff;
    end;
    v

  (* POW *)

  let powV v v' =
    let res = lift T.(v.m_val ** v') in
    let tmp = T.(v' * (v.m_val ** (v' - (one ())))) in
    setDepend res v;
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- T.(tmp * v.m_diff.(i))
      ) v.m_diff;
    res

  let vPow v v' =
    let res = lift T.(v ** v'.m_val) in
    let tmp = T.(res.m_val * (log v)) in
    setDepend res v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- T.(tmp * v'.m_diff.(i))
      ) v'.m_diff;
    res

  let pow v v' =
    let res = lift T.(v.m_val ** v'.m_val) in
    let tmp1 = T.(v'.m_val * (v.m_val ** (v'.m_val - (one ())))) in
    let tmp2 = T.(res.m_val * (log v.m_val)) in
    setDepend2 res v v';
    Array.iteri (fun i _ ->
        res.m_diff.(i) <- T.((tmp1 * v.m_diff.(i)) + (tmp2 * v'.m_diff.(i)))
      ) v.m_diff;
    res

  let ( ** ) v v' =
    match depend v, depend v' with
    | false, false -> lift T.(v.m_val ** v'.m_val)
    | true, false -> powV v v'.m_val
    | false, true -> vPow v.m_val v'
    | true, true -> pow v v'

  (* ------------------------------ *)
  (* UNARY FUNCTIONS *)
  (* ------------------------------ *)

  let ( ~+ ) v =
    let res = lift T.(+v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- T.(+v.m_diff.(i)))
        res.m_diff;
    end;
    res

  let ( ~- ) v =
    let res = lift T.(- v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- T.(-v.m_diff.(i)))
        res.m_diff;
    end;
    res

  let sqr v =
    let res = lift (T.sqr v.m_val) in
    if depend v then begin
      let tmp = T.((two ()) * v.m_val) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- T.(tmp * v.m_diff.(i)))
        res.m_diff;
    end;
    res

  let inv v =
    let res = lift T.(inv v.m_val) in
    if depend v then begin
      let tmp = T.(- inv (sqr v.m_val)) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- T.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let exp v =
    let res = lift T.(exp v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- T.(v.m_diff.(i) * res.m_val))
        res.m_diff;
    end;
    res

  let log v =
    let res = lift T.(log v.m_val) in
    if depend v then begin
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- T.(v.m_diff.(i) / v.m_val))
        res.m_diff;
    end;
    res

  let sqrt v =
    let res = lift T.(sqrt v.m_val) in
    if depend v then begin
      let tmp = T.((two ()) * res.m_val) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- T.(v.m_diff.(i) / tmp))
        res.m_diff;
    end;
    res

  let sin v =
    let res = lift T.(sin v.m_val) in
    if depend v then begin
      let tmp = T.cos v.m_val in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- T.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let cos v =
    let res = lift T.(cos v.m_val) in
    if depend v then begin
      let tmp = T.(- sin v.m_val) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- T.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let tan v =
    let res = lift T.(tan v.m_val) in
    if depend v then begin
      let tmp = T.((one ()) + (sqr res.m_val)) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- T.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let asin v =
    let res = lift T.(asin v.m_val) in
    if depend v then begin
      let tmp = T.(inv (sqrt ((one ()) - (sqr v.m_val)))) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- T.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let acos v =
    let res = lift T.(acos v.m_val) in
    if depend v then begin
      let tmp = T.(- inv (sqrt ((one ()) - (sqr v.m_val)))) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- T.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res

  let atan v =
    let res = lift T.(atan v.m_val) in
    if depend v then begin
      let tmp = T.(inv ((one ()) + (sqr v.m_val))) in
      setDepend res v;
      Array.iteri (fun i _ -> res.m_diff.(i) <- T.(v.m_diff.(i) * tmp))
        res.m_diff;
    end;
    res
end

module OrderedFTypeName(T : Types.OrderedOpS) =
struct
  include FTypeName(T)

  let ( < ) a b = T.(value a < value b)
  let ( <= ) a b = T.(value a <= value b)
  let ( > ) a b = T.(value a > value b)
  let ( >= ) a b = T.(value a >= value b)

  let min a b = if a < b then a else b
  let max a b = if a > b then a else b
end

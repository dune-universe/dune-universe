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

let user_assert b s =
if not b then begin Printf.eprintf "User assertion failed: %s\n" s; exit 1 end 

module OpFun =
  struct
    type elt = float -> float
    type t = elt ref
    type scalar = float

    let identity () = ref (fun x -> x)

    let create = identity

    let make f = ref f
    let integer i = ref (fun _ -> float i)
    let get f = !f
    let ( !! ) = get

    let to_string this =
      "<OpFun.t = (float -> float) ref>"
    let string_of_scalar = string_of_float
    let string_of_elt e = "<OpFun.elt = float -> float>"

    let copy f = ref !f
    let deepcopy = copy

    let zero () = integer 0
    let one () = integer 1
    let two () = integer 2

    let scale f a = make (fun x -> a *. (!f x))
    let translate f a = make (fun x -> (!f x) +. a)

    let diff_n _ _ _ d =
      user_assert (d = 0) "diff_n : cannot differentiate a float"
    let d_n v i_l =
      user_assert (i_l = []) "d_n : cannot get derivative of a float";
      get v

    let apply f x = !f x

    let ( ~+ ) f = f
    let ( ~- ) f = ref (fun x -> -. !f x)

    let ( + ) f1 f2 = Stdlib.(ref (fun x -> (!f1 x) +. (!f2 x)))
    let ( += ) f1 f2 = Stdlib.(f1 := (fun x -> (!f1 x) +. (!f2 x))); f1

    let ( - ) f1 f2 = Stdlib.(ref (fun x -> (!f1 x) -. (!f2 x)))
    let ( -= ) f1 f2 = Stdlib.(f1 := (fun x -> (!f1 x) -. (!f2 x))); f1

    let ( * ) f1 f2 = Stdlib.(ref (fun x -> (!f1 x) *. (!f2 x)))
    let ( *= ) f1 f2 = Stdlib.(f1 := (fun x -> (!f1 x) *. (!f2 x))); f1

    let ( / ) f1 f2 = Stdlib.(ref (fun x -> (!f1 x) /. (!f2 x)))
    let ( /= ) f1 f2 = Stdlib.(f1 := (fun x -> (!f1 x) /. (!f2 x))); f1

    let ( ** ) f1 f2 = Stdlib.(ref (fun x -> (!f1 x) ** (!f2 x)))

    let inv f = Stdlib.(ref (fun x -> 1. /. (!f x)))
    let sqr f = Stdlib.(ref (fun x -> let z = !f x in z *. z))

    let sqrt f = Stdlib.(ref (fun x -> sqrt (!f x)))
    let log f = Stdlib.(ref (fun x -> log (!f x)))
    let exp f = Stdlib.(ref (fun x -> exp (!f x)))
    let sin f = Stdlib.(ref (fun x -> sin (!f x)))
    let cos f = Stdlib.(ref (fun x -> cos (!f x)))
    let tan f = Stdlib.(ref (fun x -> tan (!f x)))
    let asin f = Stdlib.(ref (fun x -> asin (!f x)))
    let acos f = Stdlib.(ref (fun x -> acos (!f x)))
    let atan f = Stdlib.(ref (fun x -> atan (!f x)))

    let ( = ) f1 f2 = Stdlib.(raise (Failure "not implemented"))
    let ( <> ) f1 f2 = Stdlib.(raise (Failure "not implemented"))
    let ( < ) f1 f2 = Stdlib.(raise (Failure "not implemented"))
    let ( <= ) f1 f2 = Stdlib.(raise (Failure "not implemented"))
    let ( > ) f1 f2 = Stdlib.(raise (Failure "not implemented"))
    let ( >= ) f1 f2 = Stdlib.(raise (Failure "not implemented"))
  end

module F = Fadbad.F(OpFun)
module FF = Fadbad.F(F)

let f_heat1D f =
  let alpha = FF.one () in
  let dfdxx = F.d (FF.deriv f 0) 0 in
  FF.(alpha * (FF.make dfdxx))

let newton_step f x0 dt =
  let open FF in
  x0 + (dt * (f x0))

let newton_integration f x0 dt (tEnd : float) =
  let rec aux result x0 t =
    if t > tEnd then
      List.rev result
    else
      let x1 = newton_step f x0 (FF.make (fun _ -> dt)) in
      aux ((FF.get x0) :: result) x1 (t +. dt)
  in
  aux [] x0 0.


let print_fun f min max nb =
  let h = (max -. min) /. (float_of_int nb) in
  let x_l = List.init nb (fun i -> (float_of_int i) *. h +. min) in
  List.iter
    (fun x -> print_endline ((string_of_float x) ^ "\t" ^ (string_of_float (f x))))
    x_l;
  print_newline (); print_newline ()

let () =
  let tEnd = 0.2 in
  let dt = 0.05 in
  let x = F.make (fun x -> x) in
  let () = F.diff x 0 1 in
  let x = FF.lift x in
  let () = FF.diff x 0 1 in
  (* let temp0 = FF.(inv (translate (sqr (x)) 1.)) in (\* does not work! *\) *)
  let temp0 = FF.sin x in
  let result = newton_integration f_heat1D temp0 dt tEnd in
  let min = -3. in
  let max = 3. in
  let nb = 100 in
  (* let () = print_fun (F.get temp0) min max nb in *)
  (* let () = print_fun (F.d_n temp0 [0;0]) min max nb in *)
  let () =
    List.iter
      (fun t -> print_fun t min max nb)
      result
  in
  ()

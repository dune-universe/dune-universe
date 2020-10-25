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

type scalar = float
type elt = float

module Make (Op : Fadbad.OpS with type scalar = scalar and type elt = elt) =
struct
  type vec = { x : Op.t; y : Op.t }

  let zero () = { x = Op.zero (); y = Op.zero () }
  let ( ++ ) a b = let open Op in { x = a.x + b.x; y = a.y + b.y }
  let scale_v v a = let open Op in { x = scale v.x a; y = scale v.y a }

  type memory = {
    mutable lastv : vec;
    mutable initial : bool;
  }

  type 'a node = {
    alloc : unit -> memory;
    reset : memory -> unit;
    step : memory -> 'a;
  }

  let make_euler v0 f =
    let alloc () = { lastv = zero (); initial = true } in
    let reset mem = mem.lastv <- zero (); mem.initial <- true in
    let step mem dt =
      let res =
        if mem.initial then begin mem.initial <- false; v0 end
        else mem.lastv ++ (scale_v (f mem.lastv) dt)
      in
      mem.lastv <- res;
      res
    in { alloc; reset; step }

  let brusselator v =
    let open Op in
    {
      x = translate ((sqr v.x) * v.y - (scale v.x 2.7)) 1.;
      y = (scale v.x 1.7) - (sqr v.x) * v.y;
    }

  let fd = ref None
  let get_fd () = match !fd with None -> assert false | Some fd -> fd

  let init_write filename =
    fd := Some (open_out filename); Printf.fprintf (get_fd()) "t\tx\ty\n"
  let write t v =
    Printf.fprintf (get_fd ()) "%f\t%f\t%f\n" t (Op.get v.x) (Op.get v.y)
  let close_write () = close_out (get_fd ()); fd := None
end

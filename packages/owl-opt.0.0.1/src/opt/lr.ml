(** learning rate module *)

type t =
  | Fix of float (** fixed learning rate *)
  | Ada of (int -> float) (** adaptive learning rate *)

(** Example learning rates
 {[
    (* fix learning rate *)
    let lr = Lr.(Fix 1E-4)

    (* expoential decay *)
    let lr = Lr.(Ada (fun k -> exp (-0.1 *. float step))
 ]} *)

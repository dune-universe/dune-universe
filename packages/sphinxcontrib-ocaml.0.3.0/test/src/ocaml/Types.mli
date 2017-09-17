module type MT = sig
  (** Doc for manifest *)
  type manifest = int
  (** Doc' for manifest *)

  (** Doc for hidden *)
  type hidden [@@autodoc.hide]
  (** Doc' for hidden *)

  (** Doc for parameter *)
  type 'a parameter
  (** Doc' for parameter *)

  (** Doc for parameters *)
  type ('a, 'b) parameters
  (** Doc' for parameters *)

  (** Doc for variances *)
  type (-'a, +'b) variances
  (** Doc' for variances *)

  (** Doc for unused_parameters *)
  type ('a, 'b) unused_parameters = [`Foo]
  (** Doc' for unused_parameters *)

  (** Doc for record *)
  type record = {
    a: int; (** Doc for record.a *)
    mutable b: float; (** Doc for record.b *)
  }
  (** Doc' for record *)

  (** Doc for variant *)
  type variant =
    | None (** Doc for None *)
    | Single of int (** Doc for Single *)
    | Several of int * float (** Doc for Several *)
    | Tuple of (int * float) (** Doc for Tuple *)
    | Record of {
      a: int; (** Doc for Record.a *)
      mutable b: float; (** Doc for Record.b *)
    } (** Doc for Record *)
  (** Doc' for variant *)

  (** Doc for recursive1 *)
  type recursive1 =
    | A1 (** Doc for A1 *)
    | B1 of recursive2 (** Doc for B1 *)

  (** Doc for recursive2 *)
  and recursive2 =
    | A2 (** Doc for A2 *)
    | B2 of recursive1 (** Doc for B2 *)

  (** Doc for open\_ *)
  type open_ = ..
  (** Doc' for open\_ *)

  (* @todo type open_ +=
    | AO
    | BO of int *)

  (* @todo type open_ += private
    | PrivateO *)

  type private_int = private int

  type private_variant = private Private

  type private_record = private {c: int}
end

include MT

(** A reference to a type: :typ:`Types.parameters`. *)

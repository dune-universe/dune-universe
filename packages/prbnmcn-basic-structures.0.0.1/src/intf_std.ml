(** Basic module types. *)

(** Totally ordered data *)
module type Ordered = sig
  type t

  val compare : t -> t -> int
end

type 'a printer = Format.formatter -> 'a -> unit

(** Pretty-printable data *)
module type Pp = sig
  type t

  val pp : t printer
end

(** Comparable, printable and hashable data *)
module type Std = sig
  type t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val pp : t printer

  val hash : t -> int
end

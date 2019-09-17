type mode =
  | Num (** Numeric mode *)
  | Alphanum (** Alphabet-numeric mode *)
  | Data (** 8-bit data mode *)
  | Kanji (** Kanji (shift-jis) mode *)
  | Structure (** Internal use only *)
  | Eci (** ECI mode *)
  | Fnc1_first (** FNC1, first position *)
  | Fnc1_second (** FNC2, second position *)
(** Encoding mode *)

module QRinput : sig
  type t
  val create : unit -> t
  val append : t -> mode -> string -> int
end

module QRcode : sig
  type t
  val encode : QRinput.t -> t
  val to_png : t -> size:int -> margin:int -> outfile:string -> unit
end

module Basic : sig
  val encode : string -> ?size:int -> ?margin:int -> string -> unit
end

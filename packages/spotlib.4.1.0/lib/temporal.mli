(** Date *)

module Year : sig 
  val is_leap : int -> bool 
  val days_of_year : int -> int 
end

module Weekday : sig
  val to_string : int -> string
  (* 0:Sun, 1:Mon, ... *)
end

module Date : sig

  type t = {
    year  : int;  (* 1998 *)
    month : int; (* 1-12 *)
    day   : int
  }
  (** Comparison should work correctly between valid dates *)

  include Mtypes.Comparable with type t := t

  val to_string : t -> string
  (** Prints [t] in ISO8601's YYYY-MM-DD format *)
  
  exception Parse_error
  
  val of_string_exn : string -> t
  val of_string : string -> (t, [> `Exn of exn]) result
  (** Parses string of ISO8601's YYYY-MM-DD format *)
  
  val yday : t -> int
  (** 1--365 or 366 *)
  
  val wday : t -> int
  (** 0--6, 0 is Sunday *)
  
  val diff : t -> t -> int
  
  val random_with_invalid : unit -> t
  (** Random value generator which may contain invalids like 2000/02/31 *)
  
  val random : unit -> t
  (** Random value generator with valid dates from 1900-01-01 to 2099-12-31.
      For checking more boudnary conditions, 1,28,29,30 and 31 are chosen
      more frequently as days than the others. 
   *)

  val random_2038 : unit -> t
  (** Same as [random] but culled so that the values are between
      1970-01-01 and 2037-12-31 
  *)
end

module Time : sig

  type t = {
    hour : int; (* 00-24, 24 is only for 24:00:00 *)
    min  : int; (* 00-59 *)
    sec  : int; (* 00-60, 60 is for leap second *)
  }
  (** Time. Comparison should work for valid values. *)

  val to_string : t -> string
  val is_valid : t -> bool
  exception Parse_error
  val of_string_exn : string -> t
  val of_string : string -> (t, [> `Exn of exn]) result
    
  val seconds_of_a_day : float (* 86400 = 24 * 60 * 60 *)

  val dsec : t -> float
  (** Seconds from the 00:00:00 of the same day.
      Named from tm_wday tm_yday *)

  val random : unit -> t
  (** it may return 24:00:00 or hh:mm:60, in addition to the normal times *)
end

module TZone : sig
  type t = [ `Minus of int * int  (* hour, min *)
           | `Plus of int * int   (* hour, min *)
           | `UTC ]
  (** [`UTC] and [`Plus (0,0)] are the same.
      [`Minus (0,0)] is invalid. 
  *)

  val to_string : [< t ] -> string
  val is_valid : t -> bool

  val random : unit -> t
end

module Datetime : sig
  type t = { date : Date.t; time : Time.t; zone : TZone.t; }

  val to_string : t -> string
  exception Parse_error
  val of_string_exn : string -> t
  val of_string : string -> (t, [> `Exn of exn]) result
  val is_valid : t -> bool

  val random : unit -> t

  val epoch : t -> float
  (** Seconds from 1970-01-01T00:00:00Z. No leap seconds included.

      Note that the epoch of EOD and the epoch of D+1T00:00:00 are the same,
      but they are different [t]s.
  *)
    
  val of_utc_tm : Unix.tm -> t
end

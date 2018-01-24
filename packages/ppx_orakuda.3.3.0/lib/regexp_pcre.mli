open Pcre

type 'a t
(** The type of regexp. The parameter is to carry the pattern's type *)

val regexp : 'a t -> Pcre.regexp
(** Forget the pattern type *)

val exec_exn :
  'a t 
  -> ?iflags:irflag 
  -> ?flags:rflag list 
  -> ?pos:int 
  -> ?callout:callout 
  -> string 
  -> 'a
(** Type enriched version of [Pcre.exec]. It may raise an exception 
    
    @raise Not_found if pattern does not match.
*)

val exec :
  'a t 
  -> ?iflags:irflag 
  -> ?flags:rflag list 
  -> ?pos:int 
  -> ?callout:callout 
  -> string 
  -> 'a option
(** Type enriched version of [Pcre.exec]. It may raise an exception 
    
    @return None if pattern does not match.
*)

module Infix : sig
  val (=~) : 
    string 
    -> ?iflags:irflag 
    -> ?flags:rflag list 
    -> ?pos:int 
    -> ?callout:callout 
    -> 'a t 
    -> 'a option
  (** Infix op version of [exec_exn]. *)

  val case : string -> [> `Error of string ]
  val (==>) : 'a t -> ('a -> 'b) -> [< `Ok of 'b | `Error of string ] -> [> `Ok of 'b | `Error of string ]
  val default : (unit -> 'b) -> [< `Ok of 'b | `Error of string ] -> 'b

  (** case <string>
      |> ( {m|pat|m} ==> fun r -> ... )
      |> ( {m|pat|m} ==> fun r -> ... )
      ...
      |> default (fun () -> ... )
  *) 
    
end

val replace :
  'a t ->
  templ:string ->
  ?iflags:irflag ->
  ?flags:rflag list ->
  ?pos:int -> ?callout:callout 
  -> string -> string
(** Type enriched version of [Pcre.replace] *)

val replace_first :
  'a t ->
  templ:string ->
  ?iflags:irflag ->
  ?flags:rflag list ->
  ?pos:int -> ?callout:callout 
  -> string -> string
(** Type enriched version of [Pcre.replace_first] *)

val substitute_substrings :
  ('a -> string) ->'a t ->
  ?iflags:irflag -> ?flags:rflag list -> ?pos:int -> ?callout:callout
  -> string -> string
(** Type enriched version of [Pcre.substitute_substrings] *)

val substitute_substrings_first :
  ('a -> string) -> 'a t ->
  ?iflags:irflag -> ?flags:rflag list -> ?pos:int -> ?callout:callout 
  -> string -> string
(** Type enriched version of [Pcre.substitute_substrings_first] *)

val split :
  'a t ->
  ?iflags:irflag ->
  ?flags:rflag list ->
  ?pos:int -> ?max:int -> ?callout:callout 
  -> string -> string list
(** Same as [Pcre.split] *)

val build_case : 'a t -> ('a -> 'b) -> string -> 'b option
(** [build_case t f s], if [s] matches with [t], runs [fun x -> Some (f x)] over the result.
    
    @return [None] if not match.
*)

val build_cases : 
  ?default: (unit -> 'a) 
  -> (string -> 'a option) list 
  -> string 
  -> 'a
(** Composition of cases created by [build_case] *)

(** You have to [open Ppx_orakuda.Regexp.Literal] to use [{m|regexp|m}] and [{s|regexp|s}] for [Regexp.t] *)
module Literal : sig
  module OrakudaRegexpInternal : sig

    (** group object to access matched groups *)
    class virtual group : 
      (string * int) list (* named groups *)
      -> left:  string (* $` *)
      -> right: string (* $' *)
      -> last:  string option (* $+ *)
      -> string option array (* groups *) 
      -> object
        method _groups           : string array
        method _groups_opt       : string option array
        method _named_groups     : (string * string) list
        method _named_groups_opt : (string * string option) list
        method _group            : int -> string
        method _group_opt        : int -> string option
        method _unsafe_group     : int -> string
        method _unsafe_group_opt : int -> string option
        method _named_group      : string -> string
        method _named_group_opt  : string -> string option (*+ It still may raise an exception if the name does not exist *)
        method _left     : string
        method _right    : string
        method _last     : string
        method _last_opt : string option
      end
      
    val create : 
      string 
      -> flags: cflag list
      -> (left: string -> right: string -> last: string option -> string option array -> 'a) 
      -> 'a t
    
    val replace :
      'a t ->
      templ:string ->
      ?iflags:irflag ->
      ?flags:rflag list ->
      ?pos:int -> ?callout:callout 
      -> string -> string
    (** Type enriched version of [Pcre.replace] *)
    
    val replace_first :
      'a t ->
      templ:string ->
      ?iflags:irflag ->
      ?flags:rflag list ->
      ?pos:int -> ?callout:callout 
      -> string -> string
    (** Type enriched version of [Pcre.replace_first] *)
    
    val substitute_substrings :
      ('a -> string) ->'a t ->
      ?iflags:irflag -> ?flags:rflag list -> ?pos:int -> ?callout:callout
      -> string -> string
    (** Type enriched version of [Pcre.substitute_substrings] *)
    
    val substitute_substrings_first :
      ('a -> string) -> 'a t ->
      ?iflags:irflag -> ?flags:rflag list -> ?pos:int -> ?callout:callout 
      -> string -> string
    (** Type enriched version of [Pcre.substitute_substrings_first] *)
  end
end

(** Some functions for processing tokens from
    the BNF grammars that appear in the RFCs *)

module RFC2045 : sig
  module Token : sig
    include Mimestring.S
    (** True if the string doesn't need to be quoted *)
    val is_valid          : string -> bool
    (* Quotes a string if necessary *)
    val is_valid_or_quote : string -> string
  end
end

(*
   module RFC2822 : sig
   (** Adds EOL and WSP to a string so it can be safely inserted as a header
   field.
   Currently it doesn't ensure that lines are only 998
   characters long; thats the user's responsibility. It might be an
   interesting feature to add, though.
 *)
   val fold : string -> string

   (** Removes meaningless EOL and WSP in header field contents *)
   val unfold : string -> string
   end

*)

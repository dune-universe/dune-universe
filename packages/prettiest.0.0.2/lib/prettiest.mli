module type Width = sig
  val width : int
end

module type S = sig
  (** The type of documents. *)
  type t

  include Width

  (** Turn a string without newlines into a document. *)
  val text : string -> t

  (** Add newline. *)
  val flush : t -> t

  (** Concatenate documents horisontally without extra spacing. *)
  val hcat : t list -> t

  (** Concatenate documents horisontally with one space between each. *)
  val hsep : t list -> t

  (** Concatenate documents vertically. *)
  val vcat : t list -> t

  (** Render document. Returns [None] if the document does not fit. *)
  val render : t -> string option

  (** Allow the printer to choose which of the given document to render.
      Choices must be semantically equivalent.
  *)
  val choice : t list -> t

  (** Unit for the choice function. *)
  val fail : t

  (** The empty document. *)
  val empty : t

  (** Indent the given document any number of spaces. *)
  val nest : int -> t -> t

  (** Either concatenate document with hsep or vcat *)
  val sep : t list -> t

  (** Append [sep] to every element except the last. *)
  val intersperse : sep:t -> t list -> t list

  (** Same as {!intersperse} but map [f] across elements at the same time. *)
  val intersperse_map : f:('a -> t) -> sep:t -> 'a list -> t list

  module Infix : sig
    (** Prefix version of {!text}. *)
    val (!^) : string -> t

    (** Infix version of {!hcat}. *)
    val (<>) : t -> t -> t

    (** Infix version of {!vcat}. *)
    val ($$) : t -> t -> t

    (** Infix version of {!choice}. *)
    val (<|>) : t -> t -> t

    (** Infix version of {!hsep}. *)
    val (<+>) : t -> t -> t

    (** Either {!(<+>)} or {!($$)}. *)
    val (</>) : t -> t -> t

    (** Either {!(<+>)} or {!($$)} indented 2 spaces. *)
    val (<//>) : t -> t -> t
  end

  module Characters : sig
    val qmark : t
    val bang : t
    val at : t
    val sharp : t
    val dollar : t
    val percent : t
    val caret : t
    val ampersand : t
    val star : t

    val comma : t
    val dot : t
    val bar : t
    val colon : t
    val scolon : t
    val equals : t

    val plus : t
    val minus : t
    val underscore : t
    val tilde : t

    val squote : t
    val dquote : t
    val bquote : t

    val slash : t
    val bslash : t

    val lt : t
    val gt : t
    val lbrack : t
    val rbrack : t
    val lbrace : t
    val rbrace : t
    val lparen : t
    val rparen : t

    val space : t
  end
end

module Make (W : Width) : S

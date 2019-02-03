(*
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** An "easy" interface on top of the [Xmlm] library.
    
    This version provides more convenient (but less flexible) input and output
    functions that go to and from {!string} values.  This avoids the need to write signal code,
    which is useful for quick scripts that manipulate XML.
   
    More advanced users should go straight to the [Xmlm] library and use it
    directly, rather than be saddled with the Ezxmlm interface below.
  *)

(**  {2 Basic types } *)

(** The type of a single XML tag *)
type node = ('a Xmlm.frag as 'a) Xmlm.frag

(** The type of a list of XML tags *)
type nodes = node list

(** Raised by the query combinators *)
exception Tag_not_found of string

(**  {2 Reading XML values } *)

(** Read an XML document from an [in_channel] *)
val from_channel : in_channel -> Xmlm.dtd * nodes

(** Read an XML document directly from a [string] *)
val from_string : string -> Xmlm.dtd * nodes

(** Low-level function to read directly from an [Xmlm] input source *)
val from_input : Xmlm.input -> Xmlm.dtd * node

(** {2 Writing XML values } *)

(** Write an XML document to an [out_channel] *)
val to_channel : out_channel -> ?decl:bool -> Xmlm.dtd -> nodes -> unit

(** Write an XML document to a [string].  This goes via an intermediate
    [Buffer] and so may be slow on large documents. *)
val to_string : ?decl:bool -> ?dtd:string -> nodes -> string

(** Low-level function to write directly to an [Xmlm] output source *)
val to_output : Xmlm.output -> Xmlm.dtd * node -> unit

(** [pp fmt x] will write a string representation of the XML document [x]
    to the formatter [fmt]. *)
val pp : Format.formatter -> nodes -> unit [@@ocaml.toplevel_printer]

(** {2 Attribute handling} *)

(** Given some selected attributes and nodes (usually from [members_with_attr])
    return the ones that match the [class] and [value] supplied. *)
val filter_attrs : string -> string ->
    (Xmlm.attribute list * nodes) list -> (Xmlm.attribute list * nodes) list

(** Given some selected attributes and nodes (usually from [members_with_attr])
    return the first that matches the [class] and [value] supplied.
    Raises [Not_found] if nothing matches. *)
val filter_attr : string -> string ->
    (Xmlm.attribute list * nodes) list -> (Xmlm.attribute list * nodes)

(** [mem_attr name value attrs] returns true if the [name] key is
    with value [value] is present in the [attrs] attribute list. *)
val mem_attr : string -> string -> Xmlm.attribute list -> bool

(** [get_attr name attrs] returns the value associated with key
    [name] in the [attrs] attribute list.
    Raised [Not_found] if the attribute is not present. *)
val get_attr : string -> Xmlm.attribute list -> string

(** {2 Selectors and utility functions } *)

(** [pick_tags tag attr value] selects all the child nodes that
    match the [tag] name and contain an attribute with name [tag]
    and [value]. *)
val pick_tags : string -> string -> string -> nodes -> nodes

(** [pick_tag tag attr value] selects the first child node that
    matches the [tag] name and contain an attribute with name [tag]
    and [value].
    Raises {!Not_found} if no such node exists. *)
val pick_tag : string -> string -> string -> nodes -> node

(** Return the first tag in the list of nodes.
    Raises {!Not_found} if the nodes are empty *)
val hd : nodes -> node

(** Return all the tags but the first one in a list of nodes.
    Returns an empty list if the list is empty. *)
val tl : nodes -> nodes

(** Make a tag given a [tag] name and body attributes and nodes *)
val make_tag : string -> Xmlm.attribute list * nodes -> node

(** Convert a list of [`Data] fragments to a human-readable string.  Any elements
    within the list are ignored, and multiple [`Data] fragments are concatenated. *)
val data_to_string : nodes -> string

(** Extracts the immediate subnodes that match the given [tag] name and return
    a tuple of the attributes associated with that tag and its child nodes. *)
val members_with_attr : string -> nodes -> (Xmlm.attribute list * nodes) list

(** Extracts the immediate subnodes that match the given [tag] name, and only return
    the contents of those tags (ignoring the attributes, which can be retrieved via
    the [members_with_attr] function *)
val members : string -> nodes -> nodes list

(** Extracts the first subnode that match the given [tag] name, and raises
    [Tag_not_found] if it can't find it. *)
val member_with_attr : string -> nodes -> Xmlm.attribute list * nodes

(** Extracts the first subnode that match the given [tag] name, and raises
    [Tag_not_found] if it can't find it. Only the contents of the tag are
    returned (ignoring the attributes, which can be retrieved via the
    [member_with_attr] function instead *)
val member : string -> nodes -> nodes

(** [has_member tag subnodes] returns true if the given  [tag] name is
    present among the subnodes and false if it can't find it. *)
val has_member : string -> nodes -> bool

(** Traverses XML nodes and applies [f] to any tags that match the [tag] parameter.
    The result of the transformations is returned as a new set of nodes. *)
val filter_map : tag:string -> f:(Xmlm.attribute list -> nodes -> nodes) -> nodes -> nodes

(** Traverses XML nodes and applies [f] to any tags that match the [tag] parameter. *)
val filter_iter : tag:string -> f:(Xmlm.attribute list -> nodes -> unit) -> nodes -> unit


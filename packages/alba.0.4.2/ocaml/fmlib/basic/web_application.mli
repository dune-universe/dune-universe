(** Enviroment for Web Applications. *)

module type DECODER =
sig
    type _ t

    val return: 'msg -> 'msg t

    val float:  float  t
    val int:    int    t
    val string: string t
    val bool:   bool   t

    val field:  string -> 'msg t -> 'msg t
    val map: ('a -> 'b) -> 'a t -> 'b t
end


module type ENCODER =
sig
    type t

    val string: string -> t
    val bool:   bool -> t
    val object_: (string * t) list -> t
end




module type WEB_APPLICATION =
sig
    type _ decoder

    type encoder


    module Attribute:
    sig
        type 'msg t =
        | Style of string * string
        | Attribute of string * string
        | Property of string * encoder
        | On of string * 'msg decoder
    end


    module Dom:
    sig
        type 'msg t =
        | Text of string
        | Node of string * 'msg Attribute.t list * 'msg t list
    end


    module Command:
    sig
        type 'msg t =
        | None
        | Batch of 'msg t list

        | Http of
            string * string * string
            *
            ((string,int) result -> 'msg)
            (** [Http (type, url, data, handler)]

                The type is GET, POST, HEAD, etc.

                [data] will be sent as part of the request (makes sense only for
                POST and related request types).

                [handler] must be able to react to the response text or the
                status in case an error occurs (the famous 404 for "page not
                found").
            *)
    end


    module Subscription:
    sig
        type 'msg t =
        | None
        | Batch of 'msg t list

        | Root of string * 'msg decoder
        (** [Root (event_name, handler)] for events arriving on the root element
        of the application. *)
    end
end




module type BROWSER =
sig
    module Decoder: DECODER

    module Encoder: ENCODER

    module Make:
    functor (Vapp: WEB_APPLICATION
                    with type 'msg decoder = 'msg Decoder.t
                    and  type encoder = Encoder.t)
    ->
    sig
        val sandbox:
            'model
            -> ('model -> 'msg Vapp.Dom.t)
            -> ('msg -> 'model -> 'model)
            -> unit


        val element:
            'a Decoder.t
            -> ('a -> 'model * 'msg Vapp.Command.t)
            -> ('model -> 'msg Vapp.Dom.t)
            -> ('msg -> 'model -> 'model * 'msg Vapp.Command.t)
            -> ('model -> 'msg Vapp.Subscription.t)
            -> unit
    end
end




module Make (Browser: BROWSER):
sig
    type 'msg decoder = 'msg Browser.Decoder.t

    type encoder = Browser.Encoder.t


    module Attribute:
    sig
        type 'msg t =
        | Style of string * string
        | Attribute of string * string
        | Property of string * encoder
        | On of string * 'msg decoder

        val style: string -> string -> 'msg t

        val attribute: string -> string -> 'msg t

        val property: string -> encoder -> 'msg t

        val on: string -> 'msg decoder -> 'msg t



        val string_property: string -> string -> 'msg t
        (** [string_property name value] *)

        val bool_property: string -> bool -> 'msg t
        (** [bool_property name value] *)

        val placeholder: string -> 'msg t

        val value: string -> 'msg t
        (** Value property. Used in input elements like 'input', 'textarea'. *)

        val checked: bool -> 'msg t
        (** Indicate, if a checkbox is checked. *)

        val type_: string -> 'msg t
        (** Set the type attribute of an input element. Legal values "text"
        (default), "password", "checkbox", "radio", "color", "button", "file"
        etc. *)


        val class_: string -> 'msg t
        (** Set the class attribute of the element. *)


        val onClick: 'msg -> 'msg t

        val onDoubleClick: 'msg -> 'msg t

        val onMouseDown: 'msg -> 'msg t

        val onMouseUp: 'msg -> 'msg t

        val onMouseEnter: 'msg -> 'msg t

        val onMouseLeave: 'msg -> 'msg t

        val onMouseOver: 'msg -> 'msg t

        val onMouseOut: 'msg -> 'msg t



        val onKeyDown: (string -> 'msg) -> 'msg t

        val onKeyUp: (string -> 'msg) -> 'msg t


        val onInput: (string -> 'msg) -> 'msg t
        (** React on input of an input element like 'input', 'textarea', etc. *)

        val onCheck: (bool -> 'msg) -> 'msg t
        (** React on a checkbox click. *)
    end


    module Dom:
    sig
        type 'msg t =
        | Text of string
        | Node of string * 'msg Attribute.t list * 'msg t list


        type 'msg attributes = 'msg Attribute.t list
        type 'msg children   = 'msg t list

        val text: string -> 'msg t

        val node: string -> 'msg attributes -> 'msg children -> 'msg t

        val div: 'msg attributes -> 'msg children -> 'msg t

        val span: 'msg attributes -> 'msg children -> 'msg t

        val pre: 'msg attributes -> 'msg children -> 'msg t

        val p: 'msg attributes -> 'msg children -> 'msg t

        val ol: 'msg attributes -> 'msg children -> 'msg t

        val ul: 'msg attributes -> 'msg children -> 'msg t

        val li: 'msg attributes -> 'msg children -> 'msg t

        val h1: 'msg attributes -> 'msg children -> 'msg t

        val h2: 'msg attributes -> 'msg children -> 'msg t

        val h3: 'msg attributes -> 'msg children -> 'msg t

        val h4: 'msg attributes -> 'msg children -> 'msg t

        val h5: 'msg attributes -> 'msg children -> 'msg t

        val h6: 'msg attributes -> 'msg children -> 'msg t


        val b: 'msg attributes -> 'msg children -> 'msg t
        (** Bold text *)

        val i: 'msg attributes -> 'msg children -> 'msg t
        (** Italic text *)

        val strong: 'msg attributes -> 'msg children -> 'msg t
        (** Important text *)



        val button: 'msg attributes -> 'msg children -> 'msg t

        val input: 'msg attributes -> 'msg children -> 'msg t

        val textarea: 'msg attributes -> 'msg children -> 'msg t
    end


    module Command:
    sig
        type 'msg t =
        | None
        | Batch of 'msg t list

        | Http of
            string  (* type: GET, POST, ... *)
            *
            string  (* url *)
            *
            string  (* data to be sent with the request *)
            *
            ((string,int) result -> 'msg)
              (* response text or status e.g. 404 for not found *)


        val http_get: string -> ((string,int) result -> 'msg) -> 'msg t
        (** [http_get url handler] Make a http get request to [url] and handle
        the response with [handler].
        *)
    end


    module Subscription:
    sig
        type 'msg t =
        | None
        | Batch of 'msg t list

        | Root of string * 'msg decoder (* events on the root element *)
    end
end

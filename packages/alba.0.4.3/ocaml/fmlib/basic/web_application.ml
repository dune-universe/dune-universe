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
            string  (* type: GET, POST, ... *)
            *
            string  (* url *)
            *
            string  (* data to be sent with the request *)
            *
            ((string,int) result -> 'msg)
              (* response text or status e.g. 404 for not found *)
    end


    module Subscription:
    sig
        type 'msg t =
        | None
        | Batch of 'msg t list

        | Root of string * 'msg decoder (* events on the root element *)
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




module Make (Browser: BROWSER) =
struct
    module Decoder = Browser.Decoder
    module Encoder = Browser.Encoder

    type 'msg decoder = 'msg Decoder.t

    type encoder = Encoder.t


    module Attribute =
    struct
        type 'msg t =
        | Style of string * string
        | Attribute of string * string
        | Property of string * encoder
        | On of string * 'msg decoder

        let style (name: string) (value: string): 'msg t =
            Style (name, value)


        let attribute (name: string) (value: string): 'msg t =
            Attribute (name, value)


        let property (name: string) (value: encoder): 'msg t =
            Property (name, value)

        let on (name: string) (handler: 'msg decoder): 'msg t =
            On (name, handler)




        let string_property (name: string) (value: string): 'msg t =
            property name (Encoder.string value)

        let bool_property (name: string) (value: bool): 'msg t =
            property name (Encoder.bool value)

        let placeholder (value: string): 'msg t =
            attribute "placeholder" value

        let value (value: string): 'msg t =
            string_property "value" value

        let checked (value: bool): 'msg t =
            bool_property "checked" value

        let type_ (value: string): 'msg t =
            attribute "type" value

        let class_ (value: string): 'msg t =
            attribute "class" value


        let onClick (msg: 'msg): 'msg t =
            on "click" (Decoder.return msg)

        let onDoubleClick (msg: 'msg): 'msg t =
            on "doubleclick" (Decoder.return msg)

        let onMouseDown (msg: 'msg): 'msg t =
            on "mousedown" (Decoder.return msg)

        let onMouseUp (msg: 'msg): 'msg t =
            on "mouseup" (Decoder.return msg)

        let onMouseEnter (msg: 'msg): 'msg t =
            on "mouseenter" (Decoder.return msg)

        let onMouseLeave (msg: 'msg): 'msg t =
            on "mouseleave" (Decoder.return msg)

        let onMouseOver (msg: 'msg): 'msg t =
            on "mouseover" (Decoder.return msg)

        let onMouseOut (msg: 'msg): 'msg t =
            on "mouseout" (Decoder.return msg)




        let onKeyDown (f: string -> 'msg): 'msg t =
            on
                "keydown"
                Decoder.(map f (field "key" string))

        let onKeyUp (f: string -> 'msg): 'msg t =
            on
                "keyup"
                Decoder.(map f (field "key" string))




        let onInput (f: string -> 'msg): 'msg t =
            on
                "input"
                Decoder.(
                    field
                        "target"
                        (map
                            f
                            (field "value" string)))


        let onCheck (f: bool -> 'msg): 'msg t =
            on
                "click"
                Decoder.(
                    field
                        "target"
                        (map f
                             (field "checked" bool)))
    end



    module Dom =
    struct
        type 'msg t =
        | Text of string
        | Node of string * 'msg Attribute.t list * 'msg t list


        type 'msg attributes = 'msg Attribute.t list
        type 'msg children   = 'msg t list


        let text (s: string): 'msg t =
            Text s


        let node
            (tag: string)
            (attrs: 'msg attributes)
            (children: 'msg children)
            : 'msg t
            =
            Node (tag, attrs, children)


        let div (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "div" attrs children


        let span (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "span" attrs children



        let pre (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "pre" attrs children


        let p (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "p" attrs children


        let ol (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "ol" attrs children


        let ul (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "ul" attrs children


        let li (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "li" attrs children


        let h1 (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "h1" attrs children


        let h2 (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "h2" attrs children


        let h3 (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "h3" attrs children


        let h4 (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "h4" attrs children


        let h5 (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "h5" attrs children


        let h6 (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "h6" attrs children


        let b (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "b" attrs children

        let i (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "i" attrs children

        let strong (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "strong" attrs children





        let button (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "button" attrs children


        let input (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "input" attrs children

        let textarea (attrs: 'msg attributes) (children: 'msg children): 'msg t =
            node "textarea" attrs children
    end


    module Command =
    struct
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

        let http_get
            (url: string)
            (handler: (string,int) result -> 'msg)
            : 'msg t
            =
            Http ("GET", url, "", handler)
    end


    module Subscription =
    struct
        type 'msg t =
        | None
        | Batch of 'msg t list

        | Root of string * 'msg decoder (* events on the root element *)
    end
end


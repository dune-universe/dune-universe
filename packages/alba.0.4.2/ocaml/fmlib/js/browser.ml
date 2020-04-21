open Js_of_ocaml
open Fmlib
open Common





(* Javascript interface *)


type js_string = Js.js_string

type js_any = Js.Unsafe.top

let raise_js (message: string): 'a =
    Js.(
        raise_js_error
            (new%js error_constr
                (Js.string message)))





class type primitive_number =
object
    method valueOf_float: unit -> float Js.meth
end



class type event =
object
    method stopPropagation: unit Js.meth
    method preventDefault:  unit Js.meth
end




class type style =
object
    method setProperty:    js_string Js.t -> js_string Js.t -> unit Js.meth
    method removeProperty: js_string Js.t -> unit Js.meth

end



class type eventTarget =
object
    method addEventListener:
        js_string Js.t -> ('a Js.t -> unit) Js.callback -> unit Js.meth
        (* Do not use on text nodes! *)

    method removeEventListener:
        js_string Js.t -> ('a Js.t -> unit) Js.callback -> unit Js.meth
        (* Do not use on text nodes! *)
end




class type node =
object
    inherit eventTarget

    method nodeType: int Js.readonly_prop

    method parentNode:  node Js.t Js.Opt.t Js.readonly_prop


    (* The following methods can be used only on elements *)

    method innerHTML:   js_string Js.t Js.prop

    method firstChild:  node Js.t Js.Opt.t Js.readonly_prop

    method lastChild:   node Js.t Js.Opt.t Js.readonly_prop

    method nextSibling: node Js.t Js.Opt.t Js.readonly_prop

    method style: style Js.t Js.readonly_prop

    method setAttribute: js_string Js.t -> js_string Js.t -> unit Js.meth

    method removeAttribute: js_string Js.t -> unit Js.meth

    method appendChild: node Js.t -> unit Js.meth

    method removeChild: node Js.t -> unit Js.meth

    method replaceChild: node Js.t -> node Js.t -> unit Js.meth
    (* [parent##replaceChild newChild oldChild] *)

    method focus: unit -> unit Js.meth

    method tagName: js_string Js.t Js.readonly_prop
end





class type document =
object
    inherit eventTarget

    method body: node Js.t Js.readonly_prop

    method _URL: js_string Js.t Js.readonly_prop

    method title: js_string Js.t Js.prop

    method createElement:  js_string Js.t -> node Js.t Js.meth

    method createTextNode: js_string Js.t -> node Js.t Js.meth

    method getElementById: js_string Js.t -> node Js.t Js.Opt.t Js.meth

    method hasFocus: unit -> bool Js.t Js.meth

    method activeElement: node Js.t Js.Opt.t Js.readonly_prop
end






class type history =
object
    method pushState:
        'a Js.t Js.Opt.t -> js_string Js.t -> js_string Js.t -> unit Js.meth
        (* [pushState state title path] [title] is currently not used, [path]
        must not change origin. *)
end





class type window =
object
    inherit eventTarget

    method history: history Js.t Js.readonly_prop

    method document: document Js.t Js.readonly_prop

    method requestAnimationFrame:
        (float -> unit) Js.callback -> unit Js.meth
end



class type xmlHttpRequest =
object
    inherit eventTarget

    method open_: js_string Js.t -> js_string Js.t -> unit Js.meth

    method send: js_string Js.t -> unit Js.meth

    method readyState: int Js.readonly_prop
    (*
        0: request not initialized
        1: open has been called
        2: send has been called
        3: loading
        4: complete
    *)

    method status: int Js.readonly_prop
    (* 200: Ok, 403: forbidden, 404: not found. *)

    method statusText: js_string Js.t Js.readonly_prop

    method responseText: js_string Js.t Js.readonly_prop
end



let get_time (): float =
    Js.Unsafe.global##.Date##now ()

let get_window (): window Js.t =
    Js.Unsafe.global


let new_http_request (): xmlHttpRequest Js.t =
    let request = Js.Unsafe.global##._XMLHttpRequest
    in
    new%js request





module Decoder =
struct
    type path_element =
    | Name of string
    | Index of int

    type error =
    | Int
    | Bool
    | String
    | Field of string

    type problem = error * path_element list


    module Result = Monad.Result (struct type t = problem end)


    type 'a t =
        Common.Void.t Js.t -> 'a option

    let return (a: 'a): 'a t =
        fun _ -> Some a


    let typeof (js: 'a Js.t): string =
        Js.to_string (Js.typeof js)


    let float: float t =
        fun js ->
            if typeof js = "number" then
                Some ( (Js.Unsafe.coerce js)##valueOf_float () )
            else
                None

    let int: int t =
        fun js ->
            Option.(
                float js >>= fun fl ->
                if Float.is_integer fl then
                    Some (Float.to_int fl)
                else
                    None
            )

    let string: string t =
        fun js ->
            if Js.to_string (Js.typeof js) = "string" then
                Some ( Js.to_string (Js.Unsafe.coerce js) )
            else
                None

    let bool: bool t =
        fun js ->
            if Js.to_string (Js.typeof js) = "boolean" then
                Some ( Js.to_bool (Js.Unsafe.coerce js) )
            else
                None

    let field (name: string) (decode: 'a t): 'a t =
        fun obj ->
            Option.(
                Js.Optdef.to_option (Js.Unsafe.get obj (Js.string name))
                >>=
                decode
            )


    let map (f: 'a -> 'b) (decode: 'a t): 'b t =
        fun obj -> Option.( map f (decode obj) )

    let (>>=) (d: 'a t) (f: 'a -> 'b t): 'b t =
        fun obj ->
            Option.( d obj >>= fun a -> f a obj )
end





module Encoder =
struct
    type t = Common.Void.t Js.t

    let string (s: string): t =
        Js.(Unsafe.coerce (string s))

    let bool (b: bool): t =
        Js.(Unsafe.coerce (bool b))

    let object_ (_: (string * t) list): t =
        assert false (* nyi *)
end





module Make
    (App: Web_application.WEB_APPLICATION
            with type 'a decoder = 'a Decoder.t
            and  type encoder = Encoder.t)
=
struct
    module Vdom = App.Dom
    module Command = App.Command
    module Subscription = App.Subscription

    type handler = (Void.t Js.t -> unit) Js.callback

    module Tree =
    struct
        module Attributes =
        struct
            type 'msg t = {
                mutable styles: string String_map.t;
                mutable attributes: string String_map.t;
                mutable properties: Encoder.t String_map.t;
                mutable handlers: handler String_map.t;
            }



            let make
                (make_handler: 'msg Decoder.t -> handler)
                (attributes: 'msg App.Attribute.t list)
                : 'msg t
                =
                let module VA = App.Attribute in
                List.fold_left
                    (fun attributes attr ->
                        match attr with
                        | VA.Style (name, value) ->
                            {attributes with
                                styles =
                                    String_map.add
                                        name
                                        value
                                        attributes.styles}

                        | VA.Attribute (name, value) ->
                            {attributes with
                                attributes =
                                    String_map.add
                                        name
                                        value
                                        attributes.attributes}

                        | VA.Property (name, value) ->
                            {attributes with
                                properties =
                                    String_map.add
                                        name
                                        value
                                        attributes.properties}

                        | VA.On (name, decode) ->
                            {attributes with
                                handlers =
                                    String_map.add
                                        name
                                        (make_handler decode)
                                        attributes.handlers}
                    )
                    {
                        styles = String_map.empty;
                        attributes = String_map.empty;
                        properties = String_map.empty;
                        handlers   = String_map.empty;
                    }
                    attributes


            let set_style
                (node: node Js.t)
                (name: string)
                (value: string)
                : unit
                =
                node##.style##setProperty
                    (Js.string name)
                    (Js.string value)


            let remove_style
                (node: node Js.t)
                (name: string)
                (_:    string)
                : unit
                =
                node##.style##removeProperty (Js.string name)


            let set_property
                (node: node Js.t)
                (name: string)
                (value: Encoder.t)
                : unit
                =
                Js.Unsafe.set node (Js.string name) value


            let remove_property
                (node: node Js.t)
                (name: string)
                (_:    Encoder.t)
                : unit
                =
                Js.Unsafe.delete node (Js.string name)


            let set_attribute
                (node: node Js.t)
                (name: string)
                (value: string)
                : unit
                =
                node##setAttribute (Js.string name) (Js.string value)


            let remove_attribute
                (node: node Js.t)
                (name: string)
                (_:    string)
                : unit
                =
                node##removeAttribute (Js.string name)



            let add_handler
                (node: node Js.t)
                (name: string)
                (value: handler)
                : unit
                =
                node##addEventListener (Js.string name) value


            let remove_handler
                (node: node Js.t)
                (name: string)
                (value: handler)
                : unit
                =
                node##removeEventListener (Js.string name) value


            let update_style
                (node: node Js.t)
                (name: string)
                (new_value: string)
                (value: string)
                =
                if new_value <> value then
                    set_style node name new_value


            let update_property
                (node: node Js.t)
                (name: string)
                (new_value: Encoder.t)
                (_: Encoder.t)
                =
                set_property node name new_value


            let update_attribute
                (node: node Js.t)
                (name: string)
                (new_value: string)
                (value: string)
                =
                if new_value <> value then
                    set_attribute node name new_value


            let update_handler
                (node: node Js.t)
                (name: string)
                (new_value: handler)
                (value: handler)
                =
                remove_handler node name value;
                add_handler node name new_value



            let add (attributes: 'msg t) (node: node Js.t): unit =
                (* add [attributes] to [node] *)
                String_map.fold
                    (fun name value _ -> set_style node name value)
                    attributes.styles
                    ();
                String_map.fold
                    (fun name value _ -> set_property node name value)
                    attributes.properties
                    ();
                String_map.fold
                    (fun name value _ -> set_attribute node name value)
                    attributes.attributes
                    ();
                String_map.fold
                    (fun name value _ -> add_handler node name value)
                    attributes.handlers
                    ()


            let update
                (new_attributes: 'msg t)
                (attributes: 'msg t)
                (node: node Js.t)
                : unit
                =
                let rec update news olds add upd rem =
                    match news, olds with
                    | [], [] ->
                        ()
                    | (name_new, value_new) :: news1, (name, value) :: olds1 ->
                        let cmp = String.compare name_new name in
                        if cmp = -1 then
                        (
                            add name_new value_new;
                            update news1 olds add upd rem
                        )
                        else if cmp = 0 then
                        (
                            upd name value_new value;
                            update news1 olds1 add upd rem
                        )
                        else
                        (
                            rem name value;
                            update news olds1 add upd rem
                        )
                    | [], _ ->
                        List.iter
                            (fun (name, value) -> rem name value)
                            olds
                    | _, [] ->
                        List.iter
                            (fun (name, value) -> add name value)
                            news
                in
                update
                    (String_map.bindings new_attributes.styles)
                    (String_map.bindings attributes.styles)
                    (set_style node)
                    (update_style node)
                    (remove_style node);
                update
                    (String_map.bindings new_attributes.properties)
                    (String_map.bindings attributes.properties)
                    (set_property node)
                    (update_property node)
                    (remove_property node);
                update
                    (String_map.bindings new_attributes.attributes)
                    (String_map.bindings attributes.attributes)
                    (set_attribute node)
                    (update_attribute node)
                    (remove_attribute node);
                update
                    (String_map.bindings new_attributes.handlers)
                    (String_map.bindings attributes.handlers)
                    (add_handler node)
                    (update_handler node)
                    (remove_handler node)
        end (* Attributes *)




        (* Start Tree main part *)

        type 'msg t =
            {   node: node Js.t;
                data: 'msg t0
            }

        and 'msg t0 =
        | Text of string
        | Node of string * 'msg Attributes.t ref * 'msg t list ref


        let node (tree: 'msg t): node Js.t =
            tree.node

        let data (tree: 'msg t): 'msg t0 =
            tree.data

        let make_text (text: string) (node: node Js.t): 'msg t =
            { node; data = Text text }

        let fill_node
            (tag: string)
            (nd: node Js.t)
            (attributes: 'msg Attributes.t)
            (children: 'msg t list)
            : 'msg t
            =
            ignore (
                List.fold_left
                    (fun () child ->
                        nd##appendChild (node child))
                    ()
                    children
            );
            Attributes.add attributes nd;
            { node = nd; data = Node (tag, ref attributes, ref children) }


        let rec update
            (vdom: 'msg Vdom.t)
            (tree: 'msg t)
            (make_tree: 'msg Vdom.t -> 'msg t)
            (make_handler: 'msg Decoder.t -> handler)
            : 'msg t option
            =
            let update vdom tree =
                update vdom tree make_tree make_handler
            in
            match vdom, (data tree) with
            | Vdom.Text text,
              Text old_text when text = old_text
              ->
                None

            | Vdom.Node (tag, attributes, children),
              Node (old_tag, old_attributes, old_children)
              when tag = old_tag
              ->
                (* update attributes *)
                (
                    let open Attributes in
                    let attributes = make make_handler attributes in
                    update
                        attributes
                        !old_attributes
                        (node tree);
                    old_attributes := attributes
                );
                old_children :=
                (
                    let rec update_cs parent children old_children =
                        match children, old_children with
                        | [], [] ->
                            []

                        | [], _ ->
                            (* vdom has no more children, but actual dom
                            still has. Remove the remaining children in the
                            actual dom. *)
                            let parent = node tree in
                            List.iter
                                (fun child ->
                                    parent##removeChild (node child))
                                old_children;
                            []

                        | _, [] ->
                            (* vdom has more children than the actual dom.
                            *)
                            let children_rev =
                                let parent = node tree in
                                List.fold_left
                                    (fun cs child ->
                                        let child = make_tree child in
                                        parent##appendChild (node child);
                                        child :: cs)
                                    [] children
                            in
                            List.rev children_rev

                        | child :: children, old_child :: old_children ->
                            (
                                match update child old_child with
                                | None ->
                                    old_child
                                    ::
                                    update_cs parent children old_children
                                | Some child ->
                                    (node parent)##replaceChild
                                        (node child)
                                        (node old_child);
                                    child
                                    ::
                                    update_cs parent children old_children
                            )
                        in
                        update_cs tree children !old_children
                );
                None

            | _, _ ->
                Some (make_tree vdom)
    end (* Tree *)




    type ('model,'msg) t = {
        window:   window Js.t;
        root:     node Js.t;
        view:     'model -> 'msg Vdom.t;
        update:   'msg -> 'model -> 'model * 'msg Command.t;
        subscription: 'model -> 'msg Subscription.t;
        mutable model: 'model;
        mutable dirty: bool;
        mutable tree: 'msg Tree.t option;
    }


    let make
        (window: window Js.t)
        (root: node Js.t)
        (model: 'model)
        (view: 'model -> 'msg Vdom.t)
        (update: 'msg -> 'model -> 'model * 'msg Command.t)
        (subscription: 'model -> 'msg Subscription.t)
        : ('model,'msg) t
        =
        {   window;
            root;
            view;
            update;
            subscription;
            model;
            dirty = true;
            tree  = None;
        }


    let view (state: ('model, 'msg) t): 'msg Vdom.t =
        state.view state.model


    let rec update (message: 'msg) (state: ('model, 'msg) t): unit =
        let model, command = state.update message state.model
        in
        state.dirty <- true;
        state.model <- model;
        do_command state command


    and do_command (state: ('model,'msg) t) (command: 'msg Command.t): unit =
        let rec perform command =
            match command with
            | Command.None ->
                ()

            | Command.Batch lst ->
                List.iter perform lst

            | Command.Http (type_, url, data, handler) ->
                Printf.printf "http request <%s> <%s>\n" type_ url;
                let request = new_http_request () in
                request##open_
                    (Js.string type_)
                    (Js.string url);
                request##addEventListener
                    (Js.string "readystatechange")
                    (Js.wrap_callback
                        (fun _ ->
                            if request##.readyState = 4 then
                                ( let status = request##.status in
                                  let result =
                                    if 200 <= status && status < 300 then
                                        Ok (Js.to_string request##.responseText)
                                    else
                                        Error status
                                  in
                                  update (handler result) state
                                )
                        )
                    );
                request##send (Js.string data)
        in
        perform command




    let make_event_handler
        (state: ('model, 'msg) t)
        (decode: 'msg Decoder.t)
        : ('a Js.t -> unit) Js.callback
        =
        Js.wrap_callback
            (fun event ->
                match decode event with
                | None ->
                    Printf.printf "cannot decode event\n"
                | Some message ->
                    update message state
            )




    let make_tree
        (state: ('model, 'msg) t)
        (vdom: 'msg Vdom.t)
        : 'msg Tree.t
        =
        let open Vdom in
        let doc = state.window##.document in
        let rec make vdom =
            match vdom with
            | Text str ->
                Tree.make_text
                    str
                    (doc##createTextNode (Js.string str))

            | Node (tag, attributes, children) ->
                let node =
                    doc##createElement (Js.string tag)
                and attributes =
                    Tree.Attributes.make
                        (make_event_handler state)
                        attributes
                in
                Tree.fill_node
                    tag
                    node
                    attributes
                    (List.map make children)
        in
        make vdom





    let remove_children (node: node Js.t): unit =
        let rec remove () =
            match Js.Opt.to_option node##.firstChild with
            | None ->
                ()
            | Some child ->
                node##removeChild child;
                remove ()
        in
        remove ()



    let update_dom (state: ('model, 'msg) t): unit =
        match state.tree with
        | None ->
            (* No tree available. Build the tree and attach it to the root node.
            *)
            let tree =
                make_tree
                    state
                    (view state)
            and root = state.root
            in
            remove_children root;
            root##appendChild (Tree.node tree);
            state.tree <- Some tree;
            state.dirty <- false

        | Some tree ->
            (* Tree available. Update the tree or build a new one. *)
            match
                Tree.update
                    (view state)
                    tree
                    (make_tree state)
                    (make_event_handler state)
            with
            | None ->
                (* Tree has been updated. *)
                state.dirty <- false

            | Some new_tree ->
                (* Tree has been built newly. *)
                state.root##replaceChild
                    (Tree.node new_tree)
                    (Tree.node tree);
                state.tree <- Some new_tree;
                state.dirty <- false



    let rec animate (state: ('model, 'msg) t) (_: 'a): unit =
        if state.dirty then
            update_dom state
        else
            ();
        state.window##requestAnimationFrame
            (Js.wrap_callback (animate state))




    let sandbox
        (model: 'model)
        (view: 'model -> 'msg Vdom.t)
        (update: 'msg -> 'model -> 'model)
        : unit
        =
        let window = get_window () in
        window##addEventListener
            (Js.string "load")
            (Js.wrap_callback
                (fun _ ->
                    let state =
                        make
                            window
                            window##.document##.body
                            model
                            view
                            (fun msg model ->
                                update msg model, Command.None)
                            (fun _ -> Subscription.None)
                    in
                    state.window##requestAnimationFrame
                        (Js.wrap_callback (animate state))
                )
            )

    let element
        (decode: 'a Decoder.t)
        (init: 'a -> 'model * 'msg App.Command.t)
        (view: 'model -> 'msg Vdom.t)
        (update: 'msg -> 'model -> 'model * 'msg App.Command.t)
        (subscription: 'model -> 'msg App.Subscription.t)
        : unit
        =
        let decode_init: (string*'a) Decoder.t =
            Decoder.(
                field "node" string
                >>= fun name ->
                    map
                        (fun data -> name,data)
                        (field "data" decode)
            )
        in
        Js.export "Alba"
            (object%js
                method init (js: Void.t Js.t): unit =
                        match decode_init js with
                        | None ->
                            raise_js "Cannot decode init"
                        | Some (root_name, (data: 'a)) ->
                            let window = get_window () in
                            let root =
                                match
                                    Js.Opt.to_option
                                        (window##.document##getElementById
                                            (Js.string root_name))
                                with
                                | None ->
                                    raise_js ("Cannot find node " ^ root_name)
                                | Some root ->
                                    root
                            in
                            let model, command = init data
                            in
                            let state =
                                make
                                    window
                                    root
                                    model
                                    view
                                    update
                                    subscription
                            in
                            state.window##requestAnimationFrame
                                (Js.wrap_callback (animate state));
                            do_command state command
            end);
        ()
end

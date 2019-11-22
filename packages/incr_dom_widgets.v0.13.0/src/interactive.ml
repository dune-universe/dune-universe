open Import
open Core_kernel
open Vdom

(* We hit the default height limit of 128 fairly quickly. *)
let () =
  let max_height = Incr.State.max_height_allowed Incr.State.t in
  (* 2057 is an arbitrary choice, increase if necessary *)
  let max_height = max 2057 max_height in
  Incr.State.set_max_height_allowed Incr.State.t max_height
;;

(* An [Interactive.t] consists of:
   - A [value], which incrementally updates as the user changes it
   - A [render] function, which constructs a list of Virtual_dom nodes

   [render] takes in an [inject] function that specifies how to convert updates into
   Virtual_dom events, which is used in the event handlers of the Virtual_dom nodes.

   It's more natural to think of the type of [inject] as [('a -> Event.t)] rather than
   [(unit -> Event.t)], since its purpose is to convert a value update into an [Event.t].

   The reason it's not implemented this way is that if a complex ['a Interactive.t]
   is created by composing simpler [_ Interactive.t]s, then when the value of a simpler
   part changes, the resulting [Event.t] should reflect the new value of ['a], not the new
   value of the simpler part.

   So, what we actually do is:

   - Update the Incr.Var.t for the simple part
   - Incr.stabilize ()
   - Read out the new value of ['a]

   The [inject] function is defined in [render].
*)

type 'a t =
  { value : 'a Incr.t
  ; render : (unit -> Event.t) -> Node.t list Incr.t
  }
[@@deriving fields]

let make_counter () =
  let counter = ref 0 in
  fun () ->
    let () = incr counter in
    !counter
;;

let next_key =
  let next_id = make_counter () in
  fun () -> "form_" ^ Int.to_string (next_id ())
;;

let of_incr value =
  let render _ = Incr.return [] in
  Fields.create ~value ~render
;;

let return x = of_incr (Incr.return x)

let bind (type a b) (x : a t) ~(f : a -> b t) : b t =
  let open Incr.Let_syntax in
  let bti : b t Incr.t =
    let%map value = x.value in
    f value
  in
  let value : b Incr.t =
    let%bind bt = bti in
    bt.value
  in
  let render inject =
    let nodes x = x.render inject in
    let%map outer_nodes = nodes x
    and inner_nodes = bti >>= nodes in
    outer_nodes @ inner_nodes
  in
  Fields.create ~value ~render
;;

let render t ~on_input ~inject =
  let observer = Incr.observe t.value in
  let inject () =
    let () = Incr.stabilize () in
    inject (on_input (Incr.Observer.value_exn observer))
  in
  Incr.map (t.render inject) ~f:(fun nodes -> Node.div [] nodes)
;;

let current_value t =
  let observer = Incr.observe t.value in
  let () = Incr.stabilize () in
  Incr.Observer.value_exn observer
;;

let map t ~f =
  let value = Incr.map t.value ~f in
  let render = t.render in
  Fields.create ~value ~render
;;

let map_nodes t ~f =
  let open Incr.Let_syntax in
  let render inject =
    let%map nodes = t.render inject in
    f nodes
  in
  Fields.create ~value:t.value ~render
;;

let map_nodes_value_dependent t ~f =
  let open Incr.Let_syntax in
  let render inject =
    let%map nodes = t.render inject
    and value = t.value in
    f value nodes
  in
  Fields.create ~value:t.value ~render
;;

let both a b =
  let value = Incr.map2 a.value b.value ~f:(fun a b -> a, b) in
  let render inject = Incr.map2 (a.render inject) (b.render inject) ~f:List.append in
  Fields.create ~value ~render
;;

let wrap_in_div ?(attrs = []) t = map_nodes t ~f:(fun nodes -> [ Node.div attrs nodes ])

module Primitives = struct
  let create ~init ~render =
    let var = Incr.Var.create init in
    let value = Incr.Var.watch var in
    let render inject =
      let inject x =
        let () = Incr.Var.set var x in
        inject ()
      in
      render ~inject ~value
    in
    Fields.create ~value ~render
  ;;

  type 'a primitive = ?attrs:Attr.t list -> ?id:string -> unit -> 'a t

  let bootstrap_text_attrs = []
  let bootstrap_text_area_attrs = [ Attr.class_ "textarea" ]
  let bootstrap_button_attrs = [ Attr.classes [ "btn"; "btn-primary" ] ]

  let bootstrap_dropdown_attrs =
    [ Attr.classes [ "btn"; "btn-outline-primary"; "btn-sm"; "dropdown-toggle" ] ]
  ;;

  let default_text_attrs = []
  let default_text_area_attrs = []
  let default_button_attrs = []
  let default_dropdown_attrs = []

  let shared_setup ~id =
    let key = next_key () in
    let id = Option.value id ~default:key in
    key, id
  ;;

  let of_nodes nodes =
    let value = Incr.return () in
    let nodes = Incr.return nodes in
    let render _ = nodes in
    Fields.create ~value ~render
  ;;

  let text_or_text_area ~which_one ?init ~attrs ?id () =
    let open Incr.Let_syntax in
    let init = Option.value init ~default:"" in
    let key, id = shared_setup ~id in
    create ~init ~render:(fun ~inject ~value ->
      let%map value = value in
      let on_input = Attr.on_input (fun _ev text -> inject text) in
      let attrs = Attr.id id :: on_input :: attrs in
      [ (match which_one with
          | `Text -> Node.input ~key (Attr.type_ "text" :: Attr.value value :: attrs) []
          | `Text_area -> Node.textarea ~key attrs [ Node.text value ])
      ])
  ;;

  let text ?init ?(attrs = default_text_attrs) =
    text_or_text_area ~which_one:`Text ?init ~attrs
  ;;

  let text_area ?init ?(attrs = default_text_area_attrs) =
    text_or_text_area ~which_one:`Text_area ?init ~attrs
  ;;

  module Button_state = struct
    type t =
      | Pressed
      | Not_pressed
  end

  let button ~text ?(attrs = default_button_attrs) ?id () =
    let init = Button_state.Not_pressed in
    let key, id = shared_setup ~id in
    create ~init ~render:(fun ~inject ~value:(_ : Button_state.t Incr.t) ->
      let on_click =
        Attr.on_click (fun _ ->
          Event.Many [ inject Button_state.Pressed; inject Button_state.Not_pressed ])
      in
      let attrs = Attr.id id :: Attr.type_ "button" :: on_click :: attrs in
      Incr.return [ Node.button ~key attrs [ Node.text text ] ])
  ;;

  let disabled_button ~text ?(attrs = default_button_attrs) ?id () =
    let key = next_key () in
    let id = Option.value id ~default:key in
    let attrs = [ Attr.id id; Attr.type_ "button"; Attr.disabled ] @ attrs in
    let nodes = [ Node.button ~key attrs [ Node.text text ] ] in
    of_nodes nodes
  ;;

  let dropdown_exn ~options ?(init = 0) ?(attrs = default_dropdown_attrs) ?id () =
    let names, meanings = List.unzip options in
    let open Incr.Let_syntax in
    let key, id = shared_setup ~id in
    let t =
      create ~init ~render:(fun ~inject ~value:selected_idx ->
        let%map selected_idx = selected_idx in
        let select_options =
          List.mapi names ~f:(fun idx text ->
            let selected_attr =
              if selected_idx = idx
              then [ Attr.create "selected" "selected" ]
              else []
            in
            let option_attr = selected_attr @ [ Attr.value (Int.to_string idx) ] in
            Node.option option_attr [ Node.text text ])
        in
        let on_input = Attr.on_input (fun _ev text -> inject (Int.of_string text)) in
        let attrs = Attr.id id :: on_input :: attrs in
        [ Node.select ~key attrs select_options ])
    in
    map t ~f:(fun selected_index -> List.nth_exn meanings selected_index)
  ;;

  let dropdown_with_blank_exn ~options ?init ?attrs ?id () =
    let options = List.map options ~f:(fun (label, value) -> label, Some value) in
    let options = ("", None) :: options in
    let init = Option.map init ~f:(fun x -> x + 1) in
    dropdown_exn ~options ?init ?attrs ?id ()
  ;;

  let checkbox ?(init = false) ?(attrs = []) ?id () =
    let open Incr.Let_syntax in
    let key, id = shared_setup ~id in
    create ~init ~render:(fun ~inject ~value ->
      let%map value = value in
      let attrs = (if value then [ Attr.checked ] else []) @ attrs in
      (* jjackson: I couldn't figure out how to obtain the current state of the checkbox
         directly from the event, so we have to find the checkbox in the DOM and look at
         its state, which we avoid in the other primitives as it creates more room for
         error.
      *)
      let on_click _ev =
        let element = Dom_html.document##getElementById (Js.string id) in
        let checked =
          match Dom_html.opt_tagged element with
          | Some (Dom_html.Input el) -> Js.to_bool el##.checked
          | _ ->
            let () =
              Async_js.log_s
                [%message
                  "Couldn't determine the state of the checkbox. The form might not \
                   work properly."
                    (id : string)]
            in
            value
        in
        inject checked
      in
      let attrs =
        Attr.type_ "checkbox" :: Attr.id id :: Attr.on_click on_click :: attrs
      in
      [ Node.input ~key attrs [] ])
  ;;

  let message msg = of_nodes [ Node.text msg ]
  let line_break = of_nodes [ Node.div [] [] ]
  let nodes = of_nodes
end

module T = struct
  include Monad.Make (struct
      type nonrec 'a t = 'a t

      let return = return
      let map = map
      let map = `Custom map
      let bind = bind
    end)
end

let all = T.all
let all_unit = T.all_unit
let ignore_m = T.ignore_m
let join = T.join
let ( >>| ) = T.( >>| )
let ( >>= ) = T.( >>= )

module Monad_infix = T.Monad_infix

module Let_syntax = struct
  let return = return
  let ( >>| ) = ( >>| )
  let ( >>= ) = ( >>= )

  module Let_syntax = struct
    let return = return
    let bind = bind
    let map = map
    let both = both

    module Open_on_rhs = Primitives
  end
end

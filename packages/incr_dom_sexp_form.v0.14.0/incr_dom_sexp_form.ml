open Import
open Core_kernel
open Vdom
module Interactive = Incr_dom_interactive

(* Roughly speaking, a ['a Sexp_form.t] is a function from a default value to a
   ['a Or_error Interactive.t]. The default value is represented as a list of sexps.
   An indentation is passed to each [Sexp_form.t] in addition to the default value. *)

module Parse_state = struct
  (* Technically indentation isn't related to parsing, but it's included in [Parse_state]
     because it needs to be passed to each node on creation. *)
  type t =
    { indentation : int
    ; default : Sexp.t list option
    }
  [@@deriving fields, sexp]

  let create ~default () = { indentation = 0; default }
  let indent t = { t with indentation = t.indentation + 1 }
  let map_default t ~f = { t with default = Option.map t.default ~f }
  let has_default t = Option.is_some t.default
  let erase_default t = { t with default = None }

  let require_empty t =
    match t.default with
    | None | Some [] -> ()
    | Some sexps -> raise_s [%message "Expected ')' but found" (sexps : Sexp.t list)]
  ;;

  let transform_first_sexp t ~f =
    map_default t ~f:(function
      | [] -> failwith "Unexpected ')'"
      | x :: xs -> f x :: xs)
  ;;

  let consume t expected =
    let default =
      match t.default with
      | None -> None
      | Some (x :: xs) when Sexp.equal x expected -> Some xs
      | Some (found :: _) ->
        raise_s [%message "Parse failure" (expected : Sexp.t) (found : Sexp.t)]
      | Some [] -> raise_s [%message "Unexpected ')'" (expected : Sexp.t)]
    in
    { t with default }
  ;;

  let with_default t default = { t with default = Some default }
end

module Init_result = struct
  type 'a t =
    { parse_state : Parse_state.t
    ; form : 'a Or_error.t Interactive.t
    }
  [@@deriving fields]
end

type 'a t = Parse_state.t -> 'a Init_result.t

(* As a default style, we use Arial for editor messages so that the user can distinguish
   editor messages from text which is part of the type being edited (as this text is in
   monospaced font).
*)

let default_editor_message_style_term = Css_gen.font_family [ "Arial" ]
let default_editor_message_attr = Attr.style default_editor_message_style_term

let error_attr =
  Attr.style
    Css_gen.(background_color (`Name "#FFCCCC") @> default_editor_message_style_term)
;;

module Init = struct
  type 'a t =
    | Simple
    | With_default of
        { default : 'a
        ; sexp_of_t : 'a -> Sexp.t
        ; between : Node.t option
        ; diff : original:Sexp.t -> updated:Sexp.t -> Node.t
        }

  let simple = Simple

  let default = function
    | Simple -> None
    | With_default { default; sexp_of_t; between = _; diff = _ } ->
      Some [ sexp_of_t default ]
  ;;

  let with_default ~sexp_of_t ~default ~diff ?between () =
    With_default { default; between; sexp_of_t; diff }
  ;;

  let no_diff ~original:(_ : Sexp.t) ~updated:(_ : Sexp.t) = Node.text ""

  let node_after ~value t =
    match t with
    | Simple -> Node.span [] []
    | With_default { default; sexp_of_t; between; diff } ->
      let diff_node =
        match value with
        | Error _ ->
          Node.span
            [ error_attr ]
            [ Node.text "Can't display diff because there's an error in the form." ]
        | Ok t ->
          let original = sexp_of_t default in
          let updated = sexp_of_t t in
          diff ~original ~updated
      in
      let between_node = Option.value between ~default:(Node.div [] []) in
      Node.div [ Attr.style Css_gen.(text_align `Center) ] [ between_node; diff_node ]
  ;;
end

let to_interactive ~init t =
  let default = Init.default init in
  let parse_state = Parse_state.create ~default () in
  let { Init_result.form; parse_state } = t parse_state in
  let () = Parse_state.require_empty parse_state in
  Interactive.map_nodes_value_dependent form ~f:(fun value nodes ->
    [ Node.create "pre" [] nodes; Init.node_after ~value init ])
;;

module T = struct
  include Applicative.Make (struct
      type nonrec 'a t = 'a t

      let return x parse_state =
        let form = x |> Or_error.return |> Interactive.return in
        Init_result.Fields.create ~parse_state ~form
      ;;

      let map t ~f parse_state =
        let { Init_result.parse_state; form } = t parse_state in
        let form = Interactive.map form ~f:(fun x -> Or_error.map x ~f) in
        Init_result.Fields.create ~parse_state ~form
      ;;

      let map = `Custom map

      let apply t1 t2 parse_state =
        let open Interactive.Let_syntax in
        let { Init_result.parse_state; form = form1 } = t1 parse_state in
        let { Init_result.parse_state; form = form2 } = t2 parse_state in
        let form =
          let%map a1 = form1
          and a2 = form2 in
          Or_error.map2 a1 a2 ~f:Fn.id
        in
        Init_result.Fields.create ~parse_state ~form
      ;;
    end)
end

include T

let ok form = Interactive.map form ~f:(fun () -> Ok ())

let const_form form parse_state =
  let form = ok form in
  Init_result.Fields.create ~parse_state ~form
;;

let space = const_form (Interactive.Primitives.message " ")

let handle_error_interactive where interactive =
  let interactive =
    Interactive.map_nodes_value_dependent interactive ~f:(fun value nodes ->
      match value with
      | Error _ | Ok (Ok _) -> nodes
      | Ok (Error err) ->
        let error_message_node = Node.text (Error.to_string_hum err) in
        let error_node = Node.span [ error_attr ] [ error_message_node ] in
        let space_node = Node.text " " in
        (match where with
         | `Before -> error_node :: space_node :: nodes
         | `After -> nodes @ [ space_node; error_node ]))
  in
  Interactive.map interactive ~f:Or_error.join
;;

let handle_error ~where t parse_state =
  let { Init_result.parse_state; form } = t parse_state in
  let form = handle_error_interactive where form in
  Init_result.Fields.create ~parse_state ~form
;;

let validate ~where t ~f =
  map t ~f:(fun x -> Or_error.map (f x) ~f:(fun () -> x)) |> handle_error ~where
;;

let validate_interactive ~where t interactive ~f =
  let open Interactive.Let_syntax in
  (fun parse_state ->
     let { Init_result.parse_state; form } = t parse_state in
     let form =
       let%map a = form
       and b = interactive in
       Or_error.map a ~f:(fun a -> Or_error.map (f a b) ~f:(fun () -> a))
     in
     Init_result.Fields.create ~parse_state ~form)
  |> handle_error ~where
;;

module Case = struct
  type 'a sexp_form = 'a t

  type 'a t =
    { name : string
    (* [has_been_applied] is used to determine whether the value should be wrapped in
       parentheses when converted to a sexp. For instance, [A] is converted as [A] but
       [B of int] is converted as [(B 123)]. *)
    ; has_been_applied : bool
    ; inner : 'a sexp_form
    }
  [@@deriving fields]

  let apply t sexp_form =
    let { name; has_been_applied; inner } = t in
    let inner = if has_been_applied then inner <* space else inner in
    let inner = inner <*> sexp_form in
    Fields.create ~name ~inner ~has_been_applied:true
  ;;

  let of_raw_parts ~name ~constructor =
    let inner = return constructor in
    let has_been_applied = false in
    Fields.create ~name ~inner ~has_been_applied
  ;;

  let map t ~f = { t with inner = T.map t.inner ~f }

  let of_variant v =
    let { Variantslib.Variant.name; constructor; rank = _ } = v in
    of_raw_parts ~name ~constructor
  ;;
end

module Record_field = struct
  type nonrec ('record, 'a) t = 'a t
end

module Record_builder = struct
  type nonrec ('record, 'a) t = 'a t
end

module Primitives = struct
  let const sexp parse_state =
    let form = Interactive.Primitives.message (Sexp.to_string_hum sexp) |> ok in
    let parse_state = Parse_state.consume parse_state sexp in
    Init_result.Fields.create ~parse_state ~form
  ;;

  let const_atom s = const (Sexp.Atom s)

  let text_internal ?placeholder ?width ~parse_state_to_interactive_initial_value parse =
    (fun parse_state ->
       let initial_value, parse_state =
         parse_state_to_interactive_initial_value parse_state
       in
       let form =
         let open Interactive.Let_syntax in
         let placeholder_attr = Option.map placeholder ~f:(fun x -> Attr.placeholder x) in
         let width_attr =
           Option.map width ~f:(fun width -> Attr.create "size" (Int.to_string width))
         in
         let attrs =
           List.filter_opt [ width_attr; placeholder_attr ]
           @ Interactive.Primitives.default_text_attrs
         in
         let%map_open user_text = text ~attrs ?init:initial_value () in
         Ok (parse user_text)
       in
       Init_result.Fields.create ~parse_state ~form)
    |> handle_error ~where:`After
  ;;

  let string ?placeholder ?width () =
    let parse_state_to_interactive_initial_value parse_state =
      let raise_exn ~reason =
        let msg =
          sprintf
            "Expected a non-empty list of sexps with an atom as the first element when \
             parsing the initial value, but %s"
            reason
        in
        raise_s [%message msg (parse_state : Parse_state.t)]
      in
      match Parse_state.default parse_state with
      | None -> None, parse_state
      | Some (Sexp.Atom atom :: rest) ->
        Some atom, Parse_state.with_default parse_state rest
      | Some (Sexp.List _ :: _) -> raise_exn ~reason:"the first element was a list"
      | Some [] -> raise_exn ~reason:"the list was empty"
    in
    text_internal
      ?placeholder
      ?width
      ~parse_state_to_interactive_initial_value
      Or_error.return
  ;;

  let sexp ~override_error ~text_to_sexp () =
    let parse_state_to_interactive_initial_value parse_state =
      match Parse_state.default parse_state with
      | None -> None, parse_state
      | Some (sexp :: rest) ->
        Some (Sexp.to_string_hum sexp), Parse_state.with_default parse_state rest
      | Some [] ->
        raise_s
          [%message
            "Expected a non-empty list of sexps when parsing the initial value, but the \
             list was empty"
              (parse_state : Parse_state.t)]
    in
    text_internal ~parse_state_to_interactive_initial_value (fun x ->
      Or_error.try_with (fun () -> text_to_sexp x)
      |> Result.map_error ~f:override_error)
  ;;

  let from_ppx_sexp_raw t_of_sexp =
    sexp ~override_error:Fn.id () ~text_to_sexp:Sexp.of_string
    |> map ~f:(fun s -> Or_error.try_with (fun () -> t_of_sexp s))
    |> handle_error ~where:`After
  ;;

  let from_ppx_sexp ~t_of_sexp ?(on_error = Fn.id) () =
    let text_to_sexp text =
      if not
           (String.contains text '('
            || String.contains text ')'
            || String.contains text '"')
      then Sexp.Atom text
      else Sexp.of_string text
    in
    sexp ~override_error:on_error () ~text_to_sexp
    |> map ~f:(fun s ->
      Or_error.try_with (fun () -> t_of_sexp s) |> Result.map_error ~f:on_error)
    |> handle_error ~where:`After
  ;;

  let int =
    map (string ~width:10 ()) ~f:(fun x ->
      Or_error.try_with (fun () -> Int.of_string x)
      |> Result.map_error ~f:(fun _ -> Error.of_string "This should be an integer."))
    |> handle_error ~where:`After
  ;;

  let enclose t parse_state =
    let { Parse_state.indentation; default } = parse_state in
    let left_default, right_default =
      match default with
      | Some (Sexp.List xs :: rest) -> Some xs, Some rest
      | None -> None, None
      | Some (Sexp.Atom atom :: _) ->
        raise_s [%message "Expected '(', found an atom" (atom : string)]
      | Some [] -> failwith "Expected '(', found ')'"
    in
    let left_parse_state =
      Parse_state.Fields.create ~indentation:(indentation + 1) ~default:left_default
    in
    let right_parse_state =
      Parse_state.Fields.create ~indentation ~default:right_default
    in
    let { Init_result.parse_state = left_result; form } = t left_parse_state in
    let () = Parse_state.require_empty left_result in
    let open Interactive.Let_syntax in
    let form =
      let%map_open () = message "("
      and value = form
      and () = message ")" in
      value
    in
    Init_result.Fields.create ~form ~parse_state:right_parse_state
  ;;

  let newline_nodes parse_state =
    let spaces = String.make (Parse_state.indentation parse_state) ' ' in
    [ Node.div [] []; Node.text spaces ]
  ;;

  let newline : unit t =
    fun parse_state ->
      let form =
        Interactive.Primitives.nodes (newline_nodes parse_state)
        |> Interactive.map ~f:(fun x -> Ok x)
      in
      Init_result.Fields.create ~parse_state ~form
  ;;

  let on_new_line t = newline *> t
  let case_on_new_line (case : 'a Case.t) = { case with inner = on_new_line case.inner }
  let field t field = on_new_line (enclose (const_atom (Field.name field) *> space *> t))
  let record ~create = return create
  let finish_record t = enclose t

  let nonempty_string ?placeholder ?width () =
    validate ~where:`After (string ?placeholder ?width ()) ~f:(fun s ->
      if String.is_empty s
      then Or_error.error_string "This field should not be empty."
      else Ok ())
  ;;

  let positive_int =
    validate ~where:`After int ~f:(fun x ->
      if x <= 0
      then Or_error.error_string "This integer should be positive."
      else Ok ())
  ;;

  let non_negative_int =
    validate ~where:`After int ~f:(fun x ->
      if x < 0
      then Or_error.error_string "This integer should not be negative."
      else Ok ())
  ;;

  let case_raw = Case.of_raw_parts
  let case = Case.of_variant
  let ( <|*> ) = Case.apply
  let ( <.*> ) = ( <*> )

  module Match = struct
    type t =
      { index : int
      ; left : Sexp.t list
      ; right : Sexp.t list
      }
    [@@deriving fields]
  end

  let find_matching cases parse_state =
    let fail () =
      let cases = List.map cases ~f:Case.name in
      let sexp = Option.value_exn (Parse_state.default parse_state) in
      raise_s [%message "No matching case" (cases : string list) (sexp : Sexp.t list)]
    in
    let cases_with_index = List.mapi cases ~f:Tuple2.create in
    let find_matching ~left ~right ~atom =
      let index =
        List.find cases_with_index ~f:(fun (_, case) ->
          String.equal (Case.name case) atom)
      in
      match index with
      | None -> fail ()
      | Some (index, _) -> Match.Fields.create ~index ~left ~right
    in
    Option.map (Parse_state.default parse_state) ~f:(function
      | Atom atom :: right -> find_matching ~left:[] ~right ~atom
      | List (Atom atom :: left) :: right -> find_matching ~left ~right ~atom
      | _ -> fail ())
  ;;

  let variant ?don't_state_options_in_error cases =
    let maybe_enclose form =
      Interactive.map_nodes_value_dependent form ~f:(fun (_, enclose) nodes ->
        match enclose with
        | `Enclose -> [ Node.text "(" ] @ nodes @ [ Node.text ")" ]
        | `Don't_enclose -> nodes)
      |> Interactive.map ~f:fst
    in
    (fun parse_state ->
       let match_ = find_matching cases parse_state in
       let cases_as_editors =
         List.mapi cases ~f:(fun index case ->
           let { Case.name; inner; has_been_applied } = case in
           let form_and_enclose =
             lazy
               (let parse_state =
                  match match_ with
                  | Some match_ ->
                    if index = Match.index match_
                    then Parse_state.with_default parse_state (Match.left match_)
                    else Parse_state.erase_default parse_state
                  | None -> parse_state
                in
                let parse_state =
                  if has_been_applied
                  then Parse_state.indent parse_state
                  else parse_state
                in
                let enclose = if has_been_applied then `Enclose else `Don't_enclose in
                let inner = if has_been_applied then space *> inner else inner in
                let { Init_result.parse_state; form } = inner parse_state in
                let () = Parse_state.require_empty parse_state in
                Ok (form, enclose))
           in
           name, form_and_enclose)
       in
       let parse_state =
         match match_ with
         | None -> parse_state
         | Some match_ -> Parse_state.with_default parse_state (Match.right match_)
       in
       let default_index =
         match match_ with
         | None -> 0
         | Some match_ -> Match.index match_ + 1
       in
       let error_message =
         match don't_state_options_in_error with
         | Some () -> "Please select an option."
         | None ->
           let options = List.map cases ~f:Case.name |> String.concat ~sep:" | " in
           "Please select an option: " ^ options ^ "."
       in
       let first_option = lazy (Or_error.error_string error_message) in
       let options = ("", first_option) :: cases_as_editors in
       let open Interactive.Let_syntax in
       let form =
         let%bind_open choice = dropdown_exn ~init:default_index ~options ~attrs:[] () in
         match Lazy.force choice with
         | Error err -> return (Ok (Error err), `Don't_enclose)
         | Ok (form, enclose) ->
           (match%map form with
            | Error err -> Error err, enclose
            | Ok value -> Ok (Ok value), enclose)
       in
       let form = maybe_enclose form in
       Init_result.Fields.create ~parse_state ~form)
    |> handle_error ~where:`After
  ;;

  let enumeration ?don't_state_options_in_error elts ~to_string =
    let cases =
      List.map elts ~f:(fun x -> case_raw ~name:(to_string x) ~constructor:x)
    in
    variant ?don't_state_options_in_error cases
  ;;

  module List_editor : sig
    val list
      :  ?element_name:string
      -> ?gated_deletion:unit
      -> ?max_size:int
      -> ?add_and_remove_button_attrs:Attr.t list
      -> ?editor_message_attr:Attr.t
      -> order:[ `Ordered | `Unordered ]
      -> 'a t
      -> 'a list t
  end = struct
    let add_text ?element_name ~order =
      let order_text =
        match order with
        | `Ordered -> [ "here" ]
        | `Unordered -> []
      in
      let element_text = Option.value_map element_name ~default:[] ~f:List.return in
      let text = "Add" :: (element_text @ order_text) in
      String.concat text ~sep:" "
    ;;

    let remove_text = function
      | None -> "Delete"
      | Some x -> "Delete this " ^ x
    ;;

    let allow_deletion_text = function
      | None -> "Allow deletion"
      | Some x -> "Allow " ^ x ^ " deletion"
    ;;

    module State = struct
      type 'a t =
        { list_var : 'a list Incr.Var.t
        ; deletion_stack_var : 'a list Incr.Var.t
        }
      [@@deriving fields]

      let create ~initial_list =
        let list_var = Incr.Var.create initial_list in
        let deletion_stack_var = Incr.Var.create [] in
        { list_var; deletion_stack_var }
      ;;

      let default_modifying_button_attr =
        [ Attr.style
            Css_gen.(
              font_family [ "Arial" ]
              @> font_size (`Pt 10.0)
              @> uniform_padding (`Px 1)
              @> margin_top (`Px 2)
              @> margin_bottom (`Px 2))
        ]
      ;;

      let state_modifying_button
            ?(attrs = default_modifying_button_attr)
            state
            ~new_values
            ~descr
        =
        let open Interactive.Let_syntax in
        match%map_open button ~text:descr ~attrs () with
        | Not_pressed -> None
        | Pressed ->
          let list, deletion_stack = new_values () in
          let () = Incr.Var.set state.list_var list in
          let () = Incr.Var.set state.deletion_stack_var deletion_stack in
          Some list
      ;;

      let add_at_button
            ?attrs
            state
            ~list
            ~deletion_stack
            ~index
            ~add_text
            ~t
            ~init_blank
        =
        let new_values () =
          let new_form, new_deletion_stack =
            match deletion_stack with
            | x :: xs -> x, xs
            | [] ->
              let new_form = t init_blank |> Init_result.form in
              new_form, []
          in
          let left, right = List.split_n list index in
          left @ [ new_form ] @ right, new_deletion_stack
        in
        state_modifying_button ?attrs state ~new_values ~descr:add_text
      ;;

      let remove_at_button ?attrs state ~list ~deletion_stack ~index ~remove_text =
        let new_values () =
          let left, right = List.split_n list index in
          match right with
          | deleted :: rest -> left @ rest, deleted :: deletion_stack
          | [] -> failwith "internal error in sexp_editor"
        in
        state_modifying_button ?attrs state ~new_values ~descr:remove_text
      ;;

      let values state =
        Incr.Var.watch state.list_var, Incr.Var.watch state.deletion_stack_var
      ;;
    end

    let list
          ?element_name
          ?gated_deletion
          ?max_size
          ?add_and_remove_button_attrs
          ?(editor_message_attr = default_editor_message_attr)
          ~order
          t
      =
      let add_text = add_text ?element_name ~order in
      let remove_text = remove_text element_name in
      let allow_deletion_text = allow_deletion_text element_name in
      let open Interactive.Let_syntax in
      enclose (fun parse_state ->
        let initial_list, parse_state =
          match Parse_state.default parse_state with
          | None -> [], parse_state
          | Some sexps ->
            let initial_list =
              List.map sexps ~f:(fun x ->
                let parse_state = Parse_state.with_default parse_state [ x ] in
                let { Init_result.parse_state; form } = t parse_state in
                let () = Parse_state.require_empty parse_state in
                form)
            in
            let parse_state = Parse_state.with_default parse_state [] in
            initial_list, parse_state
        in
        let init_blank = Parse_state.erase_default parse_state in
        let state = State.create ~initial_list in
        let list, deletion_stack = State.values state in
        let form =
          let deletion_enabled_checkbox = Interactive.Primitives.checkbox () in
          let%bind_open list = Interactive.of_incr list
          and deletion_stack = Interactive.of_incr deletion_stack in
          let adding_enabled =
            match max_size with
            | None -> true
            | Some x -> List.length list < x
          in
          let%bind_open deletion_enabled =
            match gated_deletion with
            | None -> return true
            | Some () ->
              if List.is_empty list
              then return false
              else (
                let%map_open deletion_enabled = deletion_enabled_checkbox
                and () =
                  nodes
                    [ Node.span
                        [ editor_message_attr ]
                        [ Node.text (" " ^ allow_deletion_text) ]
                    ]
                in
                deletion_enabled)
          in
          (* Each element in the list can have up to two buttons associated with it:
             - an add button to add an element immediately before it
             - a remove button to remove it

             For each form in [list], the corresponding form in [forms_with_buttons]
             contains the original form plus its associated add/remove buttons, and its
             value consists of the original form's value along with any new list
             produced by an associated button being pressed ([None] if no button was
             pressed).
          *)
          let forms_with_buttons =
            List.mapi list ~f:(fun index form ->
              let button_with_new_list f ~sep =
                let%map_open new_list =
                  f
                    ?attrs:add_and_remove_button_attrs
                    state
                    ~list
                    ~deletion_stack
                    ~index
                and () = sep in
                new_list
              in
              let%map_open () = nodes (newline_nodes parse_state)
              and add_button_new_list =
                match order, adding_enabled with
                | `Ordered, true ->
                  button_with_new_list
                    (State.add_at_button ~add_text ~t ~init_blank)
                    ~sep:(nodes (newline_nodes parse_state))
                | `Unordered, _ | _, false -> return None
              and remove_button_new_list =
                if deletion_enabled
                then
                  button_with_new_list
                    (State.remove_at_button ~remove_text)
                    ~sep:(message " ")
                else return None
              and value = form in
              let new_list =
                Option.first_some add_button_new_list remove_button_new_list
              in
              value, new_list)
          in
          let%bind_open forms_with_buttons = Interactive.all forms_with_buttons
          and () =
            if (not (List.is_empty list)) && adding_enabled
            then nodes (newline_nodes parse_state)
            else return ()
          and last_add_button_new_list =
            if adding_enabled
            then
              State.add_at_button
                ?attrs:add_and_remove_button_attrs
                state
                ~list
                ~deletion_stack
                ~index:(List.length list)
                ~add_text
                ~t
                ~init_blank
            else return None
          in
          let default, new_lists = List.unzip forms_with_buttons in
          (* If there are multiple new lists (because multiple add/remove buttons were
             pressed at roughly the same time), only the first new list is used and the
             other button presses are ignored.
          *)
          let new_list =
            List.find_map (last_add_button_new_list :: new_lists) ~f:Fn.id
            |> Option.map ~f:Interactive.all
          in
          let%map value = Option.value new_list ~default:(return default) in
          Or_error.all value
        in
        Init_result.Fields.create ~form ~parse_state)
    ;;
  end

  include List_editor

  let option t =
    map (list ~max_size:1 ~element_name:"optional value" ~order:`Unordered t) ~f:List.hd
  ;;

  let sexp_option_field t field_param =
    let inner = field (option t) field_param in
    let field_name = Field.name field_param in
    fun parse_state ->
      let parse_state =
        let open Sexp in
        Parse_state.map_default parse_state ~f:(function
          | List [ Atom x; y ] :: z when String.equal x field_name ->
            List [ Atom x; List [ y ] ] :: z
          | sexps -> List [ Atom field_name; List [] ] :: sexps)
      in
      inner parse_state
  ;;

  let bool ~true_ ~false_ =
    let inner =
      variant
        [ case_raw ~name:true_ ~constructor:true
        ; case_raw ~name:false_ ~constructor:false
        ]
    in
    fun parse_state ->
      let parse_state =
        let open Sexp in
        Parse_state.map_default parse_state ~f:(function
          | Atom "true" :: x -> Atom true_ :: x
          | Atom "false" :: x -> Atom false_ :: x
          | x -> x)
      in
      inner parse_state
  ;;

  let bool_true_false = bool ~true_:"true" ~false_:"false"
  let bool_yes_no = bool ~true_:"yes" ~false_:"no"
  let tuple2 a b = return (fun a b -> a, b) <*> a <* space <*> b |> enclose

  let tuple3 a b c =
    return (fun a b c -> a, b, c) <*> a <* space <*> b <* space <*> c |> enclose
  ;;

  let tuple4 a b c d =
    return (fun a b c d -> a, b, c, d)
    <*> a
    <* space
    <*> b
    <* space
    <*> c
    <* space
    <*> d
    |> enclose
  ;;

  let tuple5 a b c d e =
    return (fun a b c d e -> a, b, c, d, e)
    <*> a
    <* space
    <*> b
    <* space
    <*> c
    <* space
    <*> d
    <* space
    <*> e
    |> enclose
  ;;

  let unit = enclose (return ())

  let assoc_map ?element_name ?gated_deletion ?max_size ~key ~data ~of_alist_exn () =
    list ?element_name ?gated_deletion ?max_size ~order:`Unordered (tuple2 key data)
    |> map ~f:(fun x -> Or_error.try_with (fun () -> of_alist_exn x))
    |> handle_error ~where:`Before
  ;;

  let set ?element_name ?gated_deletion ?max_size ~of_list t =
    list ?element_name ?gated_deletion ?max_size ~order:`Unordered t |> map ~f:of_list
  ;;

  let recursive x =
    let rec f parse_state = (x f) parse_state in
    f
  ;;

  let collapse_indicator = "(...)"

  let collapse ?(editor_message_attr = default_editor_message_attr) t parse_state =
    let initially_checked = Parse_state.has_default parse_state in
    let { Init_result.parse_state; form } = t parse_state in
    let open Interactive.Let_syntax in
    let form =
      let%bind_open checked = checkbox ~init:initially_checked ()
      and () = nodes [ Node.span [ editor_message_attr ] [ Node.text " Collapse " ] ] in
      let%map_open value =
        if checked
        then
          Interactive.map_nodes_value_dependent form ~f:(fun value _nodes ->
            match value with
            | Error _ ->
              [ Node.span
                  [ error_attr ]
                  [ Node.text "There is an error in the collapsed form." ]
              ]
            | Ok _ -> [])
        else form
      and () = message (if checked then collapse_indicator else "") in
      value
    in
    Init_result.Fields.create ~parse_state ~form
  ;;

  let defaulting_to ~default ~sexp_of_t t =
    let default_param = [ sexp_of_t default ] in
    fun parse_state ->
      let { Parse_state.default; indentation } = parse_state in
      let default = Some (Option.value default ~default:default_param) in
      let parse_state = { Parse_state.default; indentation } in
      t parse_state
  ;;

  let dropdown_with_other ~other ~sexp_of_t options =
    let other_string = "Other" in
    let enumerated_options =
      List.map options ~f:(fun (name, constructor) -> case_raw ~name ~constructor)
    in
    let other_option = case_raw ~name:other_string ~constructor:Fn.id <|*> other in
    let inner = variant (enumerated_options @ [ other_option ]) in
    fun parse_state ->
      let parse_state =
        Parse_state.transform_first_sexp parse_state ~f:(fun sexp ->
          let enumerated_match =
            List.find options ~f:(fun (_, option) ->
              Sexp.equal sexp (sexp_of_t option))
          in
          let open Sexp in
          match enumerated_match with
          | None -> List [ Atom other_string; sexp ]
          | Some (name, _) -> Atom name)
      in
      inner parse_state
  ;;
end

include Primitives

module Unsafe = struct
  include T

  module Let_syntax = struct
    include T

    module Let_syntax = struct
      include T
      module Open_on_rhs = Primitives
    end
  end
end

let map_that_can_fail t ~a_to_b ~b_to_a ~sexp_of_a ~b_of_sexp =
  (fun parse_state ->
     let parse_state =
       Parse_state.transform_first_sexp parse_state ~f:(fun sexp ->
         sexp |> b_of_sexp |> b_to_a |> sexp_of_a)
     in
     let { Init_result.form; parse_state } = t parse_state in
     let form =
       Interactive.map
         form
         ~f:(Or_error.map ~f:(fun x -> Or_error.try_with (fun () -> a_to_b x)))
     in
     { Init_result.form; parse_state })
  |> handle_error ~where:`Before
  |> handle_error ~where:`Before
;;

let map t ~a_to_b ~b_to_a ~sexp_of_a ~b_of_sexp =
  map_that_can_fail
    t
    ~a_to_b:(fun x -> Or_error.return (a_to_b x))
    ~b_to_a
    ~sexp_of_a
    ~b_of_sexp
;;

let test ~form ~value ~sexp_of_t ~equal ~on_failure =
  let init = Init.with_default ~sexp_of_t ~default:value ~diff:Init.no_diff () in
  let parsed_value = to_interactive ~init form |> Interactive.current_value in
  let result =
    match parsed_value with
    | Error err -> Error (Error.to_string_hum err)
    | Ok parsed_value ->
      if equal value parsed_value
      then Ok ()
      else
        Error
          (sprintf
             !"%{Sexp#hum}\nwas parsed as\n%{Sexp#hum}"
             (sexp_of_t value)
             (sexp_of_t parsed_value))
  in
  match result, on_failure with
  | Ok (), _ -> ()
  | Error msg, `Raise -> failwith msg
  | Error msg, `Print -> print_endline msg
;;

let test_list ~form ~values ~sexp_of_t ~equal ~on_failure =
  List.iter values ~f:(fun value -> test ~form ~value ~sexp_of_t ~equal ~on_failure)
;;

let test_sequence ~form ~values ~sexp_of_t ~equal ~on_failure =
  Sequence.iter values ~f:(fun value -> test ~form ~value ~sexp_of_t ~equal ~on_failure)
;;

let%test_module _ =
  (module struct
    let test = test ~on_failure:`Print
    let test_list = test_list ~on_failure:`Print
    let test_sequence = test_sequence ~on_failure:`Print

    module type M = sig
      type t

      val equal : t -> t -> bool
      val sexp_of_t : t -> Sexp.t
      val quickcheck_generator : t Quickcheck.Generator.t
    end

    let test_form_randomly (type a) (module M : M with type t = a) form =
      let values =
        Sequence.take (Quickcheck.random_sequence M.quickcheck_generator) 100
      in
      test_sequence ~form ~values ~sexp_of_t:M.sexp_of_t ~equal:M.equal
    ;;

    let%expect_test "the [string] primitive is correct" =
      test_form_randomly (module String) (string ());
      [%expect {| |}]
    ;;

    let%expect_test "the [int] primitive is correct" =
      test_form_randomly (module Int) int;
      [%expect {| |}]
    ;;

    let%expect_test "the [from_ppx_sexp_raw] primitive is correct" =
      let open Date in
      test_form_randomly (module Date) (from_ppx_sexp_raw t_of_sexp);
      [%expect {| |}];
      let open String in
      test_form_randomly (module String) (from_ppx_sexp_raw t_of_sexp);
      [%expect {| |}];
      let open Sexp in
      test_form_randomly (module Sexp) (from_ppx_sexp_raw t_of_sexp);
      [%expect {| |}]
    ;;

    let%expect_test "the [from_ppx_sexp] primitive is correct" =
      let open Date in
      test_form_randomly (module Date) (from_ppx_sexp ~t_of_sexp ());
      [%expect {| |}];
      let open String in
      let form = from_ppx_sexp ~t_of_sexp () in
      test_form_randomly (module String) form;
      test ~form ~value:"\\\\\\" ~sexp_of_t ~equal;
      [%expect {| |}];
      let open Sexp in
      test_form_randomly (module Sexp) (from_ppx_sexp ~t_of_sexp ());
      [%expect {| |}]
    ;;

    let%expect_test "the [bool] primitives are correct" =
      test_form_randomly (module Bool) bool_true_false;
      test_form_randomly (module Bool) bool_yes_no;
      [%expect {| |}]
    ;;

    let%expect_test "the [tuple5] primitive is correct" =
      let form = tuple5 int int (string ()) bool_true_false bool_yes_no in
      let values = [ 1, 2, "foo", true, false; 12, 33, "bar", false, false ] in
      test_list
        ~form
        ~values
        ~sexp_of_t:[%sexp_of: int * int * string * bool * bool]
        ~equal:[%compare.equal: int * int * string * bool * bool];
      [%expect {| |}]
    ;;

    let%expect_test "the [option] primitive is correct" =
      let form = option int in
      let values = [ None; Some 0; Some 42 ] in
      test_list
        ~form
        ~values
        ~sexp_of_t:[%sexp_of: int option]
        ~equal:[%compare.equal: int option];
      [%expect {| |}]
    ;;

    let%expect_test "the [list] primitive is correct" =
      List.iter [ `Ordered; `Unordered ] ~f:(fun order ->
        let form = list ~order int in
        let values = [ []; [ 1 ]; [ 23; 44 ] ] in
        test_list
          ~form
          ~values
          ~sexp_of_t:[%sexp_of: int list]
          ~equal:[%compare.equal: int list]);
      [%expect {| |}]
    ;;

    module Foo = struct
      type t =
        | A
        | B of int
        | C of int * t
      [@@deriving variants, sexp_of, compare]

      let equal = [%compare.equal: t]
    end

    let%expect_test "the [variant] primitive is correct" =
      let open Foo in
      let form =
        recursive (fun form ->
          variant
            (Variants.fold
               ~init:[]
               ~a:(fun acc a -> case a :: acc)
               ~b:(fun acc b -> (case b <|*> int) :: acc)
               ~c:(fun acc c -> (case c <|*> int <|*> form) :: acc)))
      in
      let values = [ A; B 12; C (42, A); C (17, B (-44)); C (0, C (1, C (123, A))) ] in
      test_list ~form ~values ~sexp_of_t ~equal;
      [%expect {| |}]
    ;;

    module Bar = struct
      type t =
        { a : int
        ; b : string option [@sexp.option]
        ; c : t option
        }
      [@@deriving fields, sexp_of, compare]

      let equal = [%compare.equal: t]
    end

    let%expect_test "the [record] primitive is correct" =
      let open Bar in
      let form =
        recursive (fun form ->
          record ~create:(fun a b c -> Fields.create ~a ~b ~c)
          <.*> field int Fields.a
          <.*> sexp_option_field (string ()) Fields.b
          <.*> field (option form) Fields.c
          |> finish_record)
      in
      let values =
        [ { a = 1; b = None; c = None }
        ; { a = 1; b = Some "foo"; c = None }
        ; { a = 4; b = None; c = Some { a = 12; b = Some "baz"; c = None } }
        ]
      in
      test_list ~form ~values ~sexp_of_t ~equal;
      [%expect {| |}]
    ;;

    let%expect_test "the [dropdown_with_other] primitive is correct" =
      let open Int in
      let values = [ 1; 2; 3; 4; 1234 ] in
      let form =
        dropdown_with_other ~other:int ~sexp_of_t [ "one", 1; "3", 2; "4", 2 ]
      in
      test_list ~form ~values ~sexp_of_t ~equal
    ;;
  end)
;;

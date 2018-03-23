open Core_kernel
open Incr_dom
open Incr.Let_syntax
open Incr_dom_widgets

open Vdom

open! Int.Replace_polymorphic_compare

module User_id = Unique_id.Int()

module Activity = struct
  type t =
    | Name of string
    | Category of string * t list
  [@@deriving compare, sexp]
end

module Info : sig
  type t [@@deriving compare, sexp_of]
  val create_exn : name:string -> value:string -> t
  val name  : t -> string
  val value : t -> string
end = struct
  type t = string * string [@@deriving compare, sexp_of]

  let create_exn ~name ~value =
    if String.is_empty name || String.is_empty value
    then (failwith "One of the info fields is empty")
    else (name, value)

  let name  = fst
  let value = snd
end

module Office = struct
  type t = NYC | LDN | HKG
  [@@deriving compare, sexp, enumerate]

  let to_string_hum = function
    | NYC -> "New York"
    | LDN -> "London"
    | HKG -> "Hong Kong"
end

module User = struct

  module Group = struct
    module T = struct
      type t =
        | Tech
        | Trading
        | Other
      [@@deriving compare, sexp, enumerate]
    end
    include T
    include Sexpable.To_stringable(T)
  end


  type t = {
    firstname  : string;
    lastname   : string;
    group      : Group.t;
    fulltime   : bool;
    office     : Office.t;
    activities : Activity.t list;
    info       : Info.t list;
  } [@@deriving compare, fields, sexp_of]

  let form =
    let open Form.Description in
    let activities_form =
      sexp
        ~sexp_of:[%sexp_of: Activity.t list]
        ~of_sexp:[%of_sexp: Activity.t list]
    in
    let open Form.Description.Let_syntax in
    let unvalidated_info =
      let%map_open name = string <^ Info.name
      and value = string <^ Info.value
      in name, value
    in
    let info_form =
      conv unvalidated_info
        ~f:(fun (name, value) (_, _) ~block_id:info_block ->
          match Info.create_exn ~name ~value with
          | v -> Ok v
          | exception exn ->
            Error [Form.Form_error.create ~id:info_block (Error.of_exn exn)]
        )
    in
    let unvalidated_user =
      let open Of_record in
      build_for_record (
        Fields.make_creator
          ~firstname:(field string)
          ~lastname:(field string)
          ~group:(field (variant Group.all ~equal:[%compare.equal: Group.t]))
          ~fulltime:(field bool)
          ~office:(field (variant Office.all ~equal:[%compare.equal: Office.t]))
          ~activities:(field activities_form)
          ~info:(field (list info_form))
      )
    in
    let user_form =
      conv unvalidated_user
        ~f:(fun ({ firstname; lastname; group = _; fulltime = _;
                   office = _; activities = _; info = _; } as t)
             (firstname_field, (lastname_field, (_, _))) ~block_id:user_block ->
             let errors =
               List.filter_opt
                 [ if String.is_empty firstname
                   then
                     (Some (Form.Form_error.create ~id:firstname_field
                              (Error.of_string "Firstname can't be empty")))
                   else None
                 ; if String.is_empty lastname
                   then
                     (Some (Form.Form_error.create ~id:lastname_field
                              (Error.of_string "Lastname can't be empty")))
                   else None
                 ; if String.equal firstname lastname
                   then
                     (Some (Form.Form_error.create ~id:user_block
                              (Error.of_string "Firstname and Lastname must be different")))
                   else None
                 ]
             in
             if List.is_empty errors
             then (Ok t)
             else (Error errors)
           )
    in
    let user_id_opt = not_editable ~default:None in
    let user_edit_form =
      let%map_open user_form = user_form <^ fst
      and user_id_opt = user_id_opt <^ snd
      in user_form, user_id_opt
    in
    Form.create ~name:"user edit form" user_edit_form
  ;;

end

let loaded_users : User.t User_id.Map.t =
  let info_list_of_tuples tuples =
    List.map tuples ~f:(fun (name, value) -> Info.create_exn ~name ~value)
  in
  let open User in
  [ {
    firstname = "Bobby";
    lastname = "Tables";
    group = Tech;
    fulltime = false;
    office = LDN;
    activities = [Name "couch"];
    info = info_list_of_tuples
                [ "favourite language", "sql"
                ; "achievements", "best student in school"];
  }
  ; {
    firstname = "Firstname";
    lastname = "Lastname";
    group = Other;
    fulltime = true;
    office = NYC;
    activities = [Name "ocaml"; Category ("physical",[Name "cycling"; Name "jogging"])];
    info = info_list_of_tuples
                [ "email", "firstname.lastname@example.com"
                ; "joined date", "01.01.1970"
                ; "nicknames", "First, Last, Buddy" ];
  }
  ]
  |> List.map ~f:(fun u -> User_id.create (), u)
  |> User_id.Map.of_alist_exn
;;

let init =
  let i, u = Map.min_elt_exn loaded_users in
  u, Some i

let form_state = Form.State.create ~init User.form

module Model = struct
  type t = {
    form_state   : Form.State.t;
    users        : User.t User_id.Map.t;
    tick_counter : int;
  } [@@deriving compare]

  let cutoff t1 t2 = compare t1 t2 = 0
end

module State = struct type t = unit end

module Action = struct
  type t =
    | Update_form_state of Form.State.t sexp_opaque
    | Load_form of (User.t * User_id.t) option
    | Update_user of User.t * User_id.t option * Form.State.t sexp_opaque
    | Tick
  [@@deriving sexp_of]

  let should_log _ = true
end

let update_visibility m = m

let apply_action (action : Action.t) (model : Model.t) (_state : State.t) : Model.t =
  match action with
  | Tick ->
    { model with tick_counter = succ model.tick_counter }
  | Update_form_state form_state -> { model with form_state; }
  | Update_user (user, idx, form_state) ->
    let idx, next_form_state =
      match idx with
      | None -> User_id.create (), Form.State.create User.form
      | Some idx -> idx, form_state
    in
    let users = Map.set model.users ~key:idx ~data:user in
    { model with users; form_state = next_form_state; }
  | Load_form u_opt ->
    let init = Option.map u_opt ~f:(fun (u,i) -> u, Some i) in
    let form_state = Form.State.create ?init User.form in
    { model with form_state; }

let on_startup ~schedule _ =
  Async_kernel.Clock_ns.every (Time_ns.Span.of_sec 5.) (fun () -> schedule (Action.Tick));
  Async_kernel.return ()

let view (model : Model.t Incr.t) ~inject : Vdom.Node.t Incr.t =
  let username (user : User.t) =
    String.lowercase ((String.prefix user.firstname 1) ^ user.lastname)
  in
  let%map model = model in
  let state = model.form_state in
  let user_to_li (user_id, (user : User.t)) =
    Node.li [] [
      Node.span [
        Attr.on_click (fun _evt -> inject (Action.Load_form (Some (user, user_id))))]
        [Node.text (username user)]
    ]
  in
  let username_list =
    let users = Map.to_alist model.users in
    Node.create "ol" [] (List.map users ~f:user_to_li)
  in
  let
    (user_block_id,
     (first_id,
      (last_id,
       (group_id,
        (ft_id,
         (office_id,
           (activities_id,
            ((info_id_list, info_list_id),
             ())))))))),
    () =
    Form.State.field_ids state User.form
  in
  let div_with_err id children =
    let no_err_node = Node.div [] [] in
    let err_node err =
      let string =
        String.strip ~drop:(fun c -> Char.equal c '"') (Error.to_string_hum err)
      in
      Node.div [Attr.style ["color","red"]] [ Node.text string ]
    in
    match Form.State.error state id with
    | None -> no_err_node :: children
    | Some err -> err_node err :: children
  in
  let info_div =
    Node.div []
      ((Node.text "Misc information") ::
       (List.concat_mapi info_id_list
          ~f:(fun i (info_id, (name_id, value_id)) ->
            div_with_err info_id
              [ Form.Input.text state name_id []
              ; Form.Input.text state value_id []
              ; Node.button
                  [ Attr.type_ "button"
                  ; Attr.on_click (fun _evt ->
                      let state =
                        Form.List.remove_nth state info_list_id i
                      in
                      inject (Action.Update_form_state state))
                  ] [ Node.text "Remove" ]
              ]
          )
       ))
  in
  Node.body []
    [ Node.p []
        [ Node.text
            "Enter the first and the last names, press Add button and
             the resulting username will be added to the list."
        ]
    ; Node.div []
        (div_with_err user_block_id
           [ Node.div []
               (div_with_err first_id
                  [ Node.text "Firstname"
                  ; Form.Input.text state first_id []
                  ])
           ; Node.div []
               (div_with_err last_id
                  [ Node.text "Lastname"
                  ; Form.Input.text state last_id []
                  ])
           ; Node.div []
               [(Node.text "Group")
               ; (Form.Input.dropdown state group_id []
                    ~compare:User.Group.compare User.Group.to_string)]
           ; Node.div []
               [ Node.text "Fulltime"
               ; Form.Input.checkbox state ft_id []
               ]
           ; Node.div []
               ((Node.div [] [Node.text "Office"]) ::
                (
                  let attr = [Attr.style ["margin-left","1em"]] in
                  Form.Input.radio_button state office_id attr
                  |> List.concat_map ~f:(fun (opt, node) ->
                    [ node
                    ; Node.text (Office.to_string_hum opt)
                    ])
                ))
           ; Node.div []
               (div_with_err activities_id
                  [ Node.text "Activities"
                  ; Node.div [] []
                  ; Form.Input.textarea state activities_id []
                  ])
           ; info_div
           ; Node.div []
               [ Node.button
                   [ Attr.type_ "button"
                   ; Attr.on_click (fun _evt ->
                       let list_adder =
                         { Form.List.transform =
                             fun list ~create_new_form ->
                               list @ [create_new_form ()]
                         } in
                       let state =
                         Form.List.modify_list state info_list_id ~f:list_adder
                       in
                       inject (Action.Update_form_state state))
                   ] [ Node.text "Add" ]
               ]
           ; Node.button
               [ Attr.type_ "button"
               ; Attr.on_click (fun _evt ->
                   let user_idx_opt, new_state = Form.State.read_value state User.form in
                   match user_idx_opt with
                   | None -> inject (Action.Update_form_state new_state)
                   | Some (user, user_id) -> inject (Action.Update_user (user, user_id, new_state))
                 )
               ] [ Node.text "Submit" ]
           ; Node.button
               [ Attr.type_ "button"
               ; Attr.on_click (fun _evt -> inject (Action.Load_form None))
               ] [ Node.text "New" ]
           ]
        )
    ; Node.create "hr" [] []
    ; username_list
    ]

let on_display ~old:_ _ _ = ()

let create () =
  { Model.
    form_state;
    users = loaded_users;
    tick_counter = 0;
  }

module ServiceMappingRow = struct
  type t =
    { id : string
    ; member_id : string
    ; member_label : string
    ; created_at : Ptime.t
    }
  [@@deriving show, make, fields]

  let t =
    let encode m = Ok (m.id, (m.member_id, (m.member_label, m.created_at))) in
    let decode (id, (member_id, (member_label, created_at))) =
      Ok { id; member_id; member_label; created_at }
    in
    Caqti_type.(custom ~encode ~decode (tup2 string (tup2 string (tup2 string ptime))))
  ;;

  let equal x y =
    String.equal x.id y.id
    && String.equal x.member_id y.member_id
    && String.equal x.member_label y.member_label
  ;;
end

module QuestionnaireMappingRow = struct
  type t =
    { service_mapper : string
    ; label : string
    ; questionnaire : string
    ; created_at : Ptime.t
    ; updated_at : Ptime.t
    }
  [@@deriving show, fields]

  let t =
    let encode m =
      Ok (m.service_mapper, (m.label, (m.questionnaire, (m.created_at, m.updated_at))))
    in
    let decode (service_mapper, (label, (questionnaire, (created_at, updated_at)))) =
      Ok { service_mapper; label; questionnaire; created_at; updated_at }
    in
    Caqti_type.(
      custom ~encode ~decode (tup2 string (tup2 string (tup2 string (tup2 ptime ptime)))))
  ;;

  let create ~service_mapper ?label ~questionnaire ?created_at ?updated_at () =
    { service_mapper
    ; label = label |> Option.value ~default:""
    ; questionnaire
    ; created_at = created_at |> Option.value ~default:(Ptime_clock.now ())
    ; updated_at = updated_at |> Option.value ~default:(Ptime_clock.now ())
    }
  ;;

  let equal x y =
    String.equal x.service_mapper y.service_mapper
    && String.equal x.label y.label
    && String.equal x.questionnaire y.questionnaire
  ;;
end

module QuestionnaireMapping = struct
  type t =
    { label : string
    ; questionnaire : Ask.Model.Questionnaire.t
    ; created_at : Ptime.t
    ; updated_at : Ptime.t
    }
  [@@deriving show, make, fields]

  let to_mapping_row ~id model =
    QuestionnaireMappingRow.create
      ~service_mapper:id
      ~label:model.label
      ~questionnaire:(Ask.Model.Questionnaire.uuid model.questionnaire)
      ~created_at:model.created_at
      ~updated_at:model.updated_at
      ()
  ;;

  let equal x y =
    String.equal x.label y.label
    && String.equal
         (Ask.Model.Questionnaire.uuid x.questionnaire)
         (Ask.Model.Questionnaire.uuid y.questionnaire)
  ;;
end

module Handler = struct
  type t =
    { id : string
    ; member_id : string
    ; label : string
    ; questionnaires : (string * Ask.Model.Questionnaire.t) list
    ; created_at : Ptime.t
    ; updated_at : Ptime.t
    }
  [@@deriving show, fields]

  let equal x y =
    String.equal x.id y.id
    && String.equal x.member_id y.member_id
    && String.equal x.label y.label
    &&
    match CCList.is_empty x.questionnaires, CCList.is_empty y.questionnaires with
    | true, true -> true
    | false, false ->
      let sort_label (x, _) (y, _) = String.compare x y in
      CCList.for_all2
        (fun (lx, qx) (ly, qy) ->
          String.equal lx ly && Ask.Model.Questionnaire.equal qx qy)
        (x.questionnaires |> CCList.stable_sort sort_label)
        (y.questionnaires |> CCList.stable_sort sort_label)
    | _ -> false
  ;;

  let create
      ?id
      ~member_id
      ?label
      ~(questionnaires : (string * Ask.Model.Questionnaire.t) list)
      ?created_at
      ?updated_at
      ()
    =
    { id = id |> Option.value ~default:(Uuidm.create `V4 |> Uuidm.to_string)
    ; member_id
    ; label = label |> Option.value ~default:""
    ; questionnaires
    ; created_at = created_at |> Option.value ~default:(Ptime_clock.now ())
    ; updated_at = updated_at |> Option.value ~default:(Ptime_clock.now ())
    }
  ;;

  let create_from_models
      ~(service_mapper : ServiceMappingRow.t)
      ~(questionnaire_mapper : QuestionnaireMapping.t list)
    =
    let rec newest = function
      | [] -> Ptime_clock.now ()
      | [ x ] -> x
      | x :: xs -> if Ptime.is_later x ~than:(newest xs) then x else newest xs
    in
    let last_updated =
      CCList.map
        (fun (element : QuestionnaireMapping.t) -> element.updated_at)
        questionnaire_mapper
      |> newest
    in
    let questionnaires =
      CCList.map
        (fun (row : QuestionnaireMapping.t) -> row.label, row.questionnaire)
        questionnaire_mapper
    in
    create
      ~id:service_mapper.id
      ~member_id:service_mapper.member_id
      ~label:service_mapper.member_label
      ~questionnaires
      ~created_at:service_mapper.created_at
      ~updated_at:last_updated
  ;;

  let to_models (handler : t) =
    let service_mapper =
      ServiceMappingRow.make
        ~id:handler.id
        ~member_id:handler.member_id
        ~member_label:handler.label
        ~created_at:handler.created_at
    in
    let questionnaire_mapper =
      List.map
        (fun (label, questionnaire) ->
          QuestionnaireMapping.make
            ~label
            ~questionnaire
            ~created_at:handler.created_at
            ~updated_at:handler.created_at)
        handler.questionnaires
    in
    service_mapper, questionnaire_mapper
  ;;
end

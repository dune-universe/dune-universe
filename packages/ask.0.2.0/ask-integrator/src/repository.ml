open Lwt.Syntax

module type Sig = sig
  val clean : unit -> unit Lwt.t
  val register_cleaner : unit -> unit
  val register_migration : unit -> unit
  val lifecycles : Sihl.Container.lifecycle list

  val is_unique
    :  member_id:string
    -> member_label:string
    -> ?id:string
    -> unit
    -> bool Lwt.t

  val find_questionnaires : string -> Model.QuestionnaireMappingRow.t list Lwt.t
  val find : string -> Model.Handler.t option Lwt.t

  val find_by_member
    :  member_id:string
    -> member_label:string
    -> Model.Handler.t option Lwt.t

  val insert : Model.Handler.t -> unit Lwt.t

  val update
    :  Model.Handler.t
    -> ?member_label:string
    -> ?questionnaires:(string * Ask.Model.Questionnaire.t) list
    -> unit
    -> (string, string) result Lwt.t

  val delete : Model.Handler.t -> unit Lwt.t

  val row_to_quetionnaire_mapper
    :  Model.QuestionnaireMappingRow.t
    -> Model.QuestionnaireMapping.t option Lwt.t
end

let raise_caqti_error err =
  match err with
  | Error err -> failwith (Caqti_error.show err)
  | Ok result -> result
;;

module MariaDb = struct
  module MigrationService = Sihl.Database.Migration.MariaDb

  module DbUtils = struct
    let set_fk_check_request =
      Caqti_request.exec Caqti_type.bool "SET FOREIGN_KEY_CHECKS = ?;"
    ;;

    let with_disabled_fk_check f =
      Sihl.Database.query (fun connection ->
          let module Connection = (val connection : Caqti_lwt.CONNECTION) in
          let* () =
            Connection.exec set_fk_check_request false |> Lwt.map raise_caqti_error
          in
          Lwt.finalize
            (fun () -> f connection)
            (fun () ->
              Connection.exec set_fk_check_request true |> Lwt.map raise_caqti_error))
    ;;

    let find request id =
      Sihl.Database.query (fun connection ->
          let module Connection = (val connection : Caqti_lwt.CONNECTION) in
          Connection.find_opt request id |> Lwt.map raise_caqti_error)
    ;;

    let find_exn request id =
      Sihl.Database.query (fun connection ->
          let module Connection = (val connection : Caqti_lwt.CONNECTION) in
          Connection.find request id |> Lwt.map raise_caqti_error)
    ;;

    module Dynparam = struct
      type t = Pack : 'a Caqti_type.t * 'a -> t

      let empty = Pack (Caqti_type.unit, ())
      let add t x (Pack (t', x')) = Pack (Caqti_type.tup2 t' t, (x', x))
    end

    let is_unique_request_query table_name sql_joins sql_filter with_id () =
      let fragment =
        if with_id
        then
          Caml.Format.asprintf
            {sql| AND %s.uuid != UNHEX(REPLACE(?, '-', '')) LIMIT 1) |sql}
            table_name
        else ""
      in
      Caml.Format.asprintf
        {sql| SELECT NOT EXISTS (SELECT 1 FROM %s %s WHERE %s %s )|sql}
        table_name
        sql_joins
        sql_filter
        fragment
    ;;

    let is_unique_request table_name sql_filter request_types sql_joins with_id () =
      let sql_request =
        is_unique_request_query table_name sql_joins sql_filter with_id ()
      in
      Caqti_request.find ~oneshot:true request_types Caqti_type.bool sql_request
    ;;

    let is_unique connection table_name ~sql_filter ~values ?sql_joins ?id () =
      let sql_joins = sql_joins |> Option.value ~default:"" in
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      match id with
      | None ->
        let (Dynparam.Pack (pt, pv)) = values in
        Connection.find (is_unique_request table_name sql_filter pt sql_joins false ()) pv
        |> Lwt.map raise_caqti_error
      | Some id ->
        let values = Dynparam.add Caqti_type.string id values in
        let (Dynparam.Pack (pt, pv)) = values in
        Connection.find (is_unique_request table_name sql_filter pt sql_joins true ()) pv
        |> Lwt.map raise_caqti_error
    ;;

    let clean request connection =
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec request () |> Lwt.map raise_caqti_error
    ;;
  end

  let is_unique ~member_id ~member_label ?id () =
    Sihl.Database.query (fun connection ->
        let table_name = "ask_integration_service_mappings" in
        let sql_filter = "member_uuid = ? AND member_label = ?" in
        let values =
          DbUtils.Dynparam.empty
          |> DbUtils.Dynparam.add Caqti_type.string member_id
          |> DbUtils.Dynparam.add Caqti_type.string member_label
        in
        DbUtils.is_unique connection table_name ~sql_filter ~values ?id ())
  ;;

  let find_service_map_request =
    Caqti_request.find
      Caqti_type.string
      Model.ServiceMappingRow.t
      {sql|
          SELECT
            LOWER(CONCAT(
              SUBSTR(HEX(uuid), 1, 8), '-',
              SUBSTR(HEX(uuid), 9, 4), '-',
              SUBSTR(HEX(uuid), 13, 4), '-',
              SUBSTR(HEX(uuid), 17, 4), '-',
              SUBSTR(HEX(uuid), 21)
            )),
            LOWER(CONCAT(
              SUBSTR(HEX(member_uuid), 1, 8), '-',
              SUBSTR(HEX(member_uuid), 9, 4), '-',
              SUBSTR(HEX(member_uuid), 13, 4), '-',
              SUBSTR(HEX(member_uuid), 17, 4), '-',
              SUBSTR(HEX(member_uuid), 21)
            )),
            member_label,
            created_at
          FROM ask_integration_service_mappings
          WHERE uuid = UNHEX(REPLACE(?, '-', ''))
        |sql}
  ;;

  let find_service_map = DbUtils.find find_service_map_request
  let find_service_map_exn = DbUtils.find_exn find_service_map_request

  let find_service_map_by_member_request =
    Caqti_request.find
      Caqti_type.(tup2 string string)
      Model.ServiceMappingRow.t
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(uuid), 1, 8), '-',
            SUBSTR(HEX(uuid), 9, 4), '-',
            SUBSTR(HEX(uuid), 13, 4), '-',
            SUBSTR(HEX(uuid), 17, 4), '-',
            SUBSTR(HEX(uuid), 21)
          )),
          LOWER(CONCAT(
            SUBSTR(HEX(member_uuid), 1, 8), '-',
            SUBSTR(HEX(member_uuid), 9, 4), '-',
            SUBSTR(HEX(member_uuid), 13, 4), '-',
            SUBSTR(HEX(member_uuid), 17, 4), '-',
            SUBSTR(HEX(member_uuid), 21)
          )),
          member_label,
          created_at
        FROM ask_integration_service_mappings
        WHERE member_uuid = UNHEX(REPLACE(?, '-', '')) AND member_label = ?
      |sql}
  ;;

  let find_service_map_by_member = DbUtils.find find_service_map_by_member_request
  let find_service_map_by_member_exn = DbUtils.find_exn find_service_map_by_member_request

  let update_service_map_request =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      {sql|
        UPDATE ask_integration_service_mappings
        SET member_label = $2
        WHERE uuid = UNHEX(REPLACE($1, '-', ''));
      |sql}
  ;;

  let update_service_map model =
    let id = Model.ServiceMappingRow.id model in
    let label = Model.ServiceMappingRow.member_label model in
    Sihl.Database.query (fun connection ->
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec update_service_map_request (id, label)
        |> Lwt.map raise_caqti_error)
  ;;

  let delete_service_map_request =
    Caqti_request.exec
      Caqti_type.string
      {sql|
          DELETE FROM ask_integration_service_mappings
          WHERE uuid = UNHEX(REPLACE(?, '-', ''));
        |sql}
  ;;

  let delete_service_map model =
    let id = Model.ServiceMappingRow.id model in
    Sihl.Database.query (fun connection ->
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec delete_service_map_request id |> Lwt.map raise_caqti_error)
  ;;

  let find_questionnaires_request =
    Caqti_request.collect
      Caqti_type.string
      Model.QuestionnaireMappingRow.t
      {sql|
          SELECT
          LOWER(CONCAT(
              SUBSTR(HEX(service_map.uuid), 1, 8), '-',
              SUBSTR(HEX(service_map.uuid), 9, 4), '-',
              SUBSTR(HEX(service_map.uuid), 13, 4), '-',
              SUBSTR(HEX(service_map.uuid), 17, 4), '-',
              SUBSTR(HEX(service_map.uuid), 21)
            )),
            ask_map.questionnaire_label,
            LOWER(CONCAT(
              SUBSTR(HEX(ask_map.questionnaire), 1, 8), '-',
              SUBSTR(HEX(ask_map.questionnaire), 9, 4), '-',
              SUBSTR(HEX(ask_map.questionnaire), 13, 4), '-',
              SUBSTR(HEX(ask_map.questionnaire), 17, 4), '-',
              SUBSTR(HEX(ask_map.questionnaire), 21)
            )),
            ask_map.created_at,
            ask_map.updated_at
          FROM ask_integration_questionnaire_mappings AS ask_map
          LEFT JOIN ask_integration_service_mappings AS service_map
            ON service_map.id = ask_map.ask_integration_service_mapping
          WHERE service_map.uuid = UNHEX(REPLACE(?, '-', ''))
        |sql}
  ;;

  let find_questionnaires id =
    Sihl.Database.query (fun connection ->
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.collect_list find_questionnaires_request id
        |> Lwt.map raise_caqti_error)
  ;;

  let insert_service_mapping_request =
    Caqti_request.exec
      Model.ServiceMappingRow.t
      {sql|
        INSERT INTO ask_integration_service_mappings (
          uuid,
          member_uuid,
          member_label,
          created_at
        ) VALUES (
          UNHEX(REPLACE($1, '-', '')),
          UNHEX(REPLACE($2, '-', '')),
          $3,
          $4
        )
      |sql}
  ;;

  let insert_questionnaire_mapping_request =
    Caqti_request.exec
      Model.QuestionnaireMappingRow.t
      {sql|
          INSERT INTO ask_integration_questionnaire_mappings (
            ask_integration_service_mapping,
            questionnaire_label,
            questionnaire,
            created_at,
            updated_at
            ) VALUES (
            (SELECT id FROM ask_integration_service_mappings WHERE uuid = UNHEX(REPLACE($1, '-', ''))),
            $2,
            UNHEX(REPLACE($3, '-', '')),
            $4,
            $5
          )
        |sql}
  ;;

  let insert_questionnaire_mapping model =
    Sihl.Database.query (fun connection ->
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec insert_questionnaire_mapping_request model
        |> Lwt.map raise_caqti_error)
  ;;

  let update_questionnaire_mapping_request =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      {sql|
        UPDATE ask_integration_questionnaire_mappings
        SET questionnaire_label = $2
        WHERE id = (SELECT id FROM ask_integration_service_mappings WHERE uuid = UNHEX(REPLACE($1, '-', '')));
      |sql}
  ;;

  let update_questionnaire_mapping model =
    let id = Model.QuestionnaireMappingRow.service_mapper model in
    let label = Model.QuestionnaireMappingRow.label model in
    Sihl.Database.query (fun connection ->
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec update_questionnaire_mapping_request (id, label)
        |> Lwt.map raise_caqti_error)
  ;;

  let delete_questionnaire_mapping_request =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      {sql|
          DELETE FROM ask_integration_questionnaire_mappings
          WHERE
            id = (SELECT id FROM ask_integration_service_mappings WHERE uuid = UNHEX(REPLACE(?, '-', '')))
            AND questionnaire = UNHEX(REPLACE(?, '-', ''));
        |sql}
  ;;

  let delete_questionnaire_mapping model =
    let id = Model.QuestionnaireMappingRow.service_mapper model in
    let questionnaire = Model.QuestionnaireMappingRow.questionnaire model in
    Sihl.Database.query (fun connection ->
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        Connection.exec delete_questionnaire_mapping_request (id, questionnaire)
        |> Lwt.map raise_caqti_error)
  ;;

  let row_to_quetionnaire_mapper (model : Model.QuestionnaireMappingRow.t) =
    let* questionnaire = Ask.MariaDb.Questionnaire.find model.questionnaire in
    match questionnaire with
    | Some questionnaire ->
      Model.QuestionnaireMapping.make
        ~label:model.label
        ~questionnaire
        ~created_at:model.created_at
        ~updated_at:model.updated_at
      |> Lwt.return_some
    | _ -> Lwt.return_none
  ;;

  let row_to_quetionnaire_mapper_exn (model : Model.QuestionnaireMappingRow.t) () =
    let* mapper = row_to_quetionnaire_mapper model in
    match mapper with
    | None ->
      failwith "Couldn't generate questionnaire mapper from questionnaire mapper row!"
    | Some mapper -> mapper |> Lwt.return
  ;;

  let service_mapper_to_handler service_mapper =
    let* questionnaire_mapper =
      service_mapper |> Model.ServiceMappingRow.id |> find_questionnaires
    in
    let* questionnaire_mapper =
      Lwt_list.filter_map_s
        (fun row -> row_to_quetionnaire_mapper row)
        questionnaire_mapper
    in
    Model.Handler.create_from_models ~service_mapper ~questionnaire_mapper ()
    |> Lwt.return_some
  ;;

  let find id =
    let* service_mapper = find_service_map id in
    match service_mapper with
    | None -> Lwt.return_none
    | Some service_mapper -> service_mapper_to_handler service_mapper
  ;;

  let find_by_member ~member_id ~member_label =
    let* service_mapper = find_service_map_by_member (member_id, member_label) in
    match service_mapper with
    | None -> Lwt.return_none
    | Some service_mapper -> service_mapper_to_handler service_mapper
  ;;

  let insert model =
    let open Lwt_result.Syntax in
    let service_mapper, questionnaire_mappers = Model.Handler.to_models model in
    Sihl.Database.transaction' (fun connection ->
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        let* () = Connection.exec insert_service_mapping_request service_mapper in
        List.fold_left
          (fun result questionnaire_map ->
            let* () = result in
            let model =
              questionnaire_map
              |> Model.QuestionnaireMapping.to_mapping_row ~id:service_mapper.id
            in
            Connection.exec insert_questionnaire_mapping_request model)
          (Lwt_result.return ())
          questionnaire_mappers)
  ;;

  let update model ?member_label ?questionnaires () =
    if Option.is_none member_label && Option.is_none questionnaires
    then
      Lwt.return
        (Error "Nothing to update! Please provide at least a label or questionnaires.")
    else (
      let service_mapper, questionnaire_mapper = Model.Handler.to_models model in
      let* () =
        match member_label with
        | Some member_label
          when not (String.equal (Model.Handler.label model) member_label) ->
          update_service_map { service_mapper with member_label }
        | _ -> Lwt.return_unit
      in
      let* () =
        match questionnaires with
        | None -> Lwt.return_unit
        | Some questionnaires ->
          let handler_id = Model.Handler.id model in
          let compare_first_in_tuple (x, _) (y, _) = String.compare x y in
          let questionnaires = List.stable_sort compare_first_in_tuple questionnaires in
          (* 1. Check if some links to questionnaires were deleted *)
          let deleted_questionnaires =
            CCList.filter
              (fun mapper ->
                not
                  (List.mem
                     (Model.QuestionnaireMapping.questionnaire mapper)
                     (List.map (fun (_, x) -> x) questionnaires)))
              questionnaire_mapper
          in
          let* _ =
            Lwt_list.map_p
              (fun questionnaire_mapping_to_delete ->
                delete_questionnaire_mapping
                  (Model.QuestionnaireMapping.to_mapping_row
                     ~id:handler_id
                     questionnaire_mapping_to_delete))
              deleted_questionnaires
          in
          (* 2. Update or create all other questionnaire mapper *)
          let* _ =
            Lwt_list.map_s
              (fun (label, questionnaire) ->
                let questionnaire_id = Ask.Model.Questionnaire.uuid questionnaire in
                let existing_questionnaire_mapper =
                  List.find_opt
                    (fun mapper ->
                      let id =
                        Ask.Model.Questionnaire.uuid
                          (Model.QuestionnaireMapping.questionnaire mapper)
                      in
                      String.equal id questionnaire_id)
                    questionnaire_mapper
                in
                match existing_questionnaire_mapper with
                | Some mapper ->
                  update_questionnaire_mapping
                    (Model.QuestionnaireMapping.to_mapping_row
                       ~id:handler_id
                       { mapper with label })
                | None ->
                  let mapper =
                    Model.QuestionnaireMappingRow.create
                      ~service_mapper:handler_id
                      ~label
                      ~questionnaire:questionnaire_id
                      ()
                  in
                  insert_questionnaire_mapping mapper)
              questionnaires
          in
          Lwt.return_unit
      in
      Lwt.return (Ok "Ask integrator handler updated"))
  ;;

  let delete model =
    let service_mapper, _ = Model.Handler.to_models model in
    delete_service_map service_mapper
  ;;

  let clean_questionnaire_mappings_request =
    Caqti_request.exec
      Caqti_type.unit
      "TRUNCATE TABLE ask_integration_questionnaire_mappings;"
  ;;

  let clean_questionnaire_mappings = DbUtils.clean clean_questionnaire_mappings_request

  let clean_service_mappings_request =
    Caqti_request.exec Caqti_type.unit "TRUNCATE TABLE ask_integration_service_mappings;"
  ;;

  let clean_service_mappings = DbUtils.clean clean_service_mappings_request

  let clean () =
    DbUtils.with_disabled_fk_check (fun connection ->
        let* () = clean_questionnaire_mappings connection in
        clean_service_mappings connection)
  ;;

  let register_migration () =
    MigrationService.register_migration (Migration.MariaDB.migration ())
  ;;

  let register_cleaner () = Sihl.Cleaner.register_cleaner clean

  let lifecycles =
    [ Sihl.Database.lifecycle; MigrationService.lifecycle; Ask.MariaDb.lifecycle ]
  ;;
end

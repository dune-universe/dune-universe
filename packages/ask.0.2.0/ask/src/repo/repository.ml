open Lwt.Syntax
module Database = Sihl.Database
module RepoSql = Repository_sql
module RepoModel = Repository_model

module type Sig = sig
  val lifecycles : Sihl.Container.lifecycle list
  val register_migration : unit -> unit
  val register_cleaner : unit -> unit
  val clean : unit -> unit Lwt.t

  module Questionnaire : sig
    val find : string -> Model.Questionnaire.t option Lwt.t
    val create : template_id:string -> questionnaire_id:string -> unit Lwt.t

    val create_answer
      :  answer_id:string
      -> questionnaire_id:string
      -> question_id:string
      -> ?text:string
      -> ?asset_id:string
      -> unit
      -> unit Lwt.t

    val create_text_answer
      :  answer_id:string
      -> questionnaire_id:string
      -> question_id:string
      -> text:string
      -> unit Lwt.t

    val create_asset_answer
      :  answer_id:string
      -> questionnaire_id:string
      -> question_id:string
      -> asset_id:string
      -> unit Lwt.t

    val find_answer
      :  questionnaire_id:string
      -> question_id:string
      -> Repository_model.AnswerRow.t option Lwt.t

    val update_answer
      :  questionnaire_id:string
      -> question_id:string
      -> ?text:string
      -> ?asset_id:string
      -> unit
      -> unit Lwt.t

    val update_text_answer
      :  questionnaire_id:string
      -> question_id:string
      -> text:string
      -> unit Lwt.t

    val update_asset_answer
      :  questionnaire_id:string
      -> question_id:string
      -> asset_id:string
      -> unit Lwt.t

    val delete_answer : questionnaire_id:string -> question_id:string -> unit Lwt.t

    val find_asset_id
      :  questionnaire_id:string
      -> question_id:string
      -> string option Lwt.t

    val create_template
      :  id:string
      -> label:string
      -> description:string option
      -> unit Lwt.t

    val create_question
      :  ?connection:(module Caqti_lwt.CONNECTION)
      -> question:Model.Question.t
      -> unit Lwt.t

    val create_mapping
      :  ?connection:(module Caqti_lwt.CONNECTION)
      -> id:string
      -> template_id:string
      -> question_id:string
      -> order:int
      -> is_required:bool
      -> unit Lwt.t
  end

  module Model = Model
end

module MariaDB (MigrationService : Sihl.Contract.Migration.Sig) : Sig = struct
  module Model = Model
  module RepoSql = RepoSql.MariaDB ()
  module Sql = RepoSql.Sql
  module Migration = Database_migration.MariaDB

  let lifecycles = [ Database.lifecycle; MigrationService.lifecycle ]
  let register_migration () = MigrationService.register_migration (Migration.migration ())
  let clean = Sql.clean
  let register_cleaner () = Sihl.Cleaner.register_cleaner clean

  module Questionnaire = struct
    let find id =
      Database.query (fun connection ->
          let* questionnaire = Sql.QuestionnaireRow.find connection id in
          let* question_rows = Sql.QuestionnaireRow.find_questions connection id in
          let questionnaire =
            questionnaire
            |> CCOpt.map (fun questionnaire ->
                   RepoModel.QuestionnaireRow.to_questionnaire questionnaire question_rows)
          in
          Lwt.return questionnaire)
    ;;

    let create ~template_id ~questionnaire_id =
      Database.query (fun connection ->
          Sql.QuestionnaireRow.insert connection (questionnaire_id, template_id))
    ;;

    let create_answer ~answer_id ~questionnaire_id ~question_id ?text ?asset_id () =
      Database.query (fun connection ->
          Sql.AnswerRow.insert
            connection
            ( answer_id
            , (questionnaire_id, (questionnaire_id, (question_id, (text, asset_id)))) ))
    ;;

    let create_text_answer ~answer_id ~questionnaire_id ~question_id ~text =
      create_answer ~answer_id ~questionnaire_id ~question_id ~text ()
    ;;

    let create_asset_answer ~answer_id ~questionnaire_id ~question_id ~asset_id =
      create_answer ~answer_id ~questionnaire_id ~question_id ~asset_id ()
    ;;

    let find_answer ~questionnaire_id ~question_id =
      Database.query (fun connection ->
          Sql.QuestionnaireRow.find_answer connection (questionnaire_id, question_id))
    ;;

    let update_answer ~questionnaire_id ~question_id ?text ?asset_id () =
      Database.query (fun connection ->
          let* answer =
            find_answer ~questionnaire_id ~question_id
            |> Lwt.map
                 (Option.to_result
                    ~none:
                      (Caml.Format.asprintf
                         "Answer with questionnaire_id %s and question_id %s"
                         questionnaire_id
                         question_id))
            |> Lwt.map CCResult.get_or_failwith
          in
          let answer_id = RepoModel.AnswerRow.uuid answer in
          Sql.AnswerRow.update connection (text, asset_id, answer_id))
    ;;

    let update_text_answer ~questionnaire_id ~question_id ~text =
      update_answer ~questionnaire_id ~question_id ~text ()
    ;;

    let update_asset_answer ~questionnaire_id ~question_id ~asset_id =
      update_answer ~questionnaire_id ~question_id ~asset_id ()
    ;;

    let delete_answer ~questionnaire_id ~question_id =
      Database.query (fun connection ->
          let* answer =
            find_answer ~questionnaire_id ~question_id
            |> Lwt.map
                 (Option.to_result
                    ~none:
                      (Caml.Format.asprintf
                         "Answer with questionnaire_id %s and question_id %s"
                         questionnaire_id
                         question_id))
            |> Lwt.map CCResult.get_or_failwith
          in
          let answer_id = RepoModel.AnswerRow.uuid answer in
          Sql.AnswerRow.delete connection answer_id)
    ;;

    let find_asset_id ~questionnaire_id ~question_id =
      let* answer = find_answer ~questionnaire_id ~question_id in
      let asset_id = CCOpt.bind answer RepoModel.AnswerRow.asset in
      Lwt.return asset_id
    ;;

    let create_template ~id ~label ~description =
      Database.query (fun connection ->
          Sql.TemplateRow.insert connection (id, label, description))
    ;;

    let create_question ?connection ~question =
      match connection with
      | None ->
        Database.query (fun connection ->
            let row = RepoModel.QuestionRow.of_question question in
            Sql.QuestionRow.insert connection row)
      | Some connection ->
        let row = RepoModel.QuestionRow.of_question question in
        Sql.QuestionRow.insert connection row
    ;;

    let create_mapping ?connection ~id ~template_id ~question_id ~order ~is_required =
      match connection with
      | None ->
        Database.query (fun connection ->
            Sql.Mapping.insert
              connection
              (id, (template_id, (question_id, (order, is_required)))))
      | Some connection ->
        Sql.Mapping.insert
          connection
          (id, (template_id, (question_id, (order, is_required))))
    ;;
  end
end

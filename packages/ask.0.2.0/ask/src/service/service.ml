open Lwt.Syntax

let name = "ask"
let log_src = Logs.Src.create name

module Logs = (val Logs.src_log log_src : Logs.LOG)

module type Sig = sig
  module Questionnaire : sig
    val handle_event : Model.Event.t -> unit Lwt.t

    val instantiate_questionnaire
      :  template_id:string
      -> questionnaire_id:string
      -> string Lwt.t

    val find : string -> Model.Questionnaire.t option Lwt.t

    val create_template
      :  ?id:string
      -> label:string
      -> ?description:string
      -> unit
      -> string Lwt.t

    val answer
      :  Model.Questionnaire.t
      -> (Model.Question.t * Model.AnswerInput.t option) list
      -> unit Lwt.t

    val delete_asset_answer : questionnaire_id:string -> question_id:string -> unit Lwt.t
    val add_question : template_id:string -> order:int -> Model.Question.t -> string Lwt.t
  end

  module Internal__ : sig
    (* USE ON YOUR OWN RISK -- Internal__ functions are used for testing *)
    val clean : unit -> unit Lwt.t

    module Model : sig
      module QuestionnaireRow : sig
        val make
          :  uuid:string
          -> template_uuid:string
          -> template_label:string
          -> ?template_description:string
          -> unit
          -> Repository_model.QuestionnaireRow.t

        val to_questionnaire
          :  Repository_model.QuestionnaireRow.t
          -> Repository_model.QuestionAnswerRow.t list
          -> Model.Questionnaire.t
      end

      module QuestionAnswerRow : sig
        val make
          :  question_uuid:string
          -> ?question_label:string
          -> ?question_help_text:string
          -> question_text:string
          -> question_required:int
          -> ?question_default_value:string
          -> question_validation_regex:string
          -> question_type:string
          -> ?question_max_file_size:int
          -> ?question_mime_types:string
          -> ?question_options:string
          -> ?answer_uuid:string
          -> ?answer_text:string
          -> ?answer_asset_uuid:string
          -> ?answer_asset_filename:string
          -> ?answer_asset_size:int
          -> ?answer_asset_mime:string
          -> unit
          -> Repository_model.QuestionAnswerRow.t
      end
    end
  end

  val register : unit -> Sihl.Container.Service.t

  include Sihl.Container.Service.Sig
end

module Make (Repo : Repository.Sig) (Storage : Sihl.Contract.Storage.Sig) = struct
  module Questionnaire = struct
    exception Exception of string

    let handle_event event =
      match event with
      | Model.Event.TextAnswerCreated (questionnaire_id, question_id, text) ->
        Repo.Questionnaire.create_text_answer
          ~answer_id:(Uuidm.create `V4 |> Uuidm.to_string)
          ~questionnaire_id
          ~question_id
          ~text
      | Model.Event.AssetAnswerCreated
          (questionnaire_id, question_id, (_, filename, size, mime, data)) ->
        let asset_id = Uuidm.create `V4 |> Uuidm.to_string in
        let file =
          Sihl.Contract.Storage.{ id = asset_id; filename; filesize = size; mime }
        in
        let* _ = Storage.upload_base64 file ~base64:data in
        Repo.Questionnaire.create_asset_answer
          ~answer_id:(Uuidm.create `V4 |> Uuidm.to_string)
          ~questionnaire_id
          ~question_id
          ~asset_id
      | Model.Event.TextAnswerUpdated (questionnaire_id, question_id, text) ->
        Repo.Questionnaire.update_text_answer ~questionnaire_id ~question_id ~text
      | Model.Event.AssetAnswerUpdated
          (questionnaire_id, question_id, (_, filename, size, mime, data)) ->
        let* asset_id =
          Repo.Questionnaire.find_asset_id ~questionnaire_id ~question_id
          |> Lwt.map
               (Option.to_result
                  ~none:
                    (Caml.Format.asprintf
                       "Asset id not found for questionnaire_id %s and question_id %s"
                       questionnaire_id
                       question_id))
          |> Lwt.map CCResult.get_or_failwith
        in
        let* file = Storage.find ~id:asset_id in
        let updated_file =
          let open Sihl.Contract.Storage in
          file
          |> set_filename_stored filename
          |> set_filesize_stored size
          |> set_mime_stored mime
        in
        let* _ = Storage.update_base64 updated_file ~base64:data in
        Repo.Questionnaire.update_asset_answer ~questionnaire_id ~question_id ~asset_id
      | Model.Event.AssetAnswerDelete (questionnaire_id, question_id) ->
        let* asset_id =
          Repo.Questionnaire.find_asset_id ~questionnaire_id ~question_id
          |> Lwt.map
               (Option.to_result
                  ~none:
                    (Caml.Format.asprintf
                       "Asset id not found for questionnaire_id %s and question_id %s"
                       questionnaire_id
                       question_id))
          |> Lwt.map CCResult.get_or_failwith
        in
        let* _ = Storage.delete ~id:asset_id in
        Repo.Questionnaire.delete_answer ~questionnaire_id ~question_id
    ;;

    let instantiate_questionnaire ~template_id ~questionnaire_id =
      let* () = Repo.Questionnaire.create ~template_id ~questionnaire_id in
      Lwt.return questionnaire_id
    ;;

    let find id = Repo.Questionnaire.find id

    let create_template ?id ~label ?description () =
      let id = id |> CCOpt.value ~default:(Uuidm.create `V4 |> Uuidm.to_string) in
      let* () = Repo.Questionnaire.create_template ~id ~label ~description in
      Lwt.return id
    ;;

    let answer questionnaire answers =
      let events = Model.Questionnaire.answer questionnaire answers in
      match events with
      | Error errors ->
        Exception
          (errors
          |> CCList.head_opt
          |> Option.value ~default:"Could not answer questionnaire")
        |> raise
      | Ok events ->
        let rec handle_events events =
          match events with
          | event :: events ->
            let* () = handle_event event in
            handle_events events
          | [] -> Lwt.return ()
        in
        handle_events events
    ;;

    let add_question ~template_id ~order question =
      let question_id = Model.Question.uuid question in
      let mapping_id = Uuidm.create `V4 |> Uuidm.to_string in
      Sihl.Database.transaction (fun connection ->
          let* () = Repo.Questionnaire.create_question ~connection ~question in
          let* () =
            Repo.Questionnaire.create_mapping
              ~connection
              ~id:mapping_id
              ~template_id
              ~question_id
              ~order
              ~is_required:(Model.Question.is_required question)
          in
          Lwt.return question_id)
    ;;

    let delete_asset_answer ~questionnaire_id ~question_id =
      handle_event (Model.Event.AssetAnswerDelete (questionnaire_id, question_id))
    ;;
  end

  module Internal__ = struct
    (* USE ON YOUR OWN RISK -- Internal__ functions are used for testing *)
    let clean () = Repo.clean ()

    module Model = struct
      module QuestionnaireRow = Repository_model.QuestionnaireRow
      module QuestionAnswerRow = Repository_model.QuestionAnswerRow
    end
  end

  let start () = Lwt.return ()
  let stop _ = Lwt.return ()

  let lifecycle =
    Sihl.Container.create_lifecycle
      "ask"
      ~dependencies:(fun () -> Repo.lifecycles)
      ~start
      ~stop
  ;;

  let register () =
    Repo.register_migration ();
    Repo.register_cleaner ();
    Sihl.Container.Service.create lifecycle
  ;;
end

module MariaDb =
  Make (Repository.MariaDB (Sihl.Database.Migration.MariaDb)) (Sihl_storage.MariaDb)

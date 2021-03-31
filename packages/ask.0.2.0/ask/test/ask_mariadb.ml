open Alcotest_lwt

(* services *)
module Migration = Sihl.Database.Migration.MariaDb
module Storage = Sihl_storage.MariaDb
module Database = Sihl.Database

(* Test service *)
module TestService = Test_service.Make (Ask.MariaDb) (Storage)

let suite =
  [ ( "questionnaire model"
    , [ test_case "is_valid()" `Quick Test_model.is_valid
      ; test_case
          "creates no event if there was no answer provided"
          `Quick
          Test_model.creates_no_event
      ; test_case
          "creates TextAnswerCreated event if there was no answer before"
          `Quick
          Test_model.creates_text_answer_created_event_without_answer
      ; test_case
          "answering creates one TextAnswerCreated event"
          `Quick
          Test_model.creates_text_answer_created_event
      ; test_case
          "answering with text creates one TextAnswerUpdated event"
          `Quick
          Test_model.creates_no_event_without_answers
      ; test_case
          "answering with text creates two TextAnswerCreated events"
          `Quick
          Test_model.creates_two_events
      ; test_case
          "answering with text creates multiple events"
          `Quick
          Test_model.creates_multiple_events
      ; test_case
          "answering with text and asset creates multiple events"
          `Quick
          Test_model.creates_multiple_events_with_text_and_asset
      ; test_case
          "validate questionnaire answers"
          `Quick
          Test_model.validate_questionnaire_answers
      ; test_case "validate empty answers" `Quick Test_model.validate_empty_answers
      ] )
  ; ( "questionnaire repo model"
    , [ test_case "empty rows" `Quick Test_repo.test1
      ; test_case "rows to questionnaire" `Quick Test_repo.test2
      ] )
  ; ( "questionnaire service"
    , [ test_case
          "create empty questionnaire"
          `Quick
          TestService.create_empty_questionnaire
      ; test_case
          "add question and fetch questionnaire"
          `Quick
          TestService.add_question_and_fetch_questionnaire
      ; test_case
          "answer questionnaire partially fails"
          `Quick
          TestService.answer_questionnaire_partially_fails
      ; test_case
          "answer questionnaire fully"
          `Quick
          TestService.answer_questionnaire_fully
      ; test_case "answer file question only" `Quick TestService.answer_only_file_question
      ; test_case
          "answer already answered text questionnaire"
          `Quick
          TestService.answer_already_answered_text_questionnaire
      ; test_case
          "answer already answered file questionnaire"
          `Quick
          TestService.answer_already_answered_file_questionnaire
      ; test_case
          "delete uploaded asset answer"
          `Quick
          TestService.delete_uploaded_asset_answer
      ; test_case "clean all" `Quick TestService.clean_all
      ] )
  ]
;;

let services =
  [ TestService.register ()
  ; Database.register ()
  ; Migration.register ()
  ; Storage.register ()
  ]
;;

let () =
  let open Lwt.Syntax in
  Sihl.Configuration.read_string "DATABASE_URL_TEST_MARIADB"
  |> Option.value ~default:"mariadb://admin:password@127.0.0.1:3306/dev"
  |> Unix.putenv "DATABASE_URL";
  let () = Logs.set_level (Some Logs.Error) in
  let () = Logs.set_reporter Sihl.Log.default_reporter in
  Lwt_main.run
    (let* _ = Sihl.Container.start_services services in
     let* _ = Migration.run_all () in
     Alcotest_lwt.run "questionnaire component" suite)
;;

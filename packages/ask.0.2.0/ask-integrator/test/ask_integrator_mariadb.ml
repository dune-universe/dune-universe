open Alcotest_lwt

(* services *)
module Migration = Sihl.Database.Migration.MariaDb
module Database = Sihl.Database
module Storage = Sihl_storage.MariaDb

(* Test service *)
module AskService = Ask.MariaDb
module TestService = Test_service.Make (Ask_integrator.MariaDb) (AskService)

let suite =
  [ ( "integrator model"
    , [ test_case "ServiceMappingRow.make" `Quick Test_model.create_service_mapper
      ; test_case
          "QuestionnaireMappingRow.create"
          `Quick
          Test_model.create_questionnaire_mapping_row
      ; test_case
          "QuestionnaireMapping.make"
          `Quick
          Test_model.create_questionnaire_mapping
      ; test_case
          "QuestionnaireMapping.to_mapping_row"
          `Quick
          Test_model.questionnaire_mapping_to_row
      ; test_case "Handler.create" `Quick Test_model.create_handler
      ; test_case
          "Handler.create_from_models"
          `Quick
          Test_model.create_handler_from_models
      ; test_case "Handler.to_models" `Quick Test_model.handler_to_models
      ] )
  ; ( "integrator service"
    , [ test_case "create ask handler" `Quick TestService.create
      ; test_case "find ask handler" `Quick TestService.find
      ; test_case
          "find questionnaire with label of ask handler"
          `Quick
          TestService.find_questionnaire_with_label
      ; test_case "update ask handler" `Quick TestService.update
      ; test_case "delete ask handler" `Quick TestService.delete
      ] )
  ]
;;

let services =
  [ TestService.register ()
  ; AskService.register ()
  ; Storage.register ()
  ; Database.register ()
  ; Migration.register ()
  ]
;;

let () =
  let open Lwt.Syntax in
  Sihl.Configuration.read_string "DATABASE_URL_TEST_MARIADB"
  |> Option.value ~default:"mariadb://admin:password@127.0.0.1:3306/dev"
  |> Unix.putenv "DATABASE_URL";
  Logs.set_level (Some Logs.Error);
  Logs.set_reporter Sihl.Log.default_reporter;
  Lwt_main.run
    (let* _ = Sihl.Container.start_services services in
     let* () = Migration.run_all () in
     Alcotest_lwt.run "ask-integrator" suite)
;;

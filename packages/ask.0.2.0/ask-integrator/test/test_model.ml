module Model = Ask_integrator.Model

let create_uuid () = Uuidm.create `V4 |> Uuidm.to_string
let testable_handler = Alcotest.testable Model.Handler.pp Model.Handler.equal

let testable_service_mapper =
  Alcotest.testable Model.ServiceMappingRow.pp Model.ServiceMappingRow.equal
;;

let testable_questionnaire_mapper =
  Alcotest.testable Model.QuestionnaireMapping.pp Model.QuestionnaireMapping.equal
;;

let testable_questionnaire_mapper_row =
  Alcotest.testable Model.QuestionnaireMappingRow.pp Model.QuestionnaireMappingRow.equal
;;

let testable_questionnaire =
  Alcotest.testable Ask.Model.Questionnaire.pp Ask.Model.Questionnaire.equal
;;

let create_service_mapper _ () =
  let id = create_uuid () in
  let member_id = create_uuid () in
  let member_label = "test" in
  let created_at = Ptime_clock.now () in
  let service_mapper =
    Model.ServiceMappingRow.make ~id ~member_id ~member_label ~created_at
  in
  Alcotest.(
    check
      (triple string string string)
      "correct model fields"
      (id, member_id, member_label)
      (service_mapper.id, service_mapper.member_id, service_mapper.member_label));
  Lwt.return_unit
;;

let create_questionnaire_mapping_row _ () =
  let service_mapper = create_uuid () in
  let label = "test" in
  let questionnaire = create_uuid () in
  let model =
    Model.QuestionnaireMappingRow.create ~service_mapper ~label ~questionnaire ()
  in
  Alcotest.(
    check
      (triple string string string)
      "correct model fields"
      (service_mapper, label, questionnaire)
      (model.service_mapper, model.label, model.questionnaire));
  Lwt.return_unit
;;

let create_questionnaire_mapping _ () =
  let template_id = create_uuid () in
  let questionnaire_id = create_uuid () in
  let questionnaire_label = "Universe" in
  let questionnaire_description = "Universe description" in
  let questionnaire =
    Ask.Model.Questionnaire.make
      ~uuid:questionnaire_id
      ~template_uuid:template_id
      ~label:questionnaire_label
      ~description:questionnaire_description
      ()
  in
  let label = "person" in
  let created_at = Ptime_clock.now () in
  let updated_at = Ptime_clock.now () in
  let model =
    Model.QuestionnaireMapping.make ~label ~questionnaire ~created_at ~updated_at
  in
  Alcotest.(
    check
      (pair string testable_questionnaire)
      "correct model fields"
      (label, questionnaire)
      (model.label, model.questionnaire));
  Lwt.return_unit
;;

let questionnaire_mapping_to_row _ () =
  let service_mapper = create_uuid () in
  let label = "person" in
  let questionnaire_id = create_uuid () in
  let expected_model =
    Model.QuestionnaireMappingRow.create
      ~service_mapper
      ~label
      ~questionnaire:questionnaire_id
      ()
  in
  let template_id = create_uuid () in
  let questionnaire_label = "Universe" in
  let questionnaire_description = "Universe description" in
  let questionnaire =
    Ask.Model.Questionnaire.make
      ~uuid:questionnaire_id
      ~template_uuid:template_id
      ~label:questionnaire_label
      ~description:questionnaire_description
      ()
  in
  let created_at = Ptime_clock.now () in
  let updated_at = Ptime_clock.now () in
  let questionnaire_mapper =
    Model.QuestionnaireMapping.make ~label ~questionnaire ~created_at ~updated_at
  in
  let model =
    Model.QuestionnaireMapping.to_mapping_row ~id:service_mapper questionnaire_mapper
  in
  Alcotest.(
    check testable_questionnaire_mapper_row "correct model fields" expected_model model);
  Lwt.return_unit
;;

let create_handler _ () =
  let id = create_uuid () in
  let member_id = create_uuid () in
  let label = "user" in
  let questionnaires = [] in
  let model = Model.Handler.create ~id ~member_id ~label ~questionnaires () in
  Alcotest.(
    check
      (pair (triple string string string) (list (pair string testable_questionnaire)))
      "check handler model"
      ((id, member_id, label), questionnaires)
      ((model.id, model.member_id, model.label), model.questionnaires));
  Lwt.return_unit
;;

let create_handler_from_models _ () =
  let id = create_uuid () in
  let member_id = create_uuid () in
  let label = "user" in
  let questionnaires = [] in
  let now = Ptime_clock.now () in
  let expected_model = Model.Handler.create ~id ~member_id ~label ~questionnaires () in
  let service_mapper =
    Model.ServiceMappingRow.make ~id ~member_id ~member_label:label ~created_at:now
  in
  let model =
    Model.Handler.create_from_models ~service_mapper ~questionnaire_mapper:[] ()
  in
  Alcotest.(check testable_handler "check model" expected_model model);
  Lwt.return_unit
;;

let handler_to_models _ () =
  let id = create_uuid () in
  let member_id = create_uuid () in
  let label = "user" in
  let questionnaires = [] in
  let now = Ptime_clock.now () in
  let handler_model = Model.Handler.create ~id ~member_id ~label ~questionnaires () in
  let expected_service_mapper =
    Model.ServiceMappingRow.make ~id ~member_id ~member_label:label ~created_at:now
  in
  let service_mapper, ask_model = Model.Handler.to_models handler_model in
  Alcotest.(
    check testable_service_mapper "check model" expected_service_mapper service_mapper);
  Alcotest.(check bool "length of questionnaire mapper" true (CCList.is_empty ask_model));
  Lwt.return_unit
;;

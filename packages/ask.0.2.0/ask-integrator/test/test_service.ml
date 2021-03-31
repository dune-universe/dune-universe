open Lwt.Syntax
module Integrator = Ask_integrator
module Model = Integrator.Model
module TestSeed = Test_seed

let create_uuid () = Uuidm.create `V4 |> Uuidm.to_string

let testable_questionnaire =
  Alcotest.testable Ask.Model.Questionnaire.pp Ask.Model.Questionnaire.equal
;;

let testable_handler =
  Alcotest.testable Integrator.Model.Handler.pp Integrator.Model.Handler.equal
;;

module Make (TestService : Integrator.Sig) (AskService : Ask.Sig) = struct
  module Model = Integrator.Model
  module Seed = TestSeed.Make (TestService) (AskService)
  module Questionnaire = AskService.Questionnaire

  let lifecycle = TestService.lifecycle
  let register = TestService.register

  let create _ () =
    let* () = TestService.Internal__.clean () in
    let* () = AskService.Internal__.clean () in
    let template_label = "test-template" in
    let* questionnaire, _, _, _ =
      Seed.QuestTest.questionnaire_with_three_answered_questions ~label:template_label
      |> Lwt.map Result.get_ok
    in
    let member_id = create_uuid () in
    let member_label = "user" in
    let questionnaire_label = "personal-details" in
    let* handler =
      TestService.create
        ~member_id
        ~member_label
        ~questionnaires:[ questionnaire_label, questionnaire ]
      |> Lwt.map Result.get_ok
    in
    Alcotest.(
      check
        (pair (pair string string) (list (pair string testable_questionnaire)))
        "created handler"
        ((member_id, member_label), [ questionnaire_label, questionnaire ])
        ((handler.member_id, handler.label), handler.questionnaires));
    Lwt.return_unit
  ;;

  let find _ () =
    let* () = TestService.Internal__.clean () in
    let* () = AskService.Internal__.clean () in
    let template_label = "test-template" in
    let* questionnaire, _, _, _ =
      Seed.QuestTest.questionnaire_with_three_answered_questions ~label:template_label
      |> Lwt.map Result.get_ok
    in
    let member_id = create_uuid () in
    let member_label = "user" in
    let questionnaire_label = "personal-details" in
    let* expected_handler =
      TestService.create
        ~member_id
        ~member_label
        ~questionnaires:[ questionnaire_label, questionnaire ]
      |> Lwt.map Result.get_ok
    in
    let* handler =
      TestService.find ~member_id ~label:member_label () |> Lwt.map Option.get
    in
    Alcotest.(check testable_handler "found handler" expected_handler handler);
    Lwt.return_unit
  ;;

  let find_questionnaire_with_label _ () =
    let* () = TestService.Internal__.clean () in
    let* () = AskService.Internal__.clean () in
    let template_label = "test-template" in
    let* expected_questionnaire, _, _, _ =
      Seed.QuestTest.questionnaire_with_three_answered_questions ~label:template_label
      |> Lwt.map Result.get_ok
    in
    let member_id = create_uuid () in
    let member_label = "user" in
    let questionnaire_label = "personal-details" in
    let* _ =
      TestService.create
        ~member_id
        ~member_label
        ~questionnaires:[ questionnaire_label, expected_questionnaire ]
    in
    let* questionnaire =
      TestService.find_questionnaire_with_label
        ~member_id
        ~member_label
        ~questionnaire_label
        ()
      |> Lwt.map Option.get
    in
    Alcotest.(
      check
        testable_questionnaire
        "found questionnaire"
        expected_questionnaire
        questionnaire);
    Lwt.return_unit
  ;;

  let update _ () =
    let* () = TestService.Internal__.clean () in
    let* () = AskService.Internal__.clean () in
    let template_label = "test-template" in
    let* questionnaire, _, _, _ =
      Seed.QuestTest.questionnaire_with_three_answered_questions ~label:template_label
      |> Lwt.map Result.get_ok
    in
    let member_id = create_uuid () in
    let member_label = "user" in
    let questionnaire_label = "personal-details" in
    let expected_questionnaire_label = "personal" in
    let* handler =
      TestService.create
        ~member_id
        ~member_label
        ~questionnaires:[ questionnaire_label, questionnaire ]
      |> Lwt.map Result.get_ok
    in
    let* res =
      TestService.update
        handler
        ~member_label
        ~questionnaires:[ expected_questionnaire_label, questionnaire ]
        ()
      |> Lwt.map Result.get_ok
    in
    Alcotest.(check string "test successful" "Ask integrator handler updated" res);
    let* handler =
      TestService.find ~member_id ~label:member_label () |> Lwt.map Option.get
    in
    let new_label, _ = List.hd (Model.Handler.questionnaires handler) in
    Alcotest.(check string "found handler" expected_questionnaire_label new_label);
    Lwt.return_unit
  ;;

  let delete _ () =
    let* () = TestService.Internal__.clean () in
    let* () = AskService.Internal__.clean () in
    let template_label = "test-template" in
    let* questionnaire, _, _, _ =
      Seed.QuestTest.questionnaire_with_three_answered_questions ~label:template_label
      |> Lwt.map Result.get_ok
    in
    let member_id = create_uuid () in
    let member_label = "user" in
    let questionnaire_label = "personal-details" in
    let* handler =
      TestService.create
        ~member_id
        ~member_label
        ~questionnaires:[ questionnaire_label, questionnaire ]
      |> Lwt.map Result.get_ok
    in
    let* model = TestService.find ~member_id ~label:member_label () in
    Alcotest.(check (option testable_handler) "is created" (Some handler) model);
    let* () = TestService.delete handler in
    let* model = TestService.find ~member_id ~label:member_label () in
    Alcotest.(check (option testable_handler) "is deleted" None model);
    Lwt.return_unit
  ;;
end

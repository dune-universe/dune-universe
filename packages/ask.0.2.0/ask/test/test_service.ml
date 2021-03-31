open Lwt.Syntax
module TestSeed = Test_seed

module Make (AskService : Ask.Sig) (StorageService : Sihl.Contract.Storage.Sig) = struct
  module Model = Ask.Model
  module Seed = TestSeed.Make (AskService)
  module Questionnaire = AskService.Questionnaire

  let lifecycle = AskService.lifecycle
  let register = AskService.register

  let alco_question_answer_input =
    Alcotest.testable Model.QuestionAnswer.pp Model.QuestionAnswer.equal
  ;;

  let create_empty_questionnaire _ () =
    let* () = AskService.Internal__.clean () in
    let* template_id = Questionnaire.create_template ~label:"foobar" () in
    let* questionnaire_id =
      Questionnaire.instantiate_questionnaire
        ~template_id
        ~questionnaire_id:(Uuidm.create `V4 |> Uuidm.to_string)
    in
    let* questionnaire =
      Questionnaire.find questionnaire_id
      |> Lwt.map (Option.to_result ~none:"No questionnaire found")
      |> Lwt.map CCResult.get_or_failwith
    in
    let actual_label = Model.Questionnaire.label questionnaire in
    (let open Alcotest in
    check string)
      "is same label"
      actual_label
      "foobar";
    let questions = Model.Questionnaire.questions questionnaire in
    (let open Alcotest in
    check int)
      "has no questions"
      (List.length questions)
      0;
    Lwt.return ()
  ;;

  let add_question_and_fetch_questionnaire _ () =
    let* () = AskService.Internal__.clean () in
    let* questionnaire =
      Seed.AttributeTest.questionnaire ~label:"foobar" |> Lwt.map CCResult.get_or_failwith
    in
    let template_id = Uuidm.create `V4 |> Uuidm.to_string in
    let* _ =
      Questionnaire.add_question
        ~template_id:(Model.Questionnaire.template_uuid questionnaire)
        ~order:0
        (Model.Question.Text
           (template_id, Some "foo", None, "How old are you?", None, "", true))
    in
    let* questionnaire =
      Questionnaire.find (Model.Questionnaire.uuid questionnaire)
      |> Lwt.map (Option.to_result ~none:"No questionnaire found")
      |> Lwt.map CCResult.get_or_failwith
    in
    let expected =
      [ (let open Model.Question in
        Text (template_id, Some "foo", None, "How old are you?", None, "", true), None)
      ]
    in
    let actual = Model.Questionnaire.questions questionnaire in
    (let open Alcotest in
    check (list alco_question_answer_input))
      "has a question"
      expected
      actual;
    Lwt.return ()
  ;;

  let answer_questionnaire_partially_fails _ () =
    let* () = AskService.Internal__.clean () in
    let* questionnaire, q1, _, _ =
      Seed.AttributeTest.empty_questionnaire_with_three_questions ~label:"some label"
    in
    let questions = Model.Questionnaire.questions questionnaire in
    let answered_questions =
      match questions with
      | [ (_, None); (q2, None); (_, None) ] ->
        [ q2, Some (Model.AnswerInput.Text "Yes") ]
      | _ -> failwith "The seeded questionnaire contains unexpected questions"
    in
    Test_utils.lwt_check_raises
      (fun () -> Questionnaire.answer questionnaire answered_questions)
      (Model.Question.uuid q1 ^ ",Required")
  ;;

  let answer_questionnaire_fully _ () =
    let* () = AskService.Internal__.clean () in
    let* questionnaire, _, _, _ =
      Seed.AttributeTest.empty_questionnaire_with_three_questions ~label:"some label"
    in
    let questions = Model.Questionnaire.questions questionnaire in
    let answer1 = Some (Model.AnswerInput.Text "18") in
    let answer2 = Some (Model.AnswerInput.Text "Yes") in
    let answered_questions =
      match questions with
      | [ (q1, None); (q2, None); (q3, None) ] -> [ q1, answer1; q2, answer2; q3, None ]
      | _ -> failwith "The seeded questionnaire contains unexpected questions"
    in
    let* () = Questionnaire.answer questionnaire answered_questions in
    Lwt.return ()
  ;;

  let answer_only_file_question _ () =
    let* () = AskService.Internal__.clean () in
    let* questionnaire, _, _, _ =
      Seed.AttributeTest.empty_questionnaire_with_three_questions ~label:"some label"
    in
    let questions = Model.Questionnaire.questions questionnaire in
    let answer =
      Some
        (Model.AnswerInput.Asset
           (None, "new_diploma.pdf", 12345, "application/pdf", "VGhpcyBpcyBhIG5ldyB0ZXh0"))
    in
    let answered_questions =
      match questions with
      | [ _; _; (q, None) ] -> [ q, answer ]
      | _ -> failwith "The seeded questionnaire contains unexpected questions"
    in
    let* () = Questionnaire.answer questionnaire answered_questions in
    let* questionnaire =
      Questionnaire.find (Model.Questionnaire.uuid questionnaire)
      |> Lwt.map (Option.to_result ~none:"No questionnaire found")
      |> Lwt.map CCResult.get_or_failwith
    in
    let file_id, filename, filesize, mime =
      match Model.Questionnaire.questions questionnaire with
      | [ _
        ; _
        ; (_, Some (Model.AnswerInput.Asset (Some uuid, filename, filesize, mime, _)))
        ] -> uuid, filename, filesize, mime
      | _ -> failwith "Unexpected questions"
    in
    (let open Alcotest in
    check string)
      "has answered filename"
      "new_diploma.pdf"
      filename;
    (let open Alcotest in
    check int)
      "has answered filesize"
      12345
      filesize;
    (let open Alcotest in
    check string)
      "has answered mime"
      "application/pdf"
      mime;
    let* file = StorageService.find ~id:file_id in
    let* file_content = StorageService.download_data_base64 file in
    (let open Alcotest in
    check string)
      "has answered file content"
      "VGhpcyBpcyBhIG5ldyB0ZXh0"
      file_content;
    Lwt.return ()
  ;;

  let answer_already_answered_text_questionnaire _ () =
    let* () = AskService.Internal__.clean () in
    let* questionnaire, question1, question2, _ =
      Seed.AttributeTest.questionnaire_with_three_answered_questions ~label:"some label"
      |> Lwt.map CCResult.get_or_failwith
    in
    let answer1 = Some (Model.AnswerInput.Text "22") in
    let answer2 = Some (Model.AnswerInput.Text "No") in
    let answered_questions = [ question1, answer1; question2, answer2 ] in
    let* () = Questionnaire.answer questionnaire answered_questions in
    let* questionnaire =
      Questionnaire.find (Model.Questionnaire.uuid questionnaire)
      |> Lwt.map (Option.to_result ~none:"No questionnaire found")
      |> Lwt.map CCResult.get_or_failwith
    in
    let actual_answer1, actual_answer2 =
      match Model.Questionnaire.questions questionnaire with
      | [ (_, Some (Model.AnswerInput.Text actual_answer1))
        ; (_, Some (Model.AnswerInput.Text actual_answer2))
        ; _
        ] -> actual_answer1, actual_answer2
      | _ -> failwith "Unexpected questions"
    in
    (let open Alcotest in
    check string)
      "has answer 1"
      "22"
      actual_answer1;
    (let open Alcotest in
    check string)
      "has answer 1"
      "No"
      actual_answer2;
    Lwt.return ()
  ;;

  let answer_already_answered_file_questionnaire _ () =
    let* () = AskService.Internal__.clean () in
    let* questionnaire, _, _, question3 =
      Seed.AttributeTest.questionnaire_with_three_answered_questions ~label:"some label"
      |> Lwt.map CCResult.get_or_failwith
    in
    let answer3 =
      Some
        (Model.AnswerInput.Asset
           (None, "foo.pdf", 345, "application/pdf", "bmV3Y29udGVudA=="))
    in
    let answered_questions = [ question3, answer3 ] in
    let* () = Questionnaire.answer questionnaire answered_questions in
    let* questionnaire =
      Questionnaire.find (Model.Questionnaire.uuid questionnaire)
      |> Lwt.map (Option.to_result ~none:"No questionnaire found")
      |> Lwt.map CCResult.get_or_failwith
    in
    let file_id, filename, filesize, mime =
      match Model.Questionnaire.questions questionnaire with
      | [ _
        ; _
        ; (_, Some (Model.AnswerInput.Asset (Some uuid, filename, filesize, mime, _)))
        ] -> uuid, filename, filesize, mime
      | _ -> failwith "Unexpected questions"
    in
    (let open Alcotest in
    check string)
      "has answered filename"
      "foo.pdf"
      filename;
    (let open Alcotest in
    check int)
      "has answered filesize"
      345
      filesize;
    (let open Alcotest in
    check string)
      "has answered mime"
      "application/pdf"
      mime;
    let* file = StorageService.find ~id:file_id in
    let* file_content = StorageService.download_data_base64 file in
    (let open Alcotest in
    check string)
      "has answered file content"
      "bmV3Y29udGVudA=="
      file_content;
    Lwt.return ()
  ;;

  let delete_uploaded_asset_answer _ () =
    let* () = AskService.Internal__.clean () in
    let* questionnaire, _, _, question3 =
      Seed.AttributeTest.questionnaire_with_three_answered_questions ~label:"some label"
      |> Lwt.map CCResult.get_or_failwith
    in
    let question_id = Model.Question.uuid question3 in
    let* questionnaire_id =
      Questionnaire.find (Model.Questionnaire.uuid questionnaire)
      |> Lwt.map (Option.to_result ~none:"No questionnaire found")
      |> Lwt.map CCResult.get_or_failwith
      |> Lwt.map Model.Questionnaire.uuid
    in
    let* () = Questionnaire.delete_asset_answer ~questionnaire_id ~question_id in
    let file_id, _, _, _ =
      match Model.Questionnaire.questions questionnaire with
      | [ _
        ; _
        ; (_, Some (Model.AnswerInput.Asset (Some uuid, filename, filesize, mime, _)))
        ] -> uuid, filename, filesize, mime
      | _ -> failwith "Unexpected questions"
    in
    let* file = StorageService.find_opt ~id:file_id in
    let () =
      match file with
      | Some _ -> Alcotest.fail "File should have been deleted"
      | None -> ()
    in
    Lwt.return ()
  ;;

  let clean_all _ () =
    let* _ = Sihl.Cleaner.clean_all () in
    AskService.Internal__.clean ()
  ;;
end

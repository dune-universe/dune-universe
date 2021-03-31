open Lwt.Syntax

module Make (TestService : Ask_integrator.Sig) (AskService : Ask.Sig) = struct
  module QuestTest = struct
    module Model = Ask.Model

    let questionnaire ~label =
      let open AskService.Questionnaire in
      let* template_id = create_template ~label () in
      let* questionnaire_id =
        instantiate_questionnaire
          ~template_id
          ~questionnaire_id:(Uuidm.create `V4 |> Uuidm.to_string)
      in
      find questionnaire_id
      |> Lwt.map (Option.to_result ~none:"Seed failed, can not create questionnaire")
    ;;

    let empty_questionnaire_with_three_questions ~label =
      let open AskService.Questionnaire in
      let* template_id = create_template ~label () in
      let question1 =
        Model.Question.Text
          ( Uuidm.create `V4 |> Uuidm.to_string
          , Some "age"
          , None
          , "How old are you?"
          , None
          , ""
          , true )
      in
      let question2 =
        Model.Question.YesNo
          ( Uuidm.create `V4 |> Uuidm.to_string
          , Some "student"
          , None
          , "Are you a student?"
          , false )
      in
      let question3 =
        Model.Question.File
          ( Uuidm.create `V4 |> Uuidm.to_string
          , Some "diploma"
          , None
          , "Upload your diploma."
          , [ "application/pdf" ]
          , 100000
          , true )
      in
      let* _ = add_question ~template_id ~order:0 question1 in
      let* _ = add_question ~template_id ~order:1 question2 in
      let* _ = add_question ~template_id ~order:2 question3 in
      let* questionnaire_id =
        instantiate_questionnaire
          ~template_id
          ~questionnaire_id:(Uuidm.create `V4 |> Uuidm.to_string)
      in
      let* questionnaire =
        find questionnaire_id
        |> Lwt.map (Option.to_result ~none:"Seed failed, can not create questionnaire")
        |> Lwt.map CCResult.get_or_failwith
      in
      Lwt.return (questionnaire, question1, question2, question3)
    ;;

    let questionnaire_with_three_answered_questions ~label =
      let open AskService.Questionnaire in
      let* questionnaire, question1, question2, question3 =
        empty_questionnaire_with_three_questions ~label
      in
      let answer1 = Some (Model.AnswerInput.Text "18") in
      let answer2 = Some (Model.AnswerInput.Text "Yes") in
      let answer3 =
        Some
          (Model.AnswerInput.Asset
             (None, "new_diploma.pdf", 12345, "application/pdf", "aGVsbG9vb28="))
      in
      let* () = answer questionnaire [ question1, answer1; question2, answer2 ] in
      (* We have to upload file separately *)
      let* () = answer questionnaire [ question3, answer3 ] in
      let questionnaire_id = Model.Questionnaire.uuid questionnaire in
      let* questionnaire =
        find questionnaire_id
        |> Lwt.map (Option.to_result ~none:"No questionnaire found")
        |> Lwt.map CCResult.get_or_failwith
      in
      Lwt.return (Ok (questionnaire, question1, question2, question3))
    ;;
  end

  module IntegratorTest = struct
    module Model = Ask_integrator.Model

    let ask_integrator_with_one_filled_questionnaire
        ~member_label
        ~questionnaire_label
        ~template_label
      =
      let member_id = Uuidm.create `V4 |> Uuidm.to_string in
      let* questionnaire, _, _, _ =
        QuestTest.questionnaire_with_three_answered_questions ~label:template_label
        |> Lwt.map CCResult.get_or_failwith
      in
      TestService.create
        ~member_id
        ~member_label
        ~questionnaires:[ questionnaire_label, questionnaire ]
    ;;
  end
end

open Ask
module QuestionnaireRow = MariaDb.Internal__.Model.QuestionnaireRow
module QuestionAnswerRow = MariaDb.Internal__.Model.QuestionAnswerRow

let questionnaire = Alcotest.testable Model.Questionnaire.pp Model.Questionnaire.equal

let test1 _ () =
  let questionnaire_row =
    QuestionnaireRow.make ~uuid:"1" ~template_uuid:"2" ~template_label:"foo" ()
  in
  let expected =
    Model.Questionnaire.make
      ~uuid:"1"
      ~template_uuid:"2"
      ~label:"foo"
      ~description:""
      ~questions:[]
      ()
  in
  let actual = QuestionnaireRow.to_questionnaire questionnaire_row [] in
  (let open Alcotest in
  check questionnaire)
    "is same"
    expected
    actual;
  Lwt.return ()
;;

let test2 _ () =
  let rows =
    [ QuestionAnswerRow.make
        ~question_uuid:"1"
        ~question_label:"Current status"
        ~question_text:"How are you?"
        ~question_required:0
        ~question_help_text:"Enter your age."
        ~question_validation_regex:".*"
        ~question_type:"text"
        ~answer_uuid:"1"
        ~answer_text:"My answer."
        ()
    ; QuestionAnswerRow.make
        ~question_uuid:"2"
        ~question_label:"Well-being"
        ~question_text:"Are you well?"
        ~question_required:1
        ~question_help_text:"Enter your level of well-being."
        ~question_validation_regex:".*"
        ~question_type:"y/n"
        ~answer_uuid:"2"
        ~answer_text:"Yes"
        ()
    ]
  in
  let questionnaire_row =
    QuestionnaireRow.make
      ~uuid:"1"
      ~template_uuid:"1"
      ~template_label:"default"
      ~template_description:"This is a description."
      ()
  in
  let expected =
    Model.Questionnaire.make
      ~uuid:"1"
      ~template_uuid:"1"
      ~label:"default"
      ~description:"This is a description."
      ~questions:
        [ ( Model.Question.Text
              ( "1"
              , Some "Current status"
              , Some "Enter your age."
              , "How are you?"
              , None
              , ".*"
              , false )
          , Some (Model.AnswerInput.Text "My answer.") )
        ; ( Model.Question.YesNo
              ( "2"
              , Some "Well-being"
              , Some "Enter your level of well-being."
              , "Are you well?"
              , true )
          , Some (Model.AnswerInput.Text "Yes") )
        ]
      ()
  in
  let actual = QuestionnaireRow.to_questionnaire questionnaire_row rows in
  (let open Alcotest in
  check questionnaire)
    "is same"
    expected
    actual;
  Lwt.return ()
;;

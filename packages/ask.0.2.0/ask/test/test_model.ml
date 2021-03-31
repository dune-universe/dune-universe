module Model = Ask.Model

let input = Alcotest.testable Model.QuestionAnswer.pp Model.QuestionAnswer.equal
let event = Alcotest.testable Model.Event.pp Model.Event.equal

let is_valid _ () =
  let () =
    (let open Alcotest in
    check bool)
      "Is text valid"
      true
      (Model.Question.is_valid
         (Model.Question.Text ("1", None, None, "question", None, ".*", false))
         (Model.AnswerInput.Text "foo"))
  in
  let () =
    (let open Alcotest in
    check bool)
      "Is text valid"
      true
      (Model.Question.is_valid
         (Model.Question.Text ("1", None, None, "question", None, ".*", false))
         (Model.AnswerInput.Text ""))
  in
  let () =
    (let open Alcotest in
    check bool)
      "Is yesno valid"
      true
      (Model.Question.is_valid
         (Model.Question.YesNo ("1", None, None, "question", false))
         (Model.AnswerInput.Text "Yes"))
  in
  let () =
    (let open Alcotest in
    check bool)
      "Is year valid"
      true
      (Model.Question.is_valid
         (Model.Question.Year ("1", None, None, "question", false))
         (Model.AnswerInput.Text "1899"))
  in
  let () =
    (let open Alcotest in
    check bool)
      "Is date valid"
      true
      (Model.Question.is_valid
         (Model.Question.Date ("1", None, None, "question", false))
         (Model.AnswerInput.Text "1988-12-30"))
  in
  let () =
    (let open Alcotest in
    check bool)
      "Is yesno not valid"
      false
      (Model.Question.is_valid
         (Model.Question.YesNo ("1", None, None, "question", false))
         (Model.AnswerInput.Text "foobar"))
  in
  (let open Alcotest in
  check bool)
    "Is text not valid"
    false
    (Model.Question.is_valid
       (Model.Question.Text ("1", None, None, "question", None, ".+", false))
       (Model.AnswerInput.Text ""));
  let () =
    (let open Alcotest in
    check bool)
      "Is year not valid"
      false
      (Model.Question.is_valid
         (Model.Question.Year ("1", None, None, "question", false))
         (Model.AnswerInput.Text "11111"))
  in
  let () =
    (let open Alcotest in
    check bool)
      "Is date not valid"
      false
      (Model.Question.is_valid
         (Model.Question.Date ("1", None, None, "question", false))
         (Model.AnswerInput.Text "10-12-30"))
  in
  Lwt.return ()
;;

let creates_no_event _ () =
  let current =
    ( Model.Question.Text
        ("1", None, None, "What is your favorite animal?", None, ".*", false)
    , None )
  in
  let updated =
    ( Model.Question.Text
        ("1", None, None, "What is your favorite animal?", None, ".*", false)
    , None )
  in
  let actual = Model.QuestionAnswer.event "1" current updated in
  Lwt.return
  @@ (let open Alcotest in
     check (option event))
       "equal"
       None
       actual
;;

let creates_text_answer_created_event_without_answer _ () =
  let current =
    ( Model.Question.Text
        ("1", None, None, "What is your favorite animal?", None, ".*", false)
    , None )
  in
  let updated =
    ( Model.Question.Text
        ("1", None, None, "What is your favorite animal?", None, ".*", false)
    , Some (Model.AnswerInput.Text "Zebra") )
  in
  let actual = Model.QuestionAnswer.event "1" current updated in
  let expected = Some (Model.Event.TextAnswerCreated ("1", "1", "Zebra")) in
  Lwt.return
  @@ (let open Alcotest in
     check (option event))
       "equal"
       expected
       actual
;;

let creates_text_answer_created_event _ () =
  let question1 =
    Model.Question.Text
      ("5", None, None, "What is your favorite animal?", None, ".*", false)
  in
  let questions = [ question1, None ] in
  let questionnaire =
    Model.Questionnaire.make
      ~uuid:"1"
      ~template_uuid:"1"
      ~label:"default"
      ~description:"The default questionnaire"
      ~questions
      ()
  in
  let answer_input = [ question1, Some (Model.AnswerInput.Text "Zebra") ] in
  let expected = Ok [ Model.Event.TextAnswerCreated ("1", "5", "Zebra") ] in
  let actual = Model.Questionnaire.answer questionnaire answer_input in
  Lwt.return
  @@
  let open Alcotest in
  check (result (list event) (list string)) "equal" expected actual
;;

let creates_no_event_without_answers _ () =
  let question1 =
    Model.Question.Text
      ("3", None, None, "What is your favorite animal?", None, ".*", false)
  in
  let questions = [ question1, Some (Model.AnswerInput.Text "Giraffe") ] in
  let questionnaire =
    Model.Questionnaire.make
      ~uuid:"1"
      ~template_uuid:"1"
      ~label:"default"
      ~description:"The default questionnaire"
      ~questions
      ()
  in
  let answer_input = [ question1, Some (Model.AnswerInput.Text "Zebra") ] in
  let expected = Ok [ Model.Event.TextAnswerUpdated ("1", "3", "Zebra") ] in
  let actual = Model.Questionnaire.answer questionnaire answer_input in
  Lwt.return
  @@
  let open Alcotest in
  check (result (list event) (list string)) "equal" expected actual
;;

let creates_two_events _ () =
  let question1 =
    Model.Question.Text
      ("3", None, None, "What is your favorite animal?", None, ".*", false)
  in
  let question2 =
    Model.Question.Text ("4", None, None, "How old are you?", None, ".*", false)
  in
  let questions = [ question1, None; question2, None ] in
  let questionnaire =
    Model.Questionnaire.make
      ~uuid:"1"
      ~template_uuid:"2"
      ~label:"default"
      ~description:"The default questionnaire"
      ~questions
      ()
  in
  let answer_input =
    [ question1, Some (Model.AnswerInput.Text "Zebra")
    ; question2, Some (Model.AnswerInput.Text "33")
    ]
  in
  let expected =
    Ok
      [ Model.Event.TextAnswerCreated ("1", "3", "Zebra")
      ; Model.Event.TextAnswerCreated ("1", "4", "33")
      ]
  in
  let actual = Model.Questionnaire.answer questionnaire answer_input in
  Lwt.return
  @@
  let open Alcotest in
  check (result (list event) (list string)) "equal" expected actual
;;

let creates_multiple_events _ () =
  let question1 =
    Model.Question.Text
      ("3", None, None, "What is your favorite animal?", None, ".*", false)
  in
  let question2 =
    Model.Question.Text ("4", None, None, "How old are you?", None, ".*", false)
  in
  let question3 =
    Model.Question.Text
      ("5", None, None, "What is your favorite color?", None, ".*", false)
  in
  let questions =
    [ question1, Some (Model.AnswerInput.Text "Giraffe")
    ; question2, Some (Model.AnswerInput.Text "33")
    ; question3, None
    ]
  in
  let instance =
    Model.Questionnaire.make
      ~uuid:"1"
      ~template_uuid:"2"
      ~label:"default"
      ~description:"The default questionnaire"
      ~questions
      ()
  in
  let answerInput =
    [ question1, Some (Model.AnswerInput.Text "Zebra")
    ; question2, Some (Model.AnswerInput.Text "44")
    ; question3, Some (Model.AnswerInput.Text "red")
    ; ( Model.Question.Text ("-", None, None, "-", None, "-", false)
      , Some (Model.AnswerInput.Text "malicious data") )
    ]
  in
  let expected =
    Ok
      [ Model.Event.TextAnswerUpdated ("1", "3", "Zebra")
      ; Model.Event.TextAnswerUpdated ("1", "4", "44")
      ; Model.Event.TextAnswerCreated ("1", "5", "red")
      ]
  in
  let actual = Model.Questionnaire.answer instance answerInput in
  Lwt.return
  @@
  let open Alcotest in
  check (result (list event) (list string)) "equal" expected actual
;;

let creates_multiple_events_with_text_and_asset _ () =
  let question1 =
    Model.Question.Text
      ("3", None, None, "What is your favorite animal?", None, ".*", false)
  in
  let question2 =
    Model.Question.Text ("4", None, None, "How old are you?", None, ".*", false)
  in
  let question3 =
    Model.Question.File
      ("5", Some "CV", None, "Please upload your CV", [ "application/pdf" ], 10, false)
  in
  let question4 =
    Model.Question.Text
      ("6", None, None, "What is your favorite color?", None, ".*", false)
  in
  let question5 =
    Model.Question.File
      ( "7"
      , Some "Cert"
      , None
      , "Please upload your certificate"
      , [ "application/pdf" ]
      , 10
      , false )
  in
  let questions =
    [ question1, Some (Model.AnswerInput.Text "Giraffe")
    ; question2, Some (Model.AnswerInput.Text "33")
    ; question3, None
    ; question4, None
    ; ( question5
      , Some (Model.AnswerInput.Asset (None, "old.pdf", 1000, "application/pdf", "data"))
      )
    ]
  in
  let instance =
    Model.Questionnaire.make
      ~uuid:"1"
      ~template_uuid:"2"
      ~label:"default"
      ~description:"The default questionnaire"
      ~questions
      ()
  in
  let answerInput =
    [ question1, Some (Model.AnswerInput.Text "Zebra")
    ; question2, Some (Model.AnswerInput.Text "44")
    ; question4, Some (Model.AnswerInput.Text "red")
    ; ( Model.Question.Text ("-", None, None, "-", None, "-", false)
      , Some (Model.AnswerInput.Text "invalid data") )
    ]
  in
  let expected =
    Ok
      [ Model.Event.TextAnswerUpdated ("1", "3", "Zebra")
      ; Model.Event.TextAnswerUpdated ("1", "4", "44")
      ; Model.Event.TextAnswerCreated ("1", "6", "red")
      ]
  in
  let actual = Model.Questionnaire.answer instance answerInput in
  Lwt.return
  @@
  let open Alcotest in
  check (result (list event) (list string)) "equal" expected actual
;;

let validate_questionnaire_answers _ () =
  let open Model.Question in
  let question1 =
    File
      ( "dd37028d-9fb8-5f02-a53f-0303d6139b18"
      , Some "Upload diploma"
      , None
      , "Upload diploma"
      , [ "application/pdf" ]
      , 3
      , true )
  in
  let question2 =
    Select
      ( "a9f864d5-bf2a-30bd-a02b-0d3659542222"
      , Some "Gender"
      , None
      , "Gender"
      , [ "Male"; "Female"; "Other" ]
      , true )
  in
  let question3 =
    Date
      ( "36932680-9f23-bc09-f78b-ccedf7881336"
      , Some "Date of birth"
      , None
      , "Date of birth"
      , true )
  in
  let question4 =
    Country
      ( "5eccba59-8c0a-9505-0c93-24ce28b8bd59"
      , Some "Nationality"
      , None
      , "Nationality"
      , true )
  in
  let question5 =
    YesNo
      ( "bb1e217e-faaf-44a5-ba95-d1cb76597305"
      , Some "UBS Center Scholarship"
      , None
      , "Do you want to apply for the UBS Center Scholarship?"
      , true )
  in
  let question6 =
    Text
      ( "8364898a-c57b-4098-8abc-7937aeace34e"
      , Some "Other university"
      , None
      , "Which other universities are you applying to? Please specify."
      , None
      , ".+"
      , false )
  in
  let question7 =
    Text
      ( "8b5a5c0f-5ea4-4667-a2de-9e647eca76a0"
      , Some "Research interests"
      , None
      , "Please list your research interests in note form (e.g. applied microeconomics, \
         inequality, development)"
      , None
      , ".+"
      , true )
  in
  let questions =
    [ question1, None
    ; question2, None
    ; question3, None
    ; question4, None
    ; question5, None
    ; question6, None
    ; question7, None
    ]
  in
  let questionnaire =
    Model.Questionnaire.make
      ~uuid:""
      ~template_uuid:""
      ~label:"test-questionnaire"
      ~description:""
      ~questions
      ()
  in
  let answers = [] in
  let actual = Model.Questionnaire.answer questionnaire answers in
  let expected =
    Error
      [ "a9f864d5-bf2a-30bd-a02b-0d3659542222,Required"
      ; "36932680-9f23-bc09-f78b-ccedf7881336,Required"
      ; "5eccba59-8c0a-9505-0c93-24ce28b8bd59,Required"
      ; "bb1e217e-faaf-44a5-ba95-d1cb76597305,Required"
      ; "8b5a5c0f-5ea4-4667-a2de-9e647eca76a0,Required"
      ]
  in
  let alco_event = Alcotest.testable Model.Event.pp Model.Event.equal in
  (let open Alcotest in
  check (result (list alco_event) (list string)) "validates empty answers" expected actual);
  let answers =
    [ ( question1
      , Some
          (Model.AnswerInput.Asset
             (None, "diploma.pdf", 2000, "application/pdf", "somedata")) )
    ; question2, Some (Model.AnswerInput.Text "What")
    ; question3, Some (Model.AnswerInput.Text "1990-01-01")
    ; question5, Some (Model.AnswerInput.Text "Yes")
    ]
  in
  let actual = Model.Questionnaire.answer questionnaire answers in
  let expected =
    Error
      [ "a9f864d5-bf2a-30bd-a02b-0d3659542222,Please select on of the options"
      ; "5eccba59-8c0a-9505-0c93-24ce28b8bd59,Required"
      ; "8b5a5c0f-5ea4-4667-a2de-9e647eca76a0,Required"
      ]
  in
  (let open Alcotest in
  check (result (list alco_event) (list string)) "validates some answers" expected actual);
  let answers =
    [ ( question1
      , Some
          (Model.AnswerInput.Asset
             (None, "diploma.pdf", 2000, "application/pdf", "somedata")) )
    ; question2, Some (Model.AnswerInput.Text "Male")
    ; question3, Some (Model.AnswerInput.Text "1990-01-01")
    ; question4, Some (Model.AnswerInput.Text "Switzerland")
    ; question5, Some (Model.AnswerInput.Text "Yes")
    ; question7, Some (Model.AnswerInput.Text "Economics")
    ]
  in
  let actual = Model.Questionnaire.answer questionnaire answers in
  let expected =
    Ok
      [ Model.Event.TextAnswerCreated ("", "a9f864d5-bf2a-30bd-a02b-0d3659542222", "Male")
      ; Model.Event.TextAnswerCreated
          ("", "36932680-9f23-bc09-f78b-ccedf7881336", "1990-01-01")
      ; Model.Event.TextAnswerCreated
          ("", "5eccba59-8c0a-9505-0c93-24ce28b8bd59", "Switzerland")
      ; Model.Event.TextAnswerCreated ("", "bb1e217e-faaf-44a5-ba95-d1cb76597305", "Yes")
      ; Model.Event.TextAnswerCreated
          ("", "8b5a5c0f-5ea4-4667-a2de-9e647eca76a0", "Economics")
      ]
  in
  (let open Alcotest in
  check (result (list alco_event) (list string)) "validates ok" expected actual);
  let answers =
    [ ( question1
      , Some
          (Model.AnswerInput.Asset
             (None, "diploma.pdf", 2000, "application/pdf", "somedata")) )
    ]
  in
  let actual = Model.Questionnaire.answer questionnaire answers in
  let expected =
    Ok
      [ Model.Event.AssetAnswerCreated
          ( ""
          , "dd37028d-9fb8-5f02-a53f-0303d6139b18"
          , (None, "diploma.pdf", 2000, "application/pdf", "somedata") )
      ]
  in
  (let open Alcotest in
  check
    (result (list alco_event) (list string))
    "validates ok with just asset answer"
    expected
    actual);
  Lwt.return ()
;;

let validate_empty_answers _ () =
  let open Model.Question in
  let question1 =
    File
      ( "dd37028d-9fb8-5f02-a53f-0303d6139b18"
      , Some "Upload diploma"
      , None
      , "Upload diploma"
      , [ "application/pdf" ]
      , 3
      , false )
  in
  let question2 =
    Select
      ( "a9f864d5-bf2a-30bd-a02b-0d3659542222"
      , Some "Gender"
      , None
      , "Gender"
      , [ "Male"; "Female"; "Other" ]
      , false )
  in
  let questions = [ question1, None; question2, None ] in
  let questionnaire =
    Model.Questionnaire.make
      ~uuid:""
      ~template_uuid:""
      ~label:"test-questionnaire"
      ~description:""
      ~questions
      ()
  in
  let answers = [] in
  let actual = Model.Questionnaire.answer questionnaire answers in
  let expected = Ok [] in
  let alco_event = Alcotest.testable Model.Event.pp Model.Event.equal in
  (let open Alcotest in
  check (result (list alco_event) (list string)) "validates empty answers" expected actual);
  let answers = questions in
  let actual = Model.Questionnaire.answer questionnaire answers in
  let expected = Ok [] in
  let alco_event = Alcotest.testable Model.Event.pp Model.Event.equal in
  (let open Alcotest in
  check (result (list alco_event) (list string)) "validates empty answers" expected actual);
  Lwt.return ()
;;

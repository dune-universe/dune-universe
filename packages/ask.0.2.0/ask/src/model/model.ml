module AnswerInput = struct
  type uuid = string option [@@deriving yojson, show, eq]
  type filename = string [@@deriving yojson, show, eq]
  type size = int [@@deriving yojson, show, eq]
  type mime_type = string [@@deriving yojson, show, eq]
  type data = string [@@deriving yojson, show, eq]
  type text_answer = string [@@deriving yojson, show, eq]

  type asset_answer = uuid * filename * size * mime_type * data
  [@@deriving yojson, show, eq]

  type t =
    | Text of text_answer
    | Asset of asset_answer
  [@@deriving yojson, show, eq]

  let text answer =
    match answer with
    | Text text -> text
    | Asset (_, filename, _, _, _) -> filename
  ;;

  let mime_to_ext mime =
    match mime with
    | "application/pdf" -> "pdf"
    | _ -> "pdf"
  ;;
end

module Event = struct
  type questionnaire_id = string [@@deriving yojson, show, eq]
  type question_id = string [@@deriving yojson, show, eq]

  type t =
    | TextAnswerCreated of questionnaire_id * question_id * AnswerInput.text_answer
    | TextAnswerUpdated of questionnaire_id * question_id * AnswerInput.text_answer
    | AssetAnswerCreated of questionnaire_id * question_id * AnswerInput.asset_answer
    | AssetAnswerUpdated of questionnaire_id * question_id * AnswerInput.asset_answer
    | AssetAnswerDelete of questionnaire_id * question_id
  [@@deriving yojson, show, eq]
end

module Question = struct
  type required = bool [@@deriving yojson, show, eq]
  type label = string option [@@deriving yojson, show, eq]
  type help_text = string option [@@deriving yojson, show, eq]
  type text = string [@@deriving yojson, show, eq]
  type id = string [@@deriving yojson, show, eq]
  type regex = string [@@deriving yojson, show, eq]
  type possible_options = string list [@@deriving yojson, show, eq]
  type mime_types = string list [@@deriving yojson, show, eq]
  type max_file_size = int [@@deriving yojson, show, eq]

  type t =
    | Text of id * label * help_text * text * string option * regex * required
    | Country of id * label * help_text * text * required
    | Select of id * label * help_text * text * possible_options * required
    | YesNo of id * label * help_text * text * required
    | Date of id * label * help_text * text * required
    | File of id * label * help_text * text * mime_types * max_file_size * required
    | Year of id * label * help_text * text * required
  [@@deriving yojson, show, eq]

  let uuid question =
    match question with
    | Text (id, _, _, _, _, _, _) -> id
    | Country (id, _, _, _, _) -> id
    | Select (id, _, _, _, _, _) -> id
    | YesNo (id, _, _, _, _) -> id
    | Date (id, _, _, _, _) -> id
    | File (id, _, _, _, _, _, _) -> id
    | Year (id, _, _, _, _) -> id
  ;;

  let help_text question =
    match question with
    | Text (_, _, help_text, _, _, _, _) -> help_text
    | Country (_, _, help_text, _, _) -> help_text
    | Select (_, _, help_text, _, _, _) -> help_text
    | YesNo (_, _, help_text, _, _) -> help_text
    | Date (_, _, help_text, _, _) -> help_text
    | File (_, _, help_text, _, _, _, _) -> help_text
    | Year (_, _, help_text, _, _) -> help_text
  ;;

  let text question =
    match question with
    | Text (_, _, _, text, _, _, _) -> text
    | Country (_, _, _, text, _) -> text
    | Select (_, _, _, text, _, _) -> text
    | YesNo (_, _, _, text, _) -> text
    | Date (_, _, _, text, _) -> text
    | File (_, _, _, text, _, _, _) -> text
    | Year (_, _, _, text, _) -> text
  ;;

  let label question =
    match question with
    | Text (_, Some label, _, _, _, _, _) -> label
    | Country (_, Some label, _, _, _) -> label
    | Select (_, Some label, _, _, _, _) -> label
    | YesNo (_, Some label, _, _, _) -> label
    | Date (_, Some label, _, _, _) -> label
    | File (_, Some label, _, _, _, _, _) -> label
    | Year (_, Some label, _, _, _) -> label
    | _ -> text question
  ;;

  let is_file question =
    match question with
    | File (_, _, _, _, _, _, _) -> true
    | _ -> false
  ;;

  let is_required question =
    match question with
    | Text (_, _, _, _, _, _, required) -> required
    | Country (_, _, _, _, required) -> required
    | Select (_, _, _, _, _, required) -> required
    | YesNo (_, _, _, _, required) -> required
    | Date (_, _, _, _, required) -> required
    | File (_, _, _, _, _, _, required) -> required
    | Year (_, _, _, _, required) -> required
  ;;

  let set_optional question =
    match question with
    | Text (a, b, c, d, e, f, _) -> Text (a, b, c, d, e, f, false)
    | Country (a, b, c, d, _) -> Country (a, b, c, d, false)
    | Select (a, b, c, d, e, _) -> Select (a, b, c, d, e, false)
    | YesNo (a, b, c, d, _) -> YesNo (a, b, c, d, false)
    | Date (a, b, c, d, _) -> Date (a, b, c, d, false)
    | File (a, b, c, d, e, f, _) -> File (a, b, c, d, e, f, false)
    | Year (a, b, c, d, _) -> Year (a, b, c, d, false)
  ;;

  let validation_error uuid message = Error (Caml.Format.asprintf "%s,%s" uuid message)

  let validate question answer_input =
    match question, answer_input with
    | question, None ->
      (match is_required question with
      | true -> validation_error (uuid question) "Required"
      | false -> Ok ())
    | Text (_, _, _, _, _, regex, required), Some (AnswerInput.Text answer) ->
      let regex = Re.Pcre.regexp regex in
      (match CCString.is_empty answer, Re.Pcre.pmatch ~rex:regex answer with
      | false, true -> Ok ()
      | true, _ when required -> validation_error (uuid question) "Required"
      | true, true -> Ok ()
      | _, false -> validation_error (uuid question) "Invalid value provided")
    | Country (_, _, _, _, _), Some (AnswerInput.Text answer) ->
      (match
         ( CCString.is_empty answer
         , CCList.exists
             (fun (_, country) -> CCString.equal country answer)
             Model_utils.countries )
       with
      | true, _ -> Ok ()
      | false, true -> Ok ()
      | false, false -> validation_error (uuid question) "Invalid value provided")
    | Select (_, _, _, _, options, _), Some (AnswerInput.Text answer) ->
      (match options |> CCList.find_opt (CCString.equal answer) |> CCOpt.is_some with
      | true -> Ok ()
      | false -> validation_error (uuid question) "Please select on of the options")
    | YesNo (_, _, _, _, _), Some (AnswerInput.Text answer) ->
      let regex = Re.Pcre.regexp "^Yes$|^No$" in
      (match Re.Pcre.pmatch ~rex:regex answer with
      | true -> Ok ()
      | false -> validation_error (uuid question) "Please answer with 'Yes' or 'No'")
    | Year (_, _, _, _, _), Some (AnswerInput.Text answer) ->
      let regex = Re.Pcre.regexp "^(1|2)\\d\\d\\d$" in
      (match Re.Pcre.pmatch ~rex:regex answer with
      | true -> Ok ()
      | false -> validation_error (uuid question) "Please enter a year in the format 1999")
    | Date (_, _, _, _, _), Some (AnswerInput.Text answer) ->
      (* TODO: define allowed type of date, e.g. swiss format *)
      let regex = Re.Pcre.regexp "^(1|2)\\d\\d\\d\\-(0|1)\\d\\-(0|1|2|3)\\d$" in
      (match Re.Pcre.pmatch ~rex:regex answer with
      | true -> Ok ()
      | false ->
        validation_error (uuid question) "Please enter a date in the format 1990-11-25")
    | ( File (_, _, _, _, supported_mime_types, max_file_sizeMb, _)
      , Some (Asset (_, _, size_byte, mime_type, _)) ) ->
      let size_mb = 1 + Int.div size_byte 1000000 in
      (match size_mb <= max_file_sizeMb, CCList.mem mime_type supported_mime_types with
      | true, true -> Ok ()
      | false, true ->
        validation_error
          (uuid question)
          (Caml.Format.asprintf "Asset file size too big (max. %d)" max_file_sizeMb)
      | _, false -> validation_error (uuid question) "Invalid value provided")
    | question, _ -> validation_error (uuid question) "Invalid value provided"
  ;;

  let is_valid question answer_input =
    let validation = validate question (Some answer_input) in
    match validation with
    | Ok _ -> true
    | Error _ -> false
  ;;
end

module QuestionAnswer = struct
  type t = Question.t * AnswerInput.t option [@@deriving yojson, show, eq]

  let are_all_required_questions_answered question_input =
    question_input
    |> CCList.for_all (fun (question, answer) ->
           match Question.is_required question, answer with
           | true, None -> false
           | _, _ -> true)
  ;;

  let are_all_answered_questions_valid question_input =
    question_input
    |> CCList.for_all (fun ((question : Question.t), answer) ->
           match answer with
           | Some answer -> Question.is_valid question answer
           | None -> true)
  ;;

  let can_questions_answered_get_submitted question_input =
    are_all_required_questions_answered question_input
    && are_all_answered_questions_valid question_input
  ;;

  let filter_asset_out question_answers =
    CCList.filter
      (fun (question, _) ->
        match question with
        | Question.File _ -> false
        | _ -> true)
      question_answers
  ;;

  let update
      (question_answers : (Question.t * AnswerInput.t) list)
      (question : Question.t)
      (answer : AnswerInput.t)
    =
    let found_index =
      question_answers
      |> CCList.find_idx (fun (existing_question, _) ->
             Question.uuid existing_question == Question.uuid question)
    in
    match found_index with
    | None -> question_answers
    | Some (index_to_update, (question_to_answer, _)) ->
      CCList.mapi
        (fun index old_answer ->
          if index = index_to_update then question_to_answer, answer else old_answer)
        question_answers
  ;;

  let event questionnaire_id current updated =
    match current, updated with
    | (_, Some (AnswerInput.Text _)), (question, Some (AnswerInput.Text new_text)) ->
      Some (Event.TextAnswerUpdated (questionnaire_id, Question.uuid question, new_text))
    | (_, None), (question, Some (AnswerInput.Text new_text)) ->
      Some (Event.TextAnswerCreated (questionnaire_id, Question.uuid question, new_text))
    | (_, Some (AnswerInput.Asset _)), (question, Some (AnswerInput.Asset asset_answer))
      ->
      Some
        (Event.AssetAnswerUpdated (questionnaire_id, Question.uuid question, asset_answer))
    | (_, None), (question, Some (AnswerInput.Asset asset_answer)) ->
      Some
        (Event.AssetAnswerCreated (questionnaire_id, Question.uuid question, asset_answer))
    | (_, Some (AnswerInput.Asset _)), (question, None) ->
      Some (Event.AssetAnswerDelete (questionnaire_id, Question.uuid question))
    | _, _ -> None
  ;;
end

module Questionnaire = struct
  type t =
    { uuid : string
    ; template_uuid : string
    ; label : string
    ; description : string
    ; questions : QuestionAnswer.t list
    }
  [@@deriving yojson, show, fields, make, eq]

  let set_questions questions questionnaire = { questionnaire with questions }

  let is_ready_for_submission questionnaire =
    questionnaire.questions |> QuestionAnswer.are_all_required_questions_answered
  ;;

  let set_question_to_optional (question, answer) = Question.set_optional question, answer

  let set_question_with_id_to_optional ~question_id ~questions =
    questions
    |> CCList.map (fun (q, a) ->
           match Question.uuid q == question_id with
           | true -> set_question_to_optional (q, a)
           | false -> q, a)
  ;;

  let set_all_questions_to_optional questionnaire =
    let questions = questionnaire.questions |> CCList.map set_question_to_optional in
    { questionnaire with questions }
  ;;

  let answer (questionnaire : t) (answers : QuestionAnswer.t list) =
    let rec loop questions errors events =
      match questions with
      | ((question, _) as current) :: questions ->
        let answer =
          CCList.find_opt
            (fun (answer_question, _) -> Question.equal answer_question question)
            answers
        in
        let answer_input = CCOpt.bind answer (fun (_, answer) -> answer) in
        (match Question.validate question answer_input, answer with
        | Error msg, _ -> loop questions (CCList.cons msg errors) events
        | Ok (), Some answer ->
          (match QuestionAnswer.event questionnaire.uuid current answer with
          | Some event -> loop questions errors (CCList.cons event events)
          | None -> loop questions errors events)
        | Ok (), None -> loop questions errors events)
      | [] -> events |> CCList.rev, errors |> CCList.rev
    in
    let events, errors =
      match answers with
      | [ (question, _) ] when Question.is_file question ->
        let questions =
          questionnaire.questions
          |> List.find_all (fun (questionnaire_question, _) ->
                 Question.equal questionnaire_question question)
        in
        loop questions [] []
      | _ ->
        let questions = QuestionAnswer.filter_asset_out questionnaire.questions in
        loop questions [] []
    in
    match CCList.is_empty errors with
    | true -> Ok events
    | false -> Error errors
  ;;
end

module QuestionRow = struct
  type t =
    { id : string
    ; label : string option
    ; help_text : string option
    ; text : string
    ; default_value : string option
    ; validation_regex : string
    ; question_type : string
    ; max_file_size : int option
    ; mime_types : string option
    ; options : string option
    }
  [@@deriving make, show, eq]

  let of_question question =
    let open Model.Question in
    let question_row =
      { id = ""
      ; label = Some ""
      ; help_text = Some ""
      ; text = ""
      ; default_value = None
      ; validation_regex = ""
      ; question_type = ""
      ; max_file_size = None
      ; mime_types = None
      ; options = None
      }
    in
    match question with
    | Country (id, label, help_text, text, _) ->
      { question_row with id; label; help_text; text; question_type = "country" }
    | File (id, label, help_text, text, mime_types, max_file_size, _) ->
      { question_row with
        id
      ; label
      ; help_text
      ; text
      ; question_type = "file"
      ; max_file_size = Some max_file_size
      ; mime_types = Some (mime_types |> String.concat ",")
      }
    | Date (id, label, help_text, text, _) ->
      { question_row with id; label; help_text; text; question_type = "date" }
    | Select (id, label, help_text, text, possible_options, _) ->
      { question_row with
        id
      ; label
      ; help_text
      ; text
      ; question_type = "select"
      ; options = Some (possible_options |> String.concat ",")
      }
    | Text (id, label, help_text, text, default, regex, _) ->
      { question_row with
        id
      ; label
      ; help_text
      ; text
      ; default_value = default
      ; validation_regex = regex
      ; question_type = "text"
      }
    | Year (id, label, help_text, text, _) ->
      { question_row with id; label; help_text; text; question_type = "year" }
    | YesNo (id, label, help_text, text, _) ->
      { question_row with id; label; help_text; text; question_type = "y/n" }
  ;;

  let t =
    let encode m =
      Ok
        ( m.id
        , ( m.label
          , ( m.help_text
            , ( m.text
              , ( m.default_value
                , ( m.validation_regex
                  , (m.question_type, (m.max_file_size, (m.mime_types, m.options))) ) ) )
            ) ) )
    in
    let decode
        ( id
        , ( label
          , ( help_text
            , ( text
              , ( default_value
                , ( validation_regex
                  , (question_type, (max_file_size, (mime_types, options))) ) ) ) ) ) )
      =
      Ok
        { id
        ; label
        ; help_text
        ; text
        ; default_value
        ; validation_regex
        ; question_type
        ; max_file_size
        ; mime_types
        ; options
        }
    in
    let open Caqti_type in
    custom
      ~encode
      ~decode
      (tup2
         string
         (tup2
            (option string)
            (tup2
               (option string)
               (tup2
                  string
                  (tup2
                     (option string)
                     (tup2
                        string
                        (tup2
                           string
                           (tup2 (option int) (tup2 (option string) (option string))))))))))
  ;;
end

module QuestionAnswerRow = struct
  type t =
    { question_uuid : string
    ; question_label : string option
    ; question_help_text : string option
    ; question_text : string
    ; question_required : int
    ; question_default_value : string option
    ; question_validation_regex : string
    ; question_type : string
    ; question_max_file_size : int option
    ; question_mime_types : string option
    ; question_options : string option
    ; answer_uuid : string option
    ; answer_text : string option
    ; answer_asset_uuid : string option
    ; answer_asset_filename : string option
    ; answer_asset_size : int option
    ; answer_asset_mime : string option
    }
  [@@deriving make, show, eq]

  let int_to_bool n = n != 0

  let row_to_question_answer_input row =
    match row with
    | { question_uuid
      ; question_label
      ; question_help_text
      ; question_text
      ; question_default_value = default_value
      ; question_validation_regex = regex
      ; question_type = "text"
      ; question_required = required
      ; answer_uuid = Some _
      ; answer_text = Some text
      ; _
      } ->
      Ok
        ( Model.Question.Text
            ( question_uuid
            , question_label
            , question_help_text
            , question_text
            , default_value
            , regex
            , int_to_bool required )
        , Some (Model.AnswerInput.Text text) )
    | { question_uuid
      ; question_label
      ; question_help_text
      ; question_text
      ; question_default_value = default_value
      ; question_validation_regex = regex
      ; question_type = "text"
      ; question_required = required
      ; answer_uuid = None
      ; _
      } ->
      Ok
        ( Model.Question.Text
            ( question_uuid
            , question_label
            , question_help_text
            , question_text
            , default_value
            , regex
            , int_to_bool required )
        , None )
    | { question_uuid
      ; question_label
      ; question_help_text
      ; question_text
      ; question_type = "y/n"
      ; question_required = required
      ; answer_uuid = Some _
      ; answer_text = Some text
      ; _
      } ->
      Ok
        ( Model.Question.YesNo
            ( question_uuid
            , question_label
            , question_help_text
            , question_text
            , int_to_bool required )
        , Some (Model.AnswerInput.Text text) )
    | { question_uuid
      ; question_label
      ; question_help_text
      ; question_text
      ; question_required = required
      ; question_type = "y/n"
      ; answer_uuid = None
      ; _
      } ->
      Ok
        ( Model.Question.YesNo
            ( question_uuid
            , question_label
            , question_help_text
            , question_text
            , int_to_bool required )
        , None )
    | { question_uuid
      ; question_label
      ; question_help_text
      ; question_text
      ; question_required = required
      ; question_type = "date"
      ; answer_uuid = Some _
      ; answer_text = Some text
      ; _
      } ->
      Ok
        ( Model.Question.Date
            ( question_uuid
            , question_label
            , question_help_text
            , question_text
            , int_to_bool required )
        , Some (Model.AnswerInput.Text text) )
    | { question_uuid
      ; question_label
      ; question_help_text
      ; question_text
      ; question_required = required
      ; question_type = "date"
      ; answer_uuid = None
      ; _
      } ->
      Ok
        ( Model.Question.Date
            ( question_uuid
            , question_label
            , question_help_text
            , question_text
            , int_to_bool required )
        , None )
    | { question_uuid
      ; question_label
      ; question_help_text
      ; question_text
      ; question_required = required
      ; question_type = "country"
      ; answer_uuid = Some _
      ; answer_text = Some text
      ; _
      } ->
      Ok
        ( Model.Question.Country
            ( question_uuid
            , question_label
            , question_help_text
            , question_text
            , int_to_bool required )
        , Some (Model.AnswerInput.Text text) )
    | { question_uuid
      ; question_label
      ; question_help_text
      ; question_text
      ; question_required = required
      ; question_type = "country"
      ; answer_uuid = None
      ; _
      } ->
      Ok
        ( Model.Question.Country
            ( question_uuid
            , question_label
            , question_help_text
            , question_text
            , int_to_bool required )
        , None )
    | { question_uuid
      ; question_label = label
      ; question_help_text = help_text
      ; question_text = text
      ; question_required = required
      ; question_type = "file"
      ; question_max_file_size = Some max_file_size
      ; question_mime_types = Some supported_mime_types
      ; answer_uuid = Some _
      ; answer_asset_uuid = Some asset_id
      ; answer_asset_filename = Some filename
      ; answer_asset_size = Some size
      ; answer_asset_mime = Some mime
      ; _
      } ->
      Ok
        ( Model.Question.File
            ( question_uuid
            , label
            , help_text
            , text
            , Str.split (Str.regexp_string ",") supported_mime_types
            , max_file_size
            , int_to_bool required )
        , Some (Model.AnswerInput.Asset (Some asset_id, filename, size, mime, "")) )
    | { question_uuid
      ; question_label = label
      ; question_help_text = help_text
      ; question_text = text
      ; question_required = required
      ; question_type = "file"
      ; question_max_file_size = Some max_file_size
      ; question_mime_types = Some supported_mime_types
      ; answer_asset_uuid = None
      ; _
      } ->
      Ok
        ( Model.Question.File
            ( question_uuid
            , label
            , help_text
            , text
            , Str.split (Str.regexp_string ",") supported_mime_types
            , max_file_size
            , int_to_bool required )
        , None )
    | { question_uuid
      ; question_label
      ; question_help_text
      ; question_text
      ; question_required = required
      ; question_type = "year"
      ; answer_uuid = Some _
      ; answer_text = Some text
      ; _
      } ->
      Ok
        ( Model.Question.Year
            ( question_uuid
            , question_label
            , question_help_text
            , question_text
            , int_to_bool required )
        , Some (Model.AnswerInput.Text text) )
    | { question_uuid
      ; question_label
      ; question_help_text
      ; question_text
      ; question_required = required
      ; question_type = "year"
      ; answer_uuid = None
      ; _
      } ->
      Ok
        ( Model.Question.Year
            ( question_uuid
            , question_label
            , question_help_text
            , question_text
            , int_to_bool required )
        , None )
    | { question_uuid
      ; question_label
      ; question_help_text
      ; question_text
      ; question_required = required
      ; question_type = "select"
      ; question_options = Some options
      ; answer_uuid = Some _
      ; answer_text = Some text
      ; _
      } ->
      Ok
        ( Model.Question.Select
            ( question_uuid
            , question_label
            , question_help_text
            , question_text
            , Str.split (Str.regexp_string ",") options
            , int_to_bool required )
        , Some (Model.AnswerInput.Text text) )
    | { question_uuid
      ; question_label
      ; question_help_text
      ; question_text
      ; question_required = required
      ; question_type = "select"
      ; question_options = Some options
      ; answer_uuid = None
      ; _
      } ->
      Ok
        ( Model.Question.Select
            ( question_uuid
            , question_label
            , question_help_text
            , question_text
            , Str.split (Str.regexp_string ",") options
            , int_to_bool required )
        , None )
    | { question_type = type_; question_uuid = id; _ } ->
      let msg =
        Caml.Format.asprintf
          "Invalid question type encountered %s for question with id %s"
          type_
          id
      in
      Logs.err (fun m -> m "%s" msg);
      Error msg
  ;;

  let to_question_answer_input rows = rows |> CCResult.map_l row_to_question_answer_input

  let t =
    let encode m =
      Ok
        ( m.question_uuid
        , ( m.question_label
          , ( m.question_help_text
            , ( m.question_text
              , ( m.question_required
                , ( m.question_default_value
                  , ( m.question_validation_regex
                    , ( m.question_type
                      , ( m.question_max_file_size
                        , ( m.question_mime_types
                          , ( m.question_options
                            , ( m.answer_uuid
                              , ( m.answer_text
                                , ( m.answer_asset_uuid
                                  , ( m.answer_asset_filename
                                    , (m.answer_asset_size, m.answer_asset_mime) ) ) ) )
                            ) ) ) ) ) ) ) ) ) ) )
    in
    let decode
        ( question_uuid
        , ( question_label
          , ( question_help_text
            , ( question_text
              , ( question_required
                , ( question_default_value
                  , ( question_validation_regex
                    , ( question_type
                      , ( question_max_file_size
                        , ( question_mime_types
                          , ( question_options
                            , ( answer_uuid
                              , ( answer_text
                                , ( answer_asset_uuid
                                  , ( answer_asset_filename
                                    , (answer_asset_size, answer_asset_mime) ) ) ) ) ) )
                        ) ) ) ) ) ) ) ) )
      =
      Ok
        { question_uuid
        ; question_label
        ; question_help_text
        ; question_text
        ; question_default_value
        ; question_validation_regex
        ; question_type
        ; question_max_file_size
        ; question_mime_types
        ; question_options
        ; question_required
        ; answer_uuid
        ; answer_text
        ; answer_asset_uuid
        ; answer_asset_filename
        ; answer_asset_size
        ; answer_asset_mime
        }
    in
    let open Caqti_type in
    custom
      ~encode
      ~decode
      (tup2
         string
         (tup2
            (option string)
            (tup2
               (option string)
               (tup2
                  string
                  (tup2
                     int
                     (tup2
                        (option string)
                        (tup2
                           string
                           (tup2
                              string
                              (tup2
                                 (option int)
                                 (tup2
                                    (option string)
                                    (tup2
                                       (option string)
                                       (tup2
                                          (option string)
                                          (tup2
                                             (option string)
                                             (tup2
                                                (option string)
                                                (tup2
                                                   (option string)
                                                   (tup2 (option int) (option string)))))))))))))))))
  ;;
end

module QuestionnaireRow = struct
  type t =
    { uuid : string
    ; template_uuid : string
    ; template_label : string
    ; template_description : string option
    }
  [@@deriving make, show, eq]

  let to_questionnaire row question_rows =
    let questions =
      QuestionAnswerRow.to_question_answer_input question_rows |> CCResult.get_or_failwith
    in
    Model.Questionnaire.make
      ~uuid:row.uuid
      ~template_uuid:row.template_uuid
      ~label:row.template_label
      ~description:(row.template_description |> Option.value ~default:"")
      ~questions
      ()
  ;;

  let t =
    let encode m =
      Ok (m.uuid, m.template_uuid, m.template_label, m.template_description)
    in
    let decode (uuid, template_uuid, template_label, template_description) =
      Ok { uuid; template_uuid; template_label; template_description }
    in
    let open Caqti_type in
    custom ~encode ~decode (tup4 string string string (option string))
  ;;
end

module AnswerRow = struct
  type t =
    { uuid : string
    ; text : string option
    ; asset : string option
    }
  [@@deriving make, fields]

  let asset_exn answer =
    match answer |> asset with
    | None ->
      failwith (Caml.Format.asprintf "Asset with id %s has no asset" (uuid answer))
    | Some answer -> answer
  ;;

  let t =
    let encode m = Ok (m.uuid, m.text, m.asset) in
    let decode (uuid, text, asset) = Ok { uuid; text; asset } in
    let open Caqti_type in
    custom ~encode ~decode (tup3 string (option string) (option string))
  ;;
end

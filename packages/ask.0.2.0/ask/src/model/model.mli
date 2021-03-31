module AnswerInput : sig
  type uuid = string option
  type filename = string
  type size = int
  type mime_type = string
  type data = string
  type text_answer = string
  type asset_answer = uuid * filename * size * mime_type * data

  type t =
    | Text of text_answer
    | Asset of asset_answer

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val text : t -> string
  val mime_to_ext : string -> string
  val equal : t -> t -> bool
end

module Event : sig
  type questionnaire_id = string
  type question_id = string

  type t =
    | TextAnswerCreated of questionnaire_id * question_id * AnswerInput.text_answer
    | TextAnswerUpdated of questionnaire_id * question_id * AnswerInput.text_answer
    | AssetAnswerCreated of questionnaire_id * question_id * AnswerInput.asset_answer
    | AssetAnswerUpdated of questionnaire_id * question_id * AnswerInput.asset_answer
    | AssetAnswerDelete of questionnaire_id * question_id

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val show : t -> string
end

module Question : sig
  type required = bool
  type label = string option
  type help_text = string option
  type text = string
  type id = string
  type regex = string
  type possible_options = string list
  type mime_types = string list
  type max_file_size = int

  type t =
    | Text of id * label * help_text * text * string option * regex * required
    | Country of id * label * help_text * text * required
    | Select of id * label * help_text * text * possible_options * required
    | YesNo of id * label * help_text * text * required
    | Date of id * label * help_text * text * required
    | File of id * label * help_text * text * mime_types * max_file_size * required
    | Year of id * label * help_text * text * required

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val show : t -> string
  val uuid : t -> string
  val label : t -> string
  val help_text : t -> string option
  val text : t -> string
  val is_file : t -> bool
  val is_required : t -> bool
  val set_optional : t -> t
  val is_valid : t -> AnswerInput.t -> bool
  val validate : t -> AnswerInput.t option -> (unit, string) Result.t
end

module QuestionAnswer : sig
  type t = Question.t * AnswerInput.t option

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val show : t -> string
  val can_questions_answered_get_submitted : t list -> bool
  val filter_asset_out : t list -> t list

  val update
    :  (Question.t * AnswerInput.t) list
    -> Question.t
    -> AnswerInput.t
    -> (Question.t * AnswerInput.t) list

  val event : string -> t -> t -> Event.t option
end

module Questionnaire : sig
  type t

  val questions : t -> QuestionAnswer.t list
  val description : t -> string
  val label : t -> string
  val template_uuid : t -> string
  val uuid : t -> string
  val set_questions : QuestionAnswer.t list -> t -> t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val equal : t -> t -> bool

  val make
    :  uuid:string
    -> template_uuid:string
    -> label:string
    -> description:string
    -> ?questions:QuestionAnswer.t list
    -> unit
    -> t

  val is_ready_for_submission : t -> bool
  val set_question_to_optional : Question.t * 'a -> Question.t * 'a

  val set_question_with_id_to_optional
    :  question_id:string
    -> questions:(Question.t * 'a) list
    -> (Question.t * 'a) list

  val set_all_questions_to_optional : t -> t
  val answer : t -> QuestionAnswer.t list -> (Event.t list, string list) Result.t
end

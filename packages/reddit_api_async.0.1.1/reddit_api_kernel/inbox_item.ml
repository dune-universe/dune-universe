open! Core_kernel

module Comment = struct
  include Json_object.Utils

  include Json_object.Make_kinded_simple (struct
    let kind = "t1"
  end)

  module Type = struct
    type t =
      | Comment_reply
      | Link_reply
    [@@deriving sexp]
  end

  let id = required_field "id" (string >> Thing.Comment.Id.of_string)

  let body t markup =
    let field =
      match markup with
      | `markdown -> "body"
      | `HTML -> "body_html"
    in
    required_field field string t
  ;;

  let author = required_field "author" (string >> Username.of_string_or_deleted)
  let subreddit = required_field "subreddit" subreddit_name
  let creation_time = required_field "created_utc" time
  let score = required_field "score" int

  let parent_id =
    required_field "parent_id" (fun json ->
        let id_string = string json in
        match Thing.Fullname.of_string id_string with
        | (`Comment _ | `Link _) as v -> v
        | _ ->
          raise_s
            [%message "Unexpected Inbox.Comment.parent_id kind" (id_string : string)])
  ;;

  let new_ = required_field "new" bool

  let type_ =
    required_field "type" (fun json ->
        (match string json with
         | "post_reply" -> Link_reply
         | "comment_reply" -> Comment_reply
         | type_ ->
           raise_s [%message "Unrecognized Inbox.Comment.t type" (type_ : string)]
          : Type.t))
  ;;

  let link_id = required_field "context" (uri >> Thing.Link.Id.of_uri)
  let link_title = required_field "link_title" string
  let num_comments_in_thread = required_field "num_comments" int
end

type t =
  | Message of Thing.Message.t
  | Comment of Comment.t
[@@deriving sexp]

let of_json json =
  let kind = Json.find json [ "kind" ] |> Json.get_string in
  let data = Json.find json [ "data" ] in
  match kind with
  | "t1" -> Comment (Comment.of_json data)
  | "t4" -> Message (Thing.Message.of_json data)
  | _ -> raise_s [%message "Unrecognized Inbox_item kind" (kind : string)]
;;

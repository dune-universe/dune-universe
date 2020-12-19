open! Core_kernel

module Id = struct
  type t =
    { subreddit : Subreddit_name.t option
    ; page : string
    }
  [@@deriving sexp]
end

module Revision = struct
  include Json_object.Utils

  module Id = struct
    include Uuid
    include Uuid.Unstable

    let of_uuid = ident
    let to_uuid = ident
  end

  let page_name = required_field "page" string
  let id = required_field "id" (string >> Id.of_string)
  let reason = optional_field "reason" string
  let timestamp = required_field "timestamp" time
  let hidden = required_field "revision_hidden" bool
  let author = optional_field "author" Thing.User.of_json
end

module Permissions = struct
  include Json_object.Utils

  include Json_object.Make_kinded_simple (struct
    let kind = "wikipagesettings"
  end)

  module Level = struct
    type t =
      | Use_subreddit_wiki_permissions
      | Only_approved_contributors_for_this_page
      | Only_moderators
    [@@deriving sexp]

    let of_int_exn level =
      match level with
      | 0 -> Use_subreddit_wiki_permissions
      | 1 -> Only_approved_contributors_for_this_page
      | 2 -> Only_moderators
      | _ -> raise_s [%message "Unrecognized wiki page permission level" (level : int)]
    ;;

    let to_int t =
      match t with
      | Use_subreddit_wiki_permissions -> 0
      | Only_approved_contributors_for_this_page -> 1
      | Only_moderators -> 2
    ;;
  end

  let level = required_field "permlevel" (int >> Level.of_int_exn)
  let contributors = required_field "editors" (Json.get_list Thing.User.of_json)
  let listed = required_field "listed" bool
end

include Json_object.Utils

include Json_object.Make_kinded_simple (struct
  let kind = "wikipage"
end)

let may_revise = required_field "may_revise" bool
let revision_id = required_field "revision_id" (string >> Uuid.of_string)
let revision_by = required_field "revision_by" Thing.User.of_json

let content t markup =
  let field =
    match markup with
    | `markdown -> "content_md"
    | `HTML -> "content_html"
  in
  required_field field string t
;;

let revision_time = required_field "revision_date" time
let revision_reason = optional_field "reason" string

module Edit_conflict = struct
  include Json_object.Utils

  let diff = required_field "diffcontent" string
  let message = required_field "message" string
  let new_content = required_field "newcontent" string
  let new_revision = required_field "newrevision" (string >> Uuid.of_string)
  let reason = optional_field "reason" string
end

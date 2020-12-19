open! Core_kernel

module Common = struct
  module Id = String
  include Json_object.Utils

  let of_json = Json.get_map
  let to_json t = `O (Map.to_alist t)
  let relationship_id = required_field "rel_id" string
  let username = required_field "name" username
  let user_id = required_field "id" (string >> Thing.User.Id.of_string)
  let date = required_field "date" time
end

module Contributor = Common

module Mute = struct
  include Common

  module Id = struct
    include Id

    let to_uuid t = String.chop_prefix_exn t ~prefix:"Mute_" |> Uuid.of_string
    let of_uuid uuid = sprintf !"Mute_%{Uuid}" uuid
  end
end

module Ban = struct
  include Common

  let note = required_field "note" string
  let days_left = optional_field "days_left" int
end

module Moderator = struct
  include Common

  let permissions = required_field "mod_permissions" (Json.get_list string)
  let flair_text = optional_field "author_flair_text" string
  let flair_css_class = optional_field "author_css_class" string
end

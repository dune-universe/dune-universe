open! Core_kernel
open Thing

module Sequencer = struct
  module T = struct
    type t = More_children [@@deriving hash, compare, sexp, bin_io]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module Request = struct
  module T = struct
    type t =
      | Get of { uri : Uri_sexp.t }
      | Post_form of
          { uri : Uri_sexp.t
          ; params : (string * string list) list
          }
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

module Param_dsl = struct
  type t = (string * string list) list

  let required f key values : t = [ key, List.map values ~f ]
  let required' f key value : t = [ key, [ f value ] ]

  let optional f key values_opt : t =
    Option.to_list values_opt |> List.bind ~f:(required f key)
  ;;

  let optional' f key value_opt : t =
    Option.to_list value_opt |> List.bind ~f:(required' f key)
  ;;

  let include_optional f value_opt : t = Option.to_list value_opt |> List.bind ~f
  let _ = optional
  let combine = List.join
  let bool = Bool.to_string
  let string = Fn.id
  let int = Int.to_string

  let pagination_ (pagination : Listing.Pagination.t) =
    let key =
      match pagination with
      | Before _ -> "before"
      | After _ -> "after"
    in
    let value =
      match pagination with
      | Before page_id | After page_id -> Listing.Page_id.to_string page_id
    in
    [ key, [ value ] ]
  ;;

  let fullname_ = Thing.Fullname.to_string
  let username_ = Username.to_string
  let json = Json.value_to_string
  let time = Time.to_string_iso8601_basic ~zone:Time.Zone.utc
end

module Parameters = struct
  module Comment_sort = struct
    type t =
      | Confidence
      | Top
      | New
      | Controversial
      | Old
      | Random
      | Q_and_a
      | Live
    [@@deriving sexp]

    let to_string t =
      match t with
      | Q_and_a -> "qa"
      | _ -> sexp_of_t t |> Sexp.to_string |> String.lowercase
    ;;
  end

  module Report_target = struct
    let params_of_t t =
      match t with
      | `Modmail_conversation id ->
        [ "modmail_conv_id", [ Modmail_conversation.Id.to_string id ] ]
      | `Link _ | `Comment _ | `Message _ -> [ "thing_id", [ Param_dsl.fullname_ t ] ]
    ;;
  end

  module Flair_target = struct
    type t =
      | Link of Link.Id.t
      | User of Username.t

    let params_of_t t =
      let open Param_dsl in
      match t with
      | Link link_id -> required' fullname_ "link" (`Link link_id)
      | User username -> required' username_ "name" username
    ;;
  end

  module Color = struct
    type t = string

    let validate component value =
      let lower = Maybe_bound.Incl 0x00 in
      let upper = Maybe_bound.Incl 0xFF in
      match Maybe_bound.interval_contains_exn value ~lower ~upper ~compare with
      | true -> ()
      | false ->
        raise_s
          [%message
            "Color component out of range"
              (component : string)
              (value : int)
              (lower : int Maybe_bound.t)
              (upper : int Maybe_bound.t)]
    ;;

    let create ~red ~green ~blue =
      validate "red" red;
      validate "green" green;
      validate "blue" blue;
      sprintf "#%02X%02X%02X" red green blue
    ;;
  end

  module Sticky_state = struct
    type t =
      | Sticky of { slot : int option }
      | Unsticky
    [@@deriving sexp]

    let params_of_t t =
      let state_field bool = "state", [ Bool.to_string bool ] in
      match t with
      | Unsticky -> [ state_field false ]
      | Sticky { slot = None } -> [ state_field true ]
      | Sticky { slot = Some slot } -> [ state_field true; "num", [ Int.to_string slot ] ]
    ;;
  end

  module Modmail_recipient = struct
    type t =
      | User of Username.t
      | Internal

    let params_of_t t =
      match t with
      | Internal -> []
      | User user -> [ "to", [ Param_dsl.username_ user ] ]
    ;;
  end

  module Link_kind = struct
    module Self_post_body = struct
      type t =
        | Markdown of string
        | Richtext_json of Json.t
      [@@deriving sexp]
    end

    type t =
      | Link of { url : string }
      | Self of Self_post_body.t
      | Crosspost of Link.Id.t
    [@@deriving sexp]

    let params_of_t t =
      match t with
      | Link { url } -> [ "kind", [ "link" ]; "url", [ url ] ]
      | Self body ->
        [ "kind", [ "self" ]
        ; (match body with
          | Markdown markdown -> "text", [ markdown ]
          | Richtext_json json -> "richtext_json", [ Json.value_to_string json ])
        ]
      | Crosspost link_id ->
        [ "kind", [ "crosspost" ]
        ; "crosspost_fullname", [ Param_dsl.fullname_ (`Link link_id) ]
        ]
    ;;
  end

  module Vote_direction = struct
    type t =
      | Up
      | Neutral
      | Down
    [@@deriving sexp]

    let int_of_t t =
      match t with
      | Up -> 1
      | Neutral -> 0
      | Down -> -1
    ;;

    let params_of_t t = [ "dir", [ int_of_t t |> Int.to_string ] ]
  end

  module Info_query = struct
    type t =
      | Id of
          [ `Link of Link.Id.t | `Comment of Comment.Id.t | `Subreddit of Subreddit.Id.t ]
          list
      | Subreddit_name of Subreddit_name.t list
      | Url of Uri_sexp.t
    [@@deriving sexp]

    let params_of_t t =
      match t with
      | Id fullnames -> [ "id", List.map fullnames ~f:Param_dsl.fullname_ ]
      | Subreddit_name subreddits ->
        [ "sr_name", List.map subreddits ~f:Subreddit_name.to_string ]
      | Url uri -> [ "url", [ Uri.to_string uri ] ]
    ;;
  end

  module Duplicate_sort = struct
    type t =
      | Number_of_comments
      | New

    let params_of_t t =
      [ ( "sort"
        , match t with
          | Number_of_comments -> [ "number_of_comments" ]
          | New -> [ "new" ] )
      ]
    ;;
  end

  module Historical_span = struct
    module T = struct
      type t =
        | Hour
        | Day
        | Week
        | Month
        | Year
        | All
      [@@deriving sexp]
    end

    include T
    include Sexpable.To_stringable (T)

    let params_of_t t = [ "t", [ to_string t |> String.lowercase ] ]
  end

  module Mod_filter = struct
    type t =
      | Moderators of Username.t list
      | Admin

    let params_of_t t =
      [ ( "mod"
        , match t with
          | Admin -> [ "a" ]
          | Moderators moderators -> List.map moderators ~f:Username.to_string )
      ]
    ;;
  end

  module Links_or_comments = struct
    type t =
      | Links
      | Comments

    let params_of_t t =
      [ ( "only"
        , match t with
          | Links -> [ "links" ]
          | Comments -> [ "comments" ] )
      ]
    ;;
  end

  module How_to_distinguish = struct
    type t =
      | Mod
      | Admin
      | Special
      | Undistinguish

    let params_of_t t =
      [ ( "how"
        , [ (match t with
            | Mod -> "yes"
            | Admin -> "admin"
            | Special -> "special"
            | Undistinguish -> "no")
          ] )
      ]
    ;;
  end

  module Search_sort = struct
    type t =
      | Relevance
      | Hot
      | Top
      | New
      | Comments

    let params_of_t t =
      [ ( "sort"
        , [ (match t with
            | Relevance -> "relevance"
            | Hot -> "hot"
            | Top -> "top"
            | New -> "new"
            | Comments -> "comments")
          ] )
      ]
    ;;
  end

  module Search_type = struct
    module T = struct
      type t =
        | Subreddit
        | Link
        | User
      [@@deriving compare, sexp, enumerate]
    end

    include T
    include Comparable.Make (T)

    let to_string t =
      match t with
      | Subreddit -> "sr"
      | Link -> "link"
      | User -> "user"
    ;;
  end

  module Link_type = struct
    type t =
      | Any
      | Link
      | Self

    let params_of_t t =
      [ ( "link_type"
        , [ (match t with
            | Any -> "any"
            | Link -> "link"
            | Self -> "self")
          ] )
      ]
    ;;
  end

  module Spam_level = struct
    type t =
      | Low
      | High
      | All

    let to_string t =
      match t with
      | Low -> "low"
      | High -> "high"
      | All -> "all"
    ;;
  end

  module Subreddit_type = struct
    type t =
      | Gold_restricted
      | Archived
      | Restricted
      | Employees_only
      | Gold_only
      | Private
      | User
      | Public

    let to_string t =
      match t with
      | Gold_restricted -> "gold_restricted"
      | Archived -> "archived"
      | Restricted -> "restricted"
      | Employees_only -> "employees_only"
      | Gold_only -> "gold_only"
      | Private -> "private"
      | User -> "user"
      | Public -> "public"
    ;;
  end

  module Wiki_mode = struct
    type t =
      | Disabled
      | Mod_only
      | Anyone

    let to_string t =
      match t with
      | Disabled -> "disabled"
      | Mod_only -> "modonly"
      | Anyone -> "anyone"
    ;;
  end

  module Subscription_action = struct
    type t =
      | Subscribe
      | Unsubscribe

    let to_string t =
      match t with
      | Subscribe -> "sub"
      | Unsubscribe -> "unsub"
    ;;
  end

  module Subscription_list = struct
    type t =
      | By_id of Subreddit.Id.t list
      | By_name of Subreddit_name.t list

    let params_of_t t =
      match t with
      | By_id ids ->
        [ "sr", List.map ids ~f:(fun id -> `Subreddit id |> Thing.Fullname.to_string) ]
      | By_name names -> [ "sr_name", List.map names ~f:Subreddit_name.to_string ]
    ;;
  end

  module Image_file_extension = struct
    type t =
      | Png
      | Jpg
  end

  module Subreddit_image = struct
    type t =
      | Stylesheet_image of { name : string }
      | Header
      | Mobile_icon
      | Mobile_banner
  end

  module Relevance_or_activity = struct
    type t =
      | Relevance
      | Activity

    let to_string t =
      match t with
      | Relevance -> "relevance"
      | Activity -> "activity"
    ;;
  end

  module Subreddit_relationship = struct
    type t =
      | Subscriber
      | Contributor
      | Moderator
      | Stream_subscriber

    let to_string t =
      match t with
      | Subscriber -> "subscriber"
      | Contributor -> "contributor"
      | Moderator -> "moderator"
      | Stream_subscriber -> "streams"
    ;;
  end

  module Subreddit_listing_sort = struct
    type t =
      | Popular
      | New
      | Gold
      | Default

    let to_string t =
      match t with
      | Popular -> "popular"
      | New -> "new"
      | Gold -> "gold"
      | Default -> "default"
    ;;
  end

  module User_subreddit_sort = struct
    type t =
      | Popular
      | New

    let to_string t =
      match t with
      | Popular -> "popular"
      | New -> "new"
    ;;
  end

  module Relationship_spec = struct
    module Duration = struct
      type t =
        | Permanent
        | Days of int
      [@@deriving sexp, compare, equal]

      let params_of_t t =
        [ ( "duration"
          , match t with
            | Permanent -> []
            | Days days -> [ Int.to_string days ] )
        ]
      ;;
    end

    type t =
      | Friend
      | Moderator
      | Moderator_invite
      | Contributor
      | Banned
      | Muted
      | Wiki_banned
      | Wiki_contributor
    [@@deriving sexp]

    let to_string t =
      match t with
      | Friend -> "friend"
      | Moderator -> "moderator"
      | Moderator_invite -> "moderator_invite"
      | Contributor -> "contributor"
      | Banned -> "banned"
      | Muted -> "muted"
      | Wiki_banned -> "wikibanned"
      | Wiki_contributor -> "wikicontributor"
    ;;

    let params_of_t t = [ "type", [ to_string t ] ]
  end
end

module Api_error = struct
  type t =
    | Cohttp_raised of Exn.t
    | Reddit_reported_error of Cohttp.Response.t * Cohttp.Body.t
  [@@deriving sexp_of]
end

type 'a t =
  { request : Request.t
  ; handle_response : Cohttp.Response.t * Cohttp.Body.t -> ('a, Api_error.t) Result.t
  ; sequencer : Sequencer.t option
  }

let map t ~f =
  { t with handle_response = (fun v -> t.handle_response v |> Result.map ~f) }
;;

let call_api
    ?sequencer
    handle_response
    ?(param_list_override = Fn.id)
    ()
    ~endpoint
    ~http_verb
    ~params
  =
  let params = ("raw_json", [ "1" ]) :: params in
  let params = param_list_override params in
  let uri = sprintf "https://oauth.reddit.com%s" endpoint |> Uri.of_string in
  let request : Request.t =
    match http_verb with
    | `GET ->
      let uri = Uri.add_query_params uri params in
      Get { uri }
    | `POST -> Post_form { uri; params }
  in
  { request; handle_response; sequencer }
;;

let get = call_api ~http_verb:`GET
let post = call_api ~http_verb:`POST

let optional_subreddit_endpoint ?subreddit suffix =
  let subreddit_part =
    Option.value_map subreddit ~default:"" ~f:(sprintf !"/r/%{Subreddit_name}")
  in
  sprintf !"%s%s" subreddit_part suffix
;;

let simple_post_fullnames_as_id endpoint fullnames k =
  let endpoint = sprintf "/api/%s" endpoint in
  post ~endpoint ~params:Param_dsl.(required fullname_ "id" fullnames) k
;;

let simple_post_fullname_as_id endpoint fullname k =
  simple_post_fullnames_as_id endpoint [ fullname ] k
;;

let api_type : Param_dsl.t = [ "api_type", [ "json" ] ]

open Parameters
open Result.Let_syntax

let result_of_response (response, body) =
  match Cohttp.Response.status response with
  | #Cohttp.Code.success_status -> Ok (response, body)
  | _ -> Error (Api_error.Reddit_reported_error (response, body))
;;

let handle_json_response f response =
  let%bind _response, body = result_of_response response in
  let body_string = Cohttp.Body.to_string body in
  Json.of_string body_string |> f |> Ok
;;

let assert_no_errors =
  handle_json_response (fun json ->
      match Json.find json [ "json"; "errors" ] |> Json.get_list ident with
      | [] -> ()
      | errors ->
        raise_s
          [%message
            "Unexpected errors in body of non-error HTTP response" (errors : Json.t list)])
;;

let ignore_success_response response =
  let%bind (_ : Cohttp.Response.t * Cohttp.Body.t) = result_of_response response in
  return ()
;;

let ignore_empty_object =
  handle_json_response (fun json ->
      match json with
      | `O [] -> ()
      | _ -> raise_s [%message "Unexpected JSON response" (json : Json.t)])
;;

let link_or_comment_of_json json =
  let thing = Thing.Poly.of_json json in
  match thing with
  | (`Link _ | `Comment _) as thing -> thing
  | _ -> raise_s [%message "Expected link or comment" (thing : Thing.Poly.t)]
;;

let comment_or_more_of_json json =
  let thing = Thing.Poly.of_json json in
  match thing with
  | (`Comment _ | `More_comments _) as thing -> thing
  | _ -> raise_s [%message "Expected comment or more_comments" (thing : Thing.Poly.t)]
;;

let get_listing child_of_json = handle_json_response (Listing.of_json child_of_json)
let get_link_listing = get_listing Link.of_json
let get_subreddit_listing = get_listing Subreddit.of_json
let me = get ~endpoint:"/api/v1/me" ~params:[] (handle_json_response User.of_json)

let karma =
  get ~endpoint:"/api/v1/me/karma" ~params:[] (handle_json_response Karma_list.of_json)
;;

let trophies =
  get
    ~endpoint:"/api/v1/me/trophies"
    ~params:[]
    (handle_json_response (fun json ->
         match json with
         | `O
             [ ("kind", `String "TrophyList")
             ; ("data", `O [ ("trophies", `A trophies) ])
             ] -> List.map trophies ~f:Award.of_json
         | _ -> raise_s [%message "Unexpected \"TrophyList\" JSON" (json : Json.t)]))
;;

let with_listing_params k ?pagination ?count ?limit ?show_all =
  let listing_params =
    let open Param_dsl in
    combine
      [ include_optional pagination_ pagination
      ; optional' int "count" count
      ; optional' int "limit" limit
      ; optional' (const "all") "show" show_all
      ]
  in
  k ~listing_params
;;

let prefs' which k ~listing_params =
  let endpoint = sprintf "/prefs/%s" which in
  get k ~endpoint ~params:listing_params
;;

let prefs endpoint k = with_listing_params (prefs' endpoint k)

let userlist json =
  let of_array_item json =
    Json.find json [ "data"; "children" ] |> Json.get_list User_list.Item.of_json
  in
  match json with
  | `O _ -> of_array_item json
  | `A l -> List.concat_map l ~f:of_array_item
  | _ -> raise_s [%message "Unexpect User_list.t JSON" (json : Json.t)]
;;

let friends = prefs "friends" (handle_json_response userlist)
let blocked = prefs "blocked" (handle_json_response userlist)
let messaging = prefs "messaging" (handle_json_response userlist)
let trusted = prefs "trusted" (handle_json_response userlist)

let select_flair
    ?background_color
    ?css_class
    ?flair_template_id
    ?text
    ?text_color
    ~subreddit
    ~target
  =
  let endpoint = optional_subreddit_endpoint ~subreddit "/api/selectflair" in
  let params =
    let open Param_dsl in
    combine
      [ Flair_target.params_of_t target
      ; optional' string "background_color" background_color
      ; optional' string "css_class" css_class
      ; optional' Uuid.to_string "flair_template_id" flair_template_id
      ; optional' string "text" text
      ; optional' string "text_color" text_color
      ]
  in
  post ~endpoint ~params ignore_success_response
;;

let handle_things_response =
  handle_json_response (fun json ->
      Json.find json [ "json"; "data"; "things" ]
      |> Json.get_list ident
      |> List.hd_exn
      |> Thing.Poly.of_json)
;;

let add_comment ?return_rtjson ?richtext_json ~parent ~text =
  let endpoint = "/api/comment" in
  let params =
    let open Param_dsl in
    combine
      [ api_type
      ; required' fullname_ "thing_id" parent
      ; required' string "text" text
      ; optional' bool "return_rtjson" return_rtjson
      ; optional' json "richtext_json" richtext_json
      ]
  in
  post ~endpoint ~params (fun response ->
      match%bind handle_things_response response with
      | `Comment c -> Ok c
      | response ->
        raise_s [%message "Expected comment response" (response : Thing.Poly.t)])
;;

let delete ~id =
  let endpoint = "/api/del" in
  let params =
    let open Param_dsl in
    Param_dsl.combine [ required' fullname_ "id" id ]
  in
  post ~endpoint ~params ignore_empty_object
;;

let edit ?return_rtjson ?richtext_json ~id ~text =
  let endpoint = "/api/editusertext" in
  let params =
    let open Param_dsl in
    combine
      [ api_type
      ; required' fullname_ "thing_id" id
      ; required' string "text" text
      ; optional' bool "return_rtjson" return_rtjson
      ; optional' json "richtext_json" richtext_json
      ]
  in
  post ~endpoint ~params (fun response ->
      match%bind handle_things_response response with
      | (`Link _ | `Comment _) as v -> Ok v
      | response ->
        raise_s [%message "Expected link or comment response" (response : Thing.Poly.t)])
;;

let simple_toggle verb fullnames k direction =
  let verb =
    match direction with
    | `Do -> verb
    | `Undo -> "un" ^ verb
  in
  simple_post_fullnames_as_id verb fullnames k
;;

let simple_toggle' verb fullname k direction = simple_toggle verb [ fullname ] k direction

let hide' ~links =
  simple_toggle "hide" (List.map links ~f:(fun x -> `Link x)) ignore_empty_object
;;

let hide = hide' `Do
let unhide = hide' `Undo

let info query =
  let endpoint = optional_subreddit_endpoint "/api/info" in
  let params = Info_query.params_of_t query in
  get ~endpoint ~params (fun response ->
      let handle_json json =
        let thing = Thing.Poly.of_json json in
        match thing with
        | (`Link _ | `Comment _ | `Subreddit _) as thing -> thing
        | _ -> raise_s [%message "Unexpected kind in listing" (thing : Thing.Poly.t)]
      in
      get_listing handle_json response >>| Listing.children)
;;

let lock' ~id = simple_toggle' "lock" id ignore_empty_object
let lock = lock' `Do
let unlock = lock' `Undo
let mark_nsfw' ~link = simple_toggle' "marknsfw" (`Link link) ignore_empty_object
let mark_nsfw = mark_nsfw' `Do
let unmark_nsfw = mark_nsfw' `Undo

let more_children ?limit_children ~link ~more_comments ~sort =
  let endpoint = "/api/morechildren" in
  let children = More_comments.Details.By_children.children more_comments in
  let params =
    let open Param_dsl in
    combine
      [ api_type
      ; required Comment.Id.to_string "children" children
      ; required' fullname_ "link_id" (`Link link)
      ; optional' bool "limit_children" limit_children
      ; required' Comment_sort.to_string "sort" sort
      ]
  in
  get
    ~endpoint
    ~params
    (handle_json_response (fun json ->
         Json.find json [ "json"; "data"; "things" ]
         |> Json.get_list comment_or_more_of_json))
;;

let report
    ?from_modmail
    ?from_help_desk
    ?additional_info
    ?custom_text
    ?other_reason
    ?rule_reason
    ?site_reason
    ?sr_name
    ~target
    ~reason
  =
  let endpoint = "/api/report" in
  let params =
    let open Param_dsl in
    combine
      [ Report_target.params_of_t target
      ; api_type
      ; required' string "reason" reason
      ; optional' string "additional_info" additional_info
      ; optional' string "custom_text" custom_text
      ; optional' string "other_reason" other_reason
      ; optional' string "rule_reason" rule_reason
      ; optional' string "site_reason" site_reason
      ; optional' string "sr_name" sr_name
      ; optional' bool "from_modmail" from_modmail
      ; optional' bool "from_help_desk" from_help_desk
      ]
  in
  post ~endpoint ~params assert_no_errors
;;

let report_award ~award_id =
  let endpoint = "/api/report_award" in
  let params = [ "award_id", [ award_id ] ] in
  post ~endpoint ~params return
;;

let save ?category ~id =
  let endpoint = "/api/save" in
  let params =
    let open Param_dsl in
    combine [ required' fullname_ "id" id; optional' string "category" category ]
  in
  post ~endpoint ~params ignore_empty_object
;;

let unsave ~id =
  let endpoint = "/api/save" in
  let params = [ "id", [ Param_dsl.fullname_ id ] ] in
  post ~endpoint ~params ignore_empty_object
;;

let saved_categories = get ~endpoint:"/api/saved_categories" ~params:[] return

let send_replies ~id ~enabled =
  let endpoint = "/api/sendreplies" in
  let params =
    let open Param_dsl in
    combine [ required' fullname_ "id" id; required' bool "state" enabled ]
  in
  post ~endpoint ~params ignore_empty_object
;;

let set_contest_mode ~link ~enabled =
  let endpoint = "/api/set_contest_mode" in
  let params =
    let open Param_dsl in
    combine
      [ api_type; required' fullname_ "id" (`Link link); required' bool "state" enabled ]
  in
  post ~endpoint ~params assert_no_errors
;;

let set_subreddit_sticky ?to_profile ~link ~sticky_state =
  let endpoint = "/api/set_subreddit_sticky" in
  let params =
    let open Param_dsl in
    combine
      [ Sticky_state.params_of_t sticky_state
      ; api_type
      ; required' fullname_ "id" (`Link link)
      ; optional' bool "to_profile" to_profile
      ]
  in
  post ~endpoint ~params assert_no_errors
;;

let set_suggested_sort ~link ~sort =
  let endpoint = "/api/set_suggested_sort" in
  let params =
    let open Param_dsl in
    combine
      [ api_type
      ; required' fullname_ "id" (`Link link)
      ; required'
          string
          "sort"
          (Option.value_map sort ~f:Comment_sort.to_string ~default:"blank")
      ]
  in
  post ~endpoint ~params assert_no_errors
;;

let spoiler' ~link = simple_toggle' "spoiler" (`Link link) ignore_empty_object
let spoiler = spoiler' `Do
let unspoiler = spoiler' `Undo

let store_visits ~links =
  let endpoint = "/api/store_visits" in
  let params =
    let open Param_dsl in
    combine [ required fullname_ "links" (List.map links ~f:(fun x -> `Link x)) ]
  in
  post ~endpoint ~params return
;;

let submit
    ?ad
    ?nsfw
    ?resubmit
    ?sendreplies
    ?spoiler
    ?flair_id
    ?flair_text
    ?collection_id
    ?event_start
    ?event_end
    ?event_tz
    ~subreddit
    ~title
    ~kind
  =
  let endpoint = "/api/submit" in
  let params =
    let open Param_dsl in
    combine
      [ Link_kind.params_of_t kind
      ; api_type
      ; required' Subreddit_name.to_string "sr" subreddit
      ; required' string "title" title
      ; optional' bool "ad" ad
      ; optional' bool "nsfw" nsfw
      ; optional' bool "resubmit" resubmit
      ; optional' bool "sendreplies" sendreplies
      ; optional' bool "spoiler" spoiler
      ; (* TODO Do these have to go together? *)
        optional' string "flair_id" flair_id
      ; optional' string "flair_text" flair_text
      ; optional' string "collection_id" collection_id
      ; optional' time "event_start" event_start
      ; optional' time "event_end" event_end
      ; optional' string "event_tz" event_tz
      ]
  in
  post
    ~endpoint
    ~params
    (handle_json_response (fun json ->
         let json = Json.find json [ "json"; "data" ] in
         let id = Json.find json [ "id" ] |> Json.get_string |> Link.Id.of_string in
         let url = Json.find json [ "url" ] |> Json.get_string |> Uri.of_string in
         id, url))
;;

let vote ?rank ~direction ~target =
  let endpoint = "/api/vote" in
  let params =
    let open Param_dsl in
    combine
      [ Vote_direction.params_of_t direction
      ; required' fullname_ "id" target
      ; optional' int "rank" rank
      ]
  in
  post ~endpoint ~params ignore_empty_object
;;

let best' ~listing_params ?include_categories =
  let endpoint = "/best" in
  let params =
    let open Param_dsl in
    combine [ listing_params; optional' bool "include_categories" include_categories ]
  in
  get ~endpoint ~params get_link_listing
;;

let best = with_listing_params best'

let links_by_id ~links =
  let endpoint =
    List.map links ~f:(fun link -> Param_dsl.fullname_ (`Link link))
    |> String.concat ~sep:","
    |> sprintf "/by_id/%s"
  in
  get ~endpoint ~params:[] get_link_listing
;;

let comments
    ?subreddit
    ?comment
    ?context
    ?depth
    ?limit
    ?showedits
    ?showmore
    ?sort
    ?threaded
    ?truncate
    ~link
  =
  let endpoint =
    optional_subreddit_endpoint ?subreddit (sprintf !"/comments/%{Link.Id}" link)
  in
  let params =
    let open Param_dsl in
    combine
      [ optional' Comment.Id.to_string "comment" comment
      ; optional' int "context" context
      ; optional' int "depth" depth
      ; optional' int "limit" limit
      ; optional' bool "showedits" showedits
      ; optional' bool "showmore" showmore
      ; optional' Comment_sort.to_string "sort" sort
      ; optional' bool "threaded" threaded
      ; optional' int "truncate" truncate
      ]
  in
  get
    ~endpoint
    ~params
    (handle_json_response (fun json ->
         match Json.get_list ident json with
         | [ link_json; comment_forest_json ] ->
           let link =
             Listing.of_json Link.of_json link_json |> Listing.children |> List.hd_exn
           in
           let comment_forest =
             Listing.of_json comment_or_more_of_json comment_forest_json
             |> Listing.children
           in
           { Comment_response.link; comment_forest }
         | json -> raise_s [%message "Expected two-item response" (json : Json.t list)]))
;;

let duplicates' ~listing_params ?crossposts_only ?sort ~link =
  let endpoint = sprintf !"/duplicates/%{Link.Id}" link in
  let params =
    let open Param_dsl in
    combine
      [ listing_params
      ; optional' bool "crossposts_only" crossposts_only
      ; include_optional Duplicate_sort.params_of_t sort
      ]
  in
  get ~endpoint ~params get_link_listing
;;

let duplicates = with_listing_params duplicates'

let basic_post_listing'
    endpoint_part
    ~listing_params
    ?include_categories
    ?subreddit
    ~extra_params
  =
  let endpoint = optional_subreddit_endpoint ?subreddit endpoint_part in
  let params =
    let open Param_dsl in
    combine
      [ listing_params
      ; optional' bool "include_categories" include_categories
      ; extra_params
      ]
  in
  get ~endpoint ~params get_link_listing
;;

let basic_post_listing endpoint =
  with_listing_params (basic_post_listing' endpoint ~extra_params:[])
;;

let hot' ~listing_params ?location =
  let extra_params =
    let open Param_dsl in
    optional' string "location" location
  in
  basic_post_listing' "/hot" ~extra_params ~listing_params
;;

let hot = with_listing_params hot'
let new_ = basic_post_listing "/new"
let rising = basic_post_listing "/rising"

let top' ~listing_params ?since =
  let extra_params = Param_dsl.include_optional Historical_span.params_of_t since in
  basic_post_listing' "/top" ~extra_params ~listing_params
;;

let top = with_listing_params top'

let controversial' ~listing_params ?since =
  let extra_params = Param_dsl.include_optional Historical_span.params_of_t since in
  basic_post_listing' "/controversial" ~extra_params ~listing_params
;;

let controversial = with_listing_params controversial'

let link_id_from_redirect (response, (_ : Cohttp.Body.t)) =
  let () =
    match Cohttp.Response.status response with
    | `Found -> ()
    | status ->
      raise_s
        [%message
          "Unexpected HTTP response code"
            (status : Cohttp.Code.status_code)
            (response : Cohttp.Response.t)]
  in
  let uri =
    Cohttp.Response.headers response |> Cohttp.Header.get_location |> Option.value_exn
  in
  return (Thing.Link.Id.of_uri uri)
;;

let random ?subreddit =
  let endpoint = optional_subreddit_endpoint ?subreddit "/random" in
  get ~endpoint ~params:[] link_id_from_redirect
;;

let block_author ~id = simple_post_fullname_as_id "block" id ignore_empty_object

let collapse_message' ~messages =
  simple_toggle
    "collapse_message"
    (List.map messages ~f:(fun x -> `Message x))
    ignore_empty_object
;;

let collapse_message = collapse_message' `Do
let uncollapse_message = collapse_message' `Undo

let compose_message ?g_recaptcha_response ?from_subreddit ~to_ ~subject ~text =
  let endpoint = "/api/compose" in
  let params =
    let open Param_dsl in
    combine
      [ api_type
      ; optional' string "g-recaptcha-response" g_recaptcha_response
      ; optional' Subreddit_name.to_string "from_sr" from_subreddit
      ; required' username_ "to" to_
      ; required' string "subject" subject
      ; required' string "text" text
      ]
  in
  post ~endpoint ~params assert_no_errors
;;

let delete_message ~message =
  simple_post_fullname_as_id "del_msg" (`Message message) return
;;

let read_message' ~messages =
  simple_toggle
    "read_message"
    (List.map messages ~f:(fun x -> `Message x))
    ignore_empty_object
;;

let read_message = read_message' `Do
let unread_message = read_message' `Undo

let message_listing' k endpoint ~listing_params ?include_categories ?mid ~mark_read =
  let endpoint = "/message/" ^ endpoint in
  let params =
    let open Param_dsl in
    combine
      [ listing_params
      ; optional' bool "include_categories" include_categories
      ; optional' string "mid" mid
      ; required' bool "mark" mark_read
      ]
  in
  get ~endpoint ~params k
;;

let message_listing endpoint k = with_listing_params (message_listing' k endpoint)

let inbox =
  message_listing "inbox" (handle_json_response (Listing.of_json Inbox_item.of_json))
;;

let unread =
  message_listing "unread" (handle_json_response (Listing.of_json Inbox_item.of_json))
;;

let sent =
  message_listing
    "sent"
    (handle_json_response (Listing.of_json Message.of_json))
    ~mark_read:true
;;

let comment_replies =
  message_listing
    "comments"
    (handle_json_response (Listing.of_json Inbox_item.Comment.of_json))
;;

let subreddit_comments' ~listing_params ~subreddit =
  let endpoint = sprintf !"/r/%{Subreddit_name}/comments" subreddit in
  let params = listing_params in
  get ~endpoint ~params (handle_json_response (Listing.of_json Comment.of_json))
;;

let subreddit_comments = with_listing_params subreddit_comments'

let moderation_endpoint ?(subreddit = Subreddit_name.of_string "mod") endpoint =
  sprintf !"/r/%{Subreddit_name}/about/%s" subreddit endpoint
;;

let log' ~listing_params ?mod_filter ?subreddit ?type_ =
  let endpoint = moderation_endpoint ?subreddit "log" in
  let params =
    let open Param_dsl in
    combine
      [ listing_params
      ; include_optional Mod_filter.params_of_t mod_filter
      ; optional' string "type" type_
      ]
  in
  get ~endpoint ~params (get_listing Mod_action.of_json)
;;

let log = with_listing_params log'

let mod_listing' ~listing_params ?location ?only ?subreddit ~endpoint =
  let endpoint = moderation_endpoint ?subreddit endpoint in
  let params =
    let open Param_dsl in
    combine
      [ listing_params
      ; include_optional Links_or_comments.params_of_t only
      ; optional' string "location" location
      ]
  in
  get ~endpoint ~params (get_listing link_or_comment_of_json)
;;

let mod_listing = with_listing_params mod_listing'
let reports = mod_listing ~endpoint:"reports"
let spam = mod_listing ~endpoint:"spam"
let modqueue = mod_listing ~endpoint:"modqueue"
let unmoderated = mod_listing ~endpoint:"unmoderated"
let edited = mod_listing ~endpoint:"edited"

let accept_moderator_invite ~subreddit =
  let endpoint = sprintf !"/%{Subreddit_name}/api/accept_moderator_invite" subreddit in
  post ~endpoint ~params:api_type return
;;

let approve ~id = simple_post_fullname_as_id "approve" id ignore_empty_object

let remove ~id ~spam =
  let endpoint = "/api/remove" in
  let params =
    let open Param_dsl in
    combine [ required' fullname_ "id" id; required' bool "spam" spam ]
  in
  post ~endpoint ~params ignore_empty_object
;;

let distinguish ?sticky ~id ~how =
  let endpoint = "/api/distinguish" in
  let params =
    let open Param_dsl in
    combine
      [ api_type
      ; How_to_distinguish.params_of_t how
      ; required' fullname_ "id" id
      ; optional' bool "sticky" sticky
      ]
  in
  post
    ~endpoint
    ~params
    (handle_json_response (fun json ->
         let thing =
           Json.find json [ "json"; "data"; "things" ]
           |> Json.get_list ident
           |> List.hd_exn
           |> Thing.Poly.of_json
         in
         match thing with
         | (`Comment _ | `Link _) as thing -> thing
         | _ -> raise_s [%message "Expected comment or link" (thing : Thing.Poly.t)]))
;;

let ignore_reports' ~id = simple_toggle' "ignore_reports" id ignore_empty_object
let ignore_reports = ignore_reports' `Do
let unignore_reports = ignore_reports' `Undo

let leavecontributor ~subreddit =
  simple_post_fullname_as_id "leavecontributor" (`Subreddit subreddit) ignore_empty_object
;;

let leavemoderator ~subreddit =
  simple_post_fullname_as_id "leavemoderator" (`Subreddit subreddit) ignore_empty_object
;;

let mute_message_author' ~message =
  simple_toggle' "mute_message_author" (`Message message) ignore_empty_object
;;

let mute_message_author = mute_message_author' `Do
let unmute_message_author = mute_message_author' `Undo

let stylesheet ~subreddit =
  let endpoint = sprintf !"/r/%{Subreddit_name}/about/stylesheet" subreddit in
  get ~endpoint ~params:[] (handle_json_response Stylesheet.of_json)
;;

let create_modmail_conversation ~subject ~body ~subreddit ~to_ ~hide_author =
  let endpoint = "/api/mod/conversations" in
  let params =
    let open Param_dsl in
    combine
      [ required' string "subject" subject
      ; required' string "body" body
      ; Modmail_recipient.params_of_t to_
      ; required' Subreddit_name.to_string "srName" subreddit
      ; required' Bool.to_string "isAuthorHidden" hide_author
      ]
  in
  post ~endpoint ~params (handle_json_response Modmail.Conversation.of_json)
;;

let search'
    ~listing_params
    ?category
    ?include_facets
    ?restrict_to_subreddit
    ?since
    ?sort
    ?types
    ~query
  =
  let subreddit_part, restrict_param =
    match restrict_to_subreddit with
    | None -> "", []
    | Some subreddit ->
      sprintf !"/r/%{Subreddit_name}" subreddit, [ "restrict_sr", [ "true" ] ]
  in
  let endpoint = sprintf !"%s/search" subreddit_part in
  let params =
    let open Param_dsl in
    combine
      [ listing_params
      ; optional' string "category" category
      ; optional' bool "include_facets" include_facets
      ; required' string "q" query
      ; include_optional Historical_span.params_of_t since
      ; include_optional Search_sort.params_of_t sort
      ; optional
          Search_type.to_string
          "type"
          (Option.map types ~f:Search_type.Set.to_list)
      ; restrict_param
      ]
  in
  get
    ~endpoint
    ~params
    (handle_json_response (fun json ->
         let to_link_opt thing =
           match thing with
           | `Link link -> Some link
           | _ -> None
         in
         let to_user_or_subreddit_opt thing =
           match thing with
           | (`User _ | `Subreddit _) as v -> Some v
           | _ -> None
         in
         let listings =
           let jsons =
             match json with
             | `O _ as json -> [ json ]
             | `A listings -> listings
             | _ -> raise_s [%message "Unexpected search response" (json : Json.t)]
           in
           List.map jsons ~f:(Listing.of_json Thing.Poly.of_json)
         in
         let find_kinded_listing extract_subkind error_message =
           List.find_map listings ~f:(fun listing ->
               (* If the first element belongs in one of the result listings... *)
               match
                 Listing.children listing
                 |> List.hd
                 |> Option.bind ~f:extract_subkind
                 |> Option.is_some
               with
               | false -> None
               | true ->
                 (* ...then expect them all to be in that listing. *)
                 Some
                   (Listing.map listing ~f:(fun thing ->
                        match extract_subkind thing with
                        | Some v -> v
                        | None -> raise_s [%message error_message (json : Json.t)])))
         in
         let link_listing =
           find_kinded_listing
             to_link_opt
             "Expected only links in search response listing"
         in
         let user_or_subreddit_listing =
           find_kinded_listing
             to_user_or_subreddit_opt
             "Expected only users or subreddits in search response listing"
         in
         link_listing, user_or_subreddit_listing))
;;

let search = with_listing_params search'

let about_endpoint' endpoint k ~listing_params ?include_categories ?user ~subreddit =
  let endpoint = sprintf !"/r/%{Subreddit_name}/about/%s" subreddit endpoint in
  let params =
    let open Param_dsl in
    combine
      [ listing_params
      ; optional' bool "include_categories" include_categories
      ; optional' username_ "user" user
      ]
  in
  get ~endpoint ~params k
;;

let about_endpoint endpoint k = with_listing_params (about_endpoint' endpoint k)
let banned = about_endpoint "banned" (get_listing Relationship.Ban.of_json)
let muted = about_endpoint "muted" (get_listing Relationship.Mute.of_json)
let wiki_banned = about_endpoint "wikibanned" (get_listing Relationship.Ban.of_json)

let contributors =
  about_endpoint "contributors" (get_listing Relationship.Contributor.of_json)
;;

let wiki_contributors =
  about_endpoint "wikicontributors" (get_listing Relationship.Contributor.of_json)
;;

let moderators = about_endpoint "moderators" (get_listing Relationship.Moderator.of_json)

let removal_endpoints ~endpoint ~extra_params ~subreddit =
  let endpoint = sprintf !"/r/%{Subreddit_name}/api/%s" subreddit endpoint in
  post ~endpoint ~params:(Param_dsl.combine [ api_type; extra_params ]) assert_no_errors
;;

let delete_subreddit_image ~subreddit ~(image : Subreddit_image.t) =
  let endpoint, extra_params =
    match image with
    | Header -> "delete_sr_header", []
    | Mobile_icon -> "delete_sr_icon", []
    | Mobile_banner -> "delete_sr_banner", []
    | Stylesheet_image { name } ->
      "delete_sr_img", Param_dsl.(required' string "img_name" name)
  in
  removal_endpoints ~endpoint ~extra_params ~subreddit
;;

let search_subreddits_by_name ?exact ?include_over_18 ?include_unadvertisable ~query =
  let endpoint = "/api/search_reddit_names" in
  let params =
    let open Param_dsl in
    combine
      [ required' string "query" query
      ; optional' bool "exact" exact
      ; optional' bool "include_over_18" include_over_18
      ; optional' bool "include_unadvertisable" include_unadvertisable
      ]
  in
  get
    ~endpoint
    ~params
    (handle_json_response (fun json ->
         Json.find json [ "names" ]
         |> Json.get_list (Fn.compose Subreddit_name.of_string Json.get_string)))
;;

let create_or_edit_subreddit
    ?comment_score_hide_mins
    ?wiki_edit_age
    ?wiki_edit_karma
    ~all_original_content
    ~allow_discovery
    ~allow_images
    ~allow_post_crossposts
    ~allow_top
    ~allow_videos
    ~api_type
    ~collapse_deleted_comments
    ~crowd_control_mode
    ~description
    ~disable_contributor_requests
    ~exclude_banned_modqueue
    ~free_form_reports
    ~g_recaptcha_response
    ~header_title
    ~hide_ads
    ~key_color
    ~lang
    ~link_type
    ~name
    ~original_content_tag_enabled
    ~over_18
    ~public_description
    ~restrict_commenting
    ~restrict_posting
    ~show_media
    ~show_media_preview
    ~spam_comments
    ~spam_links
    ~spam_selfposts
    ~spoilers_enabled
    ~subreddit
    ~submit_link_label
    ~submit_text
    ~submit_text_label
    ~suggested_comment_sort
    ~title
    ~type_
    ~wiki_mode
  =
  let endpoint = "/api/site_admin" in
  let params =
    let open Param_dsl in
    combine
      [ optional' int "comment_score_hide_mins" comment_score_hide_mins
      ; optional' int "wiki_edit_age" wiki_edit_age
      ; optional' int "wiki_edit_karma" wiki_edit_karma
      ; required' bool "all_original_content" all_original_content
      ; required' bool "allow_discovery" allow_discovery
      ; required' bool "allow_images" allow_images
      ; required' bool "allow_post_crossposts" allow_post_crossposts
      ; required' bool "allow_top" allow_top
      ; required' bool "allow_videos" allow_videos
      ; api_type
      ; required' bool "collapse_deleted_comments" collapse_deleted_comments
      ; required' bool "crowd_control_mode" crowd_control_mode
      ; required' string "description" description
      ; required' bool "disable_contributor_requests" disable_contributor_requests
      ; required' bool "exclude_banned_modqueue" exclude_banned_modqueue
      ; required' bool "free_form_reports" free_form_reports
      ; optional' string "g_recaptcha_response" g_recaptcha_response
      ; required' string "header_title" header_title
      ; required' bool "hide_ads" hide_ads
      ; required' string "key_color" key_color
      ; required' string "lang" lang
      ; Link_type.params_of_t link_type
      ; required' string "name" name
      ; required' bool "original_content_tag_enabled" original_content_tag_enabled
      ; required' bool "over_18" over_18
      ; required' string "public_description" public_description
      ; required' bool "restrict_commenting" restrict_commenting
      ; required' bool "restrict_posting" restrict_posting
      ; required' bool "show_media" show_media
      ; required' bool "show_media_preview" show_media_preview
      ; required' Spam_level.to_string "spam_comments" spam_comments
      ; required' Spam_level.to_string "spam_links" spam_links
      ; required' Spam_level.to_string "spam_selfposts" spam_selfposts
      ; required' bool "spoilers_enabled" spoilers_enabled
      ; required' Subreddit_name.to_string "sr" subreddit
      ; required' string "submit_link_label" submit_link_label
      ; required' string "submit_text" submit_text
      ; required' string "submit_text_label" submit_text_label
      ; required' Comment_sort.to_string "suggested_comment_sort" suggested_comment_sort
      ; required' string "title" title
      ; required' Subreddit_type.to_string "type_" type_
      ; required' Wiki_mode.to_string "wikimode" wiki_mode
      ]
  in
  post ~endpoint ~params return
;;

let submit_text ~subreddit =
  let endpoint = sprintf !"/r/%{Subreddit_name}/api/submit_text" subreddit in
  get ~endpoint ~params:[] (handle_json_response Submit_text.of_json)
;;

let subreddit_autocomplete
    ?limit
    ?include_categories
    ?include_over_18
    ?include_profiles
    ~query
  =
  let endpoint = "/api/subreddit_autocomplete_v2" in
  let params =
    let open Param_dsl in
    combine
      [ optional' int "limit" limit
      ; optional' bool "include_categories" include_categories
      ; optional' bool "include_over_18" include_over_18
      ; optional' bool "include_profiles" include_profiles
      ; required' string "query" query
      ]
  in
  get ~endpoint ~params (get_listing Subreddit.of_json)
;;

let set_subreddit_stylesheet ?reason ~subreddit ~stylesheet_contents =
  let endpoint = sprintf !"/r/%{Subreddit_name}/api/subreddit_stylesheet" subreddit in
  let params =
    let open Param_dsl in
    combine
      [ api_type
      ; optional' string "reason" reason
      ; (* "op" is a required parameter, but the "preview" option does nothing. *)
        required' string "op" "save"
      ; required' string "stylesheet_contents" stylesheet_contents
      ]
  in
  post ~endpoint ~params assert_no_errors
;;

let subscribe ?skip_initial_defaults ~action ~subreddits =
  let endpoint = "/api/subscribe" in
  let params =
    let open Param_dsl in
    combine
      [ required' Subscription_action.to_string "action" action
      ; Subscription_list.params_of_t subreddits
      ; optional' bool "skip_initial_defaults" skip_initial_defaults
      ]
  in
  post ~endpoint ~params ignore_empty_object
;;

let search_users' ~listing_params ?sort ~query =
  let endpoint = "/users/search" in
  let params =
    let open Param_dsl in
    combine
      [ listing_params
      ; required' string "q" query
      ; optional' Relevance_or_activity.to_string "sort" sort
      ]
  in
  get ~endpoint ~params (get_listing User.of_json)
;;

let search_users = with_listing_params search_users'

let about_subreddit ~subreddit =
  let endpoint = sprintf !"/r/%{Subreddit_name}/about" subreddit in
  get ~endpoint ~params:[] (handle_json_response Subreddit.of_json)
;;

let subreddit_about ?(params = []) ~subreddit endpoint =
  let endpoint = sprintf !"/r/%{Subreddit_name}/about/%s" subreddit endpoint in
  get ~endpoint ~params
;;

let subreddit_settings ?created ?location =
  let params =
    let open Param_dsl in
    combine [ optional' bool "created" created; optional' string "location" location ]
  in
  subreddit_about ~params "edit" (handle_json_response Subreddit_settings.of_json)
;;

let subreddit_rules =
  subreddit_about "rules" (handle_json_response Subreddit_rules.of_json)
;;

let subreddit_traffic =
  subreddit_about "traffic" (handle_json_response Subreddit_traffic.of_json)
;;

let get_sticky ?number ~subreddit =
  let endpoint = sprintf !"/r/%{Subreddit_name}/about/sticky" subreddit in
  let params =
    let open Param_dsl in
    combine [ optional' int "num" number ]
  in
  get ~endpoint ~params link_id_from_redirect
;;

let get_subreddits' ~listing_params ?include_categories ~relationship =
  let endpoint = sprintf !"/subreddits/mine/%{Subreddit_relationship}" relationship in
  let params =
    let open Param_dsl in
    combine [ listing_params; optional' bool "include_categories" include_categories ]
  in
  get ~endpoint ~params get_subreddit_listing
;;

let get_subreddits = with_listing_params get_subreddits'

let search_subreddits_by_title_and_description' ~listing_params ?show_users ?sort ~query =
  let endpoint = "/subreddits/search" in
  let params =
    let open Param_dsl in
    combine
      [ listing_params
      ; optional' bool "show_users" show_users
      ; required' string "q" query
      ; optional' Relevance_or_activity.to_string "sort" sort
      ]
  in
  get ~endpoint ~params get_subreddit_listing
;;

let search_subreddits_by_title_and_description =
  with_listing_params search_subreddits_by_title_and_description'
;;

let list_subreddits' ~listing_params ?include_categories ?show_users ~sort =
  let endpoint = sprintf !"/subreddits/%{Subreddit_listing_sort}" sort in
  let params =
    let open Param_dsl in
    combine
      [ listing_params
      ; optional' bool "include_categories" include_categories
      ; optional' bool "show_users" show_users
      ]
  in
  get ~endpoint ~params get_subreddit_listing
;;

let about_user ~username =
  let endpoint = sprintf !"/user/%{Username}/about" username in
  get ~endpoint ~params:[] (handle_json_response User.of_json)
;;

let list_subreddits = with_listing_params list_subreddits'

let list_user_subreddits' ~listing_params ?include_categories ~sort =
  let endpoint = sprintf !"/users/%{User_subreddit_sort}" sort in
  let params =
    let open Param_dsl in
    combine [ listing_params; optional' bool "include_categories" include_categories ]
  in
  get ~endpoint ~params get_subreddit_listing
;;

let list_user_subreddits = with_listing_params list_user_subreddits'

let add_relationship
    ~relationship
    ~username
    ~duration
    ?subreddit
    ?note
    ?ban_reason
    ?ban_message
    ?ban_context
  =
  let endpoint = optional_subreddit_endpoint ?subreddit "/api/friend" in
  let params =
    Relationship_spec.Duration.params_of_t duration
    @ Relationship_spec.params_of_t relationship
    @
    let open Param_dsl in
    combine
      [ Relationship_spec.Duration.params_of_t duration
      ; Relationship_spec.params_of_t relationship
      ; api_type
      ; required' username_ "name" username
      ; optional' string "note" note
      ; optional' string "ban_reason" ban_reason
      ; optional' string "ban_message" ban_message
      ; optional' string "ban_context" ban_context
      ]
  in
  post ~endpoint ~params ignore_success_response
;;

let remove_relationship ~relationship ~username ?subreddit =
  let endpoint = optional_subreddit_endpoint ?subreddit "/api/unfriend" in
  let params =
    let open Param_dsl in
    combine
      [ Relationship_spec.params_of_t relationship
      ; api_type
      ; required' username_ "name" username
      ]
  in
  post ~endpoint ~params ignore_success_response
;;

let add_or_remove_wiki_editor ~act ~page:({ subreddit; page } : Wiki_page.Id.t) ~user =
  let endpoint =
    optional_subreddit_endpoint ?subreddit (sprintf "/api/wiki/alloweditor/%s" act)
  in
  let params =
    let open Param_dsl in
    combine [ required' string "page" page; required' username_ "username" user ]
  in
  post ~endpoint ~params ignore_empty_object
;;

let add_wiki_editor = add_or_remove_wiki_editor ~act:"add"
let remove_wiki_editor = add_or_remove_wiki_editor ~act:"del"

let edit_wiki_page ?previous ?reason ~content ~page:({ subreddit; page } : Wiki_page.Id.t)
  =
  let endpoint = optional_subreddit_endpoint ?subreddit "/api/wiki/edit" in
  let params =
    let open Param_dsl in
    combine
      [ required' string "content" content
      ; required' string "page" page
      ; optional' Wiki_page.Revision.Id.to_string "previous" previous
      ; optional' string "reason" reason
      ]
  in
  post ~endpoint ~params (fun (response, body) ->
      let%bind json = Cohttp.Body.to_string body |> Ok >>| Json.of_string in
      match Cohttp.Response.status response, json with
      | #Cohttp.Code.success_status, `O [] -> return (Ok ())
      | `Conflict, json -> return (Error (Wiki_page.Edit_conflict.of_json json))
      | _, _ -> Error (Api_error.Reddit_reported_error (response, body)))
;;

let toggle_wiki_revision_visibility ~page:({ subreddit; page } : Wiki_page.Id.t) ~revision
  =
  let endpoint = optional_subreddit_endpoint ?subreddit "/api/wiki/hide" in
  let params =
    let open Param_dsl in
    combine
      [ required' string "page" page
      ; required' Wiki_page.Revision.Id.to_string "revision" revision
      ]
  in
  post
    ~endpoint
    ~params
    (handle_json_response (function
        | `O [ ("status", `Bool true) ] -> `Became_hidden
        | `O [ ("status", `Bool false) ] -> `Became_visible
        | json ->
          raise_s
            [%message
              "Unexpected toggle_wiki_revision_visibility response" (json : Json.t)]))
;;

let revert_wiki_page ~page:({ subreddit; page } : Wiki_page.Id.t) ~revision =
  let endpoint = optional_subreddit_endpoint ?subreddit "/api/wiki/revert" in
  let params =
    let open Param_dsl in
    combine
      [ required' string "page" page
      ; required' Wiki_page.Revision.Id.to_string "revision" revision
      ]
  in
  post ~endpoint ~params ignore_empty_object
;;

let wiki_discussions' ~listing_params ~page:({ subreddit; page } : Wiki_page.Id.t) =
  let endpoint =
    optional_subreddit_endpoint ?subreddit (sprintf "/wiki/discussions/%s" page)
  in
  get ~endpoint ~params:listing_params get_link_listing
;;

let wiki_discussions = with_listing_params wiki_discussions'

let wiki_pages ?subreddit =
  let endpoint = optional_subreddit_endpoint ?subreddit "/wiki/pages" in
  get
    ~endpoint
    ~params:[]
    (handle_json_response (fun json ->
         Json.find json [ "data" ] |> Json.get_list Json.get_string))
;;

let subreddit_wiki_revisions' ~listing_params ?subreddit =
  let endpoint = optional_subreddit_endpoint ?subreddit "/wiki/revisions" in
  get ~endpoint ~params:listing_params (get_listing Wiki_page.Revision.of_json)
;;

let subreddit_wiki_revisions = with_listing_params subreddit_wiki_revisions'

let wiki_page_revisions' ~listing_params ~page:({ subreddit; page } : Wiki_page.Id.t) =
  let endpoint =
    optional_subreddit_endpoint ?subreddit (sprintf "/wiki/revisions/%s" page)
  in
  get ~endpoint ~params:listing_params (get_listing Wiki_page.Revision.of_json)
;;

let wiki_page_revisions = with_listing_params wiki_page_revisions'

let wiki_permissions ~page:({ subreddit; page } : Wiki_page.Id.t) =
  let endpoint =
    optional_subreddit_endpoint ?subreddit (sprintf "/wiki/settings/%s" page)
  in
  get ~endpoint ~params:[] (handle_json_response Wiki_page.Permissions.of_json)
;;

let set_wiki_permissions ~page:({ subreddit; page } : Wiki_page.Id.t) ~listed ~level =
  let endpoint =
    optional_subreddit_endpoint ?subreddit (sprintf "/wiki/settings/%s" page)
  in
  let params =
    let open Param_dsl in
    combine
      [ required' bool "listed" listed
      ; required' string "page" page
      ; required' (Fn.compose int Wiki_page.Permissions.Level.to_int) "permlevel" level
      ]
  in
  post ~endpoint ~params (handle_json_response Wiki_page.Permissions.of_json)
;;

let wiki_page ?compare_revisions ~page:({ subreddit; page } : Wiki_page.Id.t) =
  let endpoint = optional_subreddit_endpoint ?subreddit (sprintf "/wiki/%s" page) in
  let v1, v2 = Option.value compare_revisions ~default:(None, None) in
  let params =
    let open Param_dsl in
    combine
      [ required' string "page" page; optional' string "v" v1; optional' string "v2" v2 ]
  in
  get ~endpoint ~params (handle_json_response Wiki_page.of_json)
;;

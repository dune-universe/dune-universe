open! Core_kernel
open Thing

module Parameters : sig
  module Comment_sort : sig
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
  end

  module Flair_target : sig
    type t =
      | Link of Link.Id.t
      | User of Username.t
  end

  module Color : sig
    type t

    val create : red:int -> green:int -> blue:int -> t
  end

  module Sticky_state : sig
    type t =
      | Sticky of { slot : int option }
      | Unsticky
    [@@deriving sexp]
  end

  module Modmail_recipient : sig
    type t =
      | User of Username.t
      | Internal
  end

  module Link_kind : sig
    module Self_post_body : sig
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
  end

  module Vote_direction : sig
    type t =
      | Up
      | Neutral
      | Down
    [@@deriving sexp]
  end

  module Info_query : sig
    type t =
      | Id of
          [ `Link of Link.Id.t | `Comment of Comment.Id.t | `Subreddit of Subreddit.Id.t ]
          list
      | Subreddit_name of Subreddit_name.t list
      | Url of Uri.t
    [@@deriving sexp]
  end

  module Duplicate_sort : sig
    type t =
      | Number_of_comments
      | New
  end

  module Historical_span : sig
    type t =
      | Hour
      | Day
      | Week
      | Month
      | Year
      | All
    [@@deriving sexp]
  end

  module Mod_filter : sig
    type t =
      | Moderators of Username.t list
      | Admin
  end

  module Links_or_comments : sig
    type t =
      | Links
      | Comments
  end

  module How_to_distinguish : sig
    type t =
      | Mod
      | Admin
      | Special
      | Undistinguish
  end

  module Search_sort : sig
    type t =
      | Relevance
      | Hot
      | Top
      | New
      | Comments
  end

  module Search_type : sig
    type t =
      | Subreddit
      | Link
      | User
    [@@deriving sexp, enumerate]

    include Comparable.S with type t := t
  end

  module Link_type : sig
    type t =
      | Any
      | Link
      | Self
  end

  module Spam_level : sig
    type t =
      | Low
      | High
      | All
  end

  module Subreddit_type : sig
    type t =
      | Gold_restricted
      | Archived
      | Restricted
      | Employees_only
      | Gold_only
      | Private
      | User
      | Public
  end

  module Wiki_mode : sig
    type t =
      | Disabled
      | Mod_only
      | Anyone
  end

  module Subscription_action : sig
    type t =
      | Subscribe
      | Unsubscribe
  end

  module Subscription_list : sig
    type t =
      | By_id of Subreddit.Id.t list
      | By_name of Subreddit_name.t list
  end

  module Image_file_extension : sig
    type t =
      | Png
      | Jpg
  end

  module Subreddit_image : sig
    type t =
      | Stylesheet_image of { name : string }
      | Header
      | Mobile_icon
      | Mobile_banner
  end

  module Relevance_or_activity : sig
    type t =
      | Relevance
      | Activity
  end

  module Subreddit_relationship : sig
    type t =
      | Subscriber
      | Contributor
      | Moderator
      | Stream_subscriber
  end

  module Subreddit_listing_sort : sig
    type t =
      | Popular
      | New
      | Gold
      | Default
  end

  module User_subreddit_sort : sig
    type t =
      | Popular
      | New
  end

  module Relationship_spec : sig
    module Duration : sig
      type t =
        | Permanent
        | Days of int
      [@@deriving sexp, compare, equal]
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
  end
end

open Parameters

module Sequencer : sig
  type t = More_children [@@deriving sexp, bin_io]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Request : sig
  type t =
    | Get of { uri : Uri_sexp.t }
    | Post_form of
        { uri : Uri_sexp.t
        ; params : (string * string list) list
        }
  [@@deriving sexp]

  include Comparable.S with type t := t
end

module Api_error : sig
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

val map : 'a t -> f:('a -> 'b) -> 'b t

type 'a with_param_override :=
  ?param_list_override:((string * string list) list -> (string * string list) list)
  -> unit
  -> 'a

type 'a with_listing_params :=
  ?pagination:Listing.Pagination.t -> ?count:int -> ?limit:int -> ?show_all:unit -> 'a

(** Account *)

val me : User.t t with_param_override
val karma : Karma_list.t t with_param_override
val trophies : Award.t list t with_param_override
val friends : User_list.t t with_param_override with_listing_params
val blocked : User_list.t t with_param_override with_listing_params
val messaging : User_list.t t with_param_override with_listing_params
val trusted : User_list.t t with_param_override with_listing_params

(** Flair *)

val select_flair
  :  ?background_color:Color.t
  -> ?css_class:string
  -> ?flair_template_id:Uuid.t
  -> ?text:string
  -> ?text_color:Color.t
  -> subreddit:Subreddit_name.t
  -> target:Flair_target.t
  -> unit t with_param_override

(** Links and comments *)

val add_comment
  :  ?return_rtjson:bool
  -> ?richtext_json:Json.t
  -> parent:[< `Link of Link.Id.t | `Comment of Comment.Id.t | `Message of Message.Id.t ]
  -> text:string
  -> Thing.Comment.t t with_param_override

val delete
  :  id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
  -> unit t with_param_override

val edit
  :  ?return_rtjson:bool
  -> ?richtext_json:Json.t
  -> id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
  -> text:string
  -> [> `Link of Link.t | `Comment of Comment.t ] t with_param_override

val hide : links:Link.Id.t list -> unit t with_param_override
val unhide : links:Link.Id.t list -> unit t with_param_override

val lock
  :  id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
  -> unit t with_param_override

val unlock
  :  id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
  -> unit t with_param_override

val mark_nsfw : link:Link.Id.t -> unit t with_param_override
val unmark_nsfw : link:Link.Id.t -> unit t with_param_override

val more_children
  :  ?limit_children:bool
  -> link:Link.Id.t
  -> more_comments:More_comments.Details.By_children.t
  -> sort:Comment_sort.t
  -> [ `Comment of Comment.t | `More_comments of More_comments.t ] list t
     with_param_override

val report
  :  ?from_modmail:bool
  -> ?from_help_desk:bool
  -> ?additional_info:string
  -> ?custom_text:string
  -> ?other_reason:string
  -> ?rule_reason:string
  -> ?site_reason:string
  -> ?sr_name:string
  -> target:
       [< `Link of Link.Id.t
       | `Comment of Comment.Id.t
       | `Message of Message.Id.t
       | `Modmail_conversation of Modmail_conversation.Id.t
       ]
  -> reason:string
  -> unit t with_param_override

val report_award
  :  award_id:string
  -> (Cohttp.Response.t * Cohttp.Body.t) t with_param_override

val save
  :  ?category:string
  -> id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
  -> unit t with_param_override

val unsave
  :  id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
  -> unit t with_param_override

val saved_categories : (Cohttp.Response.t * Cohttp.Body.t) t with_param_override

val send_replies
  :  id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
  -> enabled:bool
  -> unit t with_param_override

val set_contest_mode : link:Link.Id.t -> enabled:bool -> unit t with_param_override

val set_subreddit_sticky
  :  ?to_profile:bool
  -> link:Link.Id.t
  -> sticky_state:Sticky_state.t
  -> unit t with_param_override

val set_suggested_sort
  :  link:Link.Id.t
  -> sort:Comment_sort.t option
  -> unit t with_param_override

val spoiler : link:Link.Id.t -> unit t with_param_override
val unspoiler : link:Link.Id.t -> unit t with_param_override

val store_visits
  :  links:Link.Id.t list
  -> (Cohttp.Response.t * Cohttp.Body.t) t with_param_override

val submit
  :  ?ad:bool
  -> ?nsfw:bool
  -> ?resubmit:bool
  -> ?sendreplies:bool
  -> ?spoiler:bool
  -> ?flair_id:string
  -> ?flair_text:string
  -> ?collection_id:string
  -> ?event_start:Time.t
  -> ?event_end:Time.t
  -> ?event_tz:string
  -> subreddit:Subreddit_name.t
  -> title:string
  -> kind:Link_kind.t
  -> (Link.Id.t * Uri.t) t with_param_override

val vote
  :  ?rank:int
  -> direction:Vote_direction.t
  -> target:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
  -> unit t with_param_override

val info
  :  ?subreddit:Subreddit_name.t
  -> Info_query.t
  -> [ `Comment of Thing.Comment.t
     | `Link of Thing.Link.t
     | `Subreddit of Thing.Subreddit.t
     ]
     list
     t
     with_param_override

(** Listings *)

val best
  : (?include_categories:bool -> Link.t Listing.t t with_param_override)
    with_listing_params

val links_by_id : links:Link.Id.t list -> Link.t Listing.t t with_param_override

val comments
  :  ?subreddit:Subreddit_name.t
  -> ?comment:Comment.Id.t
  -> ?context:int
  -> ?depth:int
  -> ?limit:int
  -> ?showedits:bool
  -> ?showmore:bool
  -> ?sort:Comment_sort.t
  -> ?threaded:bool
  -> ?truncate:int
  -> link:Link.Id.t
  -> Comment_response.t t with_param_override

val duplicates
  : (?crossposts_only:bool
     -> ?sort:Duplicate_sort.t
     -> link:Link.Id.t
     -> Link.t Listing.t t with_param_override)
    with_listing_params

val hot
  : (?location:string
     -> ?include_categories:bool
     -> ?subreddit:Subreddit_name.t
     -> Link.t Listing.t t with_param_override)
    with_listing_params

val new_
  : (?include_categories:bool
     -> ?subreddit:Subreddit_name.t
     -> Link.t Listing.t t with_param_override)
    with_listing_params

val rising
  : (?include_categories:bool
     -> ?subreddit:Subreddit_name.t
     -> Link.t Listing.t t with_param_override)
    with_listing_params

val top
  : (?since:Historical_span.t
     -> ?include_categories:bool
     -> ?subreddit:Subreddit_name.t
     -> Link.t Listing.t t with_param_override)
    with_listing_params

val controversial
  : (?since:Historical_span.t
     -> ?include_categories:bool
     -> ?subreddit:Subreddit_name.t
     -> Link.t Listing.t t with_param_override)
    with_listing_params

val random : ?subreddit:Subreddit_name.t -> Link.Id.t t with_param_override

(** Private messages *)

val block_author
  :  id:[< `Comment of Comment.Id.t | `Message of Message.Id.t ]
  -> unit t with_param_override

val collapse_message : messages:Message.Id.t list -> unit t with_param_override
val uncollapse_message : messages:Message.Id.t list -> unit t with_param_override

val compose_message
  :  ?g_recaptcha_response:string
  -> ?from_subreddit:Subreddit_name.t
  -> to_:Username.t
  -> subject:string
  -> text:string
  -> unit t with_param_override

val delete_message
  :  message:Message.Id.t
  -> (Cohttp.Response.t * Cohttp.Body.t) t with_param_override

val read_message : messages:Message.Id.t list -> unit t with_param_override
val unread_message : messages:Message.Id.t list -> unit t with_param_override

val inbox
  : (?include_categories:bool
     -> ?mid:string
     -> mark_read:bool
     -> [ `Comment of Comment.t | `Message of Message.t ] Listing.t t with_param_override)
    with_listing_params

val unread
  : (?include_categories:bool
     -> ?mid:string
     -> mark_read:bool
     -> [ `Comment of Comment.t | `Message of Message.t ] Listing.t t with_param_override)
    with_listing_params

val sent
  : (?include_categories:bool -> ?mid:string -> Message.t Listing.t t with_param_override)
    with_listing_params

val comment_replies
  : (?include_categories:bool
     -> ?mid:string
     -> mark_read:bool
     -> Comment.t Listing.t t with_param_override)
    with_listing_params

val subreddit_comments
  : (subreddit:Subreddit_name.t -> Comment.t Listing.t t with_param_override)
    with_listing_params

(** Moderation *)

val log
  : (?mod_filter:Mod_filter.t
     -> ?subreddit:Subreddit_name.t
     -> ?type_:string
     -> Mod_action.t Listing.t t with_param_override)
    with_listing_params

val reports
  : (?location:string
     -> ?only:Links_or_comments.t
     -> ?subreddit:Subreddit_name.t
     -> [ `Link of Link.t | `Comment of Comment.t ] Listing.t t with_param_override)
    with_listing_params

val spam
  : (?location:string
     -> ?only:Links_or_comments.t
     -> ?subreddit:Subreddit_name.t
     -> [ `Link of Link.t | `Comment of Comment.t ] Listing.t t with_param_override)
    with_listing_params

val modqueue
  : (?location:string
     -> ?only:Links_or_comments.t
     -> ?subreddit:Subreddit_name.t
     -> [ `Link of Link.t | `Comment of Comment.t ] Listing.t t with_param_override)
    with_listing_params

val unmoderated
  : (?location:string
     -> ?only:Links_or_comments.t
     -> ?subreddit:Subreddit_name.t
     -> [ `Link of Link.t | `Comment of Comment.t ] Listing.t t with_param_override)
    with_listing_params

val edited
  : (?location:string
     -> ?only:Links_or_comments.t
     -> ?subreddit:Subreddit_name.t
     -> [ `Link of Link.t | `Comment of Comment.t ] Listing.t t with_param_override)
    with_listing_params

val accept_moderator_invite
  :  subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp.Body.t) t with_param_override

val approve
  :  id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
  -> unit t with_param_override

val remove
  :  id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
  -> spam:bool
  -> unit t with_param_override

val distinguish
  :  ?sticky:bool
  -> id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
  -> how:How_to_distinguish.t
  -> [> `Link of Link.t | `Comment of Comment.t ] t with_param_override

val ignore_reports
  :  id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
  -> unit t with_param_override

val unignore_reports
  :  id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
  -> unit t with_param_override

val leavecontributor : subreddit:Subreddit.Id.t -> unit t with_param_override
val leavemoderator : subreddit:Subreddit.Id.t -> unit t with_param_override
val mute_message_author : message:Message.Id.t -> unit t with_param_override
val unmute_message_author : message:Message.Id.t -> unit t with_param_override
val stylesheet : subreddit:Subreddit_name.t -> Stylesheet.t t with_param_override

(** New modmail *)

val create_modmail_conversation
  :  subject:string
  -> body:string
  -> subreddit:Subreddit_name.t
  -> to_:Modmail_recipient.t
  -> hide_author:bool
  -> Modmail.Conversation.t t with_param_override

(** Search *)

val search
  : (?category:string
     -> ?include_facets:bool
     -> ?restrict_to_subreddit:Subreddit_name.t
     -> ?since:Historical_span.t
     -> ?sort:Search_sort.t
     -> ?types:Search_type.Set.t
     -> query:string
     -> (Thing.Link.t Listing.t option
        * [ `Subreddit of Thing.Subreddit.t | `User of Thing.User.t ] Listing.t option)
        t
        with_param_override)
    with_listing_params

(** Subreddits *)

val banned
  : (?include_categories:bool
     -> ?user:Username.t
     -> subreddit:Subreddit_name.t
     -> Relationship.Ban.t Listing.t t with_param_override)
    with_listing_params

val muted
  : (?include_categories:bool
     -> ?user:Username.t
     -> subreddit:Subreddit_name.t
     -> Relationship.Mute.t Listing.t t with_param_override)
    with_listing_params

val wiki_banned
  : (?include_categories:bool
     -> ?user:Username.t
     -> subreddit:Subreddit_name.t
     -> Relationship.Ban.t Listing.t t with_param_override)
    with_listing_params

val contributors
  : (?include_categories:bool
     -> ?user:Username.t
     -> subreddit:Subreddit_name.t
     -> Relationship.Contributor.t Listing.t t with_param_override)
    with_listing_params

val wiki_contributors
  : (?include_categories:bool
     -> ?user:Username.t
     -> subreddit:Subreddit_name.t
     -> Relationship.Contributor.t Listing.t t with_param_override)
    with_listing_params

val moderators
  : (?include_categories:bool
     -> ?user:Username.t
     -> subreddit:Subreddit_name.t
     -> Relationship.Moderator.t Listing.t t with_param_override)
    with_listing_params

val delete_subreddit_image
  :  subreddit:Subreddit_name.t
  -> image:Subreddit_image.t
  -> unit t with_param_override

val search_subreddits_by_name
  :  ?exact:bool
  -> ?include_over_18:bool
  -> ?include_unadvertisable:bool
  -> query:string
  -> Subreddit_name.t list t with_param_override

val create_or_edit_subreddit
  :  ?comment_score_hide_mins:int
  -> ?wiki_edit_age:int
  -> ?wiki_edit_karma:int
  -> all_original_content:bool
  -> allow_discovery:bool
  -> allow_images:bool
  -> allow_post_crossposts:bool
  -> allow_top:bool
  -> allow_videos:bool
  -> api_type:(string * string list) list
  -> collapse_deleted_comments:bool
  -> crowd_control_mode:bool
  -> description:string
  -> disable_contributor_requests:bool
  -> exclude_banned_modqueue:bool
  -> free_form_reports:bool
  -> g_recaptcha_response:string option
  -> header_title:string
  -> hide_ads:bool
  -> key_color:string
  -> lang:string
  -> link_type:Link_type.t
  -> name:string
  -> original_content_tag_enabled:bool
  -> over_18:bool
  -> public_description:string
  -> restrict_commenting:bool
  -> restrict_posting:bool
  -> show_media:bool
  -> show_media_preview:bool
  -> spam_comments:Spam_level.t
  -> spam_links:Spam_level.t
  -> spam_selfposts:Spam_level.t
  -> spoilers_enabled:bool
  -> subreddit:Subreddit_name.t
  -> submit_link_label:string
  -> submit_text:string
  -> submit_text_label:string
  -> suggested_comment_sort:Comment_sort.t
  -> title:string
  -> type_:Subreddit_type.t
  -> wiki_mode:Wiki_mode.t
  -> (Cohttp.Response.t * Cohttp.Body.t) t with_param_override

val submit_text : subreddit:Subreddit_name.t -> Submit_text.t t with_param_override

val subreddit_autocomplete
  :  ?limit:int
  -> ?include_categories:bool
  -> ?include_over_18:bool
  -> ?include_profiles:bool
  -> query:string
  -> Subreddit.t Listing.t t with_param_override

val set_subreddit_stylesheet
  :  ?reason:string
  -> subreddit:Subreddit_name.t
  -> stylesheet_contents:string
  -> unit t with_param_override

val subscribe
  :  ?skip_initial_defaults:bool
  -> action:Subscription_action.t
  -> subreddits:Subscription_list.t
  -> unit t with_param_override

val search_users
  : (?sort:Relevance_or_activity.t
     -> query:string
     -> User.t Listing.t t with_param_override)
    with_listing_params

val about_subreddit : subreddit:Subreddit_name.t -> Subreddit.t t with_param_override

val subreddit_settings
  :  ?created:bool
  -> ?location:string
  -> subreddit:Subreddit_name.t
  -> Subreddit_settings.t t with_param_override

val subreddit_rules
  :  subreddit:Subreddit_name.t
  -> Subreddit_rules.t t with_param_override

val subreddit_traffic
  :  subreddit:Subreddit_name.t
  -> Subreddit_traffic.t t with_param_override

val get_sticky
  :  ?number:int
  -> subreddit:Subreddit_name.t
  -> Link.Id.t t with_param_override

val get_subreddits
  : (?include_categories:bool
     -> relationship:Subreddit_relationship.t
     -> Subreddit.t Listing.t t with_param_override)
    with_listing_params

val search_subreddits_by_title_and_description
  : (?show_users:bool
     -> ?sort:Relevance_or_activity.t
     -> query:string
     -> Subreddit.t Listing.t t with_param_override)
    with_listing_params

val list_subreddits
  : (?include_categories:bool
     -> ?show_users:bool
     -> sort:Subreddit_listing_sort.t
     -> Subreddit.t Listing.t t with_param_override)
    with_listing_params

(** Users *)

val about_user : username:Username.t -> User.t t with_param_override

val list_user_subreddits
  : (?include_categories:bool
     -> sort:User_subreddit_sort.t
     -> Subreddit.t Listing.t t with_param_override)
    with_listing_params

val add_relationship
  :  relationship:Relationship_spec.t
  -> username:Username.t
  -> duration:Relationship_spec.Duration.t
  -> ?subreddit:Subreddit_name.t
  -> ?note:string
  -> ?ban_reason:string
  -> ?ban_message:string
  -> ?ban_context:string
  -> unit t with_param_override

val remove_relationship
  :  relationship:Relationship_spec.t
  -> username:Username.t
  -> ?subreddit:Subreddit_name.t
  -> unit t with_param_override

(** Wiki *)

val add_wiki_editor : page:Wiki_page.Id.t -> user:Username.t -> unit t with_param_override

val remove_wiki_editor
  :  page:Wiki_page.Id.t
  -> user:Username.t
  -> unit t with_param_override

val edit_wiki_page
  :  ?previous:Wiki_page.Revision.Id.t
  -> ?reason:string
  -> content:string
  -> page:Wiki_page.Id.t
  -> (unit, Wiki_page.Edit_conflict.t) Result.t t with_param_override

val toggle_wiki_revision_visibility
  :  page:Wiki_page.Id.t
  -> revision:Wiki_page.Revision.Id.t
  -> [ `Became_hidden | `Became_visible ] t with_param_override

val revert_wiki_page
  :  page:Wiki_page.Id.t
  -> revision:Wiki_page.Revision.Id.t
  -> unit t with_param_override

val wiki_discussions
  : (page:Wiki_page.Id.t -> Link.t Listing.t t with_param_override) with_listing_params

val wiki_pages : ?subreddit:Subreddit_name.t -> string list t with_param_override

val subreddit_wiki_revisions
  : (?subreddit:Subreddit_name.t -> Wiki_page.Revision.t Listing.t t with_param_override)
    with_listing_params

val wiki_page_revisions
  : (page:Wiki_page.Id.t -> Wiki_page.Revision.t Listing.t t with_param_override)
    with_listing_params

val wiki_permissions
  :  page:Wiki_page.Id.t
  -> Wiki_page.Permissions.t t with_param_override

val set_wiki_permissions
  :  page:Wiki_page.Id.t
  -> listed:bool
  -> level:Wiki_page.Permissions.Level.t
  -> Wiki_page.Permissions.t t with_param_override

val wiki_page
  :  ?compare_revisions:string option * string option
  -> page:Wiki_page.Id.t
  -> Wiki_page.t t with_param_override

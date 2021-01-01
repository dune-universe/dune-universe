open! Core_kernel

type t =
  | Comment
  | User
  | Link
  | Message
  | Subreddit
  | Award
  | More_comments
  | Modmail_conversation
[@@deriving sexp, equal]

let of_string s =
  match s with
  | "t1" -> Comment
  | "t2" -> User
  | "t3" -> Link
  | "t4" -> Message
  | "t5" -> Subreddit
  | "t6" -> Award
  | "more" -> More_comments
  | "modmail" -> Modmail_conversation
  | _ -> raise_s [%message "Unknown thing kind" s]
;;

let to_string t =
  match t with
  | Comment -> "t1"
  | User -> "t2"
  | Link -> "t3"
  | Message -> "t4"
  | Subreddit -> "t5"
  | Award -> "t6"
  | More_comments -> "more"
  | Modmail_conversation -> "modmail"
;;

let to_string_long t =
  match t with
  | Comment -> "Comment"
  | User -> "User"
  | Link -> "Link"
  | Message -> "Message"
  | Subreddit -> "Subreddit"
  | Award -> "Award"
  | More_comments -> "more"
  | Modmail_conversation -> "modmail"
;;

let of_polymorphic_tag = function
  | `Comment _ -> Comment
  | `User _ -> User
  | `Link _ -> Link
  | `Message _ -> Message
  | `Subreddit _ -> Subreddit
  | `Award _ -> Award
  | `More_comments _ -> More_comments
  | `Modmail_conversation _ -> Modmail_conversation
;;

let of_polymorphic_tag_with_uniform_data = function
  | `Comment data -> Comment, data
  | `User data -> User, data
  | `Link data -> Link, data
  | `Message data -> Message, data
  | `Subreddit data -> Subreddit, data
  | `Award data -> Award, data
  | `More_comments data -> More_comments, data
  | `Modmail_conversation data -> Modmail_conversation, data
;;

let to_polymorphic_tag
    t
    ~data
    ~award
    ~comment
    ~link
    ~message
    ~modmail_conversation
    ~more_comments
    ~subreddit
    ~user
  =
  match t with
  | Comment -> `Comment (comment data)
  | User -> `User (user data)
  | Link -> `Link (link data)
  | Message -> `Message (message data)
  | Subreddit -> `Subreddit (subreddit data)
  | Award -> `Award (award data)
  | More_comments -> `More_comments (more_comments data)
  | Modmail_conversation -> `Modmail_conversation (modmail_conversation data)
;;

let to_polymorphic_tag_uniform t ~data =
  to_polymorphic_tag
    t
    ~data
    ~award:ident
    ~comment:ident
    ~link:ident
    ~message:ident
    ~modmail_conversation:ident
    ~more_comments:ident
    ~subreddit:ident
    ~user:ident
;;

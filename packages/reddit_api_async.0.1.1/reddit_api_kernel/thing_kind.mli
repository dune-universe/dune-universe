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

include Stringable.S with type t := t

val to_string_long : t -> string

val of_polymorphic_tag
  :  [< `Award of _
     | `Comment of _
     | `Link of _
     | `Message of _
     | `Modmail_conversation of _
     | `More_comments of _
     | `Subreddit of _
     | `User of _
     ]
  -> t

val of_polymorphic_tag_with_uniform_data
  :  [< `Award of 'data
     | `Comment of 'data
     | `Link of 'data
     | `Message of 'data
     | `Modmail_conversation of 'data
     | `More_comments of 'data
     | `Subreddit of 'data
     | `User of 'data
     ]
  -> t * 'data

val to_polymorphic_tag
  :  t
  -> data:'data
  -> award:('data -> 'award)
  -> comment:('data -> 'comment)
  -> link:('data -> 'link)
  -> message:('data -> 'message)
  -> modmail_conversation:('data -> 'modmail_conversation)
  -> more_comments:('data -> 'more_comments)
  -> subreddit:('data -> 'subreddit)
  -> user:('data -> 'user)
  -> [> `Award of 'award
     | `Comment of 'comment
     | `Link of 'link
     | `Message of 'message
     | `Modmail_conversation of 'modmail_conversation
     | `More_comments of 'more_comments
     | `Subreddit of 'subreddit
     | `User of 'user
     ]

val to_polymorphic_tag_uniform
  :  t
  -> data:'data
  -> [> `Award of 'data
     | `Comment of 'data
     | `Link of 'data
     | `Message of 'data
     | `Modmail_conversation of 'data
     | `More_comments of 'data
     | `Subreddit of 'data
     | `User of 'data
     ]

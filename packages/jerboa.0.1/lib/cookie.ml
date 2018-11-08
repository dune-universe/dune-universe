(** [Jerboa.Cookie] module only contain the type definition of a cokkie and a constructor.*)

(** [Cookie.t] is the type definition of a cookie.*)
type t = Cohttp.Cookie.Set_cookie_hdr.t
type cookie = Cohttp.Cookie.cookie
type expiration = Cohttp.Cookie.expiration

(** [Cookie.create ?expiration ?path ?domain ?secure ?http_only key value] creates a cookie with the supplied inputs.*)
let create ?expiration ?path ?domain ?secure ?http_only key value =
    let cookie = (key, value) in
    Cohttp.Cookie.Set_cookie_hdr.make ?expiration ?path ?domain ?secure ?http_only cookie

let serilaize = Cohttp.Cookie.Set_cookie_hdr.serialize
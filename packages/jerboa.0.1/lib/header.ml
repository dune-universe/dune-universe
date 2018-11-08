(** [Jerboa.Header] module consists of the type definition of header and also functions working on this type.*)

(** [Header.t] the type definition of header. *)
type t = Cohttp.Header.t

(** [Header.predefined_user_agent] is the user agent used by Jerboa by default.*)
let predefined_user_agent = Cohttp.Header.user_agent

(** [Header.create] creates an empty header. *)
let create = Cohttp.Header.init

(** [Header.create_with_list header_content] creates a header with the supplied header_content.*)
let create_with_list = Cohttp.Header.of_list

(** [Header.remove header key] removes the value associated by the key.*)
let remove = Cohttp.Header.remove

(** [Header.replace header key value] replaces the value asscotiated by the key with the supplied value argument.*)
let replace = Cohttp.Header.replace

(** [Header.add header key value] adds a new value associated by the key to the header.*)
let add = Cohttp.Header.add

(** [Header.add_values header key values] adds a list of values with the supplied key argument.*)
let add_values = Cohttp.Header.add_multi

(** [Header.add_list header header_content] adds header_content to the header.*)
let add_list = Cohttp.Header.add_list

(** [Header.get header key] gets the value associated by the key.*)
let get = Cohttp.Header.get

(** [Header.get_values header key] returns list of values associated by the key.*)
let get_values = Cohttp.Header.get_multi

(** [Header.has_key header key] returns true only if the header has the key.*)
let has_key = Cohttp.Header.mem

(** [Header.is_empty header] returns true if the header is empty.*)
let is_empty = Cohttp.Header.is_empty

(** [Header.is_form header] returns true if the header is made by a form.*)
let is_form = Cohttp.Header.is_form

(** [Header.get_cookies header] gets the cookies from the header.*)
let get_cookies = Cohttp.Cookie.Set_cookie_hdr.extract

(** [Header.set_cookie ?version header cookie] sets the cookie in the header with the optionaly supplied http version.*)
let set_cookie ?version header cookie = 
  let key, value = Cookie.serilaize ?version cookie in
  add header key value

(** [Header.to_string header] returns a string representation of the header.*)
let to_string = Cohttp.Header.to_string

(** [Header.to_list_string header] returns a list of string representation of the header. Lines do not end with /r/n character.*)
let to_list_string = Cohttp.Header.to_frames
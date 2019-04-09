open Printf

type t =
  [ `Content_length of int
  | `Content_type of string
  | `Location of Uri.t
  | `Set_cookie of string
  | `Status of Http_status.t
  | `Other of string * string ]

let to_string = function
  | `Content_length l ->
      sprintf "Content-Length: %d\r\n" l
  | `Content_type s ->
      sprintf "Content-Type: %s\r\n" s
  | `Location u ->
      sprintf "Location: %s\r\n" (Uri.to_string u)
  | `Set_cookie c ->
      sprintf "Set-Cookie: %s\r\n" c
  | `Status s ->
      sprintf "Status: %d %s\r\n" (Http_status.to_int s)
        (Http_status.to_string s)
  | `Other (n, v) ->
      sprintf "%s: %s\r\n" n v

(* Ought to define more *)

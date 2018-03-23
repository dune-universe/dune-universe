module Stable = struct
  open Core.Core_stable
  module Email_simple = Email_simple.Stable
  module Email_address = Email_address.Stable
  module V1 = struct
    type t =
      { header  : Email_simple.Content.V1.t
      ; from    : [`Keep | `Change_to of Email_address.V1.t]
      ; to_     : [`Keep | `Change_to of Email_address.V1.t list]
      ; cc      : [`Keep | `Change_to of Email_address.V1.t list]
      ; subject : [`Keep | `Prepend of string]
      } [@@deriving sexp, bin_io]
  end
end

open Core

type t = Stable.V1.t =
  { header  : Email_simple.Content.t
  ; from    : [`Keep | `Change_to of Email_address.t]
  ; to_     : [`Keep | `Change_to of Email_address.t list]
  ; cc      : [`Keep | `Change_to of Email_address.t list]
  ; subject : [`Keep | `Prepend of string]
  } [@@deriving sexp_of]

let create ?(from=`Keep) ?(to_=`Keep) ?(cc=`Keep) ?(subject=`Keep) header =
  { header; from; to_; cc; subject }

let create_from_email email =
  let get_header x = Headers.last ~whitespace:`Raw (Email.headers email) x in
  let from =
    match get_header "From" with
    | None | Some "" -> `Keep
    | Some from -> `Change_to (Email_address.of_string_exn from)
  in
  let to_ =
    match get_header "To" with
    | None | Some "" -> `Keep
    | Some to_ -> `Change_to (Email_address.list_of_string_exn to_)
  in
  let cc =
    match get_header "Cc" with
    | None | Some "" -> `Keep
    | Some cc -> `Change_to (Email_address.list_of_string_exn cc)
  in
  let subject =
    match get_header "Subject" with
    | None | Some "" -> `Keep
    | Some subject -> `Prepend subject
  in
  let email =
    Email.modify_headers email
      ~f:(Headers.filter ~f:(fun ~name ~value:_ ->
        let open String.Caseless.Replace_polymorphic_compare in
        name <> "From" && name <> "To" && name <> "Cc" && name <> "Subject"))
  in
  create ~from ~to_ ~cc ~subject (Email_simple.Content.of_email email)

let content_of_email email =
  Email_simple.Content.create
    ~content_type:"message/rfc822"
    (Email.to_string email)

(* We must be very careful with the email headers that we use in the new email. We use the
   following policies:

   (1) Add "From", "To", "Cc", "Subject" according to the supplied arguments to [create]
   (2) Copy over all other headers except:
   (i) DKIM-Signature - We break the signing by altering the email content
   (ii) Return-Path - We don't want the altered email to ever bounce back to the
   original sender
   (iii) Content-Transfer-Encoding, Content-Type, Content-Disposition - We structure
   the email differently. These wouldn't make sense anymore
*)
let add { header; from; to_; cc; subject } email =
  let content = Email_simple.Content.mixed [header; content_of_email email] in
  let headers = Email.headers email in
  let get_headers x = Headers.find_all ~whitespace:`Raw headers x in
  let get_header x = Headers.last ~whitespace:`Raw headers x in
  let from =
    match from with
    | `Keep -> get_header "From" |> Option.value ~default:""
    | `Change_to addr -> Email_address.to_string addr
  in
  let to_ =
    match to_ with
    | `Keep -> get_headers "To"
    | `Change_to addrs -> List.map addrs ~f:Email_address.to_string
  in
  let cc =
    match cc with
    | `Keep -> get_headers "Cc"
    | `Change_to addrs -> List.map addrs ~f:Email_address.to_string
  in
  let subject =
    let subj = get_header "Subject" |> Option.value ~default:"" in
    match subject with
    | `Keep -> subj
    | `Prepend str -> sprintf "%s %s" str subj
  in
  let extra_headers =
    Headers.filter headers ~f:(fun ~name ~value:_ ->
      match name with
      | "From" | "To" | "Cc" | "Subject"
      | "Message-Id" | "Date"
      | "DKIM-Signature" | "Return-Path"
      | "Content-Transfer-Encoding" | "Content-Type" | "Content-Disposition" -> false
      | _ -> true)
    |> Headers.to_list
  in
  let id = get_header "Message-Id" in
  let date = get_header "Date" in
  Email_simple.Expert.create_raw ?id ?date ~from ~to_ ~cc ~subject ~extra_headers content

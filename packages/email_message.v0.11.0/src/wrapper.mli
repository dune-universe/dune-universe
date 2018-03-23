open! Core
(* This module exposes a function to add a custom wrapper around an existing email. Gmail
   will result in the following formatting:

   CUSTOM WRAPPER TEXT

   ---------- Forwarded message ----------
   From: (1)
   To: (2)
   Cc: (3)
   Date: (4)
   Subject: (5)
   (6)

   where (1)-(6) are taken from the original email. *)
type t [@@deriving sexp_of]

(* Create a [Wrapper.t] that can be used to wrap emails *)
val create
  :  ?from : [`Keep | `Change_to of Email_address.t]  (* default: `Keep *)
  -> ?to_ : [`Keep | `Change_to of Email_address.t list] (* default: `Keep *)
  -> ?cc : [`Keep | `Change_to of Email_address.t list] (* default: `Keep *)
  -> ?subject : [`Keep | `Prepend of string] (* default: `Keep *)
  -> Email_simple.Content.t
  -> t

(* Like [create], but extract [?from], [?to_], [?cc] and [?subject] from
   the headers "From:", "To:", "Cc:" and "Subject:" respectively. *)
val create_from_email
  :  Email.t
  -> t

(* Transform an email by wrapping it according to the [Wrapper.t] *)
val add :  t -> Email.t -> Email.t

module Stable : sig
  module V1 : sig type nonrec t = t [@@deriving sexp, bin_io] end
end

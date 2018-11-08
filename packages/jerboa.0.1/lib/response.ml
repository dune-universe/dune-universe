(** [Jerboa.request] module contains the type definition of the Request record and a constructor for the Response.*)

(** [Response.t] is a record, which will be used as the base response type. *)
type t = {
  status_code: Cohttp.Code.status_code;
  body: string;
  headers: Header.t option
}

(** [Response.create status_code body] creates a reponse based on the provided arguments:
  - status_code: http code to be used for the response
  - body: http body of the response
*)
let create ?headers status_code body = {
  status_code = Cohttp.Code.status_of_code status_code;
  body;
  headers;
}
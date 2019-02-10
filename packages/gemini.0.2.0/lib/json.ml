(** Json utility functions. *)


(** Result specification when deserializing raw
    json into something strongly in typed ocaml.
  *)
module Result = struct
  include Result
  (** Unify results into tuple [('a , 'b)] if both are ok,
      otherwise produce one of the errors .
  *)
  let both x y : (('a * 'b), 'error) t =
    match x with
    | Result.Ok x ->
      (match y with
       | Result.Ok y -> Result.Ok (x,y)
       | Result.Error _ as e -> e
      )
    | Result.Error _ as e -> e

end


module type S = sig
  type t [@@deriving yojson, enumerate]
  val dict : (string * t) list
  val of_string_opt : string -> t option
  val error_message : string -> string
  val of_string : string -> t
  val to_string : t -> string
  val is_csv_atom : bool
  val rev_csv_header' : string sexp_list -> 'a -> 'b -> string sexp_list
  val rev_csv_header_spec' :
    Csvfields.Csv.Spec.t sexp_list ->
    'a -> 'b -> Csvfields.Csv.Spec.t sexp_list
  val t_of_row' : 'a -> string sexp_list -> (unit -> t) * string sexp_list
  val write_row_of_t' :
    is_first:bool ->
    is_last:bool -> writer:(string -> unit) -> 'a -> 'b -> t -> unit
  val csv_header : string sexp_list
  val csv_header_spec : Csvfields.Csv.Spec.t sexp_list
  val t_of_row : string sexp_list -> t
  val row_of_t : t -> string sexp_list
  val csv_load : ?separator:char -> string -> t sexp_list
  val csv_load_in : ?separator:char -> In_channel.t -> t sexp_list
  val csv_save_fn :
    ?separator:char -> (string -> unit) -> t sexp_list -> unit
  val csv_save_out :
    ?separator:char -> Out_channel.t -> t sexp_list -> unit
  val csv_save : ?separator:char -> string -> t sexp_list -> unit
  end
(** An enumeration encodable as a json string. *)
module type ENUM_STRING = sig
  type t [@@deriving enumerate]
  val to_string : t -> string
end

(** Produce json encoders and decoders given an enumerated
    type and its string representations. *)
module Make(E:ENUM_STRING) : S with type t = E.t = struct

  module T = struct
  let dict = List.zip_exn
      (List.map E.all ~f:E.to_string)
      E.all

  let of_string_opt (s:string) : E.t option =
    List.Assoc.find dict s
      ~equal:
        (fun s s' ->
          String.equal
            (String.lowercase s)
            (String.lowercase s')
        )


  let error_message x =
    sprintf
      "String %S is not a valid enumeration value. \
       Expected one of %s"
      x (String.concat ~sep:", " (List.map ~f:fst dict))


  let of_string x = of_string_opt x |> Option.value_exn
                      ~message:(error_message x)

  let to_yojson t = `String (E.to_string t)

  let of_yojson (json : Yojson.Safe.json) =
    match json with
    | `String s ->
      (match of_string_opt s with
      | Some t -> Result.return t
      | None -> error_message s |> Result.fail
      )
   | #Yojson.Safe.json ->
        Result.failf "expected json string but got %S"
          (Yojson.Safe.to_string json)

  include E
  end
  include T
  include (Csvfields.Csv.Atom(T) :
             Csvfields.Csv.Csvable with type t := t
          )
end

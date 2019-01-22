(** Json utility functions. *)

module Result = struct
  include Result
  let both x y : (('a * 'b), 'error) t =
    match x with 
    | Result.Ok x ->
      (match y with
       | Result.Ok y -> Result.Ok (x,y)
       | Result.Error _ as e -> e
      )
    | Result.Error _ as e -> e

end

(** An enumeration encodable as a json string *)
module type ENUM_STRING = sig
  type t [@@deriving enumerate]
  val to_string : t -> string
end

(** Produce json encoders and decoders given an enumerated
    type and its string representations. *)
module Make(E:ENUM_STRING) = struct

  let dict = List.zip_exn
      (List.map E.all ~f:E.to_string)
      E.all

  let of_string (s:string) : E.t option =
    List.Assoc.find dict s
      ~equal:
        (fun s s' ->
          String.equal
            (String.lowercase s)
            (String.lowercase s')
        )

  let to_yojson t = `String (E.to_string t)

  let of_yojson (json : Yojson.Safe.json) =
    match json with
    | `String s ->
      (match of_string s with
      | Some t -> Result.Ok t
      | None ->
        Result.failf
          "String %S is not a valid enumeration value. \
           Expected one of %s"
          s (String.concat ~sep:", " (List.map ~f:fst dict))
      )
   | #Yojson.Safe.json ->
        Result.failf "expected json string but got %S"
          (Yojson.Safe.to_string json)
end

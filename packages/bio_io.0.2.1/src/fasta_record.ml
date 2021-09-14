open! Base

type t = { id : string; desc : string option; seq : string } [@@deriving sexp]

exception Exn of string [@@deriving sexp]

let create ~id ~desc ~seq = { id; desc; seq }

let of_header_exn s =
  match String.is_prefix s ~prefix:">" with
  | false ->
      let msg =
        Printf.sprintf "Header line should start with '>'.  Got: '%s'."
          (String.prefix s 0)
      in
      raise (Exn msg)
  | true -> (
      match
        s |> String.strip
        |> String.chop_prefix_exn ~prefix:">"
        |> String.split ~on:' '
      with
      (* Empty header lines get id = "" *)
      | [ id ] -> { id; desc = None; seq = "" }
      | id :: desc ->
          { id; desc = Some (String.concat ~sep:" " desc); seq = "" }
      | [] ->
          (* String.split should at least give [""]. Should never get here. *)
          assert false)

let of_header s =
  match of_header_exn s with
  | exception exn -> Or_error.error "Caught exception" exn Exn.sexp_of_t
  | result -> Or_error.return result

let to_string r =
  match r.desc with
  | None -> Printf.sprintf ">%s\n%s" r.id r.seq
  | Some desc -> Printf.sprintf ">%s %s\n%s" r.id desc r.seq

let to_string_nl ?(nl = "\n") r = to_string r ^ nl

let serialize r = Sexp.to_string_hum (sexp_of_t r)

let equal r1 r2 =
  let opt_equal = Option.equal String.equal in
  String.(r1.id = r2.id)
  && String.(r1.seq = r2.seq)
  && opt_equal r1.desc r2.desc

let ( = ) = equal

let id r = r.id
let desc r = r.desc
let seq r = r.seq
let seq_length r = String.length r.seq

let with_id id r = { r with id }
let with_seq seq r = { r with seq }
let with_desc desc r = { r with desc }

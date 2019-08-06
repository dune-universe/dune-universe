let to_unstrctrd = function
  | `Token v -> v
  | `String v -> Astring.strf "\"%s\"" v

let content_disposition ?(ty= `Attachment) ?zone ?size ?c_time ?a_time ?m_time filename =
  let open Mrmime in
  let zone = match zone with
    | None ->
      let ss = Zone.get () in
      let hh, mm = ss / 3600, abs (ss mod 3600 / 60) in
      Date.Zone.tz hh mm |> Rresult.R.get_ok
    | Some zone -> zone in
  let filename = Content_type.Parameters.v filename in
  let Content_field.Field_name content_disposition =
    Content_field.of_field_name (Field_name.v "Content-Disposition") in
  let value = match ty with
    | `Inline -> Unstructured.[ v "inline"; v ";"; sp 1 ]
    | `Attachment -> Unstructured.[ v "attachment"; v ";"; sp 1 ] in
  let value = value @ Unstructured.[ v "filename"; v "="; v (to_unstrctrd filename) ] in
  let value = match size with
    | None -> value
    | Some size ->
      let size = to_unstrctrd (Content_type.Parameters.v (string_of_int size)) in
      Unstructured.[ v "size"; v "="; v size; sp 1 ] @ value in
  let value = match c_time with
    | None -> value
    | Some c_time ->
      let date = Date.of_ptime ~zone c_time |> Date.to_string in
      let date = Content_type.Parameters.v date in
      let date = to_unstrctrd date in
      Unstructured.[ v "creation-date"; v "="; v date; sp 1 ] @ value in
  let value = match a_time with
    | None -> value
    | Some a_time ->
      let date = Date.of_ptime ~zone a_time |> Date.to_string in
      let date = Content_type.Parameters.v date in
      let date = to_unstrctrd date in
      Unstructured.[ v "read-date"; v "="; v date ; sp 1 ] @ value in
  let value = match m_time with
    | None -> value
    | Some m_time ->
      let date = Date.of_ptime ~zone m_time |> Date.to_string in
      let date = Content_type.Parameters.v date in
      let date = to_unstrctrd date in
      Unstructured.[ v "modification-date"; v "="; v date; sp 1 ] @ value in
  match Content_field.field_value content_disposition with
  | Content_field.Unstructured ->
    Content_field.make content_disposition (value :> Unstructured.t)
  | _ -> assert false
    (* XXX(dinosaure): should never occur! *)

open! Core

let utc_offset_string time ~zone =
  let utc_offset = Time.utc_offset time ~zone in
  let is_utc = Time.Span.( = ) utc_offset Time.Span.zero in
  if is_utc
  then "Z"
  else
    String.concat
      [ (if Time.Span.( < ) utc_offset Time.Span.zero then "-" else "+")
      ; Time.Ofday.to_string_trimmed
          (Time.Ofday.of_span_since_start_of_day_exn (Time.Span.abs utc_offset))
      ]
;;

let rfc822_date now =
  let zone = force Time.Zone.local in
  let offset_string =
    utc_offset_string ~zone now |> String.filter ~f:(fun c -> Char.( <> ) c ':')
  in
  let now_string = Time.format now "%a, %d %b %Y %H:%M:%S" ~zone in
  sprintf "%s %s" now_string offset_string
;;

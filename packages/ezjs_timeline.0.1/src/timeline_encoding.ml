open Json_encoding
open Timeline_types

let date =
  conv
    (fun {year; month; day; hour; minute; second; millisecond; display_date} ->
      (year, month, day, hour, minute, second, millisecond, display_date))
    (fun (year, month, day, hour, minute, second, millisecond, display_date) ->
      {year; month; day; hour; minute; second; millisecond; display_date})
  @@ obj8 (req "year" int) (opt "month" int) (opt "day" int) (opt "hour" int)
       (opt "minute" int) (opt "second" int) (opt "millisecond" int)
       (opt "display_date" string)

let text =
  conv
    (fun {headline; text} -> (headline, text))
    (fun (headline, text) -> {headline; text})
  @@ obj2 (opt "headline" string) (opt "text" string)

let media =
  conv
    (fun {url; caption; credit; thumbnail; alt; media_title; link; link_target} ->
      (url, caption, credit, thumbnail, alt, media_title, link, link_target))
    (fun (url, caption, credit, thumbnail, alt, media_title, link, link_target) ->
      {url; caption; credit; thumbnail; alt; media_title; link; link_target})
  @@ obj8 (req "url" string) (opt "caption" string) (opt "credit" string)
       (opt "thumbnail" string) (opt "alt" string) (opt "title" string)
       (opt "link" string) (opt "link_target" string)

let slide =
  conv
    (fun { start_date
         ; end_date
         ; slide_text
         ; media
         ; group
         ; slide_display_date
         ; background
         ; autolink
         ; unique_id } ->
      ( start_date
      , end_date
      , slide_text
      , media
      , group
      , slide_display_date
      , background
      , autolink
      , unique_id ))
    (fun ( start_date
         , end_date
         , slide_text
         , media
         , group
         , slide_display_date
         , background
         , autolink
         , unique_id ) ->
      { start_date
      ; end_date
      ; slide_text
      ; media
      ; group
      ; slide_display_date
      ; background
      ; autolink
      ; unique_id })
  @@ obj9 (req "start_date" date) (opt "end_date" date) (opt "text" text)
       (opt "media" media) (opt "group" string)
       (opt "display_date" string)
       (opt "background" any_value)
       (opt "autolink" bool) (opt "unique_id" string)

let era =
  conv
    (fun {era_start; era_end; era_text} -> (era_start, era_end, era_text))
    (fun (era_start, era_end, era_text) -> {era_start; era_end; era_text})
  @@ obj3 (req "start_date" date) (req "end_date" date) (opt "text" text)

let timeline =
  conv
    (fun {events; title; eras; scale} -> (events, title, eras, scale))
    (fun (events, title, eras, scale) -> {events; title; eras; scale})
  @@ obj4
       (req "events" (list slide))
       (opt "title" slide)
       (opt "eras" (list era))
       (opt "scale" string)

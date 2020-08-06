open Timeline_types

let date ?month ?day ?hour ?minute ?second ?millisecond ?display_date year =
  {year; month; day; hour; minute; second; millisecond; display_date}

let text ?headline text = {headline; text= Some text}

let media ?caption ?credit ?thumbnail ?alt ?title ?link ?target url =
  { url
  ; caption
  ; credit
  ; thumbnail
  ; alt
  ; media_title= title
  ; link
  ; link_target= target }

let slide ?end_date ?text ?media ?group ?display_date ?background ?autolink ?id
    start =
  { start_date= start
  ; end_date
  ; slide_text= text
  ; media
  ; group
  ; slide_display_date= display_date
  ; background
  ; autolink
  ; unique_id= id }

let era ?text start end_date =
  {era_start= start; era_end= end_date; era_text= text}

let timeline ?title ?eras ?scale events = {events; title; eras; scale}

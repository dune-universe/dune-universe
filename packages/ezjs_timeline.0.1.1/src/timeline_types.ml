type date =
  { year: int
  ; month: int option
  ; day: int option
  ; hour: int option
  ; minute: int option
  ; second: int option
  ; millisecond: int option
  ; display_date: string option }

type text = {headline: string option; text: string option}

type media =
  { url: string
  ; caption: string option
  ; credit: string option
  ; thumbnail: string option
  ; alt: string option
  ; media_title: string option
  ; link: string option
  ; link_target: string option }

type slide =
  { start_date: date
  ; end_date: date option
  ; slide_text: text option
  ; media: media option
  ; group: string option
  ; slide_display_date: string option
  ; background: Json_repr.any option
  ; autolink: bool option
  ; unique_id: string option }

type era = {era_start: date; era_end: date; era_text: text option}

type timeline =
  { events: slide list
  ; title: slide option
  ; eras: era list option
  ; scale: string option }

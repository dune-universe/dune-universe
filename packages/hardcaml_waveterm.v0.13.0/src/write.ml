open! Import

type styler =
  { start : (string -> unit) -> unit
  ; set : (string -> unit) -> Draw.Style.t -> unit
  ; eol : (string -> unit) -> unit
  ; finish : (string -> unit) -> unit
  }

let no_styler =
  { start = (fun _ -> ())
  ; set = (fun _ _ -> ())
  ; eol = (fun _ -> ())
  ; finish = (fun _ -> ())
  }
;;

open Draw.Style

let str_of_colour = function
  | Black -> "black"
  | Red -> "red"
  | Green -> "green"
  | Yellow -> "yellow"
  | Blue -> "blue"
  | Magenta -> "magenta"
  | Cyan -> "cyan"
  | White -> "white"
;;

let int_of_colour = function
  | Black -> 0
  | Red -> 1
  | Green -> 2
  | Yellow -> 3
  | Blue -> 4
  | Magenta -> 5
  | Cyan -> 6
  | White -> 7
;;

let html_styler =
  let prev = ref default in
  let set_style style os =
    os
      (Printf.sprintf
         "<span style=\"background-color:%s; color:%s; font-weight:%s\">"
         (str_of_colour style.bg)
         (str_of_colour style.fg)
         (if style.bold then "bold" else "normal"))
  in
  let close_style os = os "</span>" in
  { start =
      (fun os ->
         prev := default;
         set_style default os)
  ; set =
      (fun os style ->
         if Draw.Style.compare style !prev <> 0
         then (
           prev := style;
           close_style os;
           set_style style os))
  ; eol = (fun _ -> ())
  ; finish = close_style
  }
;;

let css_class_styler =
  let prev = ref default in
  let set_style style os =
    os
      (Printf.sprintf
         "<span class=\"w%i%i%s\">"
         (int_of_colour style.bg)
         (int_of_colour style.fg)
         (if style.bold then "b" else ""))
  in
  let close_style os = os "</span>" in
  { start =
      (fun os ->
         prev := default;
         set_style default os)
  ; set =
      (fun os style ->
         if Draw.Style.compare style !prev <> 0
         then (
           prev := style;
           close_style os;
           set_style style os))
  ; eol = (fun _ -> ())
  ; finish = close_style
  }
;;

let css_classes =
  let css fg bg b =
    Printf.sprintf
      ".w%i%i%s { background-color:%s; color:%s; font-weight:%s; }"
      (int_of_colour bg)
      (int_of_colour fg)
      (if b then "b" else "")
      (str_of_colour bg)
      (str_of_colour fg)
      (if b then "bold" else "normal")
  in
  let colours = [ Black; Red; Green; Yellow; Blue; Magenta; Cyan; White ] in
  let mapcat f = String.concat ~sep:"\n" (List.map colours ~f) in
  mapcat (fun fg -> mapcat (fun bg -> css fg bg false ^ "\n" ^ css fg bg true))
;;

let term_styler =
  let prev = ref None in
  let set_style style os =
    os
      (Printf.sprintf
         "\027[%i;%i%sm"
         (int_of_colour style.bg + 40)
         (int_of_colour style.fg + 30)
         (if style.bold then ";1" else ""))
  in
  let close_style os = os "\027[0m" in
  { start = (fun _ -> prev := None)
  ; set =
      (fun os style ->
         let set_style () =
           prev := Some style;
           set_style style os
         in
         match !prev with
         | Some prev' when Draw.Style.compare style prev' <> 0 -> set_style ()
         | None -> set_style ()
         | _ -> ())
  ; eol =
      (fun os ->
         prev := None;
         close_style os)
  ; finish = close_style
  }
;;

let html_escape ?(styler = no_styler) os ctx =
  let open Draw.In_memory in
  let bounds = get_bounds ctx in
  styler.start os;
  for r = 0 to bounds.h - 1 do
    for c = 0 to bounds.w - 1 do
      (* TODO styling *)
      let code = fst ctx.(r).(c) in
      styler.set os (snd ctx.(r).(c));
      os ("&#" ^ Int.to_string code)
    done;
    styler.eol os;
    os "\n"
  done;
  styler.finish os
;;

let utf8 ?(styler = no_styler) os ctx =
  let open Draw.In_memory in
  let put c =
    if c <= 0x7f
    then os (String.init 1 ~f:(fun _ -> Char.of_int_exn c))
    else if c <= 0x7FF
    then
      os
        (String.init 2 ~f:(function
           | 0 -> Char.of_int_exn ((c lsr 6) lor 0b11000000)
           | _ -> Char.of_int_exn (c land 0b00111111 lor 0b10000000)))
    else if c <= 0xFFFF
    then
      os
        (String.init 3 ~f:(function
           | 0 -> Char.of_int_exn ((c lsr 12) lor 0b11100000)
           | 1 -> Char.of_int_exn ((c lsr 6) land 0b00111111 lor 0b10000000)
           | _ -> Char.of_int_exn (c land 0b00111111 lor 0b10000000)))
    else failwith "extend utf-8 writer!"
  in
  let bounds = get_bounds ctx in
  styler.start os;
  for r = 0 to bounds.h - 1 do
    for c = 0 to bounds.w - 1 do
      styler.set os (snd ctx.(r).(c));
      put (fst ctx.(r).(c))
    done;
    styler.eol os;
    os "\n"
  done;
  styler.finish os
;;

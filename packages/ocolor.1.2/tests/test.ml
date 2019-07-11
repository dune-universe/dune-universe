open Ocolor_types
open Ocolor_config

let list_all_colors () : unit =
  set_color_capability Color24;
  List.iter
    (fun (name, (r24, g24, b24)) ->
       Ocolor_format.printf "%a%s%s 0x%02X%02X%02X   %03d, %03d, %03d%a\n"
         Ocolor_format.pp_open_style (Fg(C24{r24; g24; b24}))
         name (String.make (max 0 (30 - String.length name)) ' ')
         r24 g24 b24
         r24 g24 b24
         Ocolor_format.pp_close_style ()
    )
    Ocolor_x11.available_colors

let spectrum () : unit =
  let p i j k = Ocolor_format.printf "%a0%a" Ocolor_format.pp_open_style (Fg(C24{r24 = i; g24 = j; b24 = k})) Ocolor_format.pp_close_style () in
  for i = 0 to 255 do
    p 255 i 0
  done;
  for i = 1 to 255 do
    p (255-i) 255 0
  done;
  for i = 1 to 255 do
    p 0 255 i
  done;
  for i = 1 to 255 do
    p 0 (255-i) 255
  done;
  for i = 1 to 255 do
    p i 0 255
  done;
  for i = 1 to 255 do
    p 255 0 (255-i)
  done

let rectangle () : unit =
  let styles i j =
    [Fg(C24 {r24=i*255/50; g24 = j; b24=255-(i*255/50)})]
  in
  for i = 0 to 50 do
    for j = 0 to 200 do
      Ocolor_format.printf "%a0%a" Ocolor_format.pp_open_styles (styles i j) Ocolor_format.pp_close_styles ()
    done;
    Ocolor_format.printf "\n"
  done;
  Ocolor_format.printf "\n"

let test () : unit =
  set_color_capability Color24;
  rectangle ();
  set_color_capability Color8;
  rectangle ();
  set_color_capability Color4;
  rectangle ();
  Ocolor_format.printf "\n";
  set_color_capability Color24;
  spectrum ();
  Ocolor_format.printf "\n\n";
  set_color_capability Color8;
  spectrum ();
  Ocolor_format.printf "\n\n";
  set_color_capability Color4;
  spectrum ();
  Ocolor_format.printf "\n\n";
  let l = [
    ("font1", Font 1); ("underlined", Underlined); ("bold", Bold);
    ("faint", Faint); ("blink", Blink); ("conceal", Conceal);
    ("fraktur", Fraktur); ("reverse", Reverse_video); ("crossed", Crossed_out);
    ("framed", Framed); ("italic", Italic); ("encircled", Encircled);
    ("overlined", Overlined); ("doubly underlined", DoubleUnderlined);
  ]
  in
  let max_length = List.fold_left (fun acc (n, _) -> max (String.length n) acc) 0 l in
  List.iter
    (fun (name, style) ->
       Ocolor_format.printf "%s%s: %aFoo%a\n\n"
         name
         (String.make (max_length - (String.length name)) ' ')
         Ocolor_format.pp_open_style style
         Ocolor_format.pp_close_style ()
    )
    l;
  Ocolor_format.printf "\n\n";
  Ocolor_format.printf "v-style1  style2-> ";
  List.iter
    (fun (name, _) ->
       Ocolor_format.printf "| %s " name
    )
    l;
  Ocolor_format.printf "\n";
  Ocolor_format.printf "%s  " (String.make (max_length) ' ');
  List.iter
    (fun (name, _) ->
       Ocolor_format.printf "| %s " (String.make (String.length name) ' ')
    )
    l;
  Ocolor_format.printf "\n";
  List.iter
    (fun (name1, style1) ->
       Ocolor_format.printf "%s%s  "
         name1
         (String.make (max_length - (String.length name1)) ' ');
       List.iter
         (fun (name2, style2) ->
            let len = String.length name2 - 3 in
            Ocolor_format.printf "| %s%aFoo%a%s "
              (String.make (len / 2) ' ')
              Ocolor_format.pp_open_styles [style1; style2]
              Ocolor_format.pp_close_style ()
              (String.make (len - len / 2) ' ')
         )
         l;
       Ocolor_format.printf "\n";
       Ocolor_format.printf "%s  "
         (String.make max_length ' ');
       List.iter
         (fun (name2, _) ->
            let len = String.length name2 - 3 in
            Ocolor_format.printf "| %s "
              (String.make (len + 3) ' ')
         )
         l;
       Ocolor_format.printf "\n"
    )
    l;
  Ocolor_format.printf "\n";

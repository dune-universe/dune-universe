open Ocolor_types

let color4_map_of_list : (color4 * rgb) list -> rgb Color4Map.t =
  List.fold_left
    (fun map (c, rgb) -> Color4Map.add c rgb map)
    Color4Map.empty

(* From https://en.wikipedia.org/wiki/ANSI_escape_code#3/4_bit *)

let vga : rgb Color4Map.t =
  color4_map_of_list
    [
      black,      (0  , 0  , 0);
      red,        (170, 0  , 0);
      green,      (0  , 170, 0);
      yellow,     (170, 85 , 0);
      blue,       (0  , 0  , 170);
      magenta,    (170, 0  , 170);
      cyan,       (0  , 170, 170);
      white,      (170, 170, 170);
      hi_black,   (85 , 85 , 85);
      hi_red,     (255, 85 , 85);
      hi_green,   (85 , 255, 85);
      hi_yellow,  (255, 255, 85);
      hi_blue,    (85 , 85 , 255);
      hi_magenta, (255, 85 , 255);
      hi_cyan,    (85 , 255, 255);
      hi_white,   (255, 255, 255);
    ]

let cmd : rgb Color4Map.t =
  color4_map_of_list
    [
      black,      (0  , 0  , 0);
      red,        (128, 0  , 0);
      green,      (0  , 128, 0);
      yellow,     (128, 128 , 0);
      blue,       (0  , 0  , 128);
      magenta,    (128, 0  , 128);
      cyan,       (0  , 128, 128);
      white,      (192, 192, 192);
      hi_black,   (128, 128, 128);
      hi_red,     (255, 0  , 0);
      hi_green,   (0  , 255, 0);
      hi_yellow,  (255, 255, 0);
      hi_blue,    (0  , 0  , 255);
      hi_magenta, (255, 0  , 255);
      hi_cyan,    (0  , 255, 255);
      hi_white,   (255, 255, 255);
    ]

let terminal_app : rgb Color4Map.t =
  color4_map_of_list
    [
      black,      (0  , 0  , 0);
      red,        (194, 54 , 33);
      green,      (0  , 128, 0);
      yellow,     (128, 128, 0);
      blue,       (0  , 0  , 128);
      magenta,    (128, 0  , 128);
      cyan,       (0  , 128, 128);
      white,      (192, 192, 192);
      hi_black,   (128, 128, 128);
      hi_red,     (255, 0  , 0);
      hi_green,   (0  , 255, 0);
      hi_yellow,  (255, 255, 0);
      hi_blue,    (0  , 0  , 255);
      hi_magenta, (255, 0  , 255);
      hi_cyan,    (0  , 255, 255);
      hi_white,   (255, 255, 255);
    ]

let putty : rgb Color4Map.t =
  color4_map_of_list
    [
      black,      (0  , 0  , 0);
      red,        (187, 0  , 0);
      green,      (0  , 187, 0);
      yellow,     (187, 187, 0);
      blue,       (0  , 0  , 187);
      magenta,    (187, 0  , 187);
      cyan,       (0  , 187, 187);
      white,      (187, 187, 187);
      hi_black,   (85 , 85 , 85);
      hi_red,     (255, 85 , 85);
      hi_green,   (85 , 255, 85);
      hi_yellow,  (255, 255, 85);
      hi_blue,    (85 , 85 , 255);
      hi_magenta, (255, 85 , 255);
      hi_cyan,    (85 , 255, 255);
      hi_white,   (255, 255, 255);
    ]

let mirc : rgb Color4Map.t =
  color4_map_of_list
    [
      black,      (0  , 0  , 0);
      red,        (127,0,0 	  );
      green,      (0,147,0   	);
      yellow,     (252,127,0 	);
      blue,       (0,0,127 	  );
      magenta,    (156,0,156 	);
      cyan,       (0,147,147 	);
      white,      (210,210,210);
      hi_black,   (127,127,127);
      hi_red,     (255,0,0 	  );
      hi_green,   (0,252,0 	  );
      hi_yellow,  (255,255,0 	);
      hi_blue,    (0,0,252 	  );
      hi_magenta, (255,0,255 	);
      hi_cyan,    (0,255,255 	);
      hi_white,   (255,255,255);
    ]

let xterm : rgb Color4Map.t =
  color4_map_of_list
    [
      black,      (0  , 0  , 0);
      red,        (205, 0  , 0);
      green,      (0  , 205, 0);
      yellow,     (205, 205, 0);
      blue,       (0  , 0  , 238);
      magenta,    (205, 0  , 205);
      cyan,       (0  , 205, 205);
      white,      (229, 229, 229);
      hi_black,   (127, 127, 127);
      hi_red,     (255, 0  , 0);
      hi_green,   (0  , 255, 0);
      hi_yellow,  (255, 255, 0);
      hi_blue,    (92 , 92 , 255);
      hi_magenta, (255, 0  , 255);
      hi_cyan,    (0  , 255, 255);
      hi_black,   (255, 255, 255);
    ]

let x : rgb Color4Map.t =
  color4_map_of_list
    [
      black,     (0  , 0  , 0);
      red,       (255, 0  , 0);
      green,     (0  , 255, 0);
      yellow,    (255, 255, 0);
      blue,      (0  , 0  , 255);
      magenta,   (255, 0  , 255);
      cyan,      (0  , 255, 255);
      white,     (255, 255, 255);
      hi_green,  (144, 238, 144);
      hi_yellow, (255, 255, 224);
      hi_blue,   (173, 216, 230);
      hi_cyan,   (224, 255, 255);
    ]

let ubuntu : rgb Color4Map.t =
  color4_map_of_list
    [
      black,      (1  , 1  , 1);
      red,        (222, 56 , 43);
      green,      (57 , 181, 74);
      yellow,     (255, 199, 6);
      blue,       (0  , 111, 184);
      magenta,    (118, 38 , 113);
      cyan,       (44 , 181, 233);
      white,      (204, 204, 204);
      hi_black,   (128, 12 , 128);
      hi_red,     (255, 0  , 0);
      hi_green,   (0  , 255, 0);
      hi_yellow,  (255, 255, 0);
      hi_blue,    (0  , 0  , 255);
      hi_magenta, (255, 0  , 255);
      hi_cyan,    (0  , 255, 255);
      hi_white,   (255, 255, 255);
    ]


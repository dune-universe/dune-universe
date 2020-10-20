external (&) : ('a -> 'b) -> 'a -> 'b = "%apply"
(** Haskell's [($)]. *)

let (!%) fmt = Printf.sprintf fmt
let (!!%) fmt = Format.eprintf fmt

include XConfigurator.Make(struct let name = "camlimages" end)

let lablgtk2 = find_ocaml_package "lablgtk2"

let graphics = find_ocaml_package "graphics"

let gs = find_program "gs"

let gif = find_library
    [ by_pkg_config "libgif" 
    ; by_cc ~c_flags:[] ~link_flags:["-lgif"] ~headers:["gif_lib.h"] ~functions:["DGifOpenFileName"] 
    ]

let jpeg = find_library
    [ by_pkg_config "libjpeg"
    ; by_cc ~c_flags:[] ~link_flags:["-ljpeg"] ~headers:["jpeglib.h"] ~functions:["jpeg_read_header"]
    ]

let png = find_library
    [ by_pkg_config "libpng" 
    ; by_cc ~c_flags:[] ~link_flags:["-lpng"; "-lz"] ~headers:["png.h"] ~functions:["png_create_read_struct"] 
    ]

let tiff = find_library
    [ by_pkg_config "libtiff-4" 
    ; by_cc ~c_flags:[] ~link_flags:["-ltiff"] ~headers:["tiff.h"] ~functions:["TiffOpen"]
    ]

let freetype = find_library
    [ by_pkg_config "freetype2" ]

let exif = find_library
    [ by_pkg_config "libexif" 
    ; by_cc ~c_flags:[] ~link_flags:["-lexif"] ~headers:["exif-data.h"] ~functions:["exif_data_load_data"]
    ]

let xpm = find_library
    [ by_pkg_config "xpm"
    ; by_cc ~c_flags:[] ~link_flags:["-lXpm"] ~headers:["X11/xpm.h"] ~functions:["XpmReadFileToXpmImage"]
    ]

let rgb_txt = find_file "rgb.txt"
    [ "/etc/X11"
    ; "/usr/share/X11"
    ; "/usr/X11/share/X11"
    ]

let xs =
    [ "LABLGTK2", lablgtk2
    ; "GRAPHICS", graphics
    ; "GHOSTSCRIPT", gs
    ; "GIF", gif
    ; "JPEG", jpeg
    ; "PNG", png
    ; "TIFF", tiff
    ; "FREETYPE", freetype
    ; "EXIF", exif
    ; "XPM", xpm
    ; "RGB_TXT", rgb_txt
    ]

let () = make_header ~fname:"config.h" xs

let () =
  write_package_conf_sexps "all_"      [lablgtk2; graphics; gs; gif; png; tiff; freetype; exif; xpm];
  write_package_conf_sexps "jpeg_"     [jpeg];
  write_package_conf_sexps "gif_"      [gif];
  write_package_conf_sexps "png_"      [png];
  write_package_conf_sexps "tiff_"     [tiff];
  write_package_conf_sexps "lablgtk2_" [lablgtk2];
  write_package_conf_sexps "freetype_" [freetype];
  write_package_conf_sexps "exif_"     [exif];
  write_package_conf_sexps "xpm_"      [xpm]

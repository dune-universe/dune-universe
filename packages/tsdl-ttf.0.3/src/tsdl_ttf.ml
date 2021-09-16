open Ctypes
open Foreign
open Tsdl

module Ttf = struct

type 'a result = 'a Sdl.result

let error () = Error (`Msg (Sdl.get_error ()))

let zero_to_ok =
  let read = function 0 -> Ok () | _err -> error () in
  view ~read ~write:(fun _ -> assert false) int

let bool =
  let read = function 0 -> false | _ -> true in
  let write = function true -> 1 | false -> 0 in
  view ~read ~write:write int

let int32_as_uint32_t =
  view ~read:Unsigned.UInt32.to_int32 ~write:Unsigned.UInt32.of_int32 uint32_t
let int64_as_long =
  view ~read:Signed.Long.to_int64 ~write:Signed.Long.of_int64 long

(* let surface =
 *   view ~read:Sdl.unsafe_surface_of_ptr ~write:Sdl.unsafe_ptr_of_surface nativeint *)
let surface_result =
  let read v =
    if Nativeint.(compare v zero) = 0 then
      error ()
    else
      Ok (Sdl.unsafe_surface_of_ptr v)
  and write = function
    | Error _ -> raw_address_of_ptr @@ null
    | Ok s -> Sdl.unsafe_ptr_of_surface s
  in view ~read ~write nativeint

let rw_ops =
  view ~read:Sdl.unsafe_rw_ops_of_ptr ~write:Sdl.unsafe_ptr_of_rw_ops nativeint

type _font
type font = _font structure ptr
let font_struct : _font structure typ = structure "TTF_Font"
let font : _font structure ptr typ = ptr font_struct
let font_opt : _font structure ptr option typ = ptr_opt font_struct
let font_result =
  let read = function
    | None -> error ()
    | Some v -> Ok v
  and write = function
    | Error _ -> None
    | Ok s -> Some s
  in view ~read ~write font_opt

(* This "hack" seems to be necessary for linux if you want to use
   #require "tsdl-ttf"
   in the toplevel, see
   https://github.com/ocamllabs/ocaml-ctypes/issues/70 *)
let foreign name typ =
  foreign name typ ~from:Dl.(dlopen ~filename:"libSDL2_ttf-2.0.so"
                               ~flags:[RTLD_NOW])

let init = foreign "TTF_Init" (void @-> returning zero_to_ok)

let open_font = foreign "TTF_OpenFont" (string @-> int @-> returning font_result)

let open_font_index =
  foreign "TTF_OpenFontIndex" (string @-> int @-> int64_as_long @-> returning font_result)

let open_font_rw = foreign "TTF_OpenFontRW" (rw_ops @-> int @-> int @-> returning font_result)

let open_font_index_rw =
  foreign "TTF_OpenFontIndexRW" (rw_ops @-> int @-> int @-> int64_as_long @-> returning font_result)

(* let byte_swapped_unicode =
 *   foreign "TTF_ByteSwappedUNICODE" (int @-> returning void) *)

module Style = struct
  type t = Unsigned.uint32
  let i = Unsigned.UInt32.of_int
  let ( + ) = Unsigned.UInt32.logor
  let test f m = Unsigned.UInt32.(compare (logand f m) zero <> 0)
  let eq f f' = Unsigned.UInt32.(compare f f' = 0)
  let normal = i 0
  let bold = i 1
  let italic = i 2
  let underline = i 4
  let strikethrough = i 8
end
let get_font_style = foreign "TTF_GetFontStyle" (font @-> returning uint32_t)
let set_font_style = foreign "TTF_SetFontStyle" (font @-> uint32_t @-> returning void)

let get_font_outline = foreign "TTF_GetFontOutline" (font @-> returning int)
let set_font_outline = foreign "TTF_SetFontOutline" (font @-> int @-> returning void)

module Hinting = struct
  type t = Normal | Light | Mono | None
  let t =
    let read = function 0 -> Normal | 1 -> Light | 2 -> Mono | 3 -> None | _ -> failwith "Unexpected value" in
    let write = function Normal -> 0 | Light -> 1 | Mono -> 2 | None -> 3 in
    view ~read ~write int
end
let get_font_hinting = foreign "TTF_GetFontHinting" (font @-> returning Hinting.t)
let set_font_hinting = foreign "TTF_SetFontHinting" (font @-> Hinting.t @-> returning void)

let font_height = foreign "TTF_FontHeight" (font @-> returning int)
let font_ascent = foreign "TTF_FontAscent" (font @-> returning int)
let font_descent = foreign "TTF_FontDescent" (font @-> returning int)

let font_line_skip = foreign "TTF_FontLineSkip" (font @-> returning int)

let get_font_kerning = foreign "TTF_GetFontKerning" (font @-> returning bool)
let set_font_kerning = foreign "TTF_SetFontKerning" (font @-> bool @-> returning void)

let font_faces = foreign "TTF_FontFaces" (font @-> returning int64_as_long)

let font_face_is_fixed_width = foreign "TTF_FontFaceIsFixedWidth" (font @-> returning int)
let font_face_family_name = foreign "TTF_FontFaceFamilyName" (font @-> returning string)
let font_face_style_name = foreign "TTF_FontFaceStyleName" (font @-> returning string)

let glyph_ucs2 =
  view ~read:Unsigned.UInt16.to_int ~write:Unsigned.UInt16.of_int uint16_t

let glyph_is_provided = foreign "TTF_GlyphIsProvided" (font @-> glyph_ucs2 @-> returning bool)

module GlyphMetrics = struct
  type t = { min_x:int; max_x:int; min_y:int; max_y:int; advance:int }
end
let glyph_metrics = foreign "TTF_GlyphMetrics" (font @-> glyph_ucs2 @-> ptr int @-> ptr int @-> ptr int @-> ptr int @-> ptr int @-> returning int)
let glyph_metrics f g =
  let (min_x,max_x,min_y,max_y,advance) = (allocate int 0,
                                           allocate int 0,
                                           allocate int 0,
                                           allocate int 0,
                                           allocate int 0)
  in
  if 0 = glyph_metrics f g min_x max_x min_y max_y advance then
    Ok GlyphMetrics.({ min_x = !@ min_x;
                       max_x = !@ max_x;
                       min_y = !@ min_y;
                       max_y = !@ max_y;
                       advance = !@ advance })
  else
    error ()

let size_text = foreign "TTF_SizeText" (font @-> string @-> ptr int @-> ptr int @-> returning int)
let size_text f s =
  let (w,h) = (allocate int 0, allocate int 0) in
  if 0 = size_text f s w h then Ok (!@ w, !@ h) else error ()

let size_utf8 = foreign "TTF_SizeUTF8" (font @-> string @-> ptr int @-> ptr int @-> returning int)
let size_utf8 f s =
  let (w,h) = (allocate int 0, allocate int 0) in
  if 0 = size_utf8 f s w h then Ok (!@ w, !@ h) else error ()

(* let size_unicode =
 *   foreign "TTF_SizeUNICODE" (font @-> ptr glyph_ucs2 @-> ptr int @-> ptr int @-> returning int) *)

type _color
type color = _color structure
let color : color typ = structure "SDL_Color"
let color_r = field color "r" uint8_t
let color_g = field color "g" uint8_t
let color_b = field color "b" uint8_t
let color_a = field color "a" uint8_t
let () = seal color

let color =
  let read v =
    let (r,g,b,a) = Unsigned.UInt8.(to_int @@ getf v color_r,
                                    to_int @@ getf v color_g,
                                    to_int @@ getf v color_b,
                                    to_int @@ getf v color_a) in
    Sdl.Color.create ~r ~g ~b ~a in
  let write v =
    let c = make color in
    setf c color_r (Unsigned.UInt8.of_int (Sdl.Color.r v));
    setf c color_g (Unsigned.UInt8.of_int (Sdl.Color.g v));
    setf c color_b (Unsigned.UInt8.of_int (Sdl.Color.b v));
    setf c color_a (Unsigned.UInt8.of_int (Sdl.Color.a v));
    c
  in view ~read ~write color

let render_text_solid = foreign "TTF_RenderText_Solid" (font @-> string @-> color @-> returning surface_result)
let render_utf8_solid = foreign "TTF_RenderUTF8_Solid" (font @-> string @-> color @-> returning surface_result)
(* let render_unicode_solid = foreign "TTF_RenderUNICODE_Solid" (font @-> ptr glyph_ucs2 @-> color @-> returning surface_result) *)

let render_glyph_solid = foreign "TTF_RenderGlyph_Solid" (font @-> glyph_ucs2 @-> color @-> returning surface_result)

let render_text_shaded = foreign "TTF_RenderText_Shaded" (font @-> string @-> color @-> color @-> returning surface_result)
let render_utf8_shaded = foreign "TTF_RenderUTF8_Shaded" (font @-> string @-> color @-> color @-> returning surface_result)
(* let render_unicode_shaded = foreign "TTF_RenderUNICODE_Shaded" (font @-> ptr glyph_ucs2 @-> color @-> color @-> returning surface_result) *)

let render_glyph_shaded = foreign "TTF_RenderGlyph_Shaded" (font @-> glyph_ucs2 @-> color @-> color @-> returning surface_result)

let render_text_blended = foreign "TTF_RenderText_Blended" (font @-> string @-> color @-> returning surface_result)
let render_utf8_blended = foreign "TTF_RenderUTF8_Blended" (font @-> string @-> color @-> returning surface_result)
(* let render_unicode_blended = foreign "TTF_RenderUNICODE_Blended" (font @-> ptr glyph_ucs2 @-> color @-> returning surface_result) *)

let render_text_blended_wrapped = foreign "TTF_RenderText_Blended_Wrapped" (font @-> string @-> color @-> int32_as_uint32_t @-> returning surface_result)
let render_utf8_blended_wrapped = foreign "TTF_RenderUTF8_Blended_Wrapped" (font @-> string @-> color @-> int32_as_uint32_t @-> returning surface_result)
(* let render_unicode_blended_wrapped = foreign "TTF_RenderUNICODE_Blended_Wrapped" (font @-> ptr glyph_ucs2 @-> color @-> int32_as_uint32_t @-> returning surface_result) *)

let render_glyph_blended = foreign "TTF_RenderGlyph_Blended" (font @-> glyph_ucs2 @-> color @-> returning surface_result)

let close_font = foreign "TTF_CloseFont" (font @-> returning void)

let quit = foreign "TTF_Quit" (void @-> returning void)

let was_init = foreign "TTF_WasInit" (void @-> returning bool)

let get_font_kerning_size = foreign "TTF_GetFontKerningSize" (font @-> int @-> int @-> returning int)

end

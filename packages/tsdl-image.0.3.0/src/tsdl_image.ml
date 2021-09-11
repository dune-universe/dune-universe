open Ctypes
open Foreign
open Tsdl

module Image = struct

type 'a result = 'a Sdl.result

let error () = Error (`Msg (Sdl.get_error ()))

let bool =
  view ~read:((<>)0) ~write:(fun b -> compare b false) int

module Init = struct
  type t = Unsigned.uint32
  let i = Unsigned.UInt32.of_int
  let ( + ) = Unsigned.UInt32.logor
  let test f m = Unsigned.UInt32.(compare (logand f m) zero <> 0)
  let eq f f' = Unsigned.UInt32.(compare f f' = 0)
  let empty = i 0
  let jpg = i 1
  let png = i 2
  let tif = i 4
  let webp = i 8
end

(* This "hack" seems to be necessary for linux if you want to use
   #require "tsdl-image"
   in the toplevel, see
   https://github.com/ocamllabs/ocaml-ctypes/issues/70 *)
let foreign name typ =
  foreign name typ ~from:Dl.(dlopen ~filename:"libSDL2_image-2.0.so"
                               ~flags:[RTLD_NOW])
let init =
  foreign "IMG_Init" (uint32_t @-> returning uint32_t)

let quit =
  foreign "IMG_Quit" (void @-> returning void)

let surface =
  view
    ~read:Sdl.unsafe_surface_of_ptr
    ~write:Sdl.unsafe_ptr_of_surface
    nativeint

let texture_result =
  let read v =
    if Nativeint.(compare v zero) = 0
    then error ()
    else Ok (Sdl.unsafe_texture_of_ptr v)
  and write = function
    | Ok v -> Sdl.unsafe_ptr_of_texture v
    | Error _ -> raw_address_of_ptr null
  in
  view ~read ~write nativeint

let surface_result =
  let read v =
    if Nativeint.(compare v zero) = 0
    then error ()
    else Ok (Sdl.unsafe_surface_of_ptr v)
  and write = function
    | Ok v -> Sdl.unsafe_ptr_of_surface v
    | Error _ -> raw_address_of_ptr null
  in
  view ~read ~write nativeint

let rw_ops =
  view ~read:Sdl.unsafe_rw_ops_of_ptr ~write:Sdl.unsafe_ptr_of_rw_ops nativeint
let renderer =
  view ~read:Sdl.unsafe_renderer_of_ptr ~write:Sdl.unsafe_ptr_of_renderer nativeint

let load =
  foreign "IMG_Load" (string @-> returning surface_result)

let load_rw =
  foreign "IMG_Load_RW" (rw_ops @-> bool @-> returning surface_result)

type format = Ico | Cur | Bmp | Gif | Jpg | Lbm | Pcx | Png | Pnm | Tif | Xcf
            | Xpm | Xv | Webp | Tga
let string_of_format = function
  | Ico -> "ICO" | Cur -> "CUR" | Bmp -> "BMP" | Gif -> "GIF"
  | Jpg -> "JPG" | Lbm -> "LBM" | Pcx -> "PCX" | Png -> "PNG"
  | Pnm -> "PNM" | Tif -> "TIF" | Xcf -> "XCF" | Xpm -> "XPM"
  | Xv -> "XV" | Webp -> "WEBP" | Tga -> "TGA"

let load_typed_rw =
  foreign "IMG_LoadTyped_RW"
          (rw_ops @-> bool @-> string @-> returning surface_result)
let load_typed_rw r b f = load_typed_rw r b (string_of_format f)

let load_texture =
  foreign "IMG_LoadTexture"
    (renderer @-> string @-> returning texture_result)

let load_texture_rw =
  foreign "IMG_LoadTexture_RW"
    (renderer @-> rw_ops @-> bool @-> returning texture_result)

let load_texture_typed_rw =
  foreign "IMG_LoadTextureTyped_RW"
          (renderer @-> rw_ops @-> bool @-> string @-> returning texture_result)
let load_texture_typed_rw r o b f =
  load_texture_typed_rw r o b (string_of_format f)

let is_ico = foreign "IMG_isICO" (rw_ops @-> returning bool)
let is_cur = foreign "IMG_isCUR" (rw_ops @-> returning bool)
let is_bmp = foreign "IMG_isBMP" (rw_ops @-> returning bool)
let is_gif = foreign "IMG_isGIF" (rw_ops @-> returning bool)
let is_jpg = foreign "IMG_isJPG" (rw_ops @-> returning bool)
let is_lbm = foreign "IMG_isLBM" (rw_ops @-> returning bool)
let is_pcx = foreign "IMG_isPCX" (rw_ops @-> returning bool)
let is_png = foreign "IMG_isPNG" (rw_ops @-> returning bool)
let is_pnm = foreign "IMG_isPNM" (rw_ops @-> returning bool)
let is_tif = foreign "IMG_isTIF" (rw_ops @-> returning bool)
let is_xcf = foreign "IMG_isXCF" (rw_ops @-> returning bool)
let is_xpm = foreign "IMG_isXPM" (rw_ops @-> returning bool)
let is_xv = foreign "IMG_isXV" (rw_ops @-> returning bool)
let is_webp = foreign "IMG_isWEBP" (rw_ops @-> returning bool)
let is_format fmt = match fmt with
  | Ico -> is_ico | Cur -> is_cur | Bmp -> is_bmp | Gif -> is_gif
  | Jpg -> is_jpg | Lbm -> is_lbm | Pcx -> is_pcx | Png -> is_png
  | Pnm -> is_pnm | Tif -> is_tif | Xcf -> is_xcf | Xpm -> is_xpm
  | Xv -> is_xv | Webp -> is_webp
  | Tga -> failwith "TGA cannot safely be detected"

let load_ico_rw = foreign "IMG_LoadICO_RW" (rw_ops @-> returning surface_result)
let load_cur_rw = foreign "IMG_LoadCUR_RW" (rw_ops @-> returning surface_result)
let load_bmp_rw = foreign "IMG_LoadBMP_RW" (rw_ops @-> returning surface_result)
let load_gif_rw = foreign "IMG_LoadGIF_RW" (rw_ops @-> returning surface_result)
let load_jpg_rw = foreign "IMG_LoadJPG_RW" (rw_ops @-> returning surface_result)
let load_lbm_rw = foreign "IMG_LoadLBM_RW" (rw_ops @-> returning surface_result)
let load_pcx_rw = foreign "IMG_LoadPCX_RW" (rw_ops @-> returning surface_result)
let load_png_rw = foreign "IMG_LoadPNG_RW" (rw_ops @-> returning surface_result)
let load_pnm_rw = foreign "IMG_LoadPNM_RW" (rw_ops @-> returning surface_result)
let load_tga_rw = foreign "IMG_LoadTGA_RW" (rw_ops @-> returning surface_result)
let load_tif_rw = foreign "IMG_LoadTIF_RW" (rw_ops @-> returning surface_result)
let load_xcf_rw = foreign "IMG_LoadXCF_RW" (rw_ops @-> returning surface_result)
let load_xpm_rw = foreign "IMG_LoadXPM_RW" (rw_ops @-> returning surface_result)
let load_xv_rw = foreign "IMG_LoadXV_RW" (rw_ops @-> returning surface_result)
let load_webp_rw = foreign "IMG_LoadWEBP_RW" (rw_ops @-> returning surface_result)
let load_format_rw = function
  | Ico -> load_ico_rw | Cur -> load_cur_rw | Bmp -> load_bmp_rw | Gif -> load_gif_rw
  | Jpg -> load_jpg_rw | Lbm -> load_lbm_rw | Pcx -> load_pcx_rw | Png -> load_png_rw
  | Pnm -> load_pnm_rw | Tif -> load_tif_rw | Xcf -> load_xcf_rw | Xpm -> load_xpm_rw
  | Xv -> load_xv_rw | Webp -> load_webp_rw | Tga -> load_tga_rw

let read_xpm_from_array =
  foreign "IMG_ReadXPMFromArray" (string @-> returning surface_result)

let save_png =
  foreign "IMG_SavePNG" (surface @-> string @-> returning int)
let save_png_rw =
  foreign "IMG_SavePNG_RW" (surface @-> rw_ops @-> bool @-> returning int)

end

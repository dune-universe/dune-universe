open! Core

type t = { r: int; g: int; b: int } [@@deriving sexp, fields]
let black = { r=0; g=0; b=0 }
let white = { r=255; g=255; b=255 }
let green = { r=0; g=255; b=0 }
let red = { r=255; g=0; b=0 }
let blue = { r=0; g=0; b=255 }
let purple = { r=0x80; g=0x00; b=0x80 }
let of_hex_int i =
  { r=(i lsr 16) land 0xFF
  ; g=(i lsr 8) land 0xFF
  ; b=i land 0xFF }
let ri () = Random.int 256
let rand () = { r=ri (); g=ri (); b=ri () }

let f = Float.of_int
let i = Float.to_int
let to_gl t =
  (f t.r /. 255.), (f t.g /. 255.), (f t.b /. 255.)
let of_gl (r, g, b) =
  { r=i (r*.255.); g=i (g*.255.); b=i (b*.255.) }
let to_string t =
  sexp_of_t t |> Sexp.to_string

let shade t ~factor =
  let r, g, b =
    let f = Float.of_int in
    f t.r, f t.g, f t.b
  in
  let i = Float.to_int in
  let m x = max (min (i x) 255) 0 in
  let shade = 1.0 -. factor in
  { r = m (r *. shade)
  ; g = m (g *. shade)
  ; b = m (b *. shade) }

let of_string = function
  | "white" -> white
  | "black" -> black
  | "green" -> green
  | "red" -> red
  | "blue" -> blue
  | "rand" -> rand ()
  | "purple" -> purple
  | s -> failwithf "Color.of_string: unknown color: %s" s ()

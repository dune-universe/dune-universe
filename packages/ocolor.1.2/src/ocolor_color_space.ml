open Ocolor_types

type srgb = Srgb of float * float * float
type xyz  = XYZ  of float * float * float
type lab  = Lab  of float * float * float

let (d65 : xyz) = XYZ(0.95047, 1.0000, 1.08883)

let srgb_of_rgb (r, g, b: rgb) : srgb =
  Srgb (float_of_int r /. 255., float_of_int g /. 255., float_of_int b /. 255.)

let xyz_of_srgb (Srgb(r, g, b) : srgb) : xyz =
  let a = 0.055 in
  let f (x: float) : float =
    if x < 0.04045 then
      x /. 12.92
    else
      ((x +. a)/.(1. +. a))**2.4
  in
  let rl = f r in
  let gl = f g in
  let bl = f b in
  let x = 0.4124*.rl +. 0.3576*.gl +. 0.1805*.bl in
  let y = 0.2126*.rl +. 0.7152*.gl +. 0.0722*.bl in
  let z = 0.0193*.rl +. 0.1192*.gl +. 0.9505*.bl in
  XYZ(x, y, z)

let lab_of_xyz (XYZ(x, y, z) : xyz) : lab =
  let XYZ(xn, yn, zn) = d65 in
  let x = x /. xn in
  let y = y /. yn in
  let z = z /. zn in
  let delta : float = 6./.29. in
  let f (t: float) : float =
    if t > delta ** 3. then
      t ** (1. /. 3.)
    else
      t /. (3. *. delta ** 2.) +. 4. /. 29.
  in
  let l = 116. *. (f y) -. 16. in
  let a = 500. *. (f x -. f y) in
  let b = 200. *. (f y -. f z) in
  Lab(l, a, b)

let lab_distance (Lab(l1, a1, b1) : lab) (Lab(l2, a2, b2) : lab) : float =
  let sq (x: float) : float =
    x *. x
  in
  sqrt(sq (l1 -. l2) +. sq (a1 -. a2) +. sq (b1 -. b2))

let lab_of_rgb (c: rgb) : lab =
  c |> srgb_of_rgb |> xyz_of_srgb |> lab_of_xyz

let rgb_and_lab_of_rgb (c: rgb) : rgb * lab =
  c, lab_of_rgb c

let palettes4 : (rgb*lab) Color4Map.t ColorPaletteMap.t =
  List.fold_left
    (fun acc (c, map) -> ColorPaletteMap.add c (Color4Map.map rgb_and_lab_of_rgb map) acc)
    ColorPaletteMap.empty
    Ocolor_palettes.[
      VGA,          vga;
      CMD,          cmd;
      Terminal_app, terminal_app;
      PuTTY,        putty;
      MIRC,         mirc;
      Xterm,        xterm;
      X,            x;
      Ubuntu,       ubuntu;
    ]

let palette4 () : (rgb * lab) Color4Map.t =
  ((match Ocolor_config.get_palette () with
  | Custom_palette p -> Color4Map.map rgb_and_lab_of_rgb p
  | palette -> ColorPaletteMap.find palette palettes4)
[@warning "-4"])

let standard_palette8 : (rgb * lab) Color8Map.t =
  let p : (rgb * lab) Color8Map.t = Color8Map.empty in
  let p : (rgb * lab) Color8Map.t =
    fold_greyscale
      (fun gs p ->
         Color8Map.add (Grayscale gs) (rgb_and_lab_of_rgb (gs*255/23, gs*255/23, gs*255/23)) p
      )
      p
  in
  let p : (rgb * lab) Color8Map.t =
    fold_cube6
      (fun ({r6; g6; b6} as c6) p ->
         Color8Map.add (Cube6 c6) (rgb_and_lab_of_rgb (r6*255/5, g6*255/5, b6*255/5)) p
      )
      p
  in
  p

let palette8_of_palette4 (p: (rgb * lab) Color4Map.t) : (rgb * lab) Color8Map.t =
  let p : (rgb * lab) Color8Map.t =
    Color4Map.fold
      (fun c lab acc ->
         Color8Map.add (Standard c) lab acc
      )
      p
      standard_palette8
  in
  p

let palettes8 : (rgb * lab) Color8Map.t ColorPaletteMap.t =
  ColorPaletteMap.fold
    (fun c map acc -> ColorPaletteMap.add c (palette8_of_palette4 map) acc)
    palettes4
    ColorPaletteMap.empty

let palette8 () : (rgb * lab) Color8Map.t =
((match Ocolor_config.get_palette () with
  | Custom_palette p -> p |> Color4Map.map rgb_and_lab_of_rgb |> palette8_of_palette4
  | palette -> ColorPaletteMap.find palette palettes8)
[@warning "-4"])

let closest_color4 (map: (rgb * lab) Color4Map.t) (c: rgb) : color4 * rgb * lab * float =
  let (c4, (rgb, lab)) =
    try
      Color4Map.choose map
    with Not_found -> failwith (Printf.sprintf "%s: empty palette" __LOC__)
  in
  let c = lab_of_rgb c in
  let d = lab_distance lab c in
  let c4, rgb, lab, distance =
    Color4Map.fold
      (
        fun a (rgb, b) (min, rgb', argmin, d) ->
          let d1 = lab_distance b c in
          if d1 < d then
            (a, rgb, b, d1)
          else
            (min, rgb', argmin, d)
      )
      map
      (c4, rgb, lab, d)
  in
  c4, rgb, lab, distance

let closest_color4 (c: rgb) : color4 =
  let c, _, _, _ = closest_color4 (palette4 ()) c in
  c


let closest_color8 (map: (rgb * lab) Color8Map.t) (c: rgb) : color8 * rgb * lab =
  let (a, (rgb, b)) =
    try
      Color8Map.choose map
    with Not_found -> failwith (Printf.sprintf "%s: empty palette" __LOC__)
  in
  let c = lab_of_rgb c in
  let d = lab_distance b c in
  let a, rgb, b, _ =
    Color8Map.fold
      (
        fun a (rgb, b) (min, rgb', argmin, d) ->
          let d1 = lab_distance b c in
          if d1 < d then
            (a, rgb, b, d1)
          else
            (min, rgb', argmin, d)
      )
      map
      (a, rgb, b, d)
  in
  a, rgb, b

let closest_color8 (c: rgb) : color8 =
  let c, _, _ = closest_color8 (palette8 ()) c in
  c

let rgb_of_color4 (c: color4) : rgb =
  let p = palette4 () in
  let (r, g, b), _ = Color4Map.find c p in
  r, g, b

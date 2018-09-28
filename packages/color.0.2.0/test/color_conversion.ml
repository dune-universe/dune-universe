let test_hsl_to_rgba () =
  let open Color in
  let colors =
    [ of_hsl 162.4 0.779 0.447
    ; of_hsl 0. 0. 1.
    ; of_hsl 0. 0. 0.5
    ; of_hsl 60. 1. 0.375 ]
  in
  let colors_rgba = List.map to_rgba colors in
  let compare =
    List.map2
      (fun x y -> x = y)
      colors_rgba
      [ {Rgba.r= 25; g= 203; b= 151; a= 1.}
      ; {Rgba.r= 255; g= 255; b= 255; a= 1.}
      ; {Rgba.r= 128; g= 128; b= 128; a= 1.}
      ; {Rgba.r= 191; g= 191; b= 0; a= 1.} ]
  in
  Alcotest.(check bool)
    "can convert hsl to rgba" true
    (List.for_all (fun x -> x) compare)

let colors_rgba_hex =
  [ (0, 191, 255, "#00bfff")
  ; (98, 121, 255, "#6279ff")
  ; (198, 167, 55, "#c6a737")
  ; (198, 17, 55, "#c61137")
  ; (252, 7, 16, "#fc0710")
  ; (18, 157, 126, "#129d7e")
  ; (98, 167, 255, "#62a7ff") ]

let test_color_to_hexstring () =
  let check (r, g, b, h) =
    let c = Color.of_rgb r g b in
    Alcotest.(check string)
      "Converts color to hexstring" h (Color.to_hexstring c)
  in
  List.iter (fun x -> check x) colors_rgba_hex

let test_hexstring_to_color () =
  let open Color in
  let check (r, g, b, h) =
    match of_hexstring h with
    | Some c ->
        let c' = to_rgba c in
        {Rgba.r; g; b; a= 1.} = c'
    | None -> failwith "Something bad happened"
  in
  Alcotest.(check bool)
    "Can create color from hexstring" true
    (List.for_all (fun x -> check x) colors_rgba_hex)

let tests =
  [ ("Hsl to RGBA'", `Quick, test_hsl_to_rgba)
  ; ("Color to hexstring", `Quick, test_color_to_hexstring)
  ; ("Hexstring to color", `Quick, test_hexstring_to_color) ]

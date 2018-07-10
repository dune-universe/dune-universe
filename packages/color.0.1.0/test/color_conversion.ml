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

let tests = [("Hsl to RGBA'", `Quick, test_hsl_to_rgba)]

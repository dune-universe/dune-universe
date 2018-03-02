let () =
  Dynlink.allow_unsafe_modules true; (* for bytecode *)
  Dynlink.loadfile
    (if Dynlink.is_native then "../src/camlimages_jpeg.cmxs"
     else "../src/camlimages_jpeg.cma");
  let i = Images.load "images/jpg.jpg" [] in
  Images.save "out-dyn.bmp" None [] i

open FrameWork

let s = "hello"

let fig = Picture.tex s

module FM = File.Map

let test_mp =
  let out = "hello" in
  let ref_file = File.from_string "hello.reference" in
  let cleanup = ref (File.from_string "") in
  Test.mk ~name:"mp"
    ~clean_up:(fun () -> File.rm !cleanup)
    (fun () ->
      let f, m = Metapost.mp out [ (File.from_string "hello.mps", fig) ] in
      cleanup := f;
      Assert.File.exists f;
      Assert.File.eq ref_file f)

let test_mps =
  let out = File.from_string "hello.mps" in
  let ref = File.from_string "hello.mps.reference" in
  Test.mk ~name:"mps"
    ~clean_up:(fun () -> File.rm out)
    (fun () ->
      let l = Metapost.temp_mps "hello" [ (out, fig) ] in
      Assert.List.non_empty l;
      let f = List.hd l in
      Assert.bool (File.compare f out = 0);
      Assert.File.exists f;
      Assert.File.eq ~ignore:"%%CreationDate:" ref f)

let test_png =
  let bn = File.from_string "hello.mps" in
  let out = File.set_ext bn "png" in
  Test.mk ~name:"png"
    ~clean_up:(fun () -> File.rm out)
    ~prepare:(fun () -> Compile.reset ())
    (fun () ->
      let l = Metapost.temp_png "hello" [ (bn, fig) ] in
      Assert.List.non_empty l;
      let f = List.hd l in
      Assert.bool (File.compare f out = 0);
      Assert.File.exists f)

let tests = [ test_mp; test_mps; test_png ]

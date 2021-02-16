open FrameWork

let test_from_string =
  Test.mk ~name:"from_string" (fun () ->
      let s = "toto.mps" in
      let f = File.from_string s in
      Assert.eq ~s:"toto.mps" (File.to_string f) "toto.mps";
      let p = File.set_ext f "pdf" in
      Assert.eq ~s:"toto.pdf" (File.to_string p) "toto.pdf")

let test_dir_from_string =
  Test.mk ~name:"dir_from_string" (fun () ->
      let s = "/toto/blop/blip" in
      let f = File.Dir.from_string s in
      Assert.eq ~s (File.Dir.to_string f) s)

let tests = [ test_from_string; test_dir_from_string ]

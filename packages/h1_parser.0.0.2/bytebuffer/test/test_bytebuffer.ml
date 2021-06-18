let test_buffer () =
  let b = Bytebuffer.create 20 in
  Bytebuffer.add_string b "Hello";
  Bytebuffer.add_char b ' ';
  Bytebuffer.addf b " Foo %d bar %S" 561 "This is a string";
  Alcotest.(check string)
    "Adds content to bytebuffer" {|Hello  Foo 561 bar "This is a string"|}
    (Bytebuffer.contents_string b)

let test_resize () =
  let b = Bytebuffer.create 5 in
  Alcotest.(check int) "Original capacity" 5 (Bytebuffer.capacity b);
  Bytebuffer.add_string b "hello";
  Alcotest.(check int)
    "Buffer with same length and capacity" (Bytebuffer.length b)
    (Bytebuffer.capacity b);
  Bytebuffer.add_char b ' ';
  Alcotest.(check int)
    "Buffer automatically resizes on adding content after capacity" 12
    (Bytebuffer.capacity b);
  Bytebuffer.clear b;
  Alcotest.(check (pair int int))
    "Clearing buffer brings length to 0, but keeps expanded capacity" (0, 12)
    (Bytebuffer.length b, Bytebuffer.capacity b);
  Bytebuffer.reset b;
  Alcotest.(check (pair int int))
    "Resetting the buffer restores the capacity to the original length" (0, 5)
    (Bytebuffer.length b, Bytebuffer.capacity b)

let () =
  let open Alcotest in
  run "Bytebuffer"
    [
      ("add content", [ test_case "Adds content to buffer" `Quick test_buffer ]);
      ("resize", [ test_case "Buffer extends itself" `Quick test_resize ]);
    ]

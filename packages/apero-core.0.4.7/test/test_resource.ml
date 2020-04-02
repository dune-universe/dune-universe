open Apero

let ascii_table = List.init 256 (fun i -> char_of_int i)

let acceptable_chars = List.filter (fun c -> c > ' ' && c != '/' && c != '*' && c != '?' && c != '[' && c != ']' && c != '#') ascii_table


let check_if b ?arg line =
  let test_name =
    match arg with
    | None -> Printf.sprintf "test line %d" line
    | Some(s) -> Printf.sprintf "test line %d with %s" line s
  in
    Alcotest.(check bool) test_name b

let expr s e =
  try
    PathExpr.to_string @@ PathExpr.of_string s = e
  with 
    | Exception e -> print_endline @@ show_error e; false

let expr_matching_path e p =
  try 
    PathExpr.is_matching_path (Path.of_string p) (PathExpr.of_string e) 
  with 
    | Exception e -> print_endline @@ show_error e; false


let intersect e1 e2 = 
  try 
    PathExpr.intersect (PathExpr.of_string e1) (PathExpr.of_string e2) 
    &&
    PathExpr.intersect (PathExpr.of_string e2) (PathExpr.of_string e1) 
  with 
    | Exception e -> print_endline @@ show_error e; false

let includes ~subexpr e = 
  try 
    PathExpr.includes ~subexpr:(PathExpr.of_string subexpr) (PathExpr.of_string e)
  with 
    | Exception e -> print_endline @@ show_error e; false

let remaining_after_match p e r =
  try
    let expected = Option.map r PathExpr.of_string in
    let remain = PathExpr.remaining_after_match (Path.of_string p) (PathExpr.of_string e) in
    remain = expected
  with
    | Exception e -> print_endline @@ show_error e; false

let test_expr_validity () =
  check_if true  __LINE__ @@ expr "/a/b/c" "/a/b/c";
  check_if true  __LINE__ @@ expr "/a b c" "/a b c";
  check_if true  __LINE__ @@ expr "/a b/c" "/a b/c";
  check_if true  __LINE__ @@ expr "/*" "/*";
  check_if true  __LINE__ @@ expr "/*/" "/*";
  check_if true  __LINE__ @@ expr "/a*" "/a*";
  check_if true  __LINE__ @@ expr "/**" "/**";
  check_if true  __LINE__ @@ expr "/**/" "/**";
  check_if false __LINE__ @@ expr "/a**/c" "/a**/c";
  check_if false __LINE__ @@ expr "/**b/c" "/**b/c";
  check_if false __LINE__ @@ expr "/a**b/c" "/a**b/c";
  check_if true  __LINE__ @@ expr "/**/**" "/**/**";
  check_if false __LINE__ @@ expr "/a/b?" "/a/b";
  check_if true  __LINE__ @@ expr "/" "/";
  check_if true  __LINE__ @@ expr "/a/" "/a";
  check_if true  __LINE__ @@ expr "//a/" "/a";
  check_if true  __LINE__ @@ expr "////a///" "/a";
  check_if true  __LINE__ @@ expr "////a//b///c/" "/a/b/c";
  check_if true  __LINE__ @@ expr "" "";
  check_if true  __LINE__ @@ expr "abc" "abc";
  ()

let test_expr_matching_path_simple () =
  check_if true __LINE__ @@ expr_matching_path "/" "/";
  check_if true __LINE__ @@ expr_matching_path "/a" "/a";
  check_if true __LINE__ @@ expr_matching_path "/a/" "/a";
  check_if true __LINE__ @@ expr_matching_path "/a" "/a/";
  check_if true __LINE__ @@ expr_matching_path "/a/b" "/a/b";
  List.iter (fun c -> let arg = Printf.sprintf "/%c" c in check_if true __LINE__ ~arg @@ expr_matching_path arg arg) acceptable_chars;

  check_if true  __LINE__ @@ expr_matching_path "/*" "/abc";
  check_if true  __LINE__ @@ expr_matching_path "/*" "/abc/";
  check_if true  __LINE__ @@ expr_matching_path "/*/" "/abc";
  check_if false __LINE__ @@ expr_matching_path "/*" "/";
  check_if false __LINE__ @@ expr_matching_path "/*" "xxx";
  check_if true  __LINE__ @@ expr_matching_path "/ab*" "/abcd";
  check_if true  __LINE__ @@ expr_matching_path "/ab*d" "/abcd";
  check_if true  __LINE__ @@ expr_matching_path "/ab*" "/ab";
  check_if false __LINE__ @@ expr_matching_path "/ab/*" "/ab";
  check_if true  __LINE__ @@ expr_matching_path "/a/*/c/*/e" "/a/b/c/d/e";
  check_if true  __LINE__ @@ expr_matching_path "/a/*b/c/*d/e" "/a/xb/c/xd/e";
  check_if false __LINE__ @@ expr_matching_path "/a/*/c/*/e" "/a/c/e";
  check_if false __LINE__ @@ expr_matching_path "/a/*/c/*/e" "/a/b/c/d/x/e";
  check_if false __LINE__ @@ expr_matching_path "/ab*cd" "/abxxcxxd";
  check_if true  __LINE__ @@ expr_matching_path "/ab*cd" "/abxxcxxcd";
  check_if false __LINE__ @@ expr_matching_path "/ab*cd" "/abxxcxxcdx";
  List.iter (fun c -> let arg = Printf.sprintf "/%c" c in check_if true __LINE__ ~arg @@ expr_matching_path "/*" arg) acceptable_chars;

  check_if true  __LINE__ @@ expr_matching_path "/**" "/abc";
  check_if true  __LINE__ @@ expr_matching_path "/**" "/a/b/c";
  check_if true  __LINE__ @@ expr_matching_path "/**" "/a/b/c/";
  check_if true  __LINE__ @@ expr_matching_path "/**/" "/a/b/c";
  check_if true  __LINE__ @@ expr_matching_path "/**/" "/";
  check_if true  __LINE__ @@ expr_matching_path "/ab/**" "/ab";
  check_if true  __LINE__ @@ expr_matching_path "/**/xyz" "/a/b/xyz/d/e/f/xyz";
  check_if false __LINE__ @@ expr_matching_path "/**/xyz*xyz" "/a/b/xyz/d/e/f/xyz";
  check_if true  __LINE__ @@ expr_matching_path "/a/**/c/**/e" "/a/b/b/b/c/d/d/d/e";
  check_if true  __LINE__ @@ expr_matching_path "/a/**/c/**/e" "/a/c/e";
  check_if true  __LINE__ @@ expr_matching_path "/a/**/c/*/e/*" "/a/b/b/b/c/d/d/c/d/e/f";
  check_if false __LINE__ @@ expr_matching_path "/a/**/c/*/e/*" "/a/b/b/b/c/d/d/c/d/d/e/f";
  check_if false __LINE__ @@ expr_matching_path "/ab*cd" "/abxxcxxcdx";
  List.iter (fun c -> let arg = Printf.sprintf "/%c" c in check_if true __LINE__ ~arg @@ expr_matching_path "/**" arg) acceptable_chars;
  ()

let test_expr_intersect () = 
  check_if true  __LINE__ @@ intersect "/x/abc" "/x/abc";
  check_if false __LINE__ @@ intersect "/x/abc" "/abc";
  check_if true  __LINE__ @@ intersect "/x/*" "/x/abc";
  check_if false __LINE__ @@ intersect "/x/*" "/abc";
  check_if false __LINE__ @@ intersect "/*" "/x/abc";
  check_if true  __LINE__ @@ intersect "/x/*" "/x/abc*";
  check_if true  __LINE__ @@ intersect "/x/*abc" "/x/abc*";
  check_if true  __LINE__ @@ intersect "/x/a*" "/x/abc*";
  check_if true  __LINE__ @@ intersect "/x/a*de" "/x/abc*de";
  check_if true  __LINE__ @@ intersect "/x/a*d*e" "/x/a*e";
  check_if true  __LINE__ @@ intersect "/x/a*d*e" "/x/a*c*e";
  check_if true  __LINE__ @@ intersect "/x/a*d*e" "/x/ade";
  check_if false __LINE__ @@ intersect "/x/c*" "/x/abc*";
  check_if false __LINE__ @@ intersect "/x/*d" "/x/*e";
  ()

let test_expr_includes () = 
  check_if true  __LINE__ @@ includes ~subexpr:"/a/x" "/a/*";
  check_if true  __LINE__ @@ includes ~subexpr:"/a/*x" "/a/*";
  check_if true  __LINE__ @@ includes ~subexpr:"/a/x*" "/a/*";
  check_if true  __LINE__ @@ includes ~subexpr:"/a/*x*" "/a/*";
  check_if true  __LINE__ @@ includes ~subexpr:"/a/*bx" "/a/*x";
  check_if true  __LINE__ @@ includes ~subexpr:"/a/xb*" "/a/x*";
  check_if true  __LINE__ @@ includes ~subexpr:"/a/x*b*y" "/a/x*y";
  check_if false __LINE__ @@ includes ~subexpr:"/b/x" "/a/*";
  check_if false __LINE__ @@ includes ~subexpr:"/a/*" "/a/x";
  check_if false __LINE__ @@ includes ~subexpr:"/a/*" "/a/x*";
  check_if false __LINE__ @@ includes ~subexpr:"/a/*" "/a/*x";
  check_if false __LINE__ @@ includes ~subexpr:"/a/*" "/a/*x*";
  check_if false __LINE__ @@ includes ~subexpr:"/a/*x" "/a/*bx";
  check_if false __LINE__ @@ includes ~subexpr:"/a/x*" "/a/xb*";
  check_if false __LINE__ @@ includes ~subexpr:"/a/x*y" "/a/x*b*y";

  check_if true  __LINE__ @@ includes ~subexpr:"/a" "/a/**";
  check_if true  __LINE__ @@ includes ~subexpr:"/a/b" "/a/**";
  check_if true  __LINE__ @@ includes ~subexpr:"/a/**" "/a/**";
  check_if true  __LINE__ @@ includes ~subexpr:"/a/**/b" "/a/**";
  check_if true  __LINE__ @@ includes ~subexpr:"/a/b/**" "/a/**";
  check_if true  __LINE__ @@ includes ~subexpr:"/a/b/**" "/a/*/**";
  check_if true  __LINE__ @@ includes ~subexpr:"/a/b/**/c" "/a/**/c";
  check_if true  __LINE__ @@ includes ~subexpr:"/a/**/b/c" "/a/**/c";
  check_if true  __LINE__ @@ includes ~subexpr:"/a/e/**/b/**/d/c" "/a/**/c";
  check_if true  __LINE__ @@ includes ~subexpr:"/a/e/**/b/**/d/c" "/a/**/b/**/c";
  check_if false __LINE__ @@ includes ~subexpr:"/a/**/c/**/c" "/a/**/b/**/c";
  check_if false __LINE__ @@ includes ~subexpr:"/a/**" "/a/b/**";
  check_if false __LINE__ @@ includes ~subexpr:"/a/*/**" "/a/b/**";
  check_if false __LINE__ @@ includes ~subexpr:"/a/**" "/a/*/**";
  check_if false __LINE__ @@ includes ~subexpr:"/a/**/c" "/a/**/b/**/c";
  ()

let test_remaining_after_match () =
  check_if true  __LINE__ @@ remaining_after_match "/a/b/c" "/a/b/c/d/e"      (Some "/d/e");
  check_if true  __LINE__ @@ remaining_after_match "/a/b/c" "/a/b/c/d/**"     (Some "/d/**");
  check_if true  __LINE__ @@ remaining_after_match "/a/b/c" "/a/b/c/**"       (Some "/**");
  check_if true  __LINE__ @@ remaining_after_match "/a/b/c" "/a/b/**"         (Some "/**");
  check_if true  __LINE__ @@ remaining_after_match "/a/b/c" "/a/**"           (Some "/**");
  check_if true  __LINE__ @@ remaining_after_match "/a/b/c" "/a/*/c/d/**"     (Some "/d/**");
  check_if true  __LINE__ @@ remaining_after_match "/a/b/c" "/a/**/b/*/d/**"  (Some "/d/**");
  check_if true  __LINE__ @@ remaining_after_match "/a/b/c" "/a/**/b/**/d/**" (Some "/**/d/**");
  check_if true  __LINE__ @@ remaining_after_match "/a/b/c" "/a/b/c"          (Some "");
  check_if true  __LINE__ @@ remaining_after_match "/a/b/c" "/a/*/c"          (Some "");
  check_if true  __LINE__ @@ remaining_after_match "/a/b/c" "/a/**/c"         (Some "");
  ()

let all_tests = [
  "PathExpr validity", `Quick, test_expr_validity;
  "PathExpr matching path" , `Quick, test_expr_matching_path_simple;
  "PathExpr intersect" , `Quick, test_expr_intersect;
  "PathExpr includes" , `Quick, test_expr_includes;
  "PathExpr remaining_after_match" , `Quick, test_remaining_after_match;
]
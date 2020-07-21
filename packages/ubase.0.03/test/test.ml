let test_viet () =
  print_endline "Vietnamese test...";
  assert (Ubase.from_utf8 "V\197\169 Ng\225\187\141c Phan" = "Vu Ngoc Phan");
  assert (Ubase.from_utf8 "Vu\204\131 Ngo\204\163c Phan" = "Vu Ngoc Phan");
  
  let b = Ubase.from_utf8 "Vũ Ngọc Phan (1902-1987) là nhà văn, nhà nghiên cứu văn học hiện đại và văn học dân gian Việt Nam. Trong những năm đầu cầm bút, ông còn có bút danh là Chỉ Qua Thị."
  =
  "Vu Ngoc Phan (1902-1987) la nha van, nha nghien cuu van hoc hien dai va van hoc dan gian Viet Nam. Trong nhung nam dau cam but, ong con co but danh la Chi Qua Thi." in
  assert b;
  print_endline "OK."

let test_french () =
  print_endline "French test...";
  let b = Ubase.from_utf8 "Cette princesse était belle, quoiqu’elle eût passé la première jeunesse ; elle aimait la grandeur, la magnificence et les plaisirs. Le roi l’avait épousée lorsqu’il était encore duc d’Orléans, et qu’il avait pour aîné le dauphin, qui mourut à Tournon, prince que sa naissance et ses grandes qualités destinaient à remplir dignement la place du roi François premier, son père."
  =
  "Cette princesse etait belle, quoiqu'elle eut passe la premiere jeunesse ; elle aimait la grandeur, la magnificence et les plaisirs. Le roi l'avait epousee lorsqu'il etait encore duc d'Orleans, et qu'il avait pour aine le dauphin, qui mourut a Tournon, prince que sa naissance et ses grandes qualites destinaient a remplir dignement la place du roi Francois premier, son pere." in
  assert b;
  print_endline "OK."

let test_isolatin () =
  print_endline "Conversion from isolatin...";
  assert (Ubase.isolatin_to_utf8 "Li�ge" = "Li\195\168ge");
  print_endline "OK."
  
let () =
  test_viet ();
  test_french ();
  test_isolatin ()

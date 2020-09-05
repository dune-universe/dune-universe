let basic_test () =
  print_endline "A few accented chars...";
  assert (Ubase.from_utf8 "\195\128\195\160\195\129\195\161\195\130\195\162\195\131\195\163\195\132\195\164\195\133\195\165\195\134\195\166\195\135\195\167\195\144\195\176\195\136\195\168\195\137\195\169\195\138\195\170\195\139\195\171\195\140\195\172\195\141\195\173\195\142\195\174\195\143\195\175\195\145\195\177\195\146\195\178\195\147\195\179\195\148\195\180\195\149\195\181\195\150\195\182\197\147\195\152\195\184\195\159\195\153\195\185\195\154\195\186\195\155\195\187\195\156\195\188\195\157\195\189\195\158\195\190\197\184\195\191"
          =
          "AaAaAaAaAaAaAEaeCcDdEeEeEeEeIiIiIiIiNnOoOoOoOoOooeOossUuUuUuUuYyTHthYy");
  print_endline "OK."

let test_viet () =
  print_endline "Vietnamese test...";
  assert (Ubase.from_utf8 "V\197\169 Ng\225\187\141c Phan" = "Vu Ngoc Phan");
  assert (Ubase.from_utf8 "Vu\204\131 Ngo\204\163c Phan" = "Vu Ngoc Phan");

  let b = Ubase.from_utf8 "Vũ Ngọc Phan (1902-1987) là nhà văn, nhà nghiên cứu văn học hiện đại và văn học dân gian Việt Nam. Trong những năm đầu cầm bút, ông còn có bút danh là Chỉ Qua Thị."
          =
          "Vu Ngoc Phan (1902-1987) la nha van, nha nghien cuu van hoc hien dai va van hoc dan gian Viet Nam. Trong nhung nam dau cam but, ong con co but danh la Chi Qua Thi." in
  assert b;
  let t = "Anh xin lỗi các em bé vì đã đề tặng cuốn sách này cho một ông người lớn."
          |> Ubase.from_utf8 in
  assert (t = "Anh xin loi cac em be vi da de tang cuon sach nay cho mot ong nguoi lon.");
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
  basic_test ();
  test_viet ();
  test_french ();
  test_isolatin ()

open Printf

module L = BatList
module LO = Line_oriented

let train_test_split p lines =
  assert(p >= 0.0 && p <= 1.0);
  let n = float (L.length lines) in
  let for_training = BatFloat.round_to_int (p *. n) in
  let train, test = L.takedrop for_training lines in
  assert(L.length train = for_training);
  (train, test)

let csv_header_line nb_features =
  let buff = Buffer.create (10 * nb_features) in
  Buffer.add_string buff "Y";
  for i = 0 to nb_features - 1 do
    bprintf buff " %d" i
  done;
  Buffer.contents buff

(* dump molecules to csv file in dense format *)
let csv_dump csv_fn nb_features mols =
  LO.with_out_file csv_fn (fun out ->
      fprintf out "%s\n" (csv_header_line nb_features);
      L.iter (fun mol ->
          fprintf out "%s\n" (Mol.to_csv_line nb_features mol)
        ) mols
    )

let molecules_of_file fn =
  LO.map fn Mol.of_string

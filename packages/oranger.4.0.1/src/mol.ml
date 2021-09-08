
module A = Array
module Buff = Buffer
module Fp = Molenc.Fingerprint
module Log = Dolog.Log

type t = { name: string;
           value: float; (* regression target (like pIC50) *)
           fp: Fp.t }

let get_name mol =
  mol.name

let get_fp mol =
  mol.fp

let get_value mol =
  mol.value

let set_value mol v =
  { mol with value = v }

let of_string line =
  try Scanf.sscanf line "%s@,%f,%s"
        (fun name value fp_str ->
           let fp = Fp.of_string fp_str in
           { name; value; fp })
  with Scanf.Scan_failure msg ->
    failwith (Printf.sprintf
                "Mol.of_line: wrong line (fmt=%%s@,%%f,%%s): %s: msg: %s"
                line msg)

let to_string x =
  Printf.sprintf "%s,%f,[%s]"
    x.name x.value (Fp.to_string x.fp)

let to_csv_line max_len x =
  let dense = Fp.to_dense max_len x.fp in
  assert(A.length dense = max_len);
  let buff = Buff.create (10 * max_len) in
  (* first value on the line is the activity/pIC50 *)
  Buff.add_string buff (string_of_float x.value);
  A.iter (Printf.bprintf buff " %d") dense;
  Buff.contents buff

(* unit tests suite *)
let () =
  let str_in = "molname,-0.123000,[0:12;1:4;7:4;8:5]" in
  let x = of_string str_in in
  let actual_csv = to_csv_line 9 x in
  let str_out = to_string x in
  let expected_csv = "-0.123 12 4 0 0 0 0 0 4 5" in
  if actual_csv <> expected_csv then
    failwith (Printf.sprintf "%s <> %s" actual_csv expected_csv);
  if str_in <> str_out then
    failwith (Printf.sprintf "%s <> %s" str_in str_out)

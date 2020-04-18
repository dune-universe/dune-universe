open Printf

module L = BatList

let with_in_file fn f =
  let input = open_in_bin fn in
  let res = f input in
  close_in input;
  res

let with_out_file fn f =
  let output = open_out_bin fn in
  let res = f output in
  close_out output;
  res

(* population standard deviation *)
let stddev (l: float list) =
  let n, sx, sx2 =
    List.fold_left (fun (n, sx, sx2) x ->
        (n +. 1., sx +. x, sx2 +. (x *.x))
      ) (0., 0., 0.) l
  in
  sqrt ((sx2 -. (sx *. sx) /. n) /. n)
(* stddev [2.; 4.; 4.; 4.; 5.; 5.; 7.; 9.] = 2.0 *)

let lines_of_file fn =
  with_in_file fn (fun input ->
      let res, exn = L.unfold_exc (fun () -> input_line input) in
      if exn <> End_of_file then
        raise exn
      else res
    )

let filter_lines_of_file fn p =
  L.filter p (lines_of_file fn)

(* create tmp dir with name like: /tmp/[prfx]_XXXXXX *)
let mktemp_dir prfx =
  let ret_code, res =
    BatUnix.run_and_read
      (sprintf "mktemp -d %s/%s_XXXXXX"
         (Filename.get_temp_dir_name ())
         prfx) in
  assert(ret_code = Unix.WEXITED 0);
  BatString.strip res (* get rid of trailing '\n' *)

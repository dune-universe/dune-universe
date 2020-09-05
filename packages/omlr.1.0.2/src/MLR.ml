
module A = BatArray
module Fn = Filename
module L = BatList
module Log = Dolog.Log
module S = BatString

open Printf

type std_param = { mean: float;
                   sd: float }

type std_params = std_param array

type model_param = { mean: float;
                     sd: float;
                     w: float }

type model = model_param array

(* load all lines from given CSV file
   [skip_header] allows to ignore the first line (CSV header), if necessary.
   The first column is supposed to contain the target variable.
   All other columns are supposed to contain the parameters whose weights
   in the MLR model we want to find. *)
let read_csv_file ~randomize ~skip_header csv_fn =
  let all_lines' = match Utls.lines_of_file csv_fn with
    | [] -> failwith ("MLR.load_csv_file: no lines in " ^ csv_fn)
    | x :: xs -> if skip_header then xs else x :: xs in
  if randomize then
    let rng = BatRandom.State.make_self_init () in
    L.shuffle ~state:rng all_lines'
  else
    all_lines'

(* CSV lines to 2D array *)
let matrix_of_csv_lines ~sep all_lines =
  let fst_line = L.hd all_lines in
  let dimx = (S.count_char fst_line sep) + 1 in
  let dimy = L.length all_lines in
  let res = A.make_matrix dimx dimy 0.0 in
  L.iteri (fun y line ->
      let tokens = L.map float_of_string (S.split_on_char sep line) in
      let n = L.length tokens in
      (if n <> dimx then
         failwith (sprintf "MLR.matrix_of_csv_lines: features: %d <> %d: %s"
                     n dimx line)
      );
      L.iteri (fun x feat ->
          res.(x).(y) <- feat
        ) tokens
    ) all_lines;
  res

(* compute the standardization parameters *)
let standardization_params arr =
  let dimx = A.length arr in
  let res = A.make dimx { mean = 0.0; sd = 0.0 } in
  (* the first column (arr.(0)) is the target value;
     it must not be normalized *)
  for x = 1 to dimx - 1 do
    let col = arr.(x) in
    let mean = A.favg col in
    let sd = Utls.stddev_a col in
    res.(x) <- { mean; sd }
  done;
  res

(* apply standardization parameters, in-place *)
let standardize (std_params: std_params) arr =
  let dimx = A.length arr in
  let dimy = A.length arr.(0) in
  (* the first column (arr.(0)) is the target value;
     it must not be normalized *)
  for x = 1 to dimx - 1 do
    let std = std_params.(x) in
    for y = 0 to dimy - 1 do
      let z = arr.(x).(y) in
      arr.(x).(y) <- (z -. std.mean) /. std.sd
    done
  done

let combine_std_params_and_optim_weights
    (std_params: std_params) (weights: float array): model =
  A.map2 (fun (std: std_param) (w: float) ->
      { mean = std.mean; sd = std.sd; w }
    ) std_params weights

let dump_model_to_file fn model =
  Utls.with_out_file fn (fun out ->
      fprintf out "#mean sd w_i (line 0: intercept only)\n";
      A.iteri (fun i x ->
          if i = 0 then (* intercept *)
            let () = assert(x.mean = 0.0 && x.sd = 0.0) in
            fprintf out "%f\n" x.w
          else (* std params and model weights *)
            fprintf out "%f %f %f\n" x.mean x.sd x.w
        ) model
    )

let load_model_from_file fn: model =
  let all_lines = Utls.lines_of_file fn in
  match all_lines with
  | [] -> failwith ("MLR.load_model_from_file: no lines in " ^ fn)
  | [_] -> failwith ("MLR.load_model_from_file: only one line in " ^ fn)
  | _header :: lines ->
    let n = L.length lines in
    let res = A.make n { mean = 0.0; sd = 0.0; w = 0.0 } in
    L.iteri (fun i line ->
        if i = 0 then
          let w = float_of_string line in
          res.(i) <- { mean = 0.0; sd = 0.0; w }
        else try
            Scanf.sscanf line "%f %f %f" (fun mean sd w ->
                res.(i) <- { mean; sd; w }
              )
          with exn ->
            let () =
              Log.fatal "MLR.load_model_from_file: cannot parse: %s" line in
            raise exn
      ) lines;
    res

let regression_formula nb_columns =
  let buff = Buffer.create 80 in
  Buffer.add_string buff "c0 ~ ";
  for i = 1 to nb_columns - 1 do
    if i = 1 then bprintf buff "c%d" i
    else bprintf buff " + c%d" i
  done;
  Buffer.contents buff

(* compute standardization parameters, apply them THEN train a model *)
let train_model ~debug arr =
  let std_params = standardization_params arr in
  standardize std_params arr; (* IMPORTANT *)
  let nb_cols = A.length arr in
  let tmp_out_params_fn = Fn.temp_file ~temp_dir:"/tmp" "mlr_" ".txt" in
  (* dump matrix to file, adding CSV header line "0,1,2,3,4,..." *)
  let tmp_csv_fn = Fn.temp_file ~temp_dir:"/tmp" "mlr_" ".csv" in
  Utls.dump_to_csv_file tmp_csv_fn ',' arr;
  (* create R script *)
  let tmp_rscript_fn = Fn.temp_file ~temp_dir:"/tmp" "mlr_" ".r" in
  let regr_formula = regression_formula nb_cols in
  Utls.with_out_file tmp_rscript_fn (fun out ->
      fprintf out
        "train <- read.csv('%s', header = T, sep = ',')\n\
         model <- lm('%s', data = train)\n\
         write.table(model$coeff, file='%s', sep='\\n', \
                     row.names = F, col.names = F)\n"
        tmp_csv_fn regr_formula tmp_out_params_fn
    );
  let r_log_fn = Filename.temp_file ~temp_dir:"/tmp" "mlr_train_" ".log" in
  (* execute R script *)
  let cmd =
    sprintf "(R --vanilla --slave < %s 2>&1) > %s" tmp_rscript_fn r_log_fn in
  if debug then Log.debug "%s" cmd;
  if Sys.command cmd <> 0 then
    failwith ("MLR.train_model: R failure: " ^ cmd);
  (* extract and return learned weights *)
  let weights = A.of_list (Utls.floats_from_file tmp_out_params_fn) in
  (* clean tmp files *)
  if not debug then
    List.iter Sys.remove
      [tmp_out_params_fn; tmp_csv_fn; tmp_rscript_fn; r_log_fn];
  combine_std_params_and_optim_weights std_params weights

(* /!\ standardize (test) data /!\
   THEN apply the model to a single observation
   !!! i.e. DON'T STANDARDIZE TEST DATA BEFORE !!! *)
let predict_one (model: model) arr =
  (* standardize *)
  let dimx = A.length model in
  (* add line intercept *)
  let res = ref model.(0).w in
  for i = 1 to dimx - 1 do
    (* standardize *)
    let std = model.(i) in
    let feat' = (arr.(i) -. std.mean) /. std.sd in
    (* update prediction *)
    res := !res +. (std.w *. feat')
  done;
  !res

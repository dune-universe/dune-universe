open! Core
open Lacaml.D
open Libsvm

module Scale_parameters = struct
  type t =
    { lower : float;
      upper : float;
      min_feats : vec;
      max_feats : vec;
    }
end

let read_scale_parameters file =
  let conv_line line =
    Scanf.sscanf line "%d %g %g" (fun _index min max -> min, max)
  in
  In_channel.with_file file ~f:(fun ic ->
    match In_channel.input_line ic with
    | None -> failwith "no data"
    | Some line ->
      let lower, upper = Scanf.sscanf line "%g %g" Tuple2.create in
      let lines = In_channel.input_lines ic in
      let min_feats, max_feats =
        List.map lines ~f:conv_line |> Array.of_list |> Array.unzip
      in
      { Scale_parameters.
        lower;
        upper;
        min_feats = Vec.of_array min_feats;
        max_feats = Vec.of_array max_feats;
      })

let scale_cmd =
  Command.basic ~summary:"problem scaling"
    Command.Spec.(
      empty
      +> flag "-l" (optional float) ~doc:" x scaling lower limit (default -1)"
      +> flag "-u" (optional float) ~doc:" x scaling upper limit (default +1)"
      +> flag "-s SAVE-FILE" (optional file)
        ~doc:" save scaling parameters to SAVE-FILE"
      +> flag "-r RESTORE-FILE" (optional file)
        ~doc:" restore scaling parameters from RESTORE-FILE"
      +> anon ("DATA-FILE" %: file)
    )
    (fun lower upper save_file restore_file data_file () ->
       match Result.try_with (fun () -> Svm.Problem.load data_file) with
       | Error exn ->
         prerr_endline (Exn.to_string exn);
         exit 1
       | Ok problem ->
         let params = match restore_file with
           | None ->
             let (`Min min, `Max max) = Svm.Problem.min_max_feats problem in
             { Scale_parameters.
               lower = Option.value lower ~default:(-.1.);
               upper = Option.value upper ~default:(1.);
               min_feats = min;
               max_feats = max;
             }
           | Some file -> read_scale_parameters file
         in
         let lower = params.Scale_parameters.lower in
         let upper = params.Scale_parameters.upper in
         let min_feats = params.Scale_parameters.min_feats in
         let max_feats = params.Scale_parameters.max_feats in
         Option.iter save_file ~f:(fun file ->
           Out_channel.with_file file ~f:(fun oc ->
             let n_feats = Svm.Problem.get_n_feats problem in
             Out_channel.output_string oc (sprintf "%G %G\n" lower upper);
             for i = 1 to n_feats do
               let line = sprintf "%d %g %g\n" i min_feats.{i} max_feats.{i} in
               Out_channel.output_string oc line
             done));
         let scaled_problem =
           Svm.Problem.scale problem ~lower ~upper ~min_feats ~max_feats
         in
         Svm.Problem.output scaled_problem Out_channel.stdout)

module Svm_type = struct
  type t = [ `C_SVC | `NU_SVC | `ONE_CLASS | `EPSILON_SVR | `NU_SVR ] [@@deriving sexp]
  let of_string x = t_of_sexp (Sexp.Atom (String.uppercase x))
  let arg_type = Command.Spec.Arg_type.create of_string
end

module Kernel_type = struct
  type t = [ `LINEAR | `POLY | `RBF | `SIGMOID | `PRECOMPUTED ] [@@deriving sexp]
  let of_string x = t_of_sexp (Sexp.Atom (String.uppercase x))
  let arg_type = Command.Spec.Arg_type.create of_string
end

let train_cmd =
  Command.basic ~summary:"svm training"
    Command.Spec.(
      empty
      +> flag "-s SVM-TYPE" (optional Svm_type.arg_type)
        ~doc:" set the type of SVM (default c_svc)"
      +> flag "-k" (optional Kernel_type.arg_type)
        ~doc:" set the type of kernel function (default rbf)"
      +> flag "-d DEGREE" (optional int)
        ~doc:" set degree in kernel function (default 3)"
      +> flag "-g GAMMA" (optional float)
        ~doc:" set gamma in kernel function (default 1/num_features)"
      +> flag "-r COEF0" (optional float)
        ~doc:"set coef0 in kernel function (default 0)"
      +> flag "-c COST" (optional float)
        ~doc:" set the parameter C of c-svc, epsilon-svr and nu-svr (default 1)"
      +> flag "-n NU" (optional float)
        ~doc:" set the parameter nu of nu-svc, one-class and nu-svr (default 0.5)"
      +> flag "-e EPSILON" (optional float)
        ~doc:" set the epsilon in loss function of epsilon-svr (default 0.1)"
      +> flag "-m CACHESIZE" (optional float)
        ~doc:" set cache memory size in MB (default 100)"
      +> flag "-t TOLERANCE" (optional float)
        ~doc:" set the tolerance and termination criterion (default 0.001)"
      +> flag "-h" no_arg
        ~doc:" turn this on when shrinking heuristics should not be used"
      +> flag "-p" no_arg
        ~doc:" turn this on to train a svc or svr model with probability estimates"
      +> flag "-v N" (optional int)
        ~doc:" N-fold cross validation mode"
      +> flag "-q" no_arg
        ~doc:" quiet mode (no ouputs)"
      +> anon ("TRAINING-SET-FILE" %: file)
      +> anon (maybe ("MODEL-FILE" %: file))
    )
    (fun svm_type kernel degree gamma coef0 c nu eps cachesize tol
      turn_shrinking_off probability n_folds quiet training_set_file model_file () ->
      match Result.try_with (fun () -> Svm.Problem.load training_set_file) with
      | Error exn ->
        prerr_endline (Exn.to_string exn);
        exit 1
      | Ok problem ->
        match n_folds with
        | None ->
          let model = Svm.train
              ?svm_type
              ?kernel
              ?degree
              ?gamma
              ?coef0
              ?c
              ?nu
              ?eps
              ?cachesize
              ?tol
              ~shrinking:(if turn_shrinking_off then `off else `on)
              ~probability
              ~verbose:(not quiet)
              problem
          in
          let model_file = Option.value model_file
              ~default:(sprintf "%s.model" training_set_file)
          in
          Svm.Model.save model model_file
        | Some n_folds ->
          let predicted = Svm.cross_validation
              ?svm_type
              ?kernel
              ?degree
              ?gamma
              ?coef0
              ?c
              ?nu
              ?eps
              ?cachesize
              ?tol
              ~shrinking:(if turn_shrinking_off then `off else `on)
              ~probability
              ~verbose:(not quiet)
              ~n_folds
              problem
          in
          let expected = Svm.Problem.get_targets problem in
          match Option.value svm_type ~default:`C_SVC with
          | `C_SVC | `NU_SVC | `ONE_CLASS ->
            let accuracy = Stats.calc_accuracy expected predicted in
            printf "Cross Validation Accuracy = %g%%\n" (100. *. accuracy)
          | `EPSILON_SVR | `NU_SVR ->
            let mse = Stats.calc_mse expected predicted in
            let scc = Stats.calc_scc expected predicted in
            printf "Cross Validation Mean squared error = %g\n" mse;
            printf "Cross Validation Squared correlation coefficient = %g\n" scc)

let predict_cmd =
  Command.basic ~summary:"svm prediction"
    Command.Spec.(
      empty
      (* +> flag "-b" no_arg *)
      (*   ~doc:"wether to predict probability estimates, 0 or 1 (default 0); \\ *)
           (*         for one-class SVM only 0 is supported" *)
      +> anon ("TEST-SET-FILE" %: file)
      +> anon ("MODEL-FILE" %: file)
      +> anon ("OUTPUT-FILE" %: file)
    )
    (fun (* probability *) test_set_file model_file output_file () ->
       let model = Svm.Model.load model_file in
       match Result.try_with (fun () -> Svm.predict_from_file model test_set_file) with
       | Error exn ->
         prerr_endline (Exn.to_string exn);
         exit 1
       | Ok (`Expected expected, `Predicted predicted) ->
         let n_samples = Vec.dim predicted in
         Out_channel.with_file output_file ~f:(fun oc ->
           for i = 1 to n_samples do
             Out_channel.output_string oc (sprintf "%g\n" predicted.{i})
           done);
         match Svm.Model.get_svm_type model with
         | `C_SVC | `NU_SVC | `ONE_CLASS ->
           let n_correct = Stats.calc_n_correct expected predicted in
           let accuracy = Float.(of_int n_correct / of_int n_samples * 100.) in
           printf "Accuracy = %g%% (%d/%d) (classification)\n" accuracy n_correct n_samples
         | `EPSILON_SVR | `NU_SVR ->
           let mse = Stats.calc_mse expected predicted in
           let scc = Stats.calc_scc expected predicted in
           printf "Mean squared error = %g (regression)\n" mse;
           printf "Squared correlation coefficient = %g (regression)\n" scc)

let () =
  Exn.handle_uncaught ~exit:true (fun () ->
    Command.run ~version:"0.9.2" ~build_info:"N/A"
      (Command.group ~summary:"Command line tools for Libsvm"
         [ "scale"  , scale_cmd
         ; "train"  , train_cmd
         ; "predict", predict_cmd
         ]))

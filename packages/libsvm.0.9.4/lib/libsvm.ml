(*
   LIBSVM-OCaml - OCaml-bindings to the LIBSVM library

   Copyright (C) 2013-  Oliver Gu
   email: gu.oliver@yahoo.com

   Copyright (C) 2005  Dominik Brugger
   email: dominikbrugger@fastmail.fm
   WWW:   http://ocaml-libsvm.berlios.de

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open! Base
open Stdio
open Lacaml.D
open Printf

(* base doesn't include this stuff as opposed to core. It's small enough that we
   can just include it *)
module Float = struct
  include Float
  let robust_comparison_tolerance = 1E-7
  let ( >=. ) x y = x >= y - robust_comparison_tolerance
  let ( <=. ) x y = y >=. x
  let ( =. ) x y = x >=. y && y >=. x
end

module Svm = struct
  type problem
  type params
  type model

  type svm_type =
    | C_SVC
    | NU_SVC
    | ONE_CLASS
    | EPSILON_SVR
    | NU_SVR

  type kernel_type =
    | LINEAR
    | POLY
    | RBF
    | SIGMOID
    | PRECOMPUTED

  type svm_params = {
    svm_type : svm_type;
    kernel_type : kernel_type;
    degree : int;
    gamma : float;
    coef0 : float;
    c : float;
    nu : float;
    eps : float;
    cachesize : float;
    tol : float;
    shrinking : bool;
    probability : bool;
    nr_weight : int;
    weight_label : int list;
    weight : float list;
  }

  module Stub = struct
    type double_array
    type svm_node_array
    type svm_node_matrix

    external double_array_create :
      int -> double_array = "double_array_create_stub"
    external double_array_set :
      double_array -> int -> float -> unit = "double_array_set_stub"
    external svm_node_array_create :
      int -> svm_node_array = "svm_node_array_create_stub"
    external svm_node_array_set :
      svm_node_array -> int -> int -> float -> unit = "svm_node_array_set_stub"
    external svm_node_matrix_create :
      int -> svm_node_matrix = "svm_node_matrix_create_stub"
    external svm_node_matrix_set :
      svm_node_matrix -> int -> svm_node_array -> unit = "svm_node_matrix_set_stub"
    external svm_problem_create :
      unit -> problem = "svm_problem_create_stub"
    external svm_problem_l_set :
      problem -> int -> unit = "svm_problem_l_set_stub"
    external svm_problem_y_set :
      problem -> double_array -> unit = "svm_problem_y_set_stub"
    external svm_problem_y_get :
      problem -> int -> float = "svm_problem_y_get_stub"
    external svm_problem_x_set :
      problem -> svm_node_matrix -> unit = "svm_problem_x_set_stub"
    external svm_problem_x_get :
      problem -> int -> int -> (int * float) = "svm_problem_x_get_stub"
    external svm_problem_width :
      problem -> int -> int = "svm_problem_width_stub"
    external svm_problem_print :
      problem -> unit = "svm_problem_print_stub"
    external svm_param_create : svm_params -> params = "svm_param_create_stub"

    external svm_set_quiet_mode : unit -> unit = "svm_set_quiet_mode_stub"
    external svm_train : problem -> params -> model = "svm_train_stub"
    external svm_cross_validation :
      problem -> params -> int -> vec = "svm_cross_validation_stub"

    external svm_save_model : string -> model -> unit = "svm_save_model_stub"
    external svm_load_model : string -> model = "svm_load_model_stub"

    external svm_get_svm_type : model -> svm_type = "svm_get_svm_type_stub"
    external svm_get_kernel_type : model -> kernel_type = "svm_get_kernel_type_stub"
    external svm_get_nr_class : model -> int = "svm_get_nr_class_stub"
    external svm_get_labels : model -> int list = "svm_get_labels_stub"
    external svm_get_nr_sv : model -> int = "svm_get_nr_sv_stub"
    external svm_get_svr_probability :
      model -> float = "svm_get_svr_probability_stub"
    external svm_check_probability_model :
      model -> bool = "svm_check_probability_model_stub"

    external svm_predict_values :
      model -> svm_node_array -> float array = "svm_predict_values_stub"
    external svm_predict :
      model -> svm_node_array -> float = "svm_predict_stub"
    external svm_predict_probability :
      model -> svm_node_array -> float * float array = "svm_predict_probability_stub"
  end

  (* This function skips all entries with zero
     value and creates a sparse svm node array. *)
  let sparse_svm_node_array_of_vec v =
    let count_nonzeros v = Vec.fold (fun count x ->
      count + if Float.(x <> 0.) then 1 else 0) 0 v
    in
    let size = count_nonzeros v + 1 in
    let nodes = Stub.svm_node_array_create size in
    let pos = ref 0 in
    Vec.iteri (fun index value ->
      if Float.(value <> 0.) then begin
        Stub.svm_node_array_set nodes !pos index value;
        Caml.incr pos
      end) v;
    Stub.svm_node_array_set nodes !pos (-1) 0.;
    nodes

  let svm_node_array_of_vec v =
    let n = Vec.dim v in
    let nodes = Stub.svm_node_array_create (n+1) in
    Vec.iteri (fun index value ->
      let pos = index-1 in
      Stub.svm_node_array_set nodes pos pos value) v;
    Stub.svm_node_array_set nodes n (-1) 0.;
    nodes

  let svm_node_array_of_list l ~len =
    let size = len + 1 in
    let nodes = Stub.svm_node_array_create size in
    List.iteri l ~f:(fun pos (index, value) ->
      Stub.svm_node_array_set nodes pos index value);
    Stub.svm_node_array_set nodes len (-1) 0.;
    nodes

  let count_lines file =
    In_channel.with_file file ~f:(fun ic ->
      In_channel.fold_lines ic ~init:0 ~f:(fun count _line -> count + 1))

  let parse_line file = Staged.stage (fun line ~pos ->
    let result = Result.try_with (fun () ->
      match String.rstrip line |> String.split ~on:' ' with
      | [] -> assert false
      | x :: xs ->
        let target = Float.of_string x in
        let feats = List.map xs ~f:(fun str ->
          let index, value = String.lsplit2_exn str ~on:':' in
          Int.of_string index, Float.of_string value)
        in
        target, feats)
    in
    match result with
    | Ok x -> x
    | Error exn ->
      failwithf "%s: wrong input format at line %d: %s" file pos (Exn.to_string exn) ())

  module Problem = struct
    type t = {
      n_samples : int;
      n_feats : int;
      prob : problem;
    }

    let get_n_samples t = t.n_samples
    let get_n_feats t = t.n_feats

    let create_gen x y ~f =
      let n_samples = Mat.dim1 x in
      let n_feats = Mat.dim2 x in
      let x' = Mat.transpose_copy x in
      let m = Stub.svm_node_matrix_create n_samples in
      let v = Stub.double_array_create n_samples in
      for i = 1 to n_samples do
        let x_row = Mat.col x' i in
        Stub.svm_node_matrix_set m (i-1) (f x_row);
        Stub.double_array_set v (i-1) y.{i}
      done;
      let prob = Stub.svm_problem_create () in
      Stub.svm_problem_l_set prob n_samples;
      Stub.svm_problem_x_set prob m;
      Stub.svm_problem_y_set prob v;
      { n_samples;
        n_feats;
        prob;
      }

    let create ~x ~y = create_gen x y ~f:sparse_svm_node_array_of_vec

    let create_k ~k ~y = create_gen k y ~f:svm_node_array_of_vec

    let load file =
      let n_samples = count_lines file in
      let n_feats = ref 0 in
      let x = Stub.svm_node_matrix_create n_samples in
      let y = Stub.double_array_create n_samples in
      In_channel.with_file file ~f:(fun ic ->
        let parse_line = Staged.unstage (parse_line file) in
        let rec loop i =
          match In_channel.input_line ic with
          | None -> ()
          | Some line ->
            let target, feats = parse_line line ~pos:i in
            Stub.double_array_set y (i-1) target;
            let len = List.length feats in
            Stub.svm_node_matrix_set x (i-1) (svm_node_array_of_list feats ~len);
            n_feats := max !n_feats len;
            loop (i+1)
        in
        loop 1);
      let prob = Stub.svm_problem_create () in
      Stub.svm_problem_l_set prob n_samples;
      Stub.svm_problem_x_set prob x;
      Stub.svm_problem_y_set prob y;
      { n_samples;
        n_feats = !n_feats;
        prob;
      }

    let get_targets t =
      let n = t.n_samples in
      let y = Vec.create n in
      for i = 1 to n do
        y.{i} <- Stub.svm_problem_y_get t.prob (i-1)
      done;
      y

    let output t oc =
      let buf = Buffer.create 1024 in
      for i = 0 to t.n_samples-1 do
        Buffer.add_string buf (sprintf "%g" (Stub.svm_problem_y_get t.prob i));
        let width = Stub.svm_problem_width t.prob i in
        for j = 0 to width-1 do
          let index, value = Stub.svm_problem_x_get t.prob i j in
          Buffer.add_string buf (sprintf " %d:%g" index value);
        done;
        Buffer.add_char buf '\n';
        Out_channel.output_string oc (Buffer.contents buf);
        Buffer.clear buf
      done;
      Out_channel.flush oc

    let save t file = Out_channel.with_file file ~f:(fun oc -> output t oc)

    let min_max_feats t =
      let min_feats = Vec.make t.n_feats Float.infinity in
      let max_feats = Vec.make t.n_feats Float.neg_infinity in
      for i = 0 to t.n_samples-1 do
        let width = Stub.svm_problem_width t.prob i in
        for j = 0 to width-1 do
          let index, value = Stub.svm_problem_x_get t.prob i j in
          min_feats.{index} <- Float.min min_feats.{index} value;
          max_feats.{index} <- Float.max max_feats.{index} value;
        done;
      done;
      (`Min min_feats, `Max max_feats)

    let scale ?(lower= -.1.) ?(upper=1.) t ~min_feats ~max_feats =
      let n_samples = t.n_samples in
      let x = Stub.svm_node_matrix_create n_samples in
      let y = Stub.double_array_create n_samples in
      for i = 0 to n_samples-1 do
        let width = Stub.svm_problem_width t.prob i in
        let nodes = Stub.svm_node_array_create (width+1) in
        for j = 0 to width-1 do
          let index, value = Stub.svm_problem_x_get t.prob i j in
          if Float.(=.) value min_feats.{index} then
            Stub.svm_node_array_set nodes j index lower
          else if Float.(=.) value max_feats.{index} then
            Stub.svm_node_array_set nodes j index upper
          else
            let new_value =
              let open Float in
              lower +
              (upper - lower) *
              (value - min_feats.{index}) /
              (max_feats.{index} - min_feats.{index})
            in
            Stub.svm_node_array_set nodes j index new_value
        done;
        Stub.svm_node_array_set nodes width (-1) 0.;
        Stub.svm_node_matrix_set x i nodes;
        Stub.double_array_set y i (Stub.svm_problem_y_get t.prob i);
      done;
      let scaled_prob = Stub.svm_problem_create () in
      Stub.svm_problem_l_set scaled_prob n_samples;
      Stub.svm_problem_x_set scaled_prob x;
      Stub.svm_problem_y_set scaled_prob y;
      { n_samples;
        n_feats = t.n_feats;
        prob = scaled_prob;
      }

    let print t = Stub.svm_problem_print t.prob
  end

  module Model = struct
    type t = model

    let get_svm_type t =
      match Stub.svm_get_svm_type t with
      | C_SVC       -> `C_SVC
      | NU_SVC      -> `NU_SVC
      | ONE_CLASS   -> `ONE_CLASS
      | EPSILON_SVR -> `EPSILON_SVR
      | NU_SVR      -> `NU_SVR

    let get_n_classes t = Stub.svm_get_nr_class t

    let get_labels t =
      match Stub.svm_get_svm_type t with
      | NU_SVR | EPSILON_SVR | ONE_CLASS ->
        invalid_arg "Cannot return labels for a regression or one-class model."
      | _ -> Stub.svm_get_labels t

    let get_n_sv t = Stub.svm_get_nr_sv t

    let get_svr_probability t =
      match Stub.svm_get_svm_type t with
      | EPSILON_SVR | NU_SVR -> Stub.svm_get_svr_probability t
      | _ -> invalid_arg "The model is no regression model."

    let save t filename = Stub.svm_save_model filename t
    let load filename = Stub.svm_load_model filename
  end

  let create_params ~svm_type ~kernel ~degree ~gamma ~coef0 ~c
      ~nu ~eps ~cachesize ~tol ~shrinking ~probability ~weights =
    let svm_type = match svm_type with
      | `C_SVC       -> C_SVC
      | `NU_SVC      -> NU_SVC
      | `ONE_CLASS   -> ONE_CLASS
      | `EPSILON_SVR -> EPSILON_SVR
      | `NU_SVR      -> NU_SVR
    in
    let kernel_type = match kernel with
      | `LINEAR      -> LINEAR
      | `POLY        -> POLY
      | `RBF         -> RBF
      | `SIGMOID     -> SIGMOID
      | `PRECOMPUTED -> PRECOMPUTED
    in
    let shrinking = match shrinking with
      | `on  -> true
      | `off -> false
    in
    let weight_label, weight = List.unzip weights in
    Stub.svm_param_create {
      svm_type;
      kernel_type;
      degree;
      gamma;
      coef0;
      c;
      nu;
      eps;
      cachesize;
      tol;
      shrinking;
      probability;
      nr_weight = List.length weight;
      weight_label;
      weight;
    }

  let train
      ?(svm_type=`C_SVC)
      ?(kernel=`RBF)
      ?(degree=3)
      ?gamma
      ?(coef0=0.)
      ?(c=1.)
      ?(nu=0.5)
      ?(eps=0.1)
      ?(cachesize=100.)
      ?(tol=1e-3)
      ?(shrinking=`on)
      ?(probability=false)
      ?(weights=[])
      ?(verbose=false)
      problem =
    let params = create_params
        ~gamma:(Option.value gamma ~default:(Float.(1. / Caml.float problem.Problem.n_feats)))
        ~svm_type ~kernel ~degree ~coef0 ~c ~nu ~eps
        ~cachesize ~tol ~shrinking ~probability ~weights
    in
    if not verbose then Stub.svm_set_quiet_mode () else ();
    Stub.svm_train problem.Problem.prob params

  let cross_validation
      ?(svm_type=`C_SVC)
      ?(kernel=`RBF)
      ?(degree=3)
      ?gamma
      ?(coef0=0.)
      ?(c=1.)
      ?(nu=0.5)
      ?(eps=0.1)
      ?(cachesize=100.)
      ?(tol=1e-3)
      ?(shrinking=`on)
      ?(probability=false)
      ?(weights=[])
      ?(verbose=false)
      ~n_folds problem =
    let params = create_params
        ~gamma:(Option.value gamma ~default:(Float.(1. / Caml.float problem.Problem.n_feats)))
        ~svm_type ~kernel ~degree ~coef0 ~c ~nu ~eps
        ~cachesize ~tol ~shrinking ~probability ~weights
    in
    if not verbose then Stub.svm_set_quiet_mode () else ();
    Stub.svm_cross_validation problem.Problem.prob params n_folds

  let predict_one model ~x =
    let nodes = match Stub.svm_get_kernel_type model with
      | PRECOMPUTED -> svm_node_array_of_vec x
      | _ -> sparse_svm_node_array_of_vec x
    in
    Stub.svm_predict model nodes

  let predict model ~x =
    let n = Mat.dim1 x in
    let y = Vec.create n in
    let x' = Mat.transpose_copy x in
    for i = 1 to n do
      y.{i} <- predict_one model ~x:(Mat.col x' i)
    done;
    y

  let predict_values model ~x =
    let nodes = match Stub.svm_get_kernel_type model with
      | PRECOMPUTED -> svm_node_array_of_vec x
      | _ -> sparse_svm_node_array_of_vec x
    in
    let dec_vals = Stub.svm_predict_values model nodes in
    match Stub.svm_get_svm_type model with
    | EPSILON_SVR | NU_SVR | ONE_CLASS ->
      Array.make_matrix ~dimx:1 ~dimy:1 dec_vals.(0)
    | C_SVC | NU_SVC ->
      let n_classes = Stub.svm_get_nr_class model in
      let dec_mat = Array.make_matrix ~dimx:n_classes ~dimy:n_classes 0. in
      let count = ref 0 in
      for i = 0 to n_classes-1 do
        for j = i+1 to n_classes-1 do
          Float.(dec_mat.(i).(j) <-  dec_vals.(!count));
          Float.(dec_mat.(j).(i) <- -dec_vals.(!count));
          Caml.incr count
        done
      done;
      dec_mat

  let predict_probability model ~x =
    match Stub.svm_get_svm_type model with
    | EPSILON_SVR | NU_SVR ->
      invalid_arg "For probability estimates call Model.get_svr_probability."
    | ONE_CLASS ->
      invalid_arg "One-class problems do not support probability estimates."
    | C_SVC | NU_SVC ->
      if Stub.svm_check_probability_model model then
        let nodes = match Stub.svm_get_kernel_type model with
          | PRECOMPUTED -> svm_node_array_of_vec x
          | _ -> sparse_svm_node_array_of_vec x
        in
        Stub.svm_predict_probability model nodes
      else
        invalid_arg "Model does not support probability estimates."

  let predict_from_file model file =
    let n_samples = count_lines file in
    let expected = Vec.create n_samples in
    let predicted = Vec.create n_samples in
    In_channel.with_file file ~f:(fun ic ->
      let parse_line = Staged.unstage (parse_line file) in
      let rec loop i =
        match In_channel.input_line ic with
        | None -> (`Expected expected, `Predicted predicted)
        | Some line ->
          let target, feats = parse_line line ~pos:i in
          expected.{i} <- target;
          let nodes = svm_node_array_of_list feats ~len:(List.length feats) in
          predicted.{i} <- Stub.svm_predict model nodes;
          loop (i+1)
      in
      loop 1)
end

module Stats = struct

  let check_dimension x y ~location =
    let dimx = Vec.dim x in
    let dimy = Vec.dim y in
    if dimx <> dimy then
      invalid_argf "dimension mismatch in Stats.%s: %d <> %d" location dimx dimy ()
    else ()

  let calc_n_correct x y =
    check_dimension x y ~location:"calc_n_correct";
    Vec.fold (fun count x -> count + if Float.(x = 0.) then 1 else 0) 0 (Vec.sub x y)

  let calc_accuracy x y =
    check_dimension x y ~location:"calc_accuracy";
    let l = Vec.dim x in
    let n_correct = calc_n_correct x y in
    Float.(Caml.float n_correct / Caml.float l)

  let calc_mse x y =
    check_dimension x y ~location:"calc_mse";
    let l = Vec.dim x in
    Float.(Vec.ssqr_diff x y / Caml.(float l))

  let calc_scc x y =
    check_dimension x y ~location:"calc_scc";
    let l = Vec.dim x in
    let sum_x  = ref 0. in
    let sum_y  = ref 0. in
    let sum_xx = ref 0. in
    let sum_yy = ref 0. in
    let sum_xy = ref 0. in
    for i = 1 to l do
      let open Float in
      sum_x  := !sum_x  + x.{i};
      sum_y  := !sum_y  + y.{i};
      sum_xx := !sum_xx + x.{i} * x.{i};
      sum_yy := !sum_yy + y.{i} * y.{i};
      sum_xy := !sum_xy + x.{i} * y.{i};
    done;
    let sqr x = Float.(x * x) in
    let l = Caml.float l in
    Float.(
      sqr (l * !sum_xy - !sum_x * !sum_y) /
      ((l * !sum_xx - sqr !sum_x) * (l * !sum_yy - sqr !sum_y))
    )
end

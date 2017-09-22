open Lacaml.D
open Libsvm

module A = Archimedes

(* This program uses Support Vector Regression to approximate
   the log function (green) from its noisy observations (red).

   To compile it, please install the additional packages:
   1. archimedes: opam install archimedes
   2. gsl: opam install gsl
*)

let a = 0.1
let b = 6.0
let n_inputs = 99
let sigma = 0.2

let gen_data () =
  let default_rng = Gsl.Rng.make (Gsl.Rng.default ()) in
  let noise = Vec.init n_inputs (fun _ -> Gsl.Randist.gaussian default_rng ~sigma) in
  let inputs = Vec.linspace a b n_inputs in
  let targets = Vec.add (Vec.map log inputs) noise in
  Mat.from_col_vec inputs, targets

let () =
  let inputs, targets = gen_data () in
  let problem = Svm.Problem.create ~x:inputs ~y:targets in
  let model = Svm.train ~svm_type:`NU_SVR problem in
  let preds = Svm.predict model ~x:inputs in
  let inputs = Mat.as_vec inputs in
  let p = A.init [] ~w:600. ~h:600. in
  A.Axes.box p;
  A.set_line_width p 2.;
  A.set_color p A.Color.green;
  A.fx p log a b;
  A.set_color p A.Color.red;
  A.Vec.xy p inputs targets;
  A.set_color p A.Color.blue;
  A.Vec.xy p inputs preds;
  A.close p

(*
   Download test data:
   $ wget https://raw.githubusercontent.com/cjlin1/libsvm/master/heart_scale

$python
Python 2.7.15+ (default, Feb  3 2019, 13:13:16)
[GCC 8.2.0] on linux2
Type "help", "copyright", "credits" or "license" for more information.
>>> from svmutil import *
>>> y, x = svm_read_problem('heart_scale')
>>> m = svm_train(y[:200], x[:200], '-c 4')
*.*
optimization finished, #iter = 257
nu = 0.351161
obj = -225.628984, rho = 0.636110
nSV = 91, nBSV = 49
Total nSV = 91
>>> p_label, p_acc, p_val = svm_predict(y[200:], x[200:], m)
Accuracy = 84.2857% (59/70) (classification)

*)
open Base
open Libsvm

let x, y = Svm.load_dataset "heart_scale"
let x_train = Array.sub x ~pos:0 ~len:200
let y_train = Bigarray.Array1.sub y 1 200
let prob = Svm.Problem.create ~x:x_train ~y:y_train
let model = Svm.train ~svm_type:`C_SVC ~c:4. prob
let x_test = Array.sub x ~pos:200 ~len:70
let y_test = Bigarray.Array1.sub y 201 70
let y_pred = Array.map x_test ~f:(Svm.predict_sparse model)
let () =
  Stats.calc_accuracy (Lacaml.D.Vec.of_array y_pred) y_test
  |> Caml.Printf.printf "%f\n%!"

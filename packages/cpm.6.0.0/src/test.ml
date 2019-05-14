
open Printf

(* Example usage *)

(* first, define your score_label module *)
module SL = struct
  type t = string * float * int * bool
  let get_score (_, s, _, _) = s
  let get_label (_, _, _, l) = l
end

(* second, instantiate the ROC functor for your score_label module *)
module ROC = Cpm.MakeROC.Make (SL)

let almost_equal epsilon x_curr x_ref =
  (x_ref -. epsilon <= x_curr) && (x_curr <= x_ref +. epsilon)

(* third, call any classification performance metric you need *)
let main () =
  let create name score index label =
    (name, score, index, label) in
  let scores =
    [ create "" 14.0 0 true;
      create "" 13.0 0 true;
      create "" 12.0 0 false;
      create "" 11.0 0 true;
      create "" 10.0 0 false;
      create ""  9.0 0 false;
      create ""  8.0 0 false;
      create ""  7.0 0 false;
      create ""  6.0 0 true;
      create ""  5.0 0 false;
      create ""  4.0 0 false;
      create ""  3.0 0 false;
      create ""  2.0 0 false;
      create ""  1.0 0 false ] in
  let tpr_x = 3. /. 4. in
  let fpr_x = (5. -. 3.) /. (14. -. 4.) in
  assert(ROC.power_metric 0.35 scores = tpr_x /. (tpr_x +. fpr_x));
  assert(ROC.auc [("", 1.0, 0, true) ; (("", 0.9, 1, false))] = 1.0);
  assert(ROC.auc [("", 1.0, 0, false); (("", 0.9, 1, true))] = 0.0);
  assert(ROC.auc_a [|("", 1.0, 0, true) ; (("", 0.9, 1, false))|] = 1.0);
  assert(ROC.auc_a [|("", 1.0, 0, false); (("", 0.9, 1, true))|] = 0.0);
  let scores_02 =
    [ create "" 0.1 0 false;
      create "" 0.2 0 false;
      create "" 0.3 0 true;
      create "" 0.4 0 false;
      create "" 0.5 0 false;
      create "" 0.6 0 true;
      create "" 0.7 0 true;
      create "" 0.8 0 true;
      create "" 0.9 0 false;
      create "" 1.0 0 true ] in
  (* cross validated with 'croc-curve < test.scored-label > /dev/null' *)
  assert(ROC.auc scores_02 = 0.76);
  assert(ROC.auc_a (Array.of_list scores_02) = 0.76);
  (* cross validated with 'croc-bedroc < test.scored-label > /dev/null' *)
  assert(almost_equal 0.0001 (ROC.bedroc_auc scores_02) 0.88297);
  (* wikipedia example:
     https://en.wikipedia.org/wiki/Matthews_correlation_coefficient *)
  let tp, fp, tn, fn = 90., 4., 1., 5. in
  let scores_03 =
    (List.init 90 (fun i -> create "" 1.0 i true)) @
    (List.init 4 (fun i -> create "" 1.0 i false)) @
    [create "" 0.0 0 false] @
    (List.init 5 (fun i -> create "" 0.0 i true)) in
  let mcc = ROC.mcc 0.5 scores_03 in
  assert(mcc = ((tp *. tn -. fp *. fn) /.
                sqrt ((tp +. fp) *. (tp +. fn) *. (tn +. fp) *. (tn +. fn))));
  assert(almost_equal 0.0000001 mcc 0.135242);
  printf "all OK\n"

let () = main ()

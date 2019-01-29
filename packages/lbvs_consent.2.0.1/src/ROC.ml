(* functions for ROC analysis *)

module L = BatList
module Float = BatFloat

let trapezoid_surface x1 x2 y1 y2 =
  let base = abs_float (x1 -. x2) in
  let height = 0.5 *. (y1 +. y2) in
  base *. height

(* put molecules with the highest scores at the top of the list *)
let rank_order_by_score score_labels =
  L.sort (fun (_n1, s1, _i1, _l1) (_n2, s2, _i2, _l2) ->
      Float.compare s2 s1
    ) score_labels

(* compute the cumulated number of actives curve,
   given an already sorted list of score labels *)
let cumulated_number_actives high_scores_first =
  let sum = ref 0 in
  L.map
    (fun (_name, _score, _index, label) ->
       if label then incr sum;
       !sum
    ) high_scores_first

(* area under the ROC curve given an unsorted list of score-labels
   TP cases have the label set to true
   TN cases have the label unset *)
let auc score_labels =
  let high_scores_first = rank_order_by_score score_labels in
  let fp, tp, fp_prev, tp_prev, a, _p_prev =
    L.fold_left
      (fun (fp, tp, fp_prev, tp_prev, a, p_prev) (_ni, si, _ii, li) ->
        let new_a, new_p_prev, new_fp_prev, new_tp_prev =
          if si <> p_prev then
            a +. trapezoid_surface fp fp_prev tp tp_prev,
            si,
            fp,
            tp
          else
            a,
            p_prev,
            fp_prev,
            tp_prev
        in
        let new_tp, new_fp =
          if li then
            tp +. 1., fp
          else
            tp, fp +. 1.
        in
        (new_fp, new_tp, new_fp_prev, new_tp_prev, new_a, new_p_prev)
      )
      (0., 0., 0., 0., 0., neg_infinity)
      high_scores_first
  in
  let cum_curve = cumulated_number_actives high_scores_first in
  ((a +. trapezoid_surface fp fp_prev tp tp_prev) /. (fp *. tp),
   cum_curve)
(*$T auc
  fst (auc [("", 1.0, 0, true) ; (("", 0.9, 1, false))]) = 1.0
  fst (auc [("", 1.0, 0, false); (("", 0.9, 1, true))]) = 0.0
*)

(* proportion of actives given an unsorted list of score-labels
   TP cases have the label set to true
   TN cases have the label unset
   returns: (nb_molecules, actives_rate) *)
let actives_rate score_labels =
  let tp_count, fp_count =
    L.fold_left
      (fun (tp_c, fp_c) (_name, _score, _index, label) ->
         if label then
           (tp_c + 1, fp_c)
         else
           (tp_c, fp_c + 1)
      )
      (0, 0)
      score_labels
  in
  let nb_molecules = tp_count + fp_count in
  (nb_molecules, (float tp_count) /. (float nb_molecules))

(* enrichment rate at x (e.g. x = 0.01 --> ER @ 1%) given a list
   of unsorted score-labels
   returns: (top_n, top_actives_rate, rand_actives_rate, enr_rate) *)
let enr_rate p score_labels =
  let nb_molecules, rand_actives_rate = actives_rate score_labels in
  let top_n = Float.round_to_int (p *. (float nb_molecules)) in
  let top_p_percent_molecules =
    L.take top_n (rank_order_by_score score_labels) in
  let _, top_actives_rate = actives_rate top_p_percent_molecules in
  let enr_rate = top_actives_rate /. rand_actives_rate in
  (top_n, top_actives_rate, rand_actives_rate, enr_rate)

let only_ER p score_labels =
  let _, _, _, er = enr_rate p score_labels in
  er

(* Cf. http://jcheminf.springeropen.com/articles/10.1186/s13321-016-0189-4 for formulas:
   The power metric: a new statistically robust enrichment-type metric for
   virtual screening applications with early recovery capability
   Lopes et. al. Journal of Cheminformatics 2017 *)
let power_metric (cutoff: float) (scores_tot: Score_label.t list): float =
  let nb_actives l =
    L.length (L.filter Score_label.is_active l)
  in
  assert(cutoff > 0.0 && cutoff <= 1.0);
  let size_tot = float (List.length scores_tot) in
  let x = MyUtils.round (cutoff *. size_tot) in
  let size_x = int_of_float x in
  assert(size_x >= 1);
  let sorted = rank_order_by_score scores_tot in
  let scores_x = L.take size_x sorted in
  let actives_x = float (nb_actives scores_x) in
  let actives_tot = float (nb_actives scores_tot) in
  let tpr_x = actives_x /. actives_tot in
  let fpr_x = (x -. actives_x) /. (size_tot -. actives_tot) in
  tpr_x /. (tpr_x +. fpr_x)

(*$T power_metric
  let scores = \
    Score_label.[ create "" 14.0 0 true; \
                  create "" 13.0 0 true; \
                  create "" 12.0 0 false; \
                  create "" 11.0 0 true; \
                  create "" 10.0 0 false; \
                  create ""  9.0 0 false; \
                  create ""  8.0 0 false; \
                  create ""  7.0 0 false; \
                  create ""  6.0 0 true; \
                  create ""  5.0 0 false; \
                  create ""  4.0 0 false; \
                  create ""  3.0 0 false; \
                  create ""  2.0 0 false; \
                  create ""  1.0 0 false ] in \
  let tpr_x = 3. /. 4. in \
  let fpr_x = (5. -. 3.) /. (14. -. 4.) in \
  power_metric 0.35 scores = tpr_x /. (tpr_x +. fpr_x)
*)

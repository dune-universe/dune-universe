
module type SCORE_LABEL = sig
  type t
  val get_score: t -> float
  val get_label: t -> bool
end

module type ROC_FUNCTOR = functor (SL: SCORE_LABEL) ->
sig
  (** sort score labels putting high scores first *)
  val rank_order_by_score: SL.t list -> SL.t list

  (** in-place sort of score labels; putting high scores first *)
  val rank_order_by_score_a: SL.t array -> unit

  (** compute the cumulated actives curve given
      an already sorted list of score labels *)
  val cumulated_actives_curve: SL.t list -> int list

  (** compute Area Under the ROC curve given an already sorted list of
      score labels *)
  val roc_curve: SL.t list -> (float * float) list

  (** ROC curve (list of (FPR,TPR) values) corresponding to
      those score labels *)
  val fast_auc: SL.t list -> float

  (** compute Area Under the ROC curve given an unsorted list
      of score labels *)
  val auc: SL.t list -> float

  (** compute Area Under the ROC curve given an unsorted array
      of score labels; array will be sorted in-place *)
  val auc_a: SL.t array -> float

  (** (early) enrichment factor at given threshold (database percentage)
      given an unsorted list of score labels *)
  val enrichment_factor: float -> SL.t list -> float

  (** [initial_enhancement a score_labels] will compute
      S = sum_over_i (exp (-rank(active_i) / a))
      given an unsorted list of score labels.
      Robust Initial Enhancement (RIE) = S/<S> where <S> is
      the average S for randomly ordered score labels.
      RIE = 1.0 <=> random performance. Cf. DOI:10.1021/ci0100144
      for details. *)
  val initial_enhancement: float -> SL.t list -> float

  (** same as [initial_enhancement] but does not reorder the list
      of score_labels *)
  val fast_initial_enhancement: float -> SL.t list -> float

  (** power metric at given threshold given an unsorted list of score labels *)
  val power_metric: float -> SL.t list -> float

  (** bedroc_auc at given alpha. Default alpha = 20.0. *)
  val bedroc_auc: ?alpha:float -> SL.t list -> float

  (** Matthews' Correlation Coefficient (MCC)
      use: [mcc classif_threshold score_labels].
      scores >= threshold are predicted as targets;
      scores < threshold are predicted as non targets. *)
  val mcc: float -> SL.t list -> float

end

(* functions for ROC analysis *)
module Make: ROC_FUNCTOR = functor (SL: SCORE_LABEL) ->
struct

  module A = Array
  module L = BatList

  let trapezoid_surface x1 x2 y1 y2 =
    let base = abs_float (x1 -. x2) in
    let height = 0.5 *. (y1 +. y2) in
    base *. height

  let rev_compare_scores x y =
    BatFloat.compare (SL.get_score y) (SL.get_score x)

  (* put molecules with the highest scores at the top of the list *)
  let rank_order_by_score (score_labels: SL.t list) =
    L.stable_sort rev_compare_scores score_labels

  (* put molecules with the highest scores at the top of the array *)
  let rank_order_by_score_a (score_labels: SL.t array) =
    A.stable_sort rev_compare_scores score_labels

  (* compute the cumulated number of actives curve,
     given an already sorted list of score labels *)
  let cumulated_actives_curve (high_scores_first: SL.t list) =
    let sum = ref 0 in
    L.map (fun sl ->
        if SL.get_label sl then incr sum;
        !sum
      ) high_scores_first

  let roc_curve (score_labels: SL.t list) =
    let high_scores_first = rank_order_by_score score_labels in
    let nacts = ref 0 in
    let ndecs = ref 0 in
    let nb_act_decs =
      L.fold_left (fun acc x ->
          if SL.get_label x then incr nacts
          else incr ndecs;
          (!nacts, !ndecs) :: acc
        ) [(0, 0)] high_scores_first in
    let nb_actives = float !nacts in
    let nb_decoys = float !ndecs in
    L.rev_map (fun (na, nd) ->
        let tpr = float na /. nb_actives in
        let fpr = float nd /. nb_decoys in
        (fpr, tpr)
      ) nb_act_decs

  let fast_auc_common fold_fun high_scores_first =
    let fp, tp, fp_prev, tp_prev, a, _p_prev =
      fold_fun (fun (fp, tp, fp_prev, tp_prev, a, p_prev) sl ->
          let si = SL.get_score sl in
          let li = SL.get_label sl in
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
        ) (0., 0., 0., 0., 0., neg_infinity)
        high_scores_first
    in
    (a +. trapezoid_surface fp fp_prev tp tp_prev) /. (fp *. tp)

  (* area under the ROC curve given an already sorted list of score-labels *)
  let fast_auc high_scores_first =
    fast_auc_common L.fold_left high_scores_first

  let fast_auc_a high_scores_first =
    fast_auc_common A.fold_left high_scores_first

  (* area under the ROC curve given an unsorted list of score-labels
     TP cases have the label set to true
     TN cases have the label unset *)
  let auc (score_labels: SL.t list) =
    let high_scores_first = rank_order_by_score score_labels in
    fast_auc high_scores_first

  let auc_a (score_labels: SL.t array) =
    rank_order_by_score_a score_labels;
    fast_auc_a score_labels

  (* proportion of actives given an unsorted list of score-labels
     TP cases have the label set to true
     TN cases have the label unset
     returns: (nb_molecules, actives_rate) *)
  let actives_rate (score_labels: SL.t list) =
    let tp_count, fp_count =
      L.fold_left
        (fun (tp_c, fp_c) sl ->
           if SL.get_label sl then
             (tp_c + 1, fp_c)
           else
             (tp_c, fp_c + 1)
        ) (0, 0) score_labels
    in
    let nb_molecules = tp_count + fp_count in
    (nb_molecules, (float tp_count) /. (float nb_molecules))

  (* enrichment rate at x (e.g. x = 0.01 --> ER @ 1%) given a list
     of unsorted score-labels
     returns: (top_n, top_actives_rate, rand_actives_rate, enr_rate) *)
  let enrichment_factor (p: float) (score_labels: SL.t list) =
    let nb_molecules, rand_actives_rate = actives_rate score_labels in
    let top_n = BatFloat.round_to_int (p *. (float nb_molecules)) in
    let top_p_percent_molecules =
      L.take top_n (rank_order_by_score score_labels) in
    let _, top_actives_rate = actives_rate top_p_percent_molecules in
    let enr_rate = top_actives_rate /. rand_actives_rate in
    enr_rate

  let fast_initial_enhancement (a: float) (l: SL.t list) =
    L.fold_lefti (fun acc i x ->
        if SL.get_label x then
          let rank = float i in
          acc +. exp (-. rank /. a)
        else
          acc
      ) 0.0 l

  let initial_enhancement (a: float) (l: SL.t list) =
    fast_initial_enhancement a (rank_order_by_score l)

  let nb_actives l =
    float
      (L.fold_left (fun acc x ->
           if SL.get_label x then acc + 1
           else acc
         ) 0 l)

  (* Cf. http://jcheminf.springeropen.com/articles/10.1186/s13321-016-0189-4
     for formulas:
     The power metric: a new statistically robust enrichment-type metric for
     virtual screening applications with early recovery capability
     Lopes et. al. Journal of Cheminformatics 2017 *)
  let power_metric (cutoff: float) (scores_tot: SL.t list): float =
    assert(cutoff > 0.0 && cutoff <= 1.0);
    let size_tot = float (L.length scores_tot) in
    let x = BatFloat.round (cutoff *. size_tot) in
    let size_x = int_of_float x in
    assert(size_x >= 1);
    let sorted = rank_order_by_score scores_tot in
    let scores_x = L.take size_x sorted in
    let actives_x = nb_actives scores_x in
    let actives_tot = nb_actives scores_tot in
    let tpr_x = actives_x /. actives_tot in
    let fpr_x = (x -. actives_x) /. (size_tot -. actives_tot) in
    tpr_x /. (tpr_x +. fpr_x)

  (* formula comes from
     "Evaluating Virtual Screening Methods:  Good and Bad Metrics for the “Early Recognition” Problem"
     Jean-François Truchon * and Christopher I. Bayly. DOI: 10.1021/ci600426e
     Reference implementation in Python:
     ---
     def calculateBEDROC(self, alpha = 20.0 ):
           if alpha < 0.00001:
               os.stderr.write( "In method calculatBEDROC, the alpha parameter argument must be greater than zero." )
               sys.exit(1)
           N = float( self.getNbrTotal() )
           n = float( self.getNbrActives() )
           sum = 0.0
           for rank in self.ranks:
               sum += math.exp( -alpha * rank / N )
           ra = n/N
           factor1 = ra * math.sinh( alpha/2.0 )/( math.cosh(alpha/2.0) - math.cosh(alpha/2.0 - ra*alpha ) )
           factor2 = 1.0 / ra * (math.exp(alpha/N) - 1.0)/( 1.0 - math.exp(-alpha))
           constant = 1.0 / ( 1.0 - math.exp( alpha * ( 1.0 - ra ) ) )
           bedroc = sum * factor1 * factor2 + constant
           return bedroc
    --- *)
  let bedroc_auc ?alpha:(alpha = 20.0) (score_labels: SL.t list): float =
    let half_alpha = 0.5 *. alpha in
    let n_tot = float (L.length score_labels) in
    let n_act = nb_actives score_labels in
    let sum =
      let sorted = rank_order_by_score score_labels in
      L.fold_lefti (fun acc rank x ->
          if SL.get_label x then
            (* ranks must start at 1 *)
            acc +. exp (-.alpha *. (1.0 +. float rank) /. n_tot)
          else
            acc
        ) 0.0 sorted in
    let r_a = n_act /. n_tot in
    let factor1 = r_a *. sinh half_alpha /. (cosh half_alpha -. cosh (half_alpha -. r_a *. alpha)) in
    let factor2 = 1.0 /. r_a *. (exp (alpha /. n_tot) -. 1.0) /. (1.0 -. exp (-.alpha)) in
    let constant = 1.0 /. (1.0 -. exp (alpha *. ( 1.0 -. r_a))) in
    sum *. factor1 *. factor2 +. constant

  (* accumulator type for the mcc metric *)
  type mcc_accum = { tp: int ;
                     tn: int ;
                     fp: int ;
                     fn: int }

  (* Matthews' Correlation Coefficient (MCC)
     Biblio: Matthews, B. W. (1975).
     "Comparison of the predicted and observed secondary structure of T4 phage
     lysozyme". Biochimica et Biophysica Acta (BBA)-Protein Structure,
     405(2), 442-451. *)
  let mcc classif_threshold score_labels =
    (* formula:
       mcc = (tp * tn - fp * fn) /
             sqrt ((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)) *)
    let acc =
      L.fold_left (fun acc sl ->
          let truth = SL.get_label sl in
          let score = SL.get_score sl in
          let prediction = score >= classif_threshold in
          match (truth, prediction) with
          | (true, true)   -> { acc with tp = acc.tp + 1 } (* TP *)
          | (false, false) -> { acc with tn = acc.tn + 1 } (* TN *)
          | (true, false)  -> { acc with fn = acc.fn + 1 } (* FN *)
          | (false, true)  -> { acc with fp = acc.fp + 1 } (* FP *)
        ) { tp = 0; tn = 0; fp = 0; fn = 0 } score_labels in
    let tp = float acc.tp in
    let tn = float acc.tn in
    let fp = float acc.fp in
    let fn = float acc.fn in
    let denum' = (tp +. fp) *. (tp +. fn) *. (tn +. fp) *. (tn +. fn) in
    if denum' = 0.0 then 0.0 (* div by 0 protection *)
    else
      let num = (tp *. tn) -. (fp *. fn) in
      let denum = sqrt denum' in
      num /. denum

end

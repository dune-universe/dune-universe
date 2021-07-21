(* 
TODO: check those potentially interesting papers
- https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5374669/
- https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5860096/
- http://stat.wharton.upenn.edu/~tcai/paper/FDR-HMM.pdf
*)
open Core_kernel

(* Naive computation of score distribution *)
let naive_score_distribution pwm bg =
  let pwm = (pwm : Pwm.t :> float array array) in
  let bg = (bg : Pwm.background :> float array) in
  let module T = Float.Table in
  let n = Array.length pwm in
  let m = Array.length bg in
  let t = T.create () in
  let rec loop i score prob =
    if i >= n then T.update t score ~f:(function
        | None -> prob
        | Some p -> p +. prob
      )
    else
      for j = 0 to m - 1 do
        loop (i + 1) (score +. pwm.(i).(j)) (prob *. bg.(j))
      done
  in
  loop 0 0. 1. ;
  T.to_alist t

(** Implementation of {https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2238751/} *)
module TFM_pvalue = struct
  let array_max (t : float array) =
    let best = ref Float.neg_infinity in
    for i = 0 to Array.length t - 1 do
      let t_i = t.(i) in
      if Float.(t_i > !best) then best := t_i
    done ;
    !best

  let array_min (t : float array) =
    let best = ref Float.infinity in
    for i = 0 to Array.length t - 1 do
      let t_i = t.(i) in
      if Float.(t_i < !best) then best := t_i
    done ;
    !best

  let bs pwm ?(i = 0) ?(j = Array.length pwm - 1) () =
    let i = max i 0 in
    let j = min (max j 0) (Array.length pwm - 1) in
    let rec loop l acc =
      if l <= j then loop (l + 1) (acc +. array_max pwm.(l))
      else acc
    in
    loop i 0.

  let ws pwm ?(i = 0) ?(j = Array.length pwm - 1) () =
    let i = max i 0 in
    let j = min (max j 0) (Array.length pwm - 1) in
    let rec loop l acc =
      if l <= j then loop (l + 1) (acc +. array_min pwm.(l))
      else acc
    in
    loop i 0.

  let delta col = array_max col -. array_min col


  let matrix_permutation mat =
    let delta = Array.map mat ~f:delta in
    Array.mapi mat ~f:(fun i col -> i, col)
    |> Array.sorted_copy ~compare:(fun (i,_) (j,_) -> Float.compare delta.(j) delta.(i))
    |> Array.map ~f:snd

  let truncate x ~eps =
    Float.round_down (x /. eps) *. eps

  let approx pwm ~eps =
    Array.map pwm ~f:(Array.map ~f:(truncate ~eps))

  let fast_pvalue pwm bg alpha =
    let module T = Float.Table in
    let n = Array.length pwm in
    let m = Array.length bg in
    let bs = Array.init n ~f:(fun i -> bs pwm ~i:(i+1) ()) in
    let ws = Array.init n ~f:(fun i -> ws pwm ~i:(i+1) ()) in
    let rec loop i t p =
      if i >= n then p
      else (
        let pval = ref p in
        let t' = T.create () in
        T.iteri t ~f:(fun ~key:score ~data:prob ->
            for j = 0 to m - 1 do
              let score' = score +. pwm.(i).(j) in
              if Float.(alpha -. ws.(i) <= score') then (
                pval := !pval +. prob *. bg.(j)
              )
              else if Float.(alpha -. bs.(i) <= score') then (
                let prob = prob *. bg.(j) in
                T.update t' score' ~f:(function
                    | None -> prob
                    | Some p -> p +. prob
                  )
              )
            done
          ) ;
        loop (i + 1) t' !pval
      )
    in
    loop 0 (T.of_alist_exn [0., 1.]) 0.

  let score_distribution ?alpha ?beta pwm bg =
    let module T = Float.Table in
    let n = Array.length pwm in
    let m = Array.length bg in
    let bs = Array.init (n + 1) ~f:(fun i -> bs pwm ~i ()) in
    let ws = Array.init (n + 1) ~f:(fun i -> ws pwm ~i ()) in
    let alpha = match alpha with
      | None -> ws.(0)
      | Some x -> x
    and beta = match beta with
      | None -> bs.(0)
      | Some x -> x
    in
    let rec loop i t =
      if i >= n then t
      else (
        let t' = T.create () in
        T.iteri t ~f:(fun ~key:score ~data:prob ->
            for j = 0 to m - 1 do
              let score' = score +. pwm.(i).(j) in
              if Float.(alpha -. bs.(succ i) <= score' && score' <= beta -. ws.(succ i)) then (
                let prob = prob *. bg.(j) in
                T.update t' score' ~f:(function
                    | None -> prob
                    | Some p -> p +. prob
                  )
              )
            done
          ) ;
        loop (i + 1) t'
      )
    in
    loop 0 (T.of_alist_exn [0., 1.])
    |> T.to_alist

  exception Found_threshold of float

  let threshold pwm bg pvalue =
    try
      score_distribution pwm bg
      |> List.sort ~compare:(Fn.flip Poly.compare)
      |> List.fold ~init:0. ~f:(fun sum (s, p) ->
          let sum = sum +. p in
          if Float.(sum >= pvalue) then raise (Found_threshold s)
          else sum
        )
      |> ignore ;
      ws pwm ()
    with Found_threshold s -> s

  let threshold2 pwm bg ~alpha ~err ~pvalue =
    try
      let pval_alpha = fast_pvalue pwm bg (alpha +. err) in
      score_distribution pwm bg ~alpha:(alpha -. err) ~beta:(alpha +. err)
      |> List.sort ~compare:(Fn.flip Poly.compare)
      |> List.fold ~init:pval_alpha ~f:(fun sum (s, p) ->
          let sum = sum +. p in
          if Float.(sum >= pvalue) then raise (Found_threshold s)
          else sum
        )
      |> ignore ;
      ws pwm ()
    with Found_threshold s -> s

  let maximal_error pwm ~eps =
    let sum = ref 0. in
    for i = 0 to Array.length pwm - 1 do
      let max_i = ref Float.neg_infinity in
      for j = 0 to Array.length pwm.(i) - 1 do
        let m_i_j = pwm.(i).(j) in
        let delta = m_i_j -. truncate m_i_j ~eps in
        if Float.(delta > !max_i) then (max_i := delta)
      done ;
      sum := !sum +. !max_i
    done ;
    !sum

  let score_of_pvalue pwm bg pvalue =
    let pwm = matrix_permutation (pwm : Pwm.t :> float array array)in
    let bg = (bg : Pwm.background :> float array) in
    let eps = 0.1 in
    let pwm_eps = approx pwm ~eps in
    let alpha = threshold pwm_eps bg pvalue in
    let rec loop pwm_eps eps alpha =
      let err = maximal_error pwm ~eps in
      let p1 = fast_pvalue pwm_eps bg (alpha -. err) in
      let p2 = fast_pvalue pwm_eps bg alpha in
      if Float.robustly_compare p1 p2 <> 0
      then
        let eps = eps /. 10. in
        let pwm_eps = approx pwm ~eps in
        let alpha = threshold2 pwm_eps bg ~alpha ~err ~pvalue in
        loop pwm_eps eps alpha
      else
        alpha
    in
    loop pwm_eps eps alpha

  let score_distribution ?alpha ?beta (pwm : Pwm.t) (bg : Pwm.background) =
    score_distribution ?alpha ?beta (pwm :> float array array) (bg :> float array)

  let fast_pvalue (pwm : Pwm.t) (bg : Pwm.background) alpha =
    fast_pvalue (pwm :> float array array) (bg :> float array) alpha

end

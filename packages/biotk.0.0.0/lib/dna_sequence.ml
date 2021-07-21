open Core_kernel

let gc s =
  let c = ref 0 and n = ref 0 in
  let count = function
    'c' | 'g' | 'C' | 'G' -> incr c
        | 'n' |'N' -> incr n
        | _ -> ()
  in
  String.iter ~f:count s ;
  float !c /. float (String.length s - !n)

let incr delta gc n w =
  w := !w + delta ;
  function
  'c' | 'C' | 'g' | 'G' -> gc := !gc + delta
      | 'n' | 'N' -> n := !n + delta
      | _ -> ()

let local_gc k s =
  let n = String.length s in
  if k > n then raise (Invalid_argument "Dna_sequence.local_gc") ;
  let gc = ref 0 and unk = ref 0 and w = ref 0 and i = ref 0 in
  for i = 0 to k / 2 do incr 1 gc unk w s.[i] done ;
  let rec stream () =
    if !i >= n then Seq.Nil
    else
      let w = (Stdlib.min (n - 1) (!i + k / 2)) - (Stdlib.max 0 (!i - k / 2)) + 1 in
      let r =
        if w = !unk then 0.5
        else (float !gc) /. (float (w - !unk)) in
      if !i + k / 2 + 1 < n then incr 1 gc unk (ref 0) s.[!i + k / 2 + 1] ;
      if !i - k / 2 >= 0 then incr (- 1) gc unk (ref 0) s.[!i - k / 2] ;
      Stdlib.incr i ;
      Seq.Cons (r, stream)
  in
  stream

module type Parser = sig
  type t
  type score

  (** filter arguments:
      - position
      - GC content around this position
      - optimal score of a match finishing at this position
      - start position of the optimal match finishing at this position *)
  type 'a match_filter = int -> float -> int -> int -> 'a option

  type statistics = {
    nb_gc_levels : int ;
    values : score array array ; (* gc levels -> sorted score values *)
  }

  (** [statistics nbl parser] computes statistics for [nbl] number of GC levels *)
  val statistics : generator:(int -> float -> string) -> int -> t -> statistics
  val cdf_of_statistics : statistics -> float -> float -> float
  val average_cdf_of_statistics : statistics -> float -> float
  val bound_of_fpr : statistics -> float -> float
  val bound_for_gc_and_fpr : statistics -> gc:float -> fpr:float -> float

  (** returns (start, end), raw score, gc content, normalized score *)
  (* val fpr_filter : statistics -> float -> ((int * int) * int * float * float) match_filter *)

  (** returns location, raw score, gc content, normalized score *)
  (* val loc_fpr_filter : statistics -> float -> GLoc.t -> (GLoc.t * int * float * float) match_filter *)

(*
  val normalize : statistics -> string -> int array -> float array

  val fdr_scan : fdr:float -> t -> statistics -> string -> (int * int * float) list
  val use_location : Location.t -> (int * int * float) list -> (Location.t * float) list *)
end

module Parser_of_char(P : Wfa.Profile with type symbol = Wfa.Nucleotide.t
                                       and type score = float) = struct
  module M = Wfa.Make(Float)(Wfa.Nucleotide)(P)
  type t = M.automaton
  type 'a match_filter = int -> float -> int -> int -> 'a option

  (* let int_of_char = function
   *   | 'a' | 'A' -> 0
   *   | 'c' | 'C' -> 1
   *   | 'g' | 'G' -> 2
   *   | 't' | 'T' -> 3
   *   | 'n' | 'N' -> 4
   *   | _ -> raise (Invalid_argument "Dnaseq.int_of_char") *)

  let char =
    let open Wfa.Nucleotide in
    function
    | 'a' | 'A' -> Some a
    | 'c' | 'C' -> Some c
    | 'g' | 'G' -> Some g
    | 't' | 'T' -> Some t
    | 'n' | 'N' -> None
    | _ -> raise (Invalid_argument "Dnaseq.char")

  type statistics = {
    nb_gc_levels : int ;
    values : float array array ; (* gc levels -> score values *)
  }

  (* let gc_round n f =
   *   let n = float n in
   *   Float.(iround_exn ~dir:`Down ((f * n + 0.5) / n)) *)
  let gc_levels n = Array.init (n + 1) ~f:(fun i -> float i /. (float n))
  let gc_level n f =
    let n = float n in
    Float.(iround_exn ~dir:`Down (f * n +. 0.5))

  (* FIXME: this keeps the neg_inf scores in the beginning,
     the first values with position = -1 should be removed *)
  let arrange_score_values scores =
    Array.sort ~compare:Poly.compare scores ;
    scores

  let seq_size = 1000000

  let statistics ~generator nb_levels aut =
    let seqz = Array.map ~f:(generator seq_size) (gc_levels nb_levels) in
    {
      nb_gc_levels = nb_levels ;
      values = Array.map seqz ~f:(fun s ->
          M.scan char aut s
          |> Seq.map (fun (_, s, _) -> s)
          |> Caml.Array.of_seq
          |> arrange_score_values
        ) ;
    }

  let rec rank_aux t x a b =
    if a > b then raise Caml.Not_found
    else if a = b then a
    else (
      let i = (a + b) / 2 + 1 in
      if Float.(t.(i) <= x) then rank_aux t x i b
      else rank_aux t x a (i - 1)
    )
  let rank t x = rank_aux t x 0 (Array.length t - 1)

  let cdf a x =
    float (rank a x + 1) /. (float (Array.length a))

  let cdf_of_statistics stats gc score =
    cdf stats.values.(gc_level stats.nb_gc_levels gc) score

  let average_cdf_of_statistics stats =
    let cdfz = Array.init stats.nb_gc_levels ~f:(fun i -> cdf stats.values.(i)) in
    let n = float stats.nb_gc_levels in
    fun score ->
      (Array.fold ~f:(fun accu f -> accu +. f score) ~init:0. cdfz) /. n

  let bound_of_fpr stats fpr =
    let k = int_of_float (float seq_size *. (1. -. fpr)) in
    Array.fold ~f:(fun accu values -> Float.min accu values.(k)) ~init:Float.max_value stats.values

  let bound_for_gc_and_fpr stats ~gc ~fpr =
    let k = int_of_float (float seq_size *. (1. -. fpr)) in
    stats.values.(gc_level stats.nb_gc_levels gc).(k)

  (* let gen_fpr_filter localize stats fpr =
   *   let bound = bound_of_fpr stats fpr
   *   and cdf = cdf_of_statistics stats
   *   and tpr_bound = 1. -. fpr in
   *   fun ed gc score st ->
   *     if score < bound then None
   *     else (
   *       let nscore = cdf gc score in
   *       if nscore < tpr_bound then None
   *       else Some (
   *           localize st ed,
   *           score, gc, nscore
   *         )
   *     ) *)

  (* let loc_fpr_filter stats fpr loc =
   *   gen_fpr_filter
   *     (fun st ed -> GLoc.{ chr = loc.chr ; lo = loc.lo + st ; hi = loc.hi + ed })
   *     stats fpr
   *
   * let fpr_filter = gen_fpr_filter (fun i j -> i,j) *)

(*
  let normalize stats dna score =
    let cdf = cdf_of_statistics stats in
    let gc = local_gc (min 30 (String.length dna)) dna in
    Array.mapi (fun i x -> cdf gc.(i) x) score

  let ( += ) l e = l := e :: !l

  let fdr_scan ~fdr aut stats dna =

    let score, position = scan aut dna in
    let score = normalize stats dna score
    and r = ref [] in
    for i = Array.length score - 1 downto 0 do
      if score.(i) >= fdr
      then r += (position.(i), i - position.(i) + 1, score.(i))
    done ;
    List.sort ~cmp:compare !r

  open Location
  let use_location loc hits =
    List.map (fun (s,l,f) -> move loc (loc.st + s) (loc.st + s + l - 1), f) hits
*)
end

(* module Test = struct
 *   let balmer_hexamer = `sequence [
 *       `base (1.04, -1.8,-0.01,-1.78) ;
 *       `base (-2.47,-3.34,1.27,-1.87) ;
 *       `base (-1.7,-3.05,0.94,0.16) ;
 *       `base (-2.88,-1.3,-0.69,1.22) ;
 *       `base (-3.17,1.09,-0.85,-1.38) ;
 *       `base (1.35,-3.34,-1.36,-2.07) ;
 *     ]
 *
 *   let balmer_dr125 = `sequence [
 *       balmer_hexamer ;
 *       `disjunction [ `gap (5,5) ; `gap (2,2) ; `gap (1,1) ] ;
 *       balmer_hexamer
 *     ]
 *   module Parser = Parser_of_char(Motif.Profile)
 *   let aut = Motif.PSSM.automaton balmer_dr125
 * end *)

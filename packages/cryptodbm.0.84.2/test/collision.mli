
(* Collision detector *)

type detector

(* 2^card : approximate cardinality of the set to be tested for collisions. 
 * fail : 1/2^fail is the desired (approximate) probability of a false negative. 
 * *)
val create : card:int -> fail:int -> detector


(* Size, in bits, required for the hashcodes.
 * Hashcodes may be larger.
 * If all hashcodes are smaller, then more false negative will be reported. *)
val hashsize : detector -> int

(* Inserts a hashcode in the detector.
 * Returns true if this hashcode is unknown.
 * Returns false if this hashcode was already seen.
 * False negative are possible.
 * False positive are impossible. *)
val insert : hashcode:Z.t -> detector -> bool



(* The way it works:
 *
 * - We build n bit vectors of length 2^k.
 *
 * - The hashcode size must be n*k.
 *
 * - Assume we have already inserted (almost) all the values
 *   (that is, 2^card values).
 *
 * - When inserting a new value, let 'a be the probability of a collision in one bit vector.
 *   We have 'a < 2^card / 2^k 
 *
 * - Then, the probability of a full collision is p = 'a^n.
 *   We want p < 1/2^fail, that is 'a^n * 2^fail < 1
 *   It suffices to take k and n such that (2^(card.n))/(2^kn) * 2^fail < 1
 *
 *   fail.ln 2 < n(k.ln 2 - card.ln 2)
 *   
 *   We (arbitrarily) choose k such that 2^k = 2 * 2^card, that is, k = card + 1
 *   Then, we must choose n = fail
 *
 *)

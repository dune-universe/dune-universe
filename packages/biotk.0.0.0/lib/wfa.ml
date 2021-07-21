open Core_kernel

module type Symbol = sig
  type t
  val all : t list
  val to_string : t -> string
end

module type Score = sig
  type t
  val zero : t
  val compare : t -> t -> int
  val ( < ) : t -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
  val ( + ) : t -> t -> t
end

module type Profile = sig
  type symbol
  type t
  type score
  val score : t -> symbol -> score
  val missing_score : t -> score
end

module type S = sig
  type expression
  type automaton
  type score
  type profile
  type symbol

  val profile : profile -> expression
  val disjunction : expression list -> expression
  val sequence : expression list -> expression
  val gap : min_length:int -> max_length:int -> expression
  (* val tandem : expression -> expression -> int -> int -> expression *)
  (* val map : expression -> f:(profile -> profile) -> expression *)

  val min_score : expression -> score
  val max_score : expression -> score
  val scores : expression -> score array

  (* val string_of_expression : expression -> string *)
  val automaton : expression -> automaton
  val scan : (char -> symbol option) -> automaton -> string -> (int * score * int) Seq.t
end

module Nucleotide = struct
  type t = [ `A | `C | `G | `T ]

  let all = [ `A ; `C ; `G ; `T ]
  let to_string = function
    | `A -> "A"
    | `C -> "C"
    | `G -> "G"
    | `T -> "T"

  let a = `A
  let c = `C
  let g = `G
  let t = `T
end

module Nucleotide_frequency = struct
  type t = float * float * float * float
  type symbol = Nucleotide.t
  type score = float
  let score (a,c,g,t) b =
    let f = match b with
      | `A -> a
      | `C -> c
      | `G -> g
      | `T -> t
    in
    Float.(100. * f)
  (* let to_string (a,c,g,t) = Printf.sprintf "(%g,%g,%g,%g)" a c g t *)

  let missing_score (a,c,g,t) =
    Float.((a + c + g + t) / 4.)

  let complement (a,c,g,t) = (t,g,c,a)
end

module Nucleotide_IUPAC = struct
  type t = [
    | Nucleotide.t
    | `Disj of Nucleotide.t list
  ]
  type symbol = Nucleotide.t
  type score = int

  let score (p : t) (b : symbol) = match p with
    | #Nucleotide.t as b' -> if Poly.(b = b') then 0 else -1
    | `Disj l -> if List.mem ~equal:Poly.( = ) l b then 0 else -1

  let missing_score _ = 0
  (* let complement_base = function
   *     `a -> `t
   *   | `t -> `a
   *   | `c -> `g
   *   | `g -> `c
   *   | `n -> `n *)

  (* let complement = function
   *   | #base as b -> complement_base b
   *   | `d l -> `d (List.map complement_base l)
   *
   * let rec tag_base = function
   *   | `sequence s -> `sequence (List.map tag_base s)
   *   | `disjunction d -> `disjunction (List.map tag_base d)
   *   | `gap g as e -> e
   *   | #base as b -> `base b
   *   | `d l as e -> `base e
   *
   * let base_to_string = function
   *     `a -> "A"
   *   | `c -> "C"
   *   | `g -> "G"
   *   | `t -> "T"
   *   | `n -> "N"
   *
   * let to_string = function
   *   | #base as b -> base_to_string b
   *   | `d bases -> Printf.sprintf "[%s]" (String.concat "" (List.map base_to_string bases)) *)

end

module Make
    (Score : Score)
    (Symbol : Symbol)
    (Profile : Profile with type score = Score.t
                        and type symbol = Symbol.t)
= struct
  type expression =
    | Profile of Profile.t
    | Disjunction of expression list
    | Gap of int * int
    | Sequence of expression list

  type automaton = < initial_states : int list ;
                     nb_vertices : int ;
                     nb_transitions : int ;
                     final_state : int ;
                     transitions : (int * [`profile of Profile.t | `any] * int) list ;
                     max_state : int >

  let gap ~min_length ~max_length = Gap (min_length, max_length)

  let sequence xs = Sequence xs
  let disjunction xs = Disjunction xs
  let profile p = Profile p

  (*   let rec string_of_expression = function
 *       `base b -> C.to_string b
 *     | `disjunction exprz ->
 * Printf.sprintf "disjunction(%s)"
 *   (String.concat "," (List.map string_of_expression exprz))
 *     | `sequence exprz ->
 * Printf.sprintf "sequence(%s)"
 *   (String.concat "," (List.map string_of_expression exprz))
 *     | `gap (s,e) -> Printf.sprintf "gap(%d,%d)" s e *)


  let rec opt_score opt = function
    | Profile profile ->
      List.fold Symbol.all ~init:Score.zero ~f:(fun candidate symbol ->
          opt candidate (Profile.score profile symbol)
        )
    | Disjunction xs ->
      List.fold xs ~init:Score.zero ~f:(fun acc e ->
          opt acc (opt_score opt e)
        )
    | Sequence xs ->
      List.fold xs ~init:Score.zero ~f:(fun acc e ->
          Score.(acc + opt_score opt e)
        )
    | Gap _ -> Score.zero

  let min_score = opt_score Poly.min
  let max_score = opt_score Poly.max

  module ScoreSet = Set.Make(struct
      include Score
      let sexp_of_t _ = assert false
      let t_of_sexp _ = assert false
    end)

  let convolution xs ys =
    ScoreSet.fold xs ~init:ScoreSet.empty ~f:(fun acc x ->
        ScoreSet.fold ys ~init:acc ~f:(fun acc y ->
            ScoreSet.add acc Score.(x + y)
          )
      )

  let rec scores = function
    | Profile profile ->
      List.fold Symbol.all ~init:ScoreSet.empty ~f:(fun acc x ->
          ScoreSet.add acc (Profile.score profile x)
        )
    | Sequence exprz ->
      List.reduce_exn ~f:convolution (List.map ~f:scores exprz)
    | Disjunction exprz ->
      (* FIXME j'ai un doute, c'est sans doute une surapproximation dans le cas de la disjonction*)
      List.reduce_exn ~f:ScoreSet.union (List.map ~f:scores exprz)
    | Gap _ -> ScoreSet.singleton Score.zero

  let scores e = ScoreSet.to_array (scores e)

  (* The code that follows is adapted from WAPAM *)
  let rec gen_gap i = function
    | 0 -> []
    | n -> (i, `any, i+1) :: (gen_gap (i+1) (n-1))

  let gen_gapv i =
    let rec aux j = function
      | 0 -> []
      | n ->
        (i, `epsilon , j + 1) ::
        (j, `any, j+1) ::
        (aux (j+1) (n-1))
    in
    aux i

  let rec compilation i = function
    | Profile p  ->
      (i + 1, [ (i, `profile p, i + 1) ])
    | Sequence s ->
      List.fold s ~init:(i, []) ~f:(fun (i,accu) e ->
          let (o,t) = compilation i e in (o,t @ accu)
        )
    | Disjunction s ->
      let foreach_expr e (i,accu) =
        let (o,t) = compilation i e
        in (o + 1, (i,t,o) :: accu)
      and connect_aut input output (i,aut,o) accu : (int * [ `profile of Profile.t | `epsilon | `any ] * int) list  =
        let aut' = (input, `epsilon, i) :: (o, `epsilon, output) :: aut in
        aut' @ accu
      in
      let output, subaut =
        List.fold_right ~f:foreach_expr s ~init:(i + 1, [])
      in
      (output,
       List.fold_right ~f:(connect_aut i output) subaut ~init:[])

    | Gap (l,u) ->
      (i + u, (gen_gap i l) @ (gen_gapv (i + l) (u - l)))

  let epsilon initial final arcs =
    List.fold_left
      ~f:(fun l (i,a,f) ->
          if (f = initial)
          then (i,a,final)::l
          else l)
      ~init:arcs
      arcs

  let rec find_and_remove f = function
    | [] -> raise Caml.Not_found
    | t::q -> if (f t)
      then t, q
      else
        let x, l = find_and_remove f q
        in  x, t::l

  let check_no_epsilon l =
    List.map l ~f:(function
        | (i, (`profile _ | `any as p), j) -> (i, p, j)
        | _ -> assert false
      )

  let rec enleve_epsilon arcs =
    try
      let (i,_,f), l = find_and_remove (function (_,`epsilon,_) -> true | _ -> false) arcs in
      enleve_epsilon (epsilon i f l)
    with
      Caml.Not_found -> check_no_epsilon arcs



  let vertices =
    List.fold
      ~f:(fun accu (i,_,j) ->
          let accu'  = if not (List.mem ~equal:Poly.( = ) accu i) then i :: accu else accu
          in if not (List.mem ~equal:Poly.( = ) accu' j) then j :: accu' else accu')
      ~init:[]

  (* let out_degree g v =
   *   List.fold_left g ~init:0 ~f:(fun d (i,_,_) ->
   *       if i = v then d + 1 else d
   *     ) *)


  let epsilon_successors g v =
    List.map
      ~f:(fun (_,_,v) -> v)
      (List.filter ~f:(fun (i, e, _) -> v = i && Poly.(e = `epsilon)) g)

  let rec union l = function
      [] -> l
    | h :: t when not (List.mem ~equal:Poly.( = ) l h) -> union (h :: l) t
    | _ :: t -> union l t

  (* let rec intersection l = function
   *     [] -> []
   *   | h :: t when List.mem ~equal:Poly.( = ) l h -> h :: (intersection l t)
   *   | _ :: t -> intersection l t *)

  let rec contains l = function
      [] -> true
    | h :: t when List.mem ~equal:Poly.( = ) l h -> contains l t
    | _ -> false

  let equals l l' = contains l l' && contains l' l

  let epsilon_reachable g v =
    let rec aux accu v' =
      let accu' = union accu (epsilon_successors g v')
      in
      if equals accu accu' then accu
      else
        List.fold ~f:aux ~init:accu' accu'
    in
    aux [] v

  (* to be revived *)
  (* let rec reverse_complement = function
   *     `base b -> `base (C.complement b)
   *   | `disjunction l -> `disjunction (List.map reverse_complement l)
   *   | `gap (s,e) -> `gap (s,e)
   *   | `sequence s -> `sequence (List.rev_map reverse_complement s) *)

  (* NOTE: après suppression des epsilon transitions, les
     epsilons-predecesseurs des sommets introduits comme entrée et
     sortie des disjonctions deviennent inutiles. On pourrait les
     supprimer pour simplifier l'automate *)

  let automaton expr =
    let final_state, transitions = compilation 0 expr in
    let epsilon_free = enleve_epsilon transitions
    and initial_states = 0 :: (epsilon_reachable transitions 0)
    in
    let transitions =
      List.sort
        ~compare:(fun (x,_,y) (x',_,y') -> Poly.compare (x,y) (x',y'))
        epsilon_free
    in
    object
      method initial_states = initial_states
      method nb_vertices = List.length (vertices transitions)
      method nb_transitions = List.length transitions
      method final_state = final_state
      method transitions = transitions
      method max_state =
        List.fold transitions ~init:(-1) ~f:(fun accu (i,_,j) ->
            max accu (max i j)
          )
    end

  let set a v =
    for i = 0 to Array.length a - 1 do
      a.(i) <- v
    done

  module BScore = struct
    type t = Bottom | Value of Score.t
    let ( < ) x y = match x, y with
      | Bottom, Bottom -> false
      | Bottom, Value _ -> true
      | Value _, Bottom -> false
      | Value u, Value v -> Score.(u < v)

    let ( + ) x y = match x, y with
      | Bottom, _
      | _, Bottom -> Bottom
      | Value u, Value v -> Value Score.(u + v)

    let zero = Value Score.zero

    let value_exn = function
      | Bottom -> assert false
      | Value v -> v
  end

  let score p c = match p with
    | `any -> Score.zero
    | `profile p ->
      match c with
      | Some c -> Profile.score p c
      | None -> Profile.missing_score p

  let rec scan_aux char (aut : automaton) dna k state pos state' pos' () =
    let n = Array.length state in
    if k >= String.length dna then Seq.Nil
    else (
      set state' BScore.Bottom ;
      set pos' (-1) ;
      let c = char dna.[k] in
      List.iter aut#initial_states ~f:(fun j ->
          if BScore.(state.(j) < zero) then (state.(j) <- BScore.zero ; pos.(j) <- k)
        ) ;
      for _ = 0 to n - 1 do
        List.iter aut#transitions ~f:(fun (i,p,j) ->
            let value = BScore.(state.(i) + Value (score p c)) in
            if BScore.(state'.(j) < value) then (
              state'.(j) <- value ;
              pos'.(j) <- pos.(i)
            )
          ) ;
      done ;
      let res = k, BScore.value_exn state'.(aut#final_state), pos'.(aut#final_state) in
      Cons (res,
            scan_aux char aut dna (k + 1) state' pos' state pos)
    )

  let scan char aut dna =
    let n = aut#max_state + 1 in
    let state = Array.create ~len:n BScore.Bottom
    and state' = Array.create ~len:n BScore.zero
    and pos = Array.create ~len:n (-1)
    and pos' = Array.create ~len:n 0 in
    scan_aux char aut dna 0 state pos state' pos'
end

module PSSM = struct
  module M = Make(Float)(Nucleotide)(Nucleotide_frequency)
  include M

  let tandem e1 e2 min_length max_length =
    let gap = gap ~min_length ~max_length in
    disjunction [
      sequence [ e1 ; gap ; e2 ] ;
      sequence [ e2 ; gap ; e1 ]
    ]


  (** Ugly stuff to build pssm out of count matrices *)
  let pseudo_counts k mat =
    Array.map mat ~f:(Array.map ~f:(fun i -> k +. float i))

  let log x = log x /. log 2.

  let logodds mat =
    let foreach_pos p =
      let total = Array.fold_right ~f:(+.) p ~init:0. in
      Array.map ~f:(fun c -> log (c /. total /. 0.25)) p
    in
    Array.map ~f:foreach_pos mat

  let cast = function
      [| a ; c ; g ; t |] -> a,c,g,t
    | _ -> assert false

  let of_counts ?(prior = 0.000001) mat =
    sequence Array.(
        to_list (map (logodds (pseudo_counts prior mat)) ~f:(fun p -> profile (cast p)))
      )
end

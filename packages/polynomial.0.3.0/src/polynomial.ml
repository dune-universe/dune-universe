type natural_with_infinity = Natural of int | Infinity

module type UNIVARIATE = sig
  (** The type of the polynomial coefficients. Can be a field or more generally
      a ring. For the moment, it is restricted to prime fields.
  *)
  type scalar

  (** Represents a polynomial *)
  type polynomial

  (** Returns the polynomial [P(X) = 0] *)
  val zero : polynomial

  (** Returns the polynomial [P(X) = 1] *)
  val one : polynomial

  (** Returns the degree of the polynomial *)
  val degree : polynomial -> natural_with_infinity

  val degree_int : polynomial -> int

  (** [have_same_degree P Q] returns [true] if [P] and [Q] have the same
      degree
  *)
  val have_same_degree : polynomial -> polynomial -> bool

  (* (\** [shift_by_n P n] multiplies [P] by [X^n]. For instance,
   *     [P(X) = a_{0} + a_{1} X + ... + a_{m} X^m] will be transformed in
   *     [a_{0} X^{n} + a_{1} X^{n + 1} + ... a_{m} X^{n + m}].
   * *\)
   * val shift_by_n : polynomial -> int -> polynomial *)

  (** [get_dense_polynomial_coeffiecients P] returns the list of the
      coefficients of P, including the null coefficients, in decreasing order
      i.e. if P(X) = a_{0} + a_{1} X + ... + a_{n - 1} X^{n - 1}, the function
      will return [a_{n - 1}, ..., a_{0}]
  *)
  val get_dense_polynomial_coefficients : polynomial -> scalar list

  (* val get_dense_polynomial_coefficients_with_degree :
   *   polynomial -> (scalar * int) list *)

  (** [get_list_coefficients P] returns [(a_4,4), (a_2,2), (a_0,0)] if
      P = a_4 X^4 + a_2 X^2 + a_0*)
  val get_list_coefficients : polynomial -> (scalar * int) list

  (** [evaluation P s] computes [P(s)]. Use Horner's method in O(n). *)
  val evaluation : polynomial -> scalar -> scalar

  (** [constants s] returns the constant polynomial [P(X) = s] *)
  val constants : scalar -> polynomial

  (** [add P Q] returns [P(X) + Q(X)] *)
  val add : polynomial -> polynomial -> polynomial

  (** [sub P Q] returns [P(X) - Q(X)] *)
  val sub : polynomial -> polynomial -> polynomial

  (** [mult_by_scalar s P] returns [s*P(X)] *)
  val mult_by_scalar : scalar -> polynomial -> polynomial

  (** [is_null P] returns [true] iff [P(X) = 0] *)
  val is_null : polynomial -> bool

  (** [is_constant P] returns [true] iff [P(X) = s] for s scalar *)
  val is_constant : polynomial -> bool

  (** [opposite P] returns [-P(X)] *)
  val opposite : polynomial -> polynomial

  (** [equal P Q] returns [true] iff [P(X) = Q(X)] on S *)
  val equal : polynomial -> polynomial -> bool

  (** [of_coefficients [(x_0, y_0) ; (x_1, y_1); ... ; (x_n ; y_n)]] builds the
      polynomial Σ(a_i * X^i) as a type [polynomial].

      By default, the null coefficients will be removed as the internal
      representation of polynomials is sparsed. However, a version with null
      coefficients can be generated if required. It is not recommended to use
      this possibility as it breaks an invariant of the type [polynomial].
  *)
  val of_coefficients :
    ?remove_null_coefficients:bool -> (scalar * int) list -> polynomial

  (** [lagrange_interpolation [(x_0, y_0) ; (x_1, y_1); ... ; (x_n ; y_n)]]
      builds the unique polynomial P of degre n such that P(x_i) = y_i for i = 0...n
      using the intermediate lagrange polynomials. [lagrange_interpolation_fft] can
      be used in case of a FFT friendly scalar structure. It is supposed all x_i
      are different.
  *)
  val lagrange_interpolation : (scalar * scalar) list -> polynomial

  (** [even_polynomial P] returns the polynomial P_even containing only the even
      coefficients of P *)
  val even_polynomial : polynomial -> polynomial

  (** [odd_polynomial P] returns the polynomial P_odd containing only the odd
      coefficients of P *)
  val odd_polynomial : polynomial -> polynomial

  (** [evaluate_fft ~generator:g ~power P] evaluates P on the points [{g^i}] for
      [i = 0...power]. [power] must be a power of 2 and [generator] must be a
      power-th root of unity *)
  val evaluation_fft :
    generator:scalar -> power:Z.t -> polynomial -> scalar list

  (** [generate_random_polynomial n] returns a random polynomial of degree n *)
  val generate_random_polynomial : natural_with_infinity -> polynomial

  (** [get_highest_coefficient P] where [P(X) = a_n X^n + ... a_0] returns [a_n] *)
  val get_highest_coefficient : polynomial -> scalar

  (** [interpolation_fft ~generator ~power [y_0 ; y_1 ;
      ... y_n]] computes the interpolation using FFT Cookey Tukey. The same
      conditions than for [evaluation_fft] must hold. [x_0] must be the
      evaluation of the generator *)
  val interpolation_fft :
    generator:scalar -> power:Z.t -> scalar list -> polynomial

  (** [polynomial_multiplication P Q] computes the
      product P(X).Q(X) *)
  val polynomial_multiplication : polynomial -> polynomial -> polynomial

  (** [polynomial_multiplication_fft ~generator:g ~power:n P Q] computes the
      product P(X).Q(X) using FFT. [g] is a [power]-th roots of unity.*)
  val polynomial_multiplication_fft :
    generator:scalar -> power:Z.t -> polynomial -> polynomial -> polynomial

  val euclidian_division_opt :
    polynomial -> polynomial -> (polynomial * polynomial) option

  (** [extended_euclide P S] returns (GCD, U, V) the greatest common divisor of P and S
        and the Bezout's coefficient:
      [U P + V S = GCD] and GCD greatest coefficient is one*)
  val extended_euclide :
    polynomial -> polynomial -> polynomial * polynomial * polynomial

  (** Infix operator for [equal] *)
  val ( = ) : polynomial -> polynomial -> bool

  (** Infix operator for [add] *)
  val ( + ) : polynomial -> polynomial -> polynomial

  (** Infix operator for [polynomial_multiplication] *)
  val ( * ) : polynomial -> polynomial -> polynomial

  (** Infix operator for [sub] *)
  val ( - ) : polynomial -> polynomial -> polynomial
end

module MakeUnivariate (R : Ff_sig.PRIME) = struct
  type scalar = R.t

  (* We encode the polynomials as a list with decresaing degree.
     All coefficient are non zero.
     a_n * X^n + ... a_1 X + a0 is encoded as [a_n ; ... ; a_1 ; a_0] with a_i non zero for all i
  *)
  type polynomial = (scalar * int) list

  let degree p =
    match p with
    | [] -> Infinity
    | [(e, 0)] -> if R.is_zero e then Infinity else Natural 0
    | _ as l -> Natural (snd (List.hd l))

  let degree_int p = match degree p with Infinity -> -1 | Natural n -> n

  let have_same_degree p q = degree p = degree q

  (* let shift_by_n p n =
   *   assert (n >= 1) ;
   *   List.map (fun (c, e) -> (c, e + n)) p *)

  let zero = []

  let one = [(R.one, 0)]

  let constants c = if c = R.zero then [] else [(c, 0)]

  let is_null p = p = []

  let is_constant p =
    match p with
    | [] -> true
    | l ->
        if List.length l > 1 then false
        else
          let (_, p) = List.hd l in
          if p = 0 then true else false

  let of_coefficients ?(remove_null_coefficients = true) l =
    (* check if the powers are all positive *)
    assert (List.for_all (fun (_e, power) -> power >= 0) l) ;
    (* Remove null coefficients *)
    let l =
      if remove_null_coefficients then
        List.filter (fun (e, _power) -> not (R.is_zero e)) l
      else l
    in
    (* sort by the power, higher power first *)
    let l =
      List.fast_sort
        (fun (_e1, power1) (_e2, power2) -> Int.sub power2 power1)
        l
    in
    l

  let add p1 p2 =
    let rec inner acc l1 l2 =
      match (l1, l2) with
      | ([], l) | (l, []) -> List.concat [List.rev acc; l]
      | (l1, l2) ->
          let (e1, p1) = List.hd l1 in
          let (e2, p2) = List.hd l2 in
          if p1 = p2 && R.is_zero (R.add e1 e2) then
            inner acc (List.tl l1) (List.tl l2)
          else if p1 = p2 then
            inner ((R.add e1 e2, p1) :: acc) (List.tl l1) (List.tl l2)
          else if p1 > p2 then inner ((e1, p1) :: acc) (List.tl l1) l2
          else inner ((e2, p2) :: acc) l1 (List.tl l2)
    in
    let l = inner [] p1 p2 in
    of_coefficients l

  let mult_by_scalar a p =
    List.filter_map
      (fun (coef, power) ->
        let c = R.mul coef a in
        if R.is_zero c then None else Some (c, power))
      p

  let opposite p = List.map (fun (e, p) -> (R.negate e, p)) p

  let sub p1 p2 = add p1 (opposite p2)

  let equal p1 p2 = p1 = p2

  let get_list_coefficients p = p

  let get_dense_polynomial_coefficients polynomial =
    match polynomial with
    | [] -> [R.zero]
    | l ->
        let l = List.rev l in
        let rec to_dense acc current_i l =
          match l with
          | [] -> acc
          | (e, n) :: xs ->
              if n = current_i then to_dense (e :: acc) (current_i + 1) xs
              else to_dense (R.zero :: acc) (current_i + 1) l
        in
        to_dense [] 0 l

  (* let get_dense_polynomial_coefficients_with_degree polynomial =
   *   let coefficients = get_dense_polynomial_coefficients polynomial in
   *   let n = List.length coefficients in
   *   List.mapi (fun i c -> (c, n - i - 1)) coefficients *)

  let evaluation polynomial point =
    let divide_by_xi polynomial i =
      List.map (fun (scalar, degree) -> (scalar, degree - i)) polynomial
    in
    let reversed_polynomial = List.rev polynomial in

    let rec aux reversed_polynomial (accumulated_point, degree_accumlated) =
      match reversed_polynomial with
      | [] -> R.zero
      | (scalar, degree) :: tail ->
          let point_degree =
            R.mul
              (R.pow point (Z.of_int @@ (degree - degree_accumlated)))
              accumulated_point
          in
          let degree_accumlated = degree in
          R.mul
            point_degree
            (R.add
               scalar
               (aux
                  (divide_by_xi tail degree)
                  (point_degree, degree_accumlated)))
    in
    aux reversed_polynomial (R.one, 0)

  let assert_no_duplicate_point points =
    let points = List.map fst points in
    let points_uniq =
      List.sort_uniq (fun e1 e2 -> if R.eq e1 e2 then 0 else -1) points
    in
    assert (List.length points = List.length points_uniq)

  let intermediate_lagrange_interpolation x_i i xs =
    List.fold_left
      (fun acc (j, x_j) ->
        if i = j then acc
        else
          match acc with
          | [] -> []
          | acc ->
              let acc_1 = List.map (fun (e, p) -> (e, p + 1)) acc in
              let acc_2 = mult_by_scalar x_j (of_coefficients acc) in
              let acc = add acc_1 (opposite acc_2) in
              let scalar = R.inverse_exn R.(x_i + R.negate x_j) in
              let acc_final = mult_by_scalar scalar acc in
              acc_final)
      (constants R.one)
      xs

  let lagrange_interpolation points =
    assert_no_duplicate_point points ;
    let indexed_points = List.mapi (fun i (x_i, y_i) -> (i, x_i, y_i)) points in
    let evaluated_at = List.mapi (fun i (x_i, _) -> (i, x_i)) points in
    List.fold_left
      (fun acc (i, x_i, y_i) ->
        let l_i = intermediate_lagrange_interpolation x_i i evaluated_at in
        add acc (mult_by_scalar y_i l_i))
      []
      indexed_points

  let even_polynomial polynomial =
    match polynomial with
    | [] -> []
    | l -> List.filter (fun (_e, n) -> n mod 2 = 0) l

  let odd_polynomial polynomial =
    match polynomial with
    | [] -> []
    | l -> List.filter (fun (_e, n) -> n mod 2 = 1) l

  let filter_mapi (f : int -> 'a -> 'b option) l =
    let l = List.mapi (fun i a -> (i, a)) l in
    List.filter_map (fun (i, a) -> f i a) l

  let evaluation_fft ~generator ~power polynomial =
    assert (Z.pow (Z.of_string "2") (Z.log2 power) = power) ;
    assert (R.is_one (R.pow generator power)) ;
    (* We only take exponents module the order. It is useful for inverse fft as we divide by the power *)
    assert (Z.leq power R.order) ;
    assert (power >= Z.one) ;
    let rec inner domain coefficients =
      match coefficients with
      | [] -> failwith "Must never happen"
      | l ->
          if List.length l = 1 then l
          else
            let new_domain =
              Array.of_list
                (filter_mapi
                   (fun i e -> if i mod 2 = 0 then Some e else None)
                   (Array.to_list domain))
            in
            let odd_coeffients =
              filter_mapi
                (fun i e -> if i mod 2 = 1 then Some e else None)
                coefficients
            in
            let even_coeffients =
              filter_mapi
                (fun i e -> if i mod 2 = 0 then Some e else None)
                coefficients
            in
            let odd_fft = inner new_domain odd_coeffients in
            let even_fft = inner new_domain even_coeffients in
            let combined_fft = List.combine even_fft odd_fft in
            (* only one allocation, used for the output initialization *)
            let zero = R.zero in
            let length_odd = List.length odd_coeffients in
            let output =
              Array.init (List.length coefficients) (fun _i -> zero)
            in
            List.iteri
              (fun i (x, y) ->
                let right = R.mul y domain.(i) in
                output.(i) <- R.add x right ;
                output.(i + length_odd) <- R.add x (R.negate right))
              combined_fft ;
            Array.to_list output
    in
    let domain =
      List.init (Z.to_int power) (fun i -> R.pow generator (Z.of_int i))
    in
    (* we reverse to have the scalar first and have the correspondance of the coefficients of degree n with the index of the list *)
    let coefficients =
      List.rev (get_dense_polynomial_coefficients polynomial)
    in
    assert (List.length domain = List.length coefficients) ;
    inner (Array.of_list domain) coefficients

  let generate_random_polynomial degree =
    let rec random_non_null () =
      let r = R.random () in
      if R.is_zero r then random_non_null () else r
    in
    match degree with
    | Infinity -> []
    | Natural n when n >= 0 ->
        let coefficients = List.init n (fun _i -> R.random ()) in
        let coefficients =
          (random_non_null (), n)
          :: List.mapi (fun i c -> (c, n - i - 1)) coefficients
        in
        of_coefficients coefficients
    | _ -> failwith "The degree must be positive"

  let get_highest_coefficient polynomial =
    match polynomial with [] -> R.zero | (c, _e) :: _ -> c

  let interpolation_fft ~generator ~power points =
    let polynomial =
      of_coefficients
        ~remove_null_coefficients:false
        (List.rev (List.mapi (fun i p -> (p, i)) points))
    in
    let inverse_generator = R.inverse_exn generator in
    let inverse_fft =
      evaluation_fft ~generator:inverse_generator ~power polynomial
    in
    let polynomial =
      of_coefficients (List.rev (List.mapi (fun i p -> (p, i)) inverse_fft))
    in
    mult_by_scalar (R.inverse_exn (R.of_z power)) polynomial

  let polynomial_multiplication p q =
    let mul_by_monom (scalar, int) p =
      List.map (fun (scalar_2, int_2) -> (R.mul scalar scalar_2, int + int_2)) p
    in
    List.fold_left (fun acc monom -> add acc (mul_by_monom monom q)) zero p

  let polynomial_multiplication_fft ~generator ~power p q =
    assert (R.eq (R.pow generator power) R.one) ;
    if is_null p || is_null q then zero
    else (
      assert (have_same_degree p q) ;
      assert (Z.pow (Z.of_string "2") (Z.log2 power) = power) ;
      let p_coefficients = get_dense_polynomial_coefficients p in
      let q_coefficients = get_dense_polynomial_coefficients q in
      let zero = R.zero in
      let p_coefficients =
        List.append
          p_coefficients
          (List.init
             (Z.to_int power - List.length p_coefficients)
             (fun _i -> zero))
      in
      let p_coefficients =
        List.mapi (fun i c -> (c, i)) (List.rev p_coefficients)
      in
      let q_coefficients =
        List.append
          q_coefficients
          (List.init
             (Z.to_int power - List.length q_coefficients)
             (fun _i -> zero))
      in
      let q_coefficients =
        List.mapi (fun i c -> (c, i)) (List.rev q_coefficients)
      in
      let p' =
        evaluation_fft ~generator ~power (of_coefficients p_coefficients)
      in
      let q' =
        evaluation_fft ~generator ~power (of_coefficients q_coefficients)
      in
      let coefficients = List.map2 (fun p_x q_x -> R.mul p_x q_x) p' q' in
      interpolation_fft ~generator ~power coefficients )

  let euclidian_division_opt a b =
    if is_null b then None
    else
      let deg_b = degree_int b in
      let highest_coeff_b = get_highest_coefficient b in
      let rec aux q r =
        if degree_int r < deg_b then Some (q, r)
        else
          let diff_degree = degree_int r - deg_b in
          let rescale_factor =
            R.(get_highest_coefficient r / highest_coeff_b)
          in
          let to_sub =
            polynomial_multiplication b [(rescale_factor, diff_degree)]
          in
          aux (add q [(rescale_factor, diff_degree)]) (sub r to_sub)
      in
      aux zero a

  let extended_euclide polynomial_1 polynomial_2 =
    let n_1 = degree_int polynomial_1 and n_2 = degree_int polynomial_2 in
    if n_1 = -1 then (polynomial_2, zero, one)
    else if n_2 = -1 then (polynomial_1, one, zero)
    else
      let rec aux poly_1 u_1 v_1 poly_2 u_2 v_2 =
        let (q, r) = euclidian_division_opt poly_1 poly_2 |> Option.get in
        if is_null r then (poly_2, u_2, v_2)
        else
          aux
            poly_2
            u_2
            v_2
            r
            (sub u_1 (polynomial_multiplication q u_2))
            (sub v_1 (polynomial_multiplication q v_2))
      in
      if n_2 > n_1 then
        let (gcd, u, v) = aux polynomial_2 one zero polynomial_1 zero one in
        let rescale_factor = R.inverse_exn @@ get_highest_coefficient gcd in
        ( mult_by_scalar rescale_factor gcd,
          mult_by_scalar rescale_factor v,
          mult_by_scalar rescale_factor u )
      else
        let (gcd, u, v) = aux polynomial_1 one zero polynomial_2 zero one in
        let rescale_factor = R.inverse_exn @@ get_highest_coefficient gcd in
        ( mult_by_scalar rescale_factor gcd,
          mult_by_scalar rescale_factor u,
          mult_by_scalar rescale_factor v )

  let ( = ) = equal

  let ( + ) = add

  let ( * ) = polynomial_multiplication

  let ( - ) = sub
end

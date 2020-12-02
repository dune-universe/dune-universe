type natural_with_infinity =
  | Natural of int
  | Infinity  (** General module signature for a ring [(A, +, *, 0_A, 1_A)] *)

(** Univariate polynomials *)
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

  (** [degree_int P] returns the degree of [P] as an integer. If [P(X) = 0], returns [-1] *)
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

  (* (\** Same than [get_dense_polynomial_coefficients] but adds the degree in the output *\)
   * val get_dense_polynomial_coefficients_with_degree :
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

  (** [opposite P] returns [-P(X)] where
      [-P(X) = -a_nX^n - a_(n - 1) X^(n - 1) - ... - a_0]
  *)
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
      be used in case of a FFT friendly scalar structure. It is supposed all x_i are different.
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

  (** [euclidian_division_opt P S] returns the unique Q, R in A[X] such that
      [P = Q * S + R] if such exists, else [None] *)
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

(** [Make(Fp)] builds a module of type [T] where the coefficients are in the prime field Fp *)
module MakeUnivariate : functor (R : Ff_sig.PRIME) ->
  UNIVARIATE with type scalar = R.t

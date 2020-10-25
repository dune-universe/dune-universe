(**************************************************************************)
(*                                                                        *)
(*                                FADBADml                                *)
(*                                                                        *)
(*           OCaml port by François Bidet and Ismail Bennani              *)
(*     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      *)
(*                                                                        *)
(*                          Copyright 2019-2020                           *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C license.    *)
(*                                                                        *)
(**************************************************************************)

(* preset arbitraries *)

let non_zero_float : float QCheck.arbitrary =
  QCheck.make
    ~print:string_of_float
    ~shrink:QCheck.Shrink.nil
    ~small:(fun _ -> 1)
    (let rec non_zero_float_gen rand =
       let f = QCheck.Gen.float rand in
       if f = 0. then non_zero_float_gen rand
       else f
     in non_zero_float_gen)

let non_zero_pfloat : float QCheck.arbitrary =
  QCheck.make
    ~print:string_of_float
    ~shrink:QCheck.Shrink.nil
    ~small:(fun _ -> 1)
    (let rec non_zero_pfloat_gen rand =
       let f = QCheck.Gen.pfloat rand in
       if f = 0. then non_zero_pfloat_gen rand
       else f
     in non_zero_pfloat_gen)

let int_float : float QCheck.arbitrary =
  QCheck.make
    ~print:string_of_float
    ~shrink:QCheck.Shrink.nil
    ~small:(fun _ -> 1)
    (let int_float_gen rand =
       let f = QCheck.Gen.int rand in float f
     in int_float_gen)

let int_float_range low high : float QCheck.arbitrary =
  QCheck.make
    ~print:string_of_float
    ~shrink:QCheck.Shrink.nil
    ~small:(fun _ -> 1)
    (let int_float_range_gen rand =
       let f = QCheck.Gen.int_range low high rand in float f
     in int_float_range_gen)

(* TEST CASES *)

let fact_hashtbl =
  let res = Hashtbl.create 10 in
  Hashtbl.add res 0 1; res
let rec fact n =
  if Hashtbl.mem fact_hashtbl n then Hashtbl.find fact_hashtbl n
  else begin
    let res = n * (fact (n-1)) in
    Hashtbl.add fact_hashtbl n res;
    res
  end

let rec pochhammer f i = if i = 0 then 1. else f *. (pochhammer (f+.1.) (i-1))

module Make(Op : Fadbad.OpS) =
struct

  type unary_test = {
    uname : string;
    udesc : string;
    uarbitrary : float QCheck.arbitrary;
    ufad : Op.t -> Op.t;
    (* udfdx i x = d^if/dx^i x for i >= 1 *)
    udfdx : int -> float -> float;
  }

  type binary_test = {
    bname : string;
    bdesc : string;
    barbitrary : (float * float) QCheck.arbitrary;
    bfad : Op.t -> Op.t -> Op.t;
    (* udfdx i x = d^if/dx^i x for i >= 1 *)
    bdfdx : int -> float -> float -> float;
    (* udfdy i x = d^if/dy^i x for i >= 1 *)
    bdfdy : int -> float -> float -> float;
  }

  let pi = 3.14159265359

  let test_pos =
    {
      uname = "pos";
      udesc = "f(x) = + x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.(+x));
      udfdx = (fun i x -> match i with 0 -> x | 1 -> 1. | _ -> 0.);
    }

  let test_neg =
    {
      uname = "neg";
      udesc = "f(x) = - x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.(-x));
      udfdx = (fun i x -> match i with 0 -> -. x | 1 -> -1. | _ -> 0.);
    }

  let test_inv =
    {
      uname = "inv";
      udesc = "f(x) = 1 / x";
      uarbitrary = non_zero_float;
      ufad = (fun x -> Op.inv x);
      udfdx = (fun i x ->
        (if i mod 2 = 0 then 1. else -1.) *. (float (fact i))
          /. (x ** (float (i+1))));
    }

  let test_sqr =
    {
      uname = "sqr";
      udesc = "f(x) = x * x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.sqr x);
      udfdx = (fun i x ->
                match i with
                | 0 -> x *. x
                | 1 -> 2. *. x
                | 2 -> 2.
                | _ -> 0.);
    }

  let test_sqrt =
    {
      uname = "sqrt";
      udesc = "f(x) = sqrt x";
      uarbitrary = QCheck.pos_float;
      ufad = (fun x -> Op.sqrt x);
      udfdx = (fun i x ->
        (if i mod 2 = 0 then 1. else -1.) *. (pochhammer (-0.5) i) *.
        (x ** (0.5 -. (float i))))
    }

  let test_log =
    {
      uname = "log";
      udesc = "f(x) = log x";
      uarbitrary = non_zero_pfloat;
      ufad = (fun x -> Op.log x);
      udfdx = (fun i x ->
                match i with
                | 0 -> log x
                | _ -> test_inv.udfdx (i-1) x);
    }

  let test_sin =
    {
      uname = "sin";
      udesc = "f(x) = sin x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.sin x);
      udfdx =
        let aux i x =
          match i with
          | 0 -> sin x
          | 1 -> cos x
          | 2 -> -. sin x
          | 3 -> -. cos x
          | _ -> assert false
        in (fun i x -> aux (i mod 4) x);
    }

  let test_cos =
    {
      uname = "cos";
      udesc = "f(x) = cos x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.cos x);
      udfdx = (fun i x ->
                match i with
                | 0 -> cos x
                | _ -> -. test_sin.udfdx (i-1) x);
    }

  let test_tan =
    {
      uname = "tan";
      udesc = "f(x) = tan x";
      uarbitrary = QCheck.float;
      ufad = (fun x -> Op.tan x);
      udfdx =
        (*
           let `P(0)=X` and `P(n+1)=(1 + X²)*(P(n-1))'` a serie of polynomials
           (where P' is the derivative of P)
           then the n-th derivative of tan(x) is P(n)(tan(x))
        *)
        let get_coeff poly i =
          if i < List.length poly then List.nth poly i else 0. in
        let mul poly1 poly2 =
          let rec aux poly1 poly2 i j =
            if j > i then 0. else
            (get_coeff poly1 j) *. (get_coeff poly2 (i-j))
              +. (aux poly1 poly2 i (j+1)) in
          List.init ((List.length poly1) + (List.length poly2))
            (fun i -> aux poly1 poly2 i 0) in
        let deriv poly = List.tl (List.mapi (fun i v -> (float i) *. v) poly) in
        let eval poly x =
          let rec aux acc l =
            match l with
            | [] -> acc
            | h::t -> aux (x *. acc +. h) t
          in
          aux 0. (List.rev poly) in
        let rec poly i =
          match i with
          | 0 -> [0.;1.]
          | _ -> mul [1.;0.;1.] (deriv (poly (i-1)))
        in (fun i x -> eval (poly i) (tan x));
    }

  let test_asin =
    {
      uname = "asin";
      udesc = "f(x) = asin x";
      uarbitrary = QCheck.float_range (-1.) 1.;
      ufad = (fun x -> Op.asin x);
      udfdx = (fun i x ->
        match i with
        | 0 -> asin x
        | 1 -> 1./.(sqrt (1.-.(x**2.)))
        | 2 -> x/.((1.-.(x**2.))**1.5)
        | 3 -> (2.*.(x**2.)+.1.)/.((1.-.(x**2.))**2.5)
        | 4 -> (3.*.x*.(2.*.(x**2.)+.3.))/.((1.-.(x**2.))**3.5)
        | 5 -> (24.*.(x**4.)+.72.*.(x**2.)+.9.)/.((1.-.(x**2.))**4.5)
        | 6 -> (15.*.x*.(8.*.(x**4.)+.40.*.(x**2.)+.15.))/.((1.-.(x**2.))**5.5)
        | 7 -> (45.*.(16.*.(x**6.)+.120.*.(x**4.)+.90.*.(x**2.)+.5.))/.((1.-.(x**2.))**6.5)
        | 8 -> (315.*.x*.(16.*.(x**6.)+.168.*.(x**4.)+.210.*.(x**2.)+.35.))/.((1.-.(x**2.))**7.5)
        | 9 -> (315.*.(128.*.(x**8.)+.1792.*.(x**6.)+.3360.*.(x**4.)+.1120.*.(x**2.)+.35.))/.((1.-.(x**2.))**8.5)
        | 10 -> (2835.*.x*.(128.*.(x**8.)+.2304.*.(x**6.)+.6048.*.(x**4.)+.3360.*.(x**2.)+.315.))/.((1.-.(x**2.))**9.5)
        | _ -> Printf.eprintf "derivative %d of asin/acos not defined" i; exit 1
      );
    }

  let test_acos =
    {
      uname = "acos";
      udesc = "f(x) = acos x";
      uarbitrary = QCheck.float_range (-1.) 1.;
      ufad = (fun x -> Op.acos x);
      udfdx = (fun i x ->
        match i with
        | 0 -> acos x
        | _ -> -. test_asin.udfdx i x
      );
    }

  let test_atan =
    {
      uname = "atan";
      udesc = "f(x) = atan x";
      uarbitrary = QCheck.float_range (-.pi /. 2.) (pi /. 2.);
      ufad = (fun x -> Op.atan x);
      udfdx =
        (*
          d^n atan(x) / dx^n (x) =
            (-sign x)^(n-1) * (n-1)! * sin^n(theta(x)) * sin(n*theta(x))
          with theta(x) = arcsin(1/sqrt(1+x²))

          proof : recursion.
            (we have that d theta(x) / dx = - (sign x) * sin²(n*theta(x)))
        *)
        let sign x = if x >= 0. then 1. else -1. in
        (fun i x ->
          match i with
          | 0 -> atan x
          | _ ->
            let n = float i in
            let sin_theta = 1. /. (sqrt (1. +. x *. x)) in
            let theta = asin sin_theta in
            ((if i mod 2 = 0 then -.(sign x) else 1.) *. (float (fact (i-1))))
              /. ((1. +. x *. x) ** (n /. 2.))
              *. (sin (n *. theta))
        );
    }

  let unary = [|
    test_pos; test_neg;
    test_inv; test_sqr; test_sqrt; test_log;
    test_sin; test_cos; test_tan;
    test_asin; test_acos; test_atan;
  |]

  let test_add =
    let der i x y = match i with | 0 -> x +. y | 1 -> 1. | _ -> 0. in
    {
      bname = "add";
      bdesc = "f(x, y) = x + y";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x + y));
      bdfdx = der;
      bdfdy = der;
    }

  let test_sub =
    let der a i x y = match i with | 0 -> x -. y | 1 -> a | _ -> 0. in
    {
      bname = "sub";
      bdesc = "f(x, y) = x - y";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x - y));
      bdfdx = der 1.;
      bdfdy = der (-1.);
    }

  let test_mul =
    {
      bname = "mul";
      bdesc = "f(x, y) = x * y";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x * y));
      bdfdx = (fun i x y -> (test_pos.udfdx i x) *. y);
      bdfdy = (fun i x y -> x *. (test_pos.udfdx i y));
    }

  let test_div =
    {
      bname = "div";
      bdesc = "f(x, y) = x / y";
      barbitrary = QCheck.pair QCheck.float QCheck.float;
      bfad = (fun x y -> Op.(x / y));
      bdfdx = (fun i x y -> (test_pos.udfdx i x) /. y);
      bdfdy = (fun i x y -> x *. (test_inv.udfdx i y));
    }

  let test_pow =
    {
      bname = "pow";
      bdesc = "f(x, y) = x ^ y";
      barbitrary = QCheck.pair (QCheck.float_range 0. 100.)
                               (QCheck.float_range (-100.) 100.);
      bfad = (fun x y -> Op.(x ** y));
      bdfdx = (fun i x y ->
        (pochhammer (1. -. (float i) +. y) i) *. x ** (y -. (float i)));
      bdfdy = (fun i x y -> ((log x) ** (float i)) *. x ** y);
    }

  let binary = [| test_add; test_sub; test_mul; test_div; test_pow |]
end

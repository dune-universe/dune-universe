open Core
open Typerep_experimental.Std

let print_str str =
  let sexp = Type_struct.sexp_of_t str in
  print_endline (Sexp.to_string_hum sexp)

module S = Type_struct

let check s1 s2 =
  if Type_struct.are_equivalent s1 s2 then true else begin
    print_endline "s1:";
    print_str s1;
    print_endline "s2:";
    print_str s2;
    false
  end

(* Tests about Type_struct rewriting *)
let%test_module _ = (module struct

  let str s =
    let versioned = Type_struct.Versioned.t_of_sexp (Sexp.of_string s) in
    Type_struct.Versioned.unserialize versioned

  let () =
    let t = Type_struct.incompatible in
    assert (not (Type_struct.are_equivalent (t()) (t())))

  let%test_unit _ =
    let s1 = str
      "(V3 (Record ((has_double_array_tag false)) ((t1 (Named 10 ((Record \
((has_double_array_tag false))((a Int)))))) (t2 (Named 10 ())))))"
    in
    let s2 = str
      "(V3 (Record ((has_double_array_tag false)) ((t1 (Named 42 ((Record \
((has_double_array_tag false)) ((a Int)))))) (t2 (Named 42 ())))))"
    in
    assert (check s1 s2);
    assert (check (S.alpha_conversion s1) (S.alpha_conversion s2))

  let%test_unit _ =
    let s1 = str
      "(V3 (Record ((has_double_array_tag false)) ((t1 (Record ((has_double_array_tag false))\
((a Int)))) (t2 (Record ((has_double_array_tag false)) ((a Int)))))))"
    in
    let s2 = str
      "(V3 (Record ((has_double_array_tag false)) ((t1 (Named 0 \
((Record ((has_double_array_tag false)) ((a Int)))))) (t2 (Named 0 ())))))"
    in
    assert (check s1 s2);
    assert (check (S.reduce s1) s2)

  let%test_unit _ =
    let module M1 = struct
      type t =
      | A
      | B
      [@@deriving typerep]
    end in
    let module M2 = struct
      type t =
      | B
      | A
      [@@deriving typerep]
    end in
    assert (not (S.are_equivalent
                   (S.of_typerep M1.typerep_of_t)
                   (S.of_typerep M2.typerep_of_t)))

  let%test_unit _ =
    let module M1 = struct
      type t = [
      | `A
      | `B
      ] [@@deriving typerep]
    end in
    let module M2 = struct
      type t = [
      | `B
      | `A
      ] [@@deriving typerep]
    end in
    assert (S.are_equivalent
              (S.of_typerep M1.typerep_of_t)
              (S.of_typerep M2.typerep_of_t))
end)

(* testing merge_unify *)
let%test_module _ = (module struct

  let print a str = print_endline ("str "^a^":"); print_str str
  let check name value = if not value then print_endline (name^": false"); value

  let merge should_match a b c =
    let a = S.of_typerep a in
    let b = S.of_typerep b in
    let c = S.of_typerep c in
    let result =
      try
        let ab = S.least_upper_bound_exn a b in
        let result =
          check "should match" should_match
          && check "a <= ab" (S.Diff.is_bin_prot_subtype ~subtype:a ~supertype:ab)
          && check "b <= ab" (S.Diff.is_bin_prot_subtype ~subtype:b ~supertype:ab)
          && check "a <= c" (S.Diff.is_bin_prot_subtype ~subtype:a ~supertype:c)
          && check "b <= c" (S.Diff.is_bin_prot_subtype ~subtype:b ~supertype:c)
          && check "ab === c" (S.are_equivalent ab c)
        in
        result || begin
          print "a" a;
          print "b" b;
          print "c" c;
          print "ab" ab;
          false
        end
      with
      | exn ->
        let result = check "should not match" (not should_match) in
        result || begin
          print_endline (Exn.to_string exn);
          print "a" a;
          print "b" b;
          print "c" c;
          false
        end
    in
    assert result

  module Const(X:sig type t [@@deriving typerep] end) = struct
    let%test_unit _ =
      merge true X.typerep_of_t X.typerep_of_t X.typerep_of_t
  end

  let%test_module _ = (module Const(struct type t = int [@@deriving typerep] end))
  let%test_module _ = (module Const(struct type t = int32 [@@deriving typerep] end))
  let%test_module _ = (module Const(struct type t = int64 [@@deriving typerep] end))
  let%test_module _ = (module Const(struct type t = char [@@deriving typerep] end))
  let%test_module _ = (module Const(struct type t = float [@@deriving typerep] end))
  let%test_module _ = (module Const(struct type t = string [@@deriving typerep] end))
  let%test_module _ = (module Const(struct type t = bool [@@deriving typerep] end))
  let%test_module _ = (module Const(struct type t = unit [@@deriving typerep] end))

  module V1 = struct
    type t =
    | V1 of int
    [@@deriving typerep]
  end

  module V2 = struct
    type t =
    | V1 of int
    | V2 of string
    [@@deriving typerep]
  end

  let%test_unit _ =
    merge true V1.typerep_of_t V2.typerep_of_t V2.typerep_of_t

  module A1 = struct
    type t =
    | V1 of V2.t
    [@@deriving typerep]
  end

  module A2 = struct
    type t =
    | V1 of V1.t
    | V2 of string
    [@@deriving typerep]
  end

  module A3 = struct
    type t =
    | V1 of V2.t
    | V2 of string
    [@@deriving typerep]
  end

  let%test_unit _ =
    merge true A1.typerep_of_t A2.typerep_of_t A3.typerep_of_t

  module B1 = struct
    type t = [
    | `V1 of V2.t
    | `V3 of int
    | `R of t
    ]
    [@@deriving typerep]
  end

  module B2 = struct
    type t = [
    | `V1 of V1.t
    | `V2 of string
    | `R of t
    ]
    [@@deriving typerep]
  end

  module B3 = struct
    type t = [
    | `V1 of V2.t
    | `V2 of string
    | `V3 of int
    | `R of t
    ]
    [@@deriving typerep]
  end

  let%test_unit _ =
    merge true A1.typerep_of_t A2.typerep_of_t A3.typerep_of_t

  module Bind(X:sig type 'a t [@@deriving typerep] end) = struct
    let%test_unit _ =
      let module A = struct
        type t = A1.t X.t [@@deriving typerep]
      end in
      let module B = struct
        type t = A2.t X.t [@@deriving typerep]
      end in
      let module C = struct
        type t = A3.t X.t [@@deriving typerep]
      end in
      let module A' = struct
        type t = B1.t X.t [@@deriving typerep]
      end in
      let module B' = struct
        type t = B2.t X.t [@@deriving typerep]
      end in
      let module C' = struct
        type t = B3.t X.t [@@deriving typerep]
      end in
      merge true A.typerep_of_t B.typerep_of_t C.typerep_of_t;
      merge true A'.typerep_of_t B'.typerep_of_t C'.typerep_of_t
  end

  let%test_module _ = (module Bind(struct
    type 'a t = {
      a : 'a;
      b : 'a * 'a;
      c : int;
    } [@@deriving typerep]
  end))

  let%test_module _ = (module Bind(struct
    type 'a t =
    | A of 'a * 'a
    | B of 'a
    | C of ('a * 'a)
    | D
    [@@deriving typerep]
  end))

  let%test_module _ = (module Bind(struct
    type 'a t = [
    | `A of 'a * 'a
    | `B
    | `C of 'a
    ] [@@deriving typerep]
  end))

  let%test_module _ = (module Bind(struct
    type 'a t = ('a * 'a * ('a * 'a)) [@@deriving typerep]
  end))

  let%test_module _ = (module Bind(struct
    type 'a t = 'a option [@@deriving typerep]
  end))

  let%test_module _ = (module Bind(struct
    type 'a t = 'a list [@@deriving typerep]
  end))

  let%test_module _ = (module Bind(struct
    type 'a t = 'a array [@@deriving typerep]
  end))

  let%test_module _ = (module Bind(struct
    type 'a t = 'a lazy_t [@@deriving typerep]
  end))

  let%test_module _ = (module Bind(struct
    type 'a t = 'a ref [@@deriving typerep]
  end))

end)

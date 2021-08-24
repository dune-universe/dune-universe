(** Generic functors for building property-based tests on some algebraic structures *)

open Basic_structures
open Generic

module Abelian_group
    (N : Name)
    (X : Generator)
    (G : Basic_intf.Abelian_group with type t = X.t) =
struct
  open X
  open G

  let n s = sf "%s: %s" N.name s

  let associative op x y z = op x (op y z) = op (op x y) z

  let commutative op x y = op x y = op y x

  let zero_neutral_l x = add x zero = x

  let zero_neutral_r x = add zero x = x

  let neg_inverse x = add x (neg x) = zero

  let zero_neg_inverse = apply X.gen neg_inverse

  let () =
    add_test (n "add_associative") @@ apply_triple X.gen (associative add)

  let () = add_test (n "add_commutative") @@ apply_pair X.gen (commutative add)

  let () = add_test (n "zero_add_neutral_l") @@ apply X.gen zero_neutral_l

  let () = add_test (n "zero_add_neutral_r") @@ apply X.gen zero_neutral_r

  let () = add_test (n "zero_neg_inverse") @@ zero_neg_inverse
end

module Monoid
    (N : Name)
    (X : Generator)
    (G : Basic_intf.Monoid with type t = X.t) =
struct
  open X
  open G

  let n s = sf "%s: %s" N.name s

  let associative op x y z = op x (op y z) = op (op x y) z

  let one_neutral_l x = mul x one = x

  let one_neutral_r x = mul one x = x

  let () = add_test (n "one_neutral_l") @@ apply X.gen one_neutral_l

  let () = add_test (n "one_neutral_r") @@ apply X.gen one_neutral_r

  let () =
    add_test (n "mul_associative") @@ apply_triple X.gen (associative mul)
end

module Ring (N : Name) (X : Generator) (G : Basic_intf.Ring with type t = X.t) =
struct
  module Abelian_group_props = Abelian_group (N) (X) (G)
  module Monoid_props = Monoid (N) (X) (G)
  open X
  open G

  let n s = sf "%s: %s" N.name s

  let neg_sub x y = add x (neg y) = sub x y

  let () = add_test (n "neg_sub") @@ apply_pair X.gen neg_sub
end

module Commutative_ring
    (N : Name)
    (X : Generator)
    (G : Basic_intf.Ring with type t = X.t) =
struct
  open X
  open G

  let n s = sf "%s: %s" N.name s

  module Abelian_group_props = Abelian_group (N) (X) (G)
  module Monoid_props = Monoid (N) (X) (G)

  let () =
    add_test (n "mul_commutative")
    @@ apply_pair X.gen (fun x y -> mul x y = mul y x)
end

module Module
    (N : Name)
    (R : Generator)
    (X : Generator)
    (M : Basic_intf.Module with type t = X.t and type R.t = R.t) =
struct
  open X

  let n s = sf "%s: %s" N.name s

  module Abelian_group_props = Abelian_group (N) (X) (M)

  let distribute_over_vector_add s x y =
    let open M in
    smul s (add x y) = add (smul s x) (smul s y)

  let scalar_add_distribute_over_smul s t x =
    let open M in
    smul (M.R.add s t) x = add (smul s x) (smul t x)

  let scalar_mul_distribute_over_smul s t x =
    let open M in
    smul (M.R.mul s t) x = smul s (smul t x)

  let scalar_mul_unit x =
    let open M in
    smul M.R.one x = x

  let () =
    add_test
      (n "distribute_over_vector_add")
      Crowbar.(map [R.gen; X.gen; X.gen] distribute_over_vector_add)

  let () =
    add_test
      (n "scalar_add_distribute_over_smul")
      Crowbar.(map [R.gen; R.gen; X.gen] scalar_add_distribute_over_smul)

  let () =
    add_test
      (n "scalar_mul_distribute_over_smul")
      Crowbar.(map [R.gen; R.gen; X.gen] scalar_mul_distribute_over_smul)

  let () = add_test (n "scalar_mul_unit") Crowbar.(map [X.gen] scalar_mul_unit)
end

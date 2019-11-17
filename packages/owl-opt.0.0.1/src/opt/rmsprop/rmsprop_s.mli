module Make (P : Prms.PT) :
  Rmsprop_intf.Sig
    with type 'a t := 'a P.t
     and type fv = Owl.Algodiff.S.t
     and type prm = Owl.Algodiff.S.t

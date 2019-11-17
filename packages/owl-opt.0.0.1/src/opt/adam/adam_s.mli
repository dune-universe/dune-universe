module Make (P : Prms.PT) :
  Adam_intf.Sig
    with type fv = Owl.Algodiff.S.t
     and type prm = Owl.Algodiff.S.t
     and type 'a t := 'a P.t

module Make (P : Prms.PT) :
  Adam_intf.Sig
    with type fv = Owl.Algodiff.D.t
     and type prm = Owl.Algodiff.D.t
     and type 'a t := 'a P.t

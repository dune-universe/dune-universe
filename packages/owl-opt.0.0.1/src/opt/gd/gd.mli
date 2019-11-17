module Make (AD : Owl_algodiff_generic_sig.Sig with type A.elt = float) (P : Prms.PT) :
  Gd_intf.Sig with type 'a t := 'a P.t and type fv = AD.t and type prm = AD.t

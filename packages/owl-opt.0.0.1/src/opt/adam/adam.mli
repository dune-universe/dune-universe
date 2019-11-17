module Make (AD : Owl_algodiff_generic_sig.Sig with type A.elt = float) (P : Prms.PT) :
  Adam_intf.Sig with type fv = AD.t and type prm = AD.t and type 'a t := 'a P.t

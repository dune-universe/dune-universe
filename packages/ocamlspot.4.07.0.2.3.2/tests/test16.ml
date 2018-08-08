type u = Test15.w (* ? type w *)

module type T = sig
  type (* type T.t => *) t (* <= type T.t *) = Qoo 
  type v = t (* ? type T.t *)
end

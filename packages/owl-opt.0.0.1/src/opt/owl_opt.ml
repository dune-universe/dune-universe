(** {1 Single-precision module} *)

module S = struct
  (** Single-precision vanilla gradient descent (see: {!module: Owl_opt.S.Gd.Make}). *)
  module Gd = Gd_s

  (** Single-precision Adam (see: {!module:Owl_opt.S.Adam.Make}). *)
  module Adam = Adam_s

  (** Single-precision Rmsprop (see: {!module:Owl_opt.S.Rmsprop.Make}). *)
  module Rmsprop = Rmsprop_s
end

(** {1 Double-precision module} *)

module D = struct
  (** Double-precision vanilla gradient descent (see: {!module:Owl_opt.D.Gd.Make}). *)
  module Gd = Gd_d

  (** Double-precision Adam (see: {!module:Owl_opt.D.Adam.Make}). *)
  module Adam = Adam_d

  (** Double-precision Rmsprop (see: {!module:Owl_opt.D.Rmsprop.Make}). *)
  module Rmsprop = Rmsprop_d
end

(** {1 Prm module type} *)

module Prms = Prms

(** {1 Learning rate module} *)

module Lr = Lr

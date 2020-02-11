(* Testing that @opaque is properly detected. *)

module T = struct
  type t = A of ((int -> int)[@deriving.visitors.opaque])
  [@@deriving visitors { variety = "map" }]
end

module U = struct
  type u = B of ((int -> int)[@visitors.opaque])
  [@@deriving visitors { variety = "map" }]
end

module V = struct
  type w = C of ((int -> int)[@opaque])
  [@@deriving visitors { variety = "map" }]
end

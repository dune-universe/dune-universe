module T = struct
  include Uuidm
  let create_random_id () = v `V4
  let nil_id = nil
  let sexp_of_t t =
    Base.Sexp.Atom (Uuidm.to_string t)
end
include T
include Base.Comparator.Make(T)
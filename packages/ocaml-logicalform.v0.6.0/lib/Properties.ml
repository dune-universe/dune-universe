module type Executable =
  sig
    type t

    val eval : t -> bool array -> bool option
  end

module type NeedsValidation =
  sig
    type t

    val validate : t -> t
  end

module type Conjunctable =
  sig
    type t

    val and_ : t list -> t
  end

module type Disjunctable =
  sig
    type t

    val or_ : t list -> t
  end

module type Negatable =
  sig
    type t

    val not_ : t -> t
  end
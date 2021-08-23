(** Shadow monomorphic comparison operators. *)

let ( =. ) (x : float) (y : float) = x = y [@@inline]

let ( <. ) (x : float) (y : float) = x < y [@@inline]

let ( >. ) (x : float) (y : float) = x > y [@@inline]

let ( <=. ) (x : float) (y : float) = x <= y [@@inline]

let ( >=. ) (x : float) (y : float) = x >= y [@@inline]

let ( = ) (x : int) (y : int) = x = y [@@inline]

let ( < ) (x : int) (y : int) = x < y [@@inline]

let ( > ) (x : int) (y : int) = x > y [@@inline]

let ( <= ) (x : int) (y : int) = x <= y [@@inline]

let ( >= ) (x : int) (y : int) = x >= y [@@inline]

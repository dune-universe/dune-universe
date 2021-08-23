module type Infix_order = sig
  type 'a m

  type t

  val ( < ) : t m -> t m -> bool m

  val ( > ) : t m -> t m -> bool m

  val ( <= ) : t m -> t m -> bool m

  val ( >= ) : t m -> t m -> bool m

  val ( = ) : t m -> t m -> bool m

  val ( <> ) : t m -> t m -> bool m
end

(** Overloaded values. You need poormans_overload patch *)

module Int = struct
  let (+) = (+)
  let (-) = (-)
  let ( * ) = ( * )
  let (/) = (/)
end

module Float = struct
  let (+) = (+.)
  let (-) = (-.)
  let ( * ) = ( *. )
  let (/) = (/.)
end

module Option = Option

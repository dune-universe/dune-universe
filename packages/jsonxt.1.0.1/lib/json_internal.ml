type 'a constrained =
  [< `Assoc of (string * 'a) list
  | `Bool of bool
  | `Float of float
  | `Floatlit of string
  | `Int of int
  | `Intlit of string
  | `List of 'a list
  | `Null
  | `String of string
  | `Stringlit of string
  | `Tuple of 'a list
  | `Variant of string * 'a option ] as 'a

type 'a constrained_stream = 'a constraint 'a =
  [<
  | `Null
  | `Bool of bool
  | `Int of int
  | `Intlit of string
  | `Float of float
  | `Floatlit of string
  | `String of string
  | `Stringlit of string
  | `As
  | `Ae
  | `Os
  | `Oe
  | `Ts
  | `Te
  | `Vs
  | `Ve
  | `Name of string
  | `Infinity
  | `Neg_infinity
  | `Nan
  ] as 'a

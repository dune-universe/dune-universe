type t =
  | Array of t array
  | Bool of bool
  | Bytes of bytes
  | Char of char
  | Float of float
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Lazy of t lazy_t
  | List of t list
  | Nativeint of nativeint
  | Option of t option
  | Ref of t ref
#if OCAML_VERSION >= (4, 08, 0)
  | Result of (t, t) result
#endif
  | String of string
  | Unit of unit
  | Tuple2 of (t * t)
  | Tuple3 of (t * t * t)
[@@deriving
     visitors { variety = "iter"; concrete = true },
     visitors { variety = "map"; concrete = true },
     visitors { variety = "reduce"; ancestors=["VisitorsRuntime.addition_monoid"]; concrete = true },
     visitors { variety = "endo"; concrete = true },
     visitors { variety = "iter2"; concrete = true },
     visitors { variety = "map2"; concrete = true },
     visitors { variety = "reduce2"; ancestors=["VisitorsRuntime.addition_monoid"]; concrete = true }
]

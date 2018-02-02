type t =
  [ `Expected of
    [ `Aval of bool
    | `Comment
    | `Eoi
    | `Json
    | `Name
    | `Name_sep
    | `Omem of bool
    | `Value
    ]
  | `Illegal_BOM
  | `Illegal_bytes of string
  | `Illegal_escape of
    [ `Lone_hi_surrogate of int
    | `Lone_lo_surrogate of int
    | `Not_esc_uchar of Uchar.t
    | `Not_hex_uchar of Uchar.t
    | `Not_lo_surrogate of int
    ]
  | `Illegal_literal of string
  | `Illegal_number of string
  | `Illegal_string_uchar of Uchar.t
  | `Unclosed of [ `As | `Comment | `Os | `String ]
]

val to_string : t -> string

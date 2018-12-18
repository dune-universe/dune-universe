(*
  Copyright (C) iNuron - info@openvstorage.com
  This file is part of Open vStorage. For license information, see <LICENSE.txt>
 *)


module Error = struct
  type msg = string [@@deriving show { with_path = false} ]

  type t =
    | KineticError of int * msg
    | Generic of string * int * msg
    | Timeout of float * msg
    | Assert of msg [@@deriving show { with_path = false }]

end

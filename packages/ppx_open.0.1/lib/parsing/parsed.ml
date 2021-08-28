open Ppxlib

module Module = struct
  type t = 
    { mod_ident : string
    ; mod_alias : string option
    }
  [@@deriving eq, show]
end

module Module_type = struct
  type t = 
    { mod_type_ident : string
    ; mod_type_alias : string option
    }
  [@@deriving eq, show]
end


module Value = struct
  type t =
    { val_ident : string
    ; val_alias : string option
    }
  [@@deriving eq, show]
end

module Type = struct
  type kind =
    | Kind_open
    | Kind_closed
  [@@deriving eq, show]

  type t =
    { type_ident : string
    ; type_alias : string option
    ; type_kind : kind
    }
  [@@deriving eq, show]
end

module Item = struct
  type t =
    | Type of Type.t
    | Value of Value.t
    | Module of Module.t
    | Module_type of Module_type.t
  [@@deriving eq, show]
end

module Payload = struct
  type t =
    { open_mod_ident : Longident.t
    ; open_items : Item.t list
    }
  [@@deriving eq, show]
end
exception GettextCamomileCreate of string * exn

exception GettextCamomileRecode of string * exn

module Charset : GettextCharset.CHARSET_TYPE

module Map : GettextTypes.REALIZE_TYPE

module Hashtbl : GettextTypes.REALIZE_TYPE

module Open : GettextTypes.REALIZE_TYPE

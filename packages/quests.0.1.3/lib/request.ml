type t = {
  url: string;
  headers: Cohttp.Header.t; [@printer Header_utils.pp_header]
}
[@@deriving show]

type payload =
  | Json of Yojson.t
  | Form of (string * string) list
  | Raw of string

type authentication = Basic of string * string | Bearer of string

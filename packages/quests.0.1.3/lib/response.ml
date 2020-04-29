type t = {
  content: string;
      [@printer
        fun fmt s -> fprintf fmt {|"%s"|} (Utils.escaped_literal_newlines s)]
  status_code: int;
  headers: Cohttp.Header.t; [@printer Header_utils.pp_header]
  request: Request.t;
}
[@@deriving show]

let content t = t.content

let status_code t = t.status_code

let headers t = t.headers

let ok { status_code; _ } = 200 <= status_code && status_code < 400

let json { content; _ } = Yojson.Safe.from_string content

let result_for_status response =
  if ok response then Ok response else Error response

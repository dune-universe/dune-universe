(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

(*
 * ref: http://rnyingma.synrc.com/publications/cat/Functional%20Languages/Erlang/BEAM.pdf
 * ref: http://www.erlang.se/~bjorn/beam_file_format.html
 *
 * Supported
 *
 * Atom
 * AtU8
 * Code
 * StrT
 * ImpT
 * ExpT
 *
 * FunT x
 * LitT
 * Line
 *
 * Attr
 * CInf
 * Trac x
 * Abst
 * Dbgi
 * LocT
 *
 *)

type atom_chunk_layout_t = {
  atom_count : int32;
  atom_buf   : Bitstring.t;
}

let parse_atom_chunk_layout (_, buffer) =
  let open Aux in
  match%bitstring buffer with
  | {| "Atom" : 4*8 : string
     ; size   : 4*8 : bigendian
     ; count  : 4*8 : bigendian
     ; buf    : content_size_bits size 1 : bitstring
     ; rest   : -1 : bitstring
     |} ->
     let v = {
       atom_count = count;
       atom_buf = buf;
     } in
     Ok (v, rest |> skip_padding_for_size size 1)
  | {| _ |} ->
     Error ()


type atu8_chunk_layout_t = {
  atu8_count : int32;
  atu8_buf   : Bitstring.t;
}

let parse_atu8_chunk_layout (_, buffer) =
  let open Aux in
  match%bitstring buffer with
  | {| "AtU8" : 4*8 : string
     ; size   : 4*8 : bigendian
     ; count  : 4*8 : bigendian
     ; buf    : content_size_bits size 1 : bitstring
     ; rest   : -1 : bitstring
     |} ->
     let v = {
       atu8_count = count;
       atu8_buf = buf;
     } in
     Ok (v, rest |> skip_padding_for_size size 1)
  | {| _ |} ->
     Error ()


type code_chunk_layout_t = {
  code_info_size      : int32;
  code_version        : int32;
  code_opcode_max     : int32;
  code_labels         : int32;
  code_function_count : int32;
  code_buf            : Bitstring.t;
}

let parse_code_chunk_layout (_, buffer) =
  let open Aux in
  match%bitstring buffer with
  | {| "Code"         : 4*8 : string
     ; size           : 4*8 : bigendian
     ; info_size      : 4*8 : bigendian
     ; version        : 4*8 : bigendian
     ; opcode_max     : 4*8 : bigendian
     ; labels         : 4*8 : bigendian
     ; function_count : 4*8 : bigendian
     ; buf            : content_size_bits size 5 : bitstring
     ; rest           : -1 : bitstring
     |} ->
     let v = {
       code_info_size = info_size;
       code_version = version;
       code_opcode_max = opcode_max;
       code_labels = labels;
       code_function_count = function_count;
       code_buf = buf;
     } in
     Ok (v, rest |> skip_padding_for_size size 5)
  | {| _ |} ->
     Error "Failed to parse code_chunk"


type strt_chunk_layout_t = {
  strt_buf : Bitstring.t;
}

let parse_strt_chunk_layout (_, buffer) =
  let open Aux in
  match%bitstring buffer with
  | {| "StrT" : 4*8 : string
     ; size   : 4*8 : bigendian
     ; buf    : content_size_bits size 0 : bitstring
     ; rest   : -1 : bitstring
     |} ->
     let v = {
       strt_buf = buf;
     } in
     Ok (v, rest |> skip_padding_for_size size 0)
  | {| _ |} ->
     Error "Failed to parse str_chunk"


type impt_chunk_layout_t = {
  impt_buf : Bitstring.t;
}

let parse_impt_chunk_layout (_, buffer) =
  let open Aux in
  match%bitstring buffer with
  | {| "ImpT" : 4*8 : string
     ; size   : 4*8 : bigendian
     ; count  : 4*8 : bigendian
     ; buf    : content_size_bits size 1 : bitstring
     ; rest   : -1 : bitstring
     |} ->
     let v = {
       impt_buf = buf;
     } in
     Ok (v, rest |> skip_padding_for_size size 1)
  | {| _ |} ->
     Error "Failed to parse import_table_chunk"


type expt_chunk_layout_t = {
  expt_buf : Bitstring.t;
}

let parse_expt_chunk_layout (_, buffer) =
  let open Aux in
  match%bitstring buffer with
  | {| "ExpT" : 4*8 : string
     ; size   : 4*8 : bigendian
     ; count  : 4*8 : bigendian
     ; buf    : content_size_bits size 1 : bitstring
     ; rest   : -1 : bitstring
     |} ->
     let v = {
       expt_buf = buf;
     } in
     Ok (v, rest |> skip_padding_for_size size 0)
  | {| _ |} ->
     Error "Failed to parse export_table_chunk"


type litt_chunk_layout_t = {
  litt_buf : Bitstring.t;
}

let parse_litt_chunk_layout (_, buffer) =
  let open Aux in
  match%bitstring buffer with
  | {| "LitT" : 4*8 : string
     ; size   : 4*8 : bigendian
     ; buf    : content_size_bits size 0 : bitstring
     ; rest   : -1 : bitstring
     |} ->
     let v = {
       litt_buf = buf;
     } in
     Ok (v, rest |> skip_padding_for_size size 0)
  | {| _ |} ->
     Error "Failed to parse literal_table_chunk"


type line_chunk_layout_t = {
  line_buf : Bitstring.t;
}

let parse_line_chunk_layout (_, buffer) =
  let open Aux in
  match%bitstring buffer with
  | {| "Line" : 4*8 : string
     ; size   : 4*8 : bigendian
     ; buf    : content_size_bits size 0 : bitstring
     ; rest   : -1 : bitstring
     |} ->
     let v = {
       line_buf = buf;
     } in
     Ok (v, rest |> skip_padding_for_size size 0)
  | {| _ |} ->
     Error "Failed to parse line_table_chunk"


type attr_chunk_layout_t = {
  attr_buf : Bitstring.t;
}

let parse_attr_chunk_layout (_, buffer) =
  let open Aux in
  match%bitstring buffer with
  | {| "Attr" : 4*8 : string
     ; size   : 4*8 : bigendian
     ; buf    : content_size_bits size 0 : bitstring
     ; rest   : -1 : bitstring
     |} ->
     let v = {
       attr_buf = buf;
     } in
     Ok (v, rest |> skip_padding_for_size size 0)
  | {| _ |} ->
     Error "Failed to parse attr_table_chunk"


type cinf_chunk_layout_t = {
  cinf_buf : Bitstring.t;
}

let parse_cinf_chunk_layout (_, buffer) =
  let open Aux in
  match%bitstring buffer with
  | {| "CInf" : 4*8 : string
     ; size   : 4*8 : bigendian
     ; buf    : content_size_bits size 0 : bitstring
     ; rest   : -1 : bitstring
     |} ->
     let v = {
       cinf_buf = buf;
     } in
     Ok (v, rest |> skip_padding_for_size size 0)
  | {| _ |} ->
     Error "Failed to parse cinf_table_chunk"


type abst_chunk_layout_t = {
  abst_buf : Bitstring.t;
}

let parse_abst_chunk_layout (_, buffer) =
  let open Aux in
  match%bitstring buffer with
  | {| "Abst" : 4*8 : string
     ; size   : 4*8 : bigendian
     ; buf    : content_size_bits size 0 : bitstring
     ; rest   : -1 : bitstring
     |} ->
     let v = {
       abst_buf = buf;
     } in
     Ok (v, rest |> skip_padding_for_size size 0)
  | {| _ |} ->
     Error "Failed to parse abst_table_chunk"


type dbgi_chunk_layout_t = {
  dbgi_buf : Bitstring.t;
}

let parse_dbgi_chunk_layout (_, buffer) =
  let open Aux in
  match%bitstring buffer with
  | {| "Dbgi" : 4*8 : string
     ; size   : 4*8 : bigendian
     ; buf    : content_size_bits size 0 : bitstring
     ; rest   : -1 : bitstring
     |} ->
     let v = {
       dbgi_buf = buf;
     } in
     Ok (v, rest |> skip_padding_for_size size 0)
  | {| _ |} ->
     Error "Failed to parse dbgi_table_chunk"


type loct_chunk_layout_t = {
  loct_buf : Bitstring.t;
}

let parse_loct_chunk_layout (_, buffer) =
  let open Aux in
  match%bitstring buffer with
  | {| "LocT" : 4*8 : string
     ; size   : 4*8 : bigendian
     ; count  : 4*8 : bigendian
     ; buf    : content_size_bits size 1 : bitstring
     ; rest   : -1 : bitstring
     |} ->
     let v = {
       loct_buf = buf;
     } in
     Ok (v, rest |> skip_padding_for_size size 1)
  | {| _ |} ->
     Error "Failed to parse local_function_table_chunk"

type chunks_layout_table_t = {
  cl_atom : atom_chunk_layout_t option;
  cl_atu8 : atu8_chunk_layout_t option;
  cl_code : code_chunk_layout_t option;
  cl_strt : strt_chunk_layout_t option;
  cl_impt : impt_chunk_layout_t option;
  cl_expt : expt_chunk_layout_t option;

  (* funt : funt_chunk_layout_t option; *)
  cl_litt : litt_chunk_layout_t option;
  cl_line : line_chunk_layout_t option;

  cl_attr : attr_chunk_layout_t option;
  cl_cinf : cinf_chunk_layout_t option;
  (* trac : trac_chunk_layout_t option; *)
  cl_abst : abst_chunk_layout_t option;
  cl_dbgi : dbgi_chunk_layout_t option;
  cl_loct : loct_chunk_layout_t option;
}

let empty_chunks_layout_table =
  {
    cl_atom = None;
    cl_atu8 = None;
    cl_code = None;
    cl_strt = None;
    cl_impt = None;
    cl_expt = None;

    (* funt = None; *)
    cl_litt = None;
    cl_line = None;

    cl_attr = None;
    cl_cinf = None;
    (* trac = None; *)
    cl_abst = None;
    cl_dbgi = None;
    cl_loct = None;
  }

let parse_layout buffer =
  match%bitstring buffer with
  | {| "FOR1" : 4*8 : string
     ; length : 4*8 : bigendian
     ; "BEAM" : 4*8 : string
     ; buf    : ((Int32.to_int length)-4)*8 : bitstring
     |} ->
     let open Parser.Combinator in
     let chunks_layout_parser =
         act parse_atom_chunk_layout (fun n p -> {p with cl_atom = Some n})
       / act parse_atu8_chunk_layout (fun n p -> {p with cl_atu8 = Some n})
       / act parse_code_chunk_layout (fun n p -> {p with cl_code = Some n})
       / act parse_strt_chunk_layout (fun n p -> {p with cl_strt = Some n})
       / act parse_impt_chunk_layout (fun n p -> {p with cl_impt = Some n})
       / act parse_expt_chunk_layout (fun n p -> {p with cl_expt = Some n})

       (* / funt *)
       / act parse_litt_chunk_layout (fun n p -> {p with cl_litt = Some n})
       / act parse_line_chunk_layout (fun n p -> {p with cl_line = Some n})

       / act parse_attr_chunk_layout (fun n p -> {p with cl_attr = Some n})
       / act parse_cinf_chunk_layout (fun n p -> {p with cl_cinf = Some n})
       (* / trac *)
       / act parse_abst_chunk_layout (fun n p -> {p with cl_abst = Some n})
       / act parse_dbgi_chunk_layout (fun n p -> {p with cl_dbgi = Some n})
       / act parse_loct_chunk_layout (fun n p -> {p with cl_loct = Some n})
     in
     let parser = repeat chunks_layout_parser >> Aux.eol in
     parser (empty_chunks_layout_table, buf)
  | {| _ |} ->
     Error ("failed to read header", buffer)

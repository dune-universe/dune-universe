open Printf

type location = Text of string
              | Raw_backtrace of Printexc.raw_backtrace

exception Traced of exn * location list

let wrap loc e =
  match e with
  | Traced (e, locs) -> Traced (e, loc :: locs)
  | e -> Traced (e, [loc])

let unwrap = function
  | Traced (e, _) -> e
  | e -> e

let wrap_with_stack_trace e =
  wrap (Raw_backtrace (Printexc.get_raw_backtrace ())) e

let raise_at loc e =
  Pervasives.raise (wrap loc e)

let raise txt_loc e =
  raise_at (Text txt_loc) e

let reraise_with_stack_trace e =
  raise_at (Raw_backtrace (Printexc.get_raw_backtrace ())) e

let add_loc buf loc =
  match loc with
  | Text s ->
      bprintf buf "\n%s" s
  | Raw_backtrace x ->
      let s = Printexc.raw_backtrace_to_string x in
      let len = String.length s in
      if len > 0 then
        let n =
          if s.[len-1] = '\n' then len - 1
          else len
        in
        Buffer.add_char buf '\n';
        Buffer.add_substring buf s 0 n

let to_string_aux with_exn e =
  match e with
  | Traced (e, locs) ->
      let buf = Buffer.create 500 in
      if with_exn then
        bprintf buf "%s" (Printexc.to_string e);
      List.iter (add_loc buf) (List.rev locs);
      Buffer.contents buf
  | e ->
      if with_exn then
        Printexc.to_string e
      else
        ""

let to_string e = to_string_aux true e
let get_trace e = to_string_aux false e

let print oc e =
  output_string oc (to_string e)

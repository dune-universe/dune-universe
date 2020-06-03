type desc =
  [ `Exception 
      of exn (*+ exception of the Decode.tuple, variant or record *)
  | `Unknown_fields 
      of string (*+ type name *) 
      *  string list (*+ unknown fields *)
      *  Obj.t (*+ value built from the known fields *)
  | `Unknown_tag of 
      string (*+ type name *) 
      * string (*+ unknown tag *)
  | `Required_field_not_found 
      of string (*+ type name *)
      *  string (*+ missing fields *)
  | `Wrong_arity 
      of int (*+ expected *) 
      *  int (*+ actual *) 
      *  (string * string) option (*+ type name and tag, if tuple, None *) 
  | `Primitive_decoding_failure 
      of string (*+ message *)
  | `Sub_decoders_failed_for_one_of 
      of string (*+ type name *)
  ]

type 'target trace = [ `Node of 'target | `Pos of int | `Field of string ] list  

type 'target t = desc * 'target * 'target trace

open Format

let rec format_list sep f ppf = function
  | [] -> ()
  | [x] -> f ppf x
  | x::xs -> f ppf x; fprintf ppf sep; format_list sep f ppf xs

let format_desc ppf = function
    | `Exception exn -> 
        fprintf ppf "Exception: %s" (Printexc.to_string exn)
    | `Unknown_fields (tyname, fields, _o) ->
        fprintf ppf "@[<v2>Unknown fields of type %s:@ [ @[%a@] ]@]"
          tyname
          (format_list ";@ " (fun ppf -> fprintf ppf "%s")) fields
    | `Unknown_tag (tyname, tag) ->
        fprintf ppf "@[<v2>Unknown tag of type %s: %s@]"
          tyname
          tag
    | `Required_field_not_found (tyname, field) ->
        fprintf ppf "Required field not found of type %s: %s"
          tyname
          field
    | `Wrong_arity (exp, act, None) ->
        fprintf ppf "Wrong arity of tuple %d (expected=%d)" act exp
    | `Wrong_arity (exp, act, Some (tyname, tag)) ->
        fprintf ppf "Wrong arity of type %s of tag %s, %d (expected=%d)" tyname tag act exp
    | `Primitive_decoding_failure mes ->
        fprintf ppf "Primitive decoding failure: %s" mes
    | `Sub_decoders_failed_for_one_of name ->
        fprintf ppf "All sub decoders failed for 'one_of' annotated type  %s" name
      

let format_trace_item f ppf = function 
  | `Pos n -> fprintf ppf "#%d" n
  | `Field s -> fprintf ppf ".%s" s
  | `Node n -> fprintf ppf "@[<2>node@ @[%a@]@]" f n

let format_trace f ppf = fprintf ppf "@[<v>%a@]" (format_list "@," (format_trace_item f))

let format f ppf (desc, target, trace : 'target t) = 
  fprintf ppf "@[<v>Error: %a@,Value: @[%a@]@,Trace:@,%a@]" 
    format_desc desc
    f target
    (format_trace f) trace

module LocalException(A : sig type t end) = struct
  exception Exception of A.t t
  let exn f v = match f v with
    | Ok v -> v
    | Error e -> raise (Exception e)
  let catch f v = try Ok (f v) with Exception e -> Error e
end

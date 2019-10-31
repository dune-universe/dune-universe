open Core
open S
open Stdio

module Writer : BsonWriter = struct

    type t =
        { mutable open_docs : (int * document_type) list
        ; mutable closed_docs : (int * int) list
        ; data : Buffer.t }

    exception Invalid_state of string

    let write_cstring d str =
        Buffer.add_string d.data str;
        Buffer.add_char d.data '\x00'

    let write_field c write_value d name v =
        Buffer.add_char d.data c;
        write_cstring d name;
        write_value d v

    let write_field_no_value c d name =
        Buffer.add_char d.data c;
        write_cstring d name

    let int32_to_bytes v =
        let rec helper i acc =
            if i >= 4
            then acc
            else
                let bits_to_shift = i * 8 in
                let b =
                    Int32.(shift_right v bits_to_shift land 255l
                           |> to_int_exn
                           |> Char.of_int_exn) in
                helper (i + 1) (b::acc) in
        helper 0 []
        |> List.rev

    let write_int32' d v =
        let rec helper i =
            if i >= 4
            then ()
            else
                let bits_to_shift = i * 8 in
                let b =
                    Int32.(shift_right v bits_to_shift land 255l
                           |> to_int_exn
                           |> Char.of_int_exn) in
                Buffer.add_char d.data b;
                helper (i + 1) in
        helper 0

    let write_string' d str =
        String.length str |> Int32.of_int_exn |> write_int32' d;
        Buffer.add_string d.data str;
        Buffer.add_char d.data '\x00'

    let write_string =
        write_field '\x02' write_string'

    let write_int32 =
        write_field '\x10' write_int32'

    let write_int64' d v =
        let rec helper i =
            if i >= 8
            then ()
            else
                let bits_to_shift = i * 8 in
                let b =
                    Int64.(shift_right v bits_to_shift land 255L
                           |> to_int_exn
                           |> Char.of_int_exn) in
                Buffer.add_char d.data b;
                helper (i + 1) in
        helper 0

    let write_int64 =
        write_field '\x12' write_int64'

    let write_float' d f =
        Int64.bits_of_float f
        |> write_int64' d

    let write_float =
        write_field '\x01' write_float'

    let write_bool =
        write_field '\x08'
            (fun d b ->
                let v =
                    if b then '\x01' else '\x00' in
                Buffer.add_char d.data v)

    let write_bytes size d bytes =
        let len = Bytes.length bytes in
        if len <> size
        then sprintf "Expected bytes of length %d but got bytes of length %d" size len |> failwith
        else
            Buffer.add_bytes d.data bytes

    let write_binary' subtype d bin =
        
        (* Write the size as an int32 *)
        let c =
            match subtype with
            | Generic -> '\x00'
            | Function -> '\x01'
            | Binary_old -> '\x02'
            | UUID_old -> '\x03'
            | UUID -> '\x04'
            | MD5 -> '\x05'
            | Encrypted -> '\x06'
            | User_defined -> '\x80' in
        (* Length of the binary content. *)
        Bytes.length bin
        |> Int32.of_int_exn     
        |> write_int32' d;
        
        (* Type of the binary content *)
        Buffer.add_char d.data c;
        (* The content itself. *)
        Buffer.add_bytes d.data bin

    let write_binary d name subtype bin =
        write_field '\x05' (write_binary' subtype) d name bin

    let write_timestamp =
        write_field '\x11' write_int64'

    let write_null =
        write_field_no_value '\x0A' 

    let write_minkey =
        write_field_no_value '\xFF'

    let write_maxkey =
        write_field_no_value '\x7F'

    let write_regex buf name ~pattern:p ~options:o =
        write_field_no_value '\x0B' buf name;
        write_cstring buf p;
        write_cstring buf o

    let create initial_size =
        let doc =
            { open_docs = [ 0, Document ] 
            ; closed_docs = []
            ; data = Buffer.create initial_size } in
        write_int32' doc 0l;
        doc

    let write_start' d document_type =
        let pos = Buffer.length d.data in
        write_int32' d 0l;
        d.open_docs <- (pos, document_type)::d.open_docs

    let write_start d name document_type =
        let c = document_type_to_char document_type in
        write_field_no_value c d name;
        write_start' d document_type

    let write_document_start d name =
        write_start d name Document

    let write_array_start d name =
        write_start d name Array

    let write_close d doc_type =
        match d.open_docs with
        | [] -> raise (Invalid_state "No open docs to close")
        | (start, typ)::tl ->
                if typ = doc_type
                then
                    let close = Buffer.length d.data in
                    Buffer.add_char d.data '\x00';
                    d.open_docs <- tl;
                    d.closed_docs <- (start, close)::d.closed_docs
                else 
                    let last_open = sexp_of_document_type doc_type |> string_of_sexp in
                    let try_close = sexp_of_document_type typ |> string_of_sexp in
                    let err_msg = sprintf "Cannot close %s: Open %s at position %d" try_close last_open start in
                    raise (Invalid_state err_msg)

    let write_document_close d =
        write_close d Document

    let write_array_close d =
        write_close d Array

    let write_decimal128 =
        write_field '\x13' (write_bytes 16)

    let write_js_with_scope d name jscode =
        write_field_no_value '\x0F' d name;
        let js_start = Buffer.length d.data in
        write_int32' d 0l;
        write_string' d jscode;
        let scope_start = Buffer.length d.data in
        d.open_docs <- (js_start, Js_code_w_scope scope_start)::d.open_docs;
        write_int32' d 0l

    let write_js_with_scope_close d =
        match d.open_docs with
        | [] -> raise (Invalid_state "No open docs to close")
        | (start, Js_code_w_scope scope_start)::tl ->
            let close = Buffer.length d.data in
            Buffer.add_char d.data '\x00';
            d.open_docs <- tl;
            d.closed_docs <- (start, close)::(scope_start, close)::d.closed_docs
        | (start, typ)::_ ->
            let last_open = sexp_of_document_type typ |> string_of_sexp in
            let msg = sprintf "Cannot close js code with scope: Open %s at position %d" last_open start in
            raise (Invalid_state msg)

        (* int32 string document *)
        (* Int32 is the length in bytes of the entire code_w_scope value *)
        (* Write the field. Then write the start of a document (or should it be start of js scope?) *)

    let write_js =
        write_field '\x0D' write_string'

    let write_objectid =
        write_field '\x07' (write_bytes 12)

    let write_utc_datetime =
        write_field '\x09' write_int64'

    let to_bytes { open_docs; closed_docs; data } =
        if List.is_empty open_docs
        then
            (* Fill in all document openings *) 
            let bytes =
                Buffer.contents_bytes data in
            List.iter
                ~f:(fun (doc_open, doc_close) ->
                    let length =
                        doc_close - doc_open + 1
                        |> Int32.of_int_exn
                        |> int32_to_bytes in
                    List.iteri length ~f:(fun i b -> Bytes.set bytes (doc_open + i) b)) closed_docs;
            Ok bytes
        else Error "Unclosed document"

    let to_string writer =
        Result.map
            (to_bytes writer)
            ~f:Bytes.to_string

    let to_out_channel writer chan =
        Result.map
            (to_string writer)
            (* TODO: close the underlying channel? *)
            ~f:(fun s -> Out_channel.output_string chan s)
end

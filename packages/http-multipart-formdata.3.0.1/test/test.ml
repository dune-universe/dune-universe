type boundary_result = (Http_multipart_formdata.boundary, string) result
[@@deriving show]

let%expect_test "parse_boundary" =
  "multipart/form-data; \
   boundary=---------------------------735323031399963166993862150"
  |> Http_multipart_formdata.boundary
  |> pp_boundary_result Format.std_formatter ;
  [%expect {| (Ok ---------------------------735323031399963166993862150) |}]

type parse_result =
  ((Http_multipart_formdata.part_header * string) list, string) result
[@@deriving show]

let%expect_test "parse_parts" =
  let body =
    String.concat "\r\n"
      [ {||}
      ; {| this is a preamble text.  |}
      ; {|-----------------------------735323031399963166993862150|}
      ; {|Content-Disposition: form-data; name="text1"|}
      ; {||}
      ; {|text default|}
      ; {|-----------------------------735323031399963166993862150|}
      ; {|Content-Disposition: form-data; name="text2"|}
      ; {||}
      ; {|aωb|}
      ; {|-----------------------------735323031399963166993862150|}
      ; {|Content-Disposition: form-data; name="file1"; filename="a.txt"|}
      ; {|Content-Type: text/plain|}
      ; {||}
      ; {|Content of a.txt.|}
      ; {||}
      ; {|-----------------------------735323031399963166993862150|}
      ; {|Content-Disposition: form-data; name="file2"; filename="a.html"|}
      ; {|Content-Type: text/html|}
      ; {||}
      ; {|<!DOCTYPE html><title>Content of a.html.</title><div>thiasdasdf asdfiasdf  asdf asdf as df asdf asdf as df asdf asd fa sdf asd fas df asdf as df asd fas df asdf as df asdfas df asd fa sdf as dfa sdf asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfsadfsadfasdfasdfasdfasdfasdfsadfasdfasdfasdfasdfasdfsadfasdfasdfasdfasdfasdfasdfasdfasdfasdf
asdfasdfasdfasdfasdfasdf|}
      ; {||}
      ; {|-----------------------------735323031399963166993862150|}
      ; {|Content-Disposition: form-data; name="file3"; filename="binary"|}
      ; {|Content-Type: application/octet-stream|}
      ; {||}
      ; {|aωb|}
      ; {||}
      ; {|-----------------------------735323031399963166993862150|}
      ; {|Content-Disposition: form-data; name="file3"; filename="binary"; param1=value1; param2=value2|}
      ; {|Content-Type: application/octet-stream|}
      ; {||}
      ; {|aωb|}
      ; {|-----------------------------735323031399963166993862150--|} ]
    |> Cstruct.of_string
  in
  let boundary =
    Http_multipart_formdata.boundary
      "multipart/form-data; \
       boundary=---------------------------735323031399963166993862150"
    |> Result.get_ok
  in
  let reader =
    Http_multipart_formdata.reader ~read_buffer_size:10 boundary (`Cstruct body)
  in
  let rec loop () =
    Http_multipart_formdata.read reader
    |> function
    | `End -> ()
    | rr ->
        Http_multipart_formdata.pp_read_result Format.std_formatter rr ;
        loop ()
  in
  loop () ;
  [%expect
    {|
    Header: name: text1;
            parameters: ;
            content_type: text/plain;
            filename:
    Body: 12, text default
    Body_end
    Header: name: text2;
            parameters: ;
            content_type: text/plain;
            filename:
    Body: 4, a\207\137b
    Body_end
    Header: name: file1;
            parameters: ;
            content_type: text/plain;
            filename: a.txt
    Body: 19, Content of a.txt.\r\n
    Body_end
    Header: name: file2;
            parameters: ;
            content_type: text/html;
            filename: a.html
    Body: 58, <!DOCTYPE html><title>Content of a.html.</title><div>thias
    Body: 58, dasdf asdfiasdf  asdf asdf as df asdf asdf as df asdf asd
    Body: 58, fa sdf asd fas df asdf as df asd fas df asdf as df asdfas
    Body: 58, df asd fa sdf as dfa sdf asdfasdfasdfasdfasdfasdfasdfasdfa
    Body: 58, sdfasdfasdfsadfsadfasdfasdfasdfasdfasdfsadfasdfasdfasdfasd
    Body: 58, fasdfsadfasdfasdfasdfasdfasdfasdfasdfasdfasdf\nasdfasdfasdf
    Body: 14, asdfasdfasdf\r\n
    Body_end
    Header: name: file3;
            parameters: ;
            content_type: application/octet-stream;
            filename: binary
    Body: 6, a\207\137b\r\n
    Body_end
    Header: name: file3;
            parameters: (param1, value1); (param2, value2);
            content_type: application/octet-stream;
            filename: binary
    Body: 4, a\207\137b
    Body_end |}]

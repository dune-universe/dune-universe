open Ctypes
open Foreign
open Unsigned

type t = unit ptr

type protocol_side =
 | Server
 | Client

type connection_type =
  | Stream
  | Datagram

exception Error of int

let check_err err =
  if err < 0 then raise (Error err)

let lib_path = "/System/Library/Frameworks/Security.framework/Versions/Current/Security"

let lib = Dl.dlopen ~filename:lib_path ~flags:[] 

let error_string = foreign ~from:lib "SecCopyErrorMessageString"
  (int @-> void @-> returning (ptr void))

let string_length = foreign "CFStringGetLength"
  (ptr void @-> returning int)

let max_size = foreign "CFStringGetMaximumSizeForEncoding"
  (int @-> int @-> returning int)

let kCFStringEncodingUTF8 = 0x08000100

let get_c_string = foreign "CFStringGetCString"
  (ptr void @-> ocaml_bytes @-> int @-> int @-> returning int)

let strlen = foreign "strlen"
  (ocaml_bytes @-> returning size_t)

let string_of_cfstring str =
  let str_len = string_length str in
  let buf_len =
    max_size str_len kCFStringEncodingUTF8
  in
  let buf = Bytes.create buf_len in
  let ret =
    get_c_string str (ocaml_bytes_start buf) buf_len kCFStringEncodingUTF8
  in
  if ret = 0 then raise Not_found;
  Bytes.sub_string buf 0 (Size_t.to_int (strlen (ocaml_bytes_start buf)))

let _release = foreign "CFRelease"
  (ptr void @-> returning void)

let unknown_code code =
  Printf.sprintf "Unknown error code: %d" code

let string_of_error_code code =
  let unknown_code = unknown_code code in
  let str = error_string code () in
  if ptr_compare str null = 0 then unknown_code else
    try
      string_of_cfstring str
    with Not_found -> unknown_code

let () =
  Printexc.register_printer (function
    | Error code ->
        Some (Printf.sprintf "SecureTransport.Error(%d): %s" code (string_of_error_code code))
    | _ -> None)

let fd_io_typ = funptr
  (int @-> ptr char @-> ptr size_t @-> returning int)

let posix_read = foreign ~release_runtime_lock:true ~check_errno:true "read"
  (int @-> ptr char @-> size_t @-> returning PosixTypes.ssize_t)

let io_wrapper fn fd _buf _ret =
  let rem = Size_t.to_int (!@ _ret) in
  let rec f _buf rem =
    if rem = 0 then 0 else
      let read =
        PosixTypes.Ssize.to_int
          (fn fd _buf (Size_t.of_int rem))
      in
      if read = 0 then rem else
        let _buf =
          _buf +@ read
        in
        f _buf (rem-read)
  in
  _ret <-@ Size_t.of_int (rem-(f _buf rem));
  0

let fd_read = io_wrapper posix_read

let posix_write = foreign ~release_runtime_lock:true ~check_errno:true "write"
  (int @-> ptr char @-> size_t @-> returning PosixTypes.ssize_t)

let fd_write = io_wrapper posix_write

let _set_io_funcs = foreign ~from:lib "SSLSetIOFuncs"
  (ptr void @-> fd_io_typ @-> fd_io_typ @-> returning int) 

let _create_context = foreign ~from:lib "SSLCreateContext"
  (ptr void @-> int @-> int @-> returning (ptr void))

let init s t =
  let ctx =
    _create_context null (Obj.magic s) (Obj.magic t)
  in
  Gc.finalise _release ctx;
  check_err (_set_io_funcs ctx fd_read fd_write);
  ctx

let _set_connection = foreign ~from:lib "SSLSetConnection"
  (ptr void @-> int @-> returning int)

let set_connection ctx fd =
  check_err (_set_connection ctx (Obj.magic fd))

let _set_peer_domain_name = foreign ~from:lib "SSLSetPeerDomainName"
  (ptr void @-> string @-> int @-> returning int)

let set_peer_domain_name ctx name =
  check_err(_set_peer_domain_name ctx name (String.length name))

let _handshake = foreign ~from:lib "SSLHandshake"
  (ptr void @-> returning int)

let handshake ctx =
  check_err(_handshake ctx)

let io_typ =
  (ptr void @-> ocaml_bytes @-> size_t @-> ptr size_t @-> returning int) 

let _read = foreign ~from:lib "SSLRead" io_typ

let read ctx buf ofs len =
  let _buf = Bytes.create len in
  let _ret = allocate size_t Size_t.zero in
  let _len = Size_t.of_int len in
  check_err(_read ctx (ocaml_bytes_start _buf)  _len _ret);
  let ret = Size_t.to_int (!@ _ret) in
  if ret > 0 then
    Bytes.blit _buf 0 buf ofs ret;
  ret

let _write = foreign ~from:lib "SSLWrite" io_typ

let write ctx buf ofs len =
  let _buf = Bytes.create len in
  Bytes.blit buf ofs _buf 0 len;
  let _ret = allocate size_t Size_t.zero in
  let _len = Size_t.of_int len in
  check_err(_write ctx (ocaml_bytes_start _buf) _len _ret);
  Size_t.to_int (!@ _ret)

let _close = foreign ~from:lib "SSLClose"
  (ptr void @-> returning int)

let close ctx =
  check_err(_close ctx)

let kSecImportExportPassphrase =
  !@ (foreign_value ~from:lib "kSecImportExportPassphrase" (ptr void))

let kSecImportItemIdentity =
  !@ (foreign_value ~from:lib "kSecImportItemIdentity" (ptr void))

let kSecImportItemCertChain =
  !@ (foreign_value ~from:lib "kSecImportItemCertChain" (ptr void))

let _import_p12_certificate = foreign ~from:lib "SecPKCS12Import"
  (ptr void @-> ptr void @-> ptr (ptr void) @-> returning int)

let create_data = foreign "CFDataCreate"
  (ptr void @-> ocaml_bytes @-> int @-> returning (ptr void))

let create_dictionary = foreign "CFDictionaryCreate"
  (ptr void @-> ptr (ptr void) @-> ptr (ptr void) @-> int @-> ptr void @-> ptr void @-> returning (ptr void))

let get_dictionary_value = foreign "CFDictionaryGetValue"
  (ptr void @-> ptr void @-> returning (ptr void))

let array_count = foreign "CFArrayGetCount"
  (ptr void @-> returning int)

let array_get_at_index = foreign "CFArrayGetValueAtIndex"
  (ptr void @-> int @-> returning (ptr void))

let create_array = foreign "CFArrayCreate"
  (ptr void @-> ptr (ptr void) @-> int @-> ptr void @-> returning (ptr void))

let create_string = foreign "CFStringCreateWithCString"
  (ptr void @-> ocaml_bytes @-> int @-> returning (ptr void))

type certificate = (unit ptr)*(unit ptr)

let import_p12_certificate ?password path =
  let options =
    match password with
      | Some v ->
          let keys = CArray.of_list (ptr void) [kSecImportExportPassphrase] in
          let v =
            create_string null (ocaml_bytes_start (Bytes.of_string v)) kCFStringEncodingUTF8 
          in
          Gc.finalise _release v;
          let values = CArray.of_list (ptr void) [v] in
          create_dictionary null (CArray.start keys) (CArray.start values) 1 null null
      | None ->
          let nullp = from_voidp (ptr void) null in
          create_dictionary null nullp nullp 0 null null
  in
  Gc.finalise _release options;
  let ch = open_in_bin path in
  let len = in_channel_length ch in
  let data = Bytes.create len in
  really_input ch data 0 len;
  close_in ch;
  let data =
    create_data null (ocaml_bytes_start data) len
  in
  Gc.finalise _release data;
  let items = allocate nativeint Nativeint.zero in
  let items = to_voidp items in
  let items = from_voidp (ptr void) items in
  check_err(_import_p12_certificate data options items);
  let items = !@ items in
  if ptr_compare null items <> 0 then
    Gc.finalise _release items;
  let items =
    Array.init (array_count items) (array_get_at_index items)
  in
  let items = Array.map (fun item ->
    let identity =
      get_dictionary_value item kSecImportItemIdentity
    in
    let certificates =
      get_dictionary_value item kSecImportItemCertChain
    in
    (identity,certificates)) items
  in
  Array.to_list items

let _set_certificate = foreign ~from:lib "SSLSetCertificate"
  (ptr void @-> ptr void @-> returning int)

let set_certificate ctx (identity,certificates) =
  let c = array_count certificates in
  let a =
    CArray.make (ptr void) (c + 1)
  in
  CArray.set a 0 identity;
  for n = 1 to c do
    CArray.set a n (array_get_at_index certificates (n-1))
  done;
  let a =
    create_array null (CArray.start a) 2 null
  in
  Gc.finalise _release a;
  check_err(_set_certificate ctx a)

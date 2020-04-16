(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)
open Rresult
open Astring

module Int32 = struct
  include Int32

  external ( ~- ) : t -> t = "%int32_neg"

  (* external (+) : t -> t -> t = "%int32_add"
   * external (-) : t -> t -> t = "%int32_sub" *)
  external ( lsl ) : t -> int -> t = "%int32_lsl"

  external ( lsr ) : t -> int -> t = "%int32_lsr"

  external ( land ) : t -> t -> t = "%int32_and"

  (* external (lor) : t -> t -> t = "%int32_or" *)
  external ( lxor ) : t -> t -> t = "%int32_xor"

  let of_char c = c |> Char.to_int |> of_int
end

let convertbits_exn ~pad ~frombits ~tobits data =
  let maxv = (1 lsl tobits) - 1 in
  let ret, bits, acc =
    String.fold_left
      (fun (ret, bits, acc) v ->
        let v = Char.to_int v in
        if v < 0 || v lsr frombits <> 0 then
          invalid_arg ("convertbits_exn: invalid data " ^ string_of_int v);
        let acc = (acc lsl frombits) lor v in
        let bits = bits + frombits in
        let rec loop ret bits =
          if bits >= tobits then
            let bits = bits - tobits in
            loop (((acc lsr bits) land maxv) :: ret) bits
          else (ret, bits)
        in
        let ret, bits = loop ret bits in
        (ret, bits, acc))
      ([], 0, 0) data
  in
  let ret =
    let shift = tobits - bits in
    if pad then if bits <> 0 then ((acc lsl shift) land maxv) :: ret else ret
    else if (acc lsl shift) land maxv <> 0 || bits >= frombits then
      invalid_arg "convertbits_exn"
    else ret
  in
  let buf = Bytes.create (List.length ret) in
  let _ =
    List.fold_right
      (fun c i ->
        Bytes.set buf i (Char.of_byte c);
        succ i)
      ret 0
  in
  Bytes.unsafe_to_string buf

let convertbits ~pad ~frombits ~tobits data =
  try Ok (convertbits_exn ~pad ~frombits ~tobits data)
  with Invalid_argument msg -> Error msg

let polymod_step pre =
  let open Int32 in
  let b = pre lsr 25 in
  ((pre land 0x1ffffffl) lsl 5)
  lxor (-((b lsr 0) land 1l) land 0x3b6a57b2l)
  lxor (-((b lsr 1) land 1l) land 0x26508e6dl)
  lxor (-((b lsr 2) land 1l) land 0x1ea119fal)
  lxor (-((b lsr 3) land 1l) land 0x3d4233ddl)
  lxor (-((b lsr 4) land 1l) land 0x2a1462b3l)

let charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

let charset_rev =
  [|
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    15;
    -1;
    10;
    17;
    21;
    20;
    26;
    30;
    7;
    5;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    29;
    -1;
    24;
    13;
    25;
    9;
    8;
    23;
    -1;
    18;
    22;
    31;
    27;
    19;
    -1;
    1;
    0;
    3;
    16;
    11;
    28;
    12;
    14;
    6;
    4;
    2;
    -1;
    -1;
    -1;
    -1;
    -1;
    -1;
    29;
    -1;
    24;
    13;
    25;
    9;
    8;
    23;
    -1;
    18;
    22;
    31;
    27;
    19;
    -1;
    1;
    0;
    3;
    16;
    11;
    28;
    12;
    14;
    6;
    4;
    2;
    -1;
    -1;
    -1;
    -1;
    -1;
  |]

let invalid_hrp c = c < '\033' || c > '\126' || (c >= 'A' && c <= 'Z')

let check_hrp chk i hrp =
  try
    let chk, i =
      String.fold_left
        (fun (a, i) c ->
          if invalid_hrp c then raise Exit;
          (Int32.(polymod_step a lxor (of_char c lsr 5)), succ i))
        (chk, i) hrp
    in
    Ok (chk, i)
  with Exit -> Error "invalid hrp"

let encode5 ~hrp data =
  let datalen = String.length data in
  check_hrp 1l 0 hrp >>= fun (chk, i) ->
  (if i + 7 + datalen > 90 then Error "data too long" else Ok ()) >>= fun () ->
  let chk = polymod_step chk in
  let buf = Bytes.create String.(length hrp + 1 + datalen + 6) in
  let chk, i =
    String.fold_left
      (fun (a, i) c ->
        Bytes.set buf i c;
        ( Int32.logxor (polymod_step a) (Char.to_int c land 0x1f |> Int32.of_int),
          succ i ))
      (chk, 0) hrp
  in
  Bytes.set buf i '1';
  let chk, i =
    String.fold_left
      (fun (a, i) c ->
        let c = Char.to_int c in
        Bytes.set buf i charset.[c];
        (Int32.logxor (polymod_step a) (Int32.of_int c), succ i))
      (chk, succ i)
      data
  in
  let chk =
    List.fold_left (fun a () -> polymod_step a) chk [ (); (); (); (); (); () ]
  in
  let chk = Int32.(chk lxor 1l) in
  let compute_chk chk i =
    let shift = (5 - i) * 5 in
    Int32.((chk lsr shift) land 0x1fl |> to_int)
  in
  for j = 0 to 5 do
    Bytes.set buf (i + j) charset.[compute_chk chk j]
  done;
  Ok (Bytes.unsafe_to_string buf)

let encode ~hrp data =
  convertbits ~pad:true ~frombits:8 ~tobits:5 data >>= encode5 ~hrp

let decode bech32 =
  let len = String.length bech32 in
  (if len < 8 || len > 90 then Error "bad input length" else Ok ())
  >>= fun () ->
  match String.cut ~rev:true bech32 ~sep:"1" with
  | None -> Error "missing separator"
  | Some (hrp, payload) ->
      let hrplen = String.length hrp in
      let payloadlen = len - 1 - hrplen in
      ( if hrplen < 1 || payloadlen < 6 then Error "bad hrp/payload length"
      else Ok () )
      >>= fun () ->
      let have_upper = ref false in
      let have_lower = ref false in
      ( try
          String.fold_left
            (fun a c ->
              let cint = Char.(to_int (Ascii.lowercase c)) in
              if cint < 33 || cint > 126 then raise Exit;
              if Char.Ascii.is_upper c then have_upper := true;
              if Char.Ascii.is_lower c then have_lower := true;
              Int32.logxor (polymod_step a) (cint lsr 5 |> Int32.of_int))
            1l hrp
          |> R.ok
        with Exit -> Error ("invalid hrp char in " ^ hrp) )
      >>= fun chk ->
      let chk = polymod_step chk in
      let chk =
        String.fold_left
          (fun a c ->
            Int32.logxor (polymod_step a)
              (Char.to_int c land 0x1f |> Int32.of_int))
          chk hrp
      in
      let buf = Bytes.create (payloadlen - 6) in
      ( try
          String.fold_left
            (fun (a, i) c ->
              if Char.Ascii.is_upper c then have_upper := true;
              if Char.Ascii.is_lower c then have_lower := true;
              let ci = Char.to_int c in
              if ci land 0x80 <> 0 then raise Exit;
              let v = charset_rev.(ci) in
              if i + 6 < payloadlen then Bytes.set buf i (Char.of_byte v);
              (Int32.logxor (polymod_step a) (Int32.of_int v), succ i))
            (chk, 0) payload
          |> fst |> R.ok
        with _ -> Error "invalid data" )
      >>= fun chk ->
      if !have_upper <> !have_lower && chk = 1l then
        Ok (hrp, Bytes.unsafe_to_string buf)
      else Error "wrong chksum or mixed case"

module Segwit = struct
  module type NETWORK = sig
    type t

    val t : t

    val version : bool

    val prefix : string
  end

  module Btc = struct
    type t = [ `Btc ]

    let t = `Btc

    let version = true

    let prefix = "bc"
  end

  module Tbtc = struct
    type t = [ `Tbtc ]

    let t = `Tbtc

    let version = true

    let prefix = "tb"
  end

  module Zil = struct
    type t = [ `Zil ]

    let t = `Zil

    let version = false

    let prefix = "zil"
  end

  type 'a t = {
    network : (module NETWORK with type t = 'a);
    version : int option;
    prog : string;
  }

  let scriptPubKey { prog; version; _ } =
    match version with
    | None -> invalid_arg "scriptPubKey: version"
    | Some v when v < 0 || v > 16 -> invalid_arg "scriptPubKey: version"
    | Some version ->
        let proglen = String.length prog in
        let buf = Bytes.create (proglen + 2) in
        Bytes.set buf 0
          (Char.of_byte (match version with 0 -> 0 | n -> 0x50 + n));
        Bytes.set buf 1 (Char.of_byte proglen);
        Bytes.blit_string prog 0 buf 2 proglen;
        Bytes.unsafe_to_string buf

  let check_version v =
    if v < 0 || v > 16 then invalid_arg "invalid Segwit version"

  let create ?version network prog =
    Option.iter check_version version;
    { network; version; prog }

  let encode (type a) ({ network; version; prog } : a t) =
    let module N = (val network : NETWORK with type t = a) in
    let prog = convertbits_exn ~pad:true ~frombits:8 ~tobits:5 prog in
    let proglen = String.length prog in
    match version with
    | None ->
        let buf = Bytes.create proglen in
        Bytes.blit_string prog 0 buf 0 proglen;
        encode5 ~hrp:N.prefix (Bytes.unsafe_to_string buf)
    | Some version ->
        let buf = Bytes.create (proglen + 1) in
        Bytes.set buf 0 (Char.of_byte version);
        Bytes.blit_string prog 0 buf 1 proglen;
        encode5 ~hrp:N.prefix (Bytes.unsafe_to_string buf)

  let encode_exn t =
    match encode t with
    | Error e -> invalid_arg ("encode_exn: " ^ e)
    | Ok v -> v

  let pp ppf t = Format.fprintf ppf "%s" (encode_exn t)

  let decode (type a) network addr =
    let module N = (val network : NETWORK with type t = a) in
    decode addr >>= fun (hrp, data) ->
    let datalen = String.length data in
    (if datalen < 5 then Error "invalid segwit data" else Ok ()) >>= fun () ->
    let hrplow = String.Ascii.lowercase hrp in
    if hrplow <> N.prefix then Error ("invalid segwit hrp " ^ hrp)
    else
      Ok () >>= fun () ->
      match N.version with
      | false ->
          convertbits data ~pad:false ~frombits:5 ~tobits:8 >>= fun decoded ->
          let decodedlen = String.length decoded in
          ( if decodedlen = 0 || decodedlen < 2 || decodedlen > 40 then
            Error "invalid segwit data"
          else Ok () )
          >>= fun () ->
          ( if decodedlen <> 20 && decodedlen <> 32 then
            Error "invalid segwit length"
          else Ok () )
          >>= fun () -> Ok (create network decoded)
      | true ->
          let decoded =
            String.(sub data ~start:1 ~stop:datalen |> Sub.to_string)
          in
          convertbits decoded ~pad:false ~frombits:5 ~tobits:8
          >>= fun decoded ->
          let decodedlen = String.length decoded in
          ( if decodedlen = 0 || decodedlen < 2 || decodedlen > 40 then
            Error "invalid segwit data"
          else Ok () )
          >>= fun () ->
          let version = data.[0] |> Char.to_int in
          (if version > 16 then Error "invalid segwit version" else Ok ())
          >>= fun () ->
          ( if version = 0 && decodedlen <> 20 && decodedlen <> 32 then
            Error "invalid segwit length"
          else Ok () )
          >>= fun () -> Ok (create ~version network decoded)

  let decode_exn net t =
    match decode net t with
    | Error e -> invalid_arg ("decode_exn: " ^ e)
    | Ok v -> v
end

(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

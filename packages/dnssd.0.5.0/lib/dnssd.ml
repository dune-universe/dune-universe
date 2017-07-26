(*
 * Copyright (C) 2017 Docker Inc <dave.scott@docker.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

let src =
  let src = Logs.Src.create "dnssd" ~doc:"DNS-SD interface" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

[@@@warning "-37"]

type kDNSServiceType =
  | A
  | NS
  | MD
  | MF
  | CNAME
  | SOA
  | MB
  | MG
  | MR
  | NULL
  | WKS
  | PTR
  | HINFO
  | MINFO
  | MX
  | TXT
  | RP
  | AFSDB
  | X25
  | ISDN
  | RT
  | NSAP
  | NSAP_PTR
  | SIG
  | KEY
  | PX
  | GPOS
  | AAAA
  | LOC
  | NXT
  | EID
  | NIMLOC
  | SRV
  | ATMA
  | NAPTR
  | KX
  | CERT
  | A6
  | DNAME
  | SINK
  | OPT
  | APL
  | DS
  | SSHFP
  | IPSECKEY
  | RRSIG
  | NSEC
  | DNSKEY
  | DHCID
  | NSEC3
  | NSEC3PARAM
  | HIP
  | SPF
  | UINFO
  | UID
  | GID
  | UNSPEC
  | TKEY
  | TSIG
  | IXFR
  | AXFR
  | MAILB
  | MAILA
  | ANY

[@@@warning "+37"]

external int_of_DNSServiceType: kDNSServiceType -> int = "stub_int_of_DNSServiceType"

let kDNSServiceType_of_q_type = function
  | Dns.Packet.Q_A -> Ok A
  | Dns.Packet.Q_NS -> Ok NS
  | Dns.Packet.Q_MD -> Ok MD
  | Dns.Packet.Q_MF -> Ok MF
  | Dns.Packet.Q_CNAME -> Ok CNAME
  | Dns.Packet.Q_SOA -> Ok SOA
  | Dns.Packet.Q_MB -> Ok MB
  | Dns.Packet.Q_MG -> Ok MG
  | Dns.Packet.Q_MR -> Ok MR
  | Dns.Packet.Q_NULL -> Ok NULL
  | Dns.Packet.Q_WKS -> Ok WKS
  | Dns.Packet.Q_PTR -> Ok PTR
  | Dns.Packet.Q_HINFO -> Ok HINFO
  | Dns.Packet.Q_MINFO -> Ok MINFO
  | Dns.Packet.Q_MX -> Ok MX
  | Dns.Packet.Q_TXT -> Ok TXT
  | Dns.Packet.Q_RP -> Ok RP
  | Dns.Packet.Q_AFSDB -> Ok AFSDB
  | Dns.Packet.Q_X25 -> Ok X25
  | Dns.Packet.Q_ISDN -> Ok ISDN
  | Dns.Packet.Q_RT -> Ok RT
  | Dns.Packet.Q_NSAP -> Ok NSAP
  | Dns.Packet.Q_NSAPPTR -> Error (`Msg "NSAPPTR query type not supported")
  | Dns.Packet.Q_SIG -> Ok SIG
  | Dns.Packet.Q_KEY -> Ok KEY
  | Dns.Packet.Q_PX -> Ok PX
  | Dns.Packet.Q_GPOS -> Ok GPOS
  | Dns.Packet.Q_AAAA -> Ok AAAA
  | Dns.Packet.Q_LOC -> Ok LOC
  | Dns.Packet.Q_NXT -> Ok NXT
  | Dns.Packet.Q_EID -> Ok EID
  | Dns.Packet.Q_NIMLOC -> Ok NIMLOC
  | Dns.Packet.Q_SRV -> Ok SRV
  | Dns.Packet.Q_ATMA -> Ok ATMA
  | Dns.Packet.Q_NAPTR -> Ok NAPTR
  | Dns.Packet.Q_KM -> Error (`Msg "KM query type not supported")
  | Dns.Packet.Q_CERT -> Ok CERT
  | Dns.Packet.Q_A6 -> Ok A6
  | Dns.Packet.Q_DNAME -> Ok DNAME
  | Dns.Packet.Q_SINK -> Ok SINK
  | Dns.Packet.Q_OPT -> Ok OPT
  | Dns.Packet.Q_APL -> Ok APL
  | Dns.Packet.Q_DS -> Ok DS
  | Dns.Packet.Q_SSHFP -> Ok SSHFP
  | Dns.Packet.Q_IPSECKEY -> Ok IPSECKEY
  | Dns.Packet.Q_RRSIG -> Ok RRSIG
  | Dns.Packet.Q_NSEC -> Ok NSEC
  | Dns.Packet.Q_DNSKEY -> Ok DNSKEY
  | Dns.Packet.Q_NSEC3 -> Ok NSEC3
  | Dns.Packet.Q_NSEC3PARAM -> Ok NSEC3PARAM
  | Dns.Packet.Q_SPF -> Ok SPF
  | Dns.Packet.Q_UINFO -> Ok UINFO
  | Dns.Packet.Q_UID -> Ok UID
  | Dns.Packet.Q_GID -> Ok GID
  | Dns.Packet.Q_UNSPEC -> Ok UNSPEC
  | Dns.Packet.Q_AXFR -> Ok AXFR
  | Dns.Packet.Q_MAILB -> Ok MAILB
  | Dns.Packet.Q_MAILA -> Ok MAILA
  | Dns.Packet.Q_ANY_TYP -> Error (`Msg "ANY_TYP query type not supported")
  | Dns.Packet.Q_TA -> Error (`Msg "TA query type not supported")
  | Dns.Packet.Q_DLV -> Error (`Msg "DLV query type not supported")
  | Dns.Packet.Q_UNKNOWN x -> Error (`Msg (Printf.sprintf "Unknown query type %d" x))

type error =
  | Unknown
  | NoSuchName
  | NoMemory
  | BadParam
  | BadReference
  | BadState
  | BadFlags
  | Unsupported
  | NotInitialized
  | AlreadyRegistered
  | NameConflict
  | Invalid
  | Firewall
  | Incompatible
  | BadInterfaceIndex
  | Refused
  | NoSuchRecord
  | NoAuth
  | NoSuchKey
  | NATTraversal
  | DoubleNAT
  | BadTime
  | BadSig
  | BadKey
  | Transient
  | ServiceNotRunning
  | NATPortMappingUnsupported
  | NATPortMappingDisabled
  | NoRouter
  | PollingMode
  | Timeout

let string_of_error = function
  | Unknown -> "Unknown"
  | NoSuchName -> "NoSuchName"
  | NoMemory -> "NoMemory"
  | BadParam -> "BadParam"
  | BadReference -> "BadReference"
  | BadState -> "BadState"
  | BadFlags -> "BadFlags"
  | Unsupported -> "Unsupported"
  | NotInitialized -> "NotInitialized"
  | AlreadyRegistered -> "AlreadyRegistered"
  | NameConflict -> "NameConflict"
  | Invalid -> "Invalid"
  | Firewall -> "Firewall"
  | Incompatible -> "Incompatible"
  | BadInterfaceIndex -> "BadInterfaceIndex"
  | Refused -> "Refused"
  | NoSuchRecord -> "NoSuchRecord"
  | NoAuth -> "NoAuth"
  | NoSuchKey -> "NoSuchKey"
  | NATTraversal -> "NATTraversal"
  | DoubleNAT -> "DoubleNAT"
  | BadTime -> "BadTime"
  | BadSig -> "BadSig"
  | BadKey -> "BadKey"
  | Transient -> "Transient"
  | ServiceNotRunning -> "ServiceNotRunning"
  | NATPortMappingUnsupported -> "NATPortMappingUnsupported"
  | NATPortMappingDisabled -> "NATPortMappingDisabled"
  | NoRouter -> "NoRouter"
  | PollingMode -> "PollingMode"
  | Timeout -> "Timeout"

(* Low-level, unsafe APIs *)

type dNSServiceRef

let next_token =
  let i = ref 0 in
  fun () ->
    let this = !i in
    incr i;
    this

(* The callback fires once per result *)
type cb_result = {
  cb_fullname: string;
  cb_rrtype: int;
  cb_rrclass: int;
  cb_rrdata: Bytes.t;
  cb_ttl: int;
}

(* Accumulate the results here *)
let in_progress_calls = Hashtbl.create 7

type token = int

external query_record: string -> int -> token -> dNSServiceRef = "stub_query_record"

external query_fd: dNSServiceRef -> Unix.file_descr = "stub_query_fd"

external query_process: dNSServiceRef -> unit = "stub_query_process"

external query_deallocate: dNSServiceRef -> unit = "stub_query_deallocate"

external is_supported_on_this_platform: unit -> bool = "stub_is_supported_on_this_platform"

let common_callback token result = match result with
  | Error err ->
    Hashtbl.replace in_progress_calls token (Error err)
  | Ok this ->
    let buf = Cstruct.create (Bytes.length this.cb_rrdata) in
    Cstruct.blit_from_bytes this.cb_rrdata 0 buf 0 (Bytes.length this.cb_rrdata);
    let rr = match Dns.Packet.int_to_rr_type this.cb_rrtype with
      | None -> None
      | Some rrtype ->
        begin
          try
            let rdata = Dns.Packet.parse_rdata (Hashtbl.create 1) 0 rrtype this.cb_rrclass (Int32.of_int this.cb_ttl) buf in
            let name = Dns.Name.of_string this.cb_fullname in
            let cls = Dns.Packet.RR_IN in
            let ttl = Int32.of_int this.cb_ttl in
            if this.cb_rrclass = 1
            then Some { Dns.Packet.name; cls; flush = false; ttl; rdata }
            else None
          with Dns.Packet.Not_implemented ->
            None
        end in
    begin match rr with
    | None ->
      Log.warn (fun f ->
        let buffer = Buffer.create 128 in
        Cstruct.hexdump_to_buffer buffer buf;
        f "Failed to parse resource record: fullname = %s; rrtype = %d; rrclass = %d; rrdata(%d) = %s; ttl = %d"
          this.cb_fullname this.cb_rrtype this.cb_rrclass (Bytes.length this.cb_rrdata)
          (Buffer.contents buffer) this.cb_ttl
      )
    | Some rr ->
      if Hashtbl.mem in_progress_calls token then begin
        match Hashtbl.find in_progress_calls token with
        | Error _ -> () (* keep the error *)
        | Ok existing ->
          (* [1] list is accumulated backwards, see below [2] *)
          Hashtbl.replace in_progress_calls token (Ok (rr :: existing))
      end else Hashtbl.replace in_progress_calls token (Ok [ rr ])
    end

let query_one name ty =
  match kDNSServiceType_of_q_type ty with
  | Error (`Msg m) -> failwith m
  | Ok ty' ->
    let ty'' = int_of_DNSServiceType ty' in
    if ty'' < 0 then failwith "Unrecognised query type";
    let token = next_token () in
    let q = query_record name ty'' token in
    query_process q;
    (* [2] list is accumulated backwards, see above [1] *)
    let result = match Hashtbl.find in_progress_calls token with
      | Error e -> Error e
      | Ok xs -> Ok (List.rev xs) in
    Hashtbl.remove in_progress_calls token;
    query_deallocate q;
    result

let query requested_name ty =
  (* The DNSServiceRef API will return CNAMEs first, without resolving to
     A/AAAA/... This function recursively resolves the CNAMES while avoiding
     returning duplicate records. *)
  (* NB we only return NoSuchRecord if we find no records. This is because it
     is possible to query a CNAME which exists, but which points to a non-existent
     record. *)
  let rec loop acc name ty =
    let name' = Dns.Name.to_string name in
    match query_one name' ty with
    (* When we're recursing, ignore the NoSuchRecord error *)
    | Error NoSuchRecord when name' <> requested_name -> Ok acc
    | Error e -> Error e
    | Ok rrs ->
      let not_seen_before = List.filter (fun x -> not (List.mem x acc)) rrs in
      (* If there are any CNAMEs, resolve these too *)
      let cnames = List.rev @@ List.fold_left (fun acc rr ->
        match rr.Dns.Packet.rdata with
        | CNAME name -> name :: acc
        | _ -> acc
      ) [] not_seen_before in
      List.fold_left
        (fun acc name -> match acc with
          | Error e -> Error e
          | Ok acc ->
            loop acc name ty
        ) (Ok (acc @ not_seen_before)) cnames in
  loop [] (Dns.Name.of_string requested_name) ty

module LowLevel = struct
  type query = {
    query: dNSServiceRef;
    token: int;
    mutable cancelled: bool;
  }

  let query name ty =
    match kDNSServiceType_of_q_type ty with
    | Error (`Msg m) -> failwith m
    | Ok ty' ->
      let ty'' = int_of_DNSServiceType ty' in
      if ty'' < 0 then failwith "Unrecognised query type";
      let token = next_token () in
      let query = query_record name ty'' token in
      let cancelled = false in
      { query; token; cancelled }

  exception Cancelled

  let socket { query; cancelled; _ } =
    if cancelled then raise Cancelled;
    query_fd query

  let response { query; token; cancelled } =
    if cancelled then raise Cancelled;
    query_process query;
    (* [2] list is accumulated backwards, see above [1] *)
    let result = match Hashtbl.find in_progress_calls token with
      | Error e -> Error e
      | Ok xs -> Ok (List.rev xs) in
    Hashtbl.remove in_progress_calls token;
    query_deallocate query;
    result

  let cancel q =
    q.cancelled <- true;
    query_deallocate q.query

end

let () =
  Callback.register "ocaml-osx-dnssd" common_callback

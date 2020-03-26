type cf = {alg: [`SHA1 | `SHA256 | `SHA512]; trunc: int option}

type di =
  { c: bool
  ; q: [`A | `N | `H] * int
  ; p: [`SHA1 | `SHA256 | `SHA512] option
  ; s: int option
  ; t: int option }

type t = {cf: cf; di: di * string}

type timestamp = [`Now | `Int64 of int64]

type pinhash = [`String of string | `Digest of Cstruct.t]

open Rresult

type err = Invalid_suite_string | DataInput of string | Window of string

let t_of_string suitestring =
  let open Astring.String in
  let to_list s =
    let rec aux i l = if i < 0 then l else aux (i - 1) (s.[i] :: l) in
    aux (length s - 1) []
  in
  let of_list l = concat (List.map of_char l) in
  let die () = failwith "invalid suite string" in
  let num l mi ma =
    let rec aux l acc =
      match l with
      | [] when acc >= mi && acc <= ma -> acc
      | n :: tl when n >= '0' && n <= '9' ->
          aux tl ((acc * 10) + (int_of_char n - int_of_char '0'))
      | _ -> die ()
    in
    aux l 0
  in
  let dgst = function
    | "SHA1" -> `SHA1
    | "SHA256" -> `SHA256
    | "SHA512" -> `SHA512
    | _ -> die ()
  in
  let datainput s =
    let hms = function
      | 'H' -> (3600, 48)
      | 'M' -> (60, 59)
      | 'S' -> (1, 59)
      | _ -> die ()
    in
    let timestep = function
      | y, [] -> (y, None)
      | y, [h] -> (
          ( y
          , match to_list h with
            | ['T'; x1; s] ->
                let hms, _ = hms s in
                Some (hms * num [x1] 1 9)
            | ['T'; x1; x2; s] ->
                let hms, max = hms s in
                Some (hms * num [x1; x2] 1 max)
            | _ -> die () ) )
      | _ -> die ()
    in
    let session = function
      | y, [] -> ((y, None), [])
      | y, h :: t -> (
        match to_list h with
        | ['S'; x1; x2; x3] -> ((y, Some (num [x1; x2; x3] 0 999)), t)
        | _ -> ((y, None), h :: t) )
    in
    let pinhash = function
      | y, [] -> ((y, None), [])
      | y, h :: t -> (
        match to_list h with
        | 'P' :: r -> ((y, Some (dgst (of_list r))), t)
        | _ -> ((y, None), h :: t) )
    in
    let nah = function 'N' -> `N | 'A' -> `A | 'H' -> `H | _ -> die () in
    let questions = function
      | y, h :: t ->
          ( ( y
            , match to_list h with
              | ['Q'; s; x1; x2] -> (nah s, num [x1; x2] 4 64)
              | _ -> die () )
          , t )
      | _ -> die ()
    in
    let counter = function
      | h :: t when h = "C" -> (true, t)
      | xs -> (false, xs)
    in
    let ( |> ) x f = f x in
    let (((c, q), p), s), t =
      cuts ~sep:"-" s |> counter |> questions |> pinhash |> session |> timestep
    in
    {c; q; p; s; t}
  in
  let cryptofunction s =
    match cuts ~sep:"-" s with
    | ["HOTP"; a; t] ->
        let trunc =
          match num (to_list t) 0 10 with
          | 0 -> None
          | x when x > 3 -> Some x
          | _ -> die ()
        in
        {alg= dgst a; trunc}
    | _ -> die ()
  in
  try
    let cf_s, di_s =
      match cuts ~sep:":" suitestring with
      | ["OCRA-1"; c; d] -> (c, d)
      | _ -> die ()
    in
    Ok {cf= cryptofunction cf_s; di= (datainput di_s, suitestring)}
  with Failure _ -> Error Invalid_suite_string

let string_of_t {cf= _; di= _, s} = s

let di_of_t {cf= _; di= di, _} = di

let challenge {di= {q= qf, ql; _}, _; _} =
  let b = Mirage_crypto_rng.generate ql in
  let s = Bytes.create ql in
  let rec aux i m a =
    if i >= ql then ()
    else
      let c = char_of_int (a + (int_of_char (Cstruct.get_char b i) mod m)) in
      Bytes.set s i c ;
      aux (i + 1) m a
  in
  match qf with
  | `A -> aux 0 93 33 ; Bytes.unsafe_to_string s
  | `N -> aux 0 10 48 ; Bytes.unsafe_to_string s
  | `H -> ( match Hex.of_cstruct b with `Hex s -> s )

let crypto_function cf key buf =
  let hmac = Mirage_crypto.Hash.mac cf.alg ~key buf in
  match cf.trunc with
  | None -> hmac
  | Some x ->
      let v =
        let open Cstruct in
        let o = get_uint8 hmac (len hmac - 1) land 0x0f in
        BE.get_uint32 hmac o
      in
      let s0 =
        let open Int64 in
        to_string (logand (of_int32 v) 0x7fffffffL)
      in
      let s1 =
        let open String in
        match length s0 with
        | n when n > x -> sub s0 (n - x) x
        | n when n < x -> make (x - n) '0' ^ s0
        | _ -> s0
      in
      Cstruct.of_string s1

let format_data_input ?time (di, ss) c q p s t =
  let open Cstruct in
  let cs_64 i =
    let cs = create 8 in
    BE.set_uint64 cs 0 i ; cs
  in
  (* suite string, 0 terminated *)
  let fss = of_string (ss ^ "\000") in
  (* C, optional *)
  let fc =
    match (di.c, c) with
    | false, None -> create 0
    | true, Some i -> cs_64 i
    | false, Some _ -> failwith "no C in suite"
    | true, None -> failwith "suite requires C"
  in
  (* Q, mandatory *)
  let fq =
    let hex2bin q =
      let x = match String.length q mod 2 with 1 -> q ^ "0" | _ -> q in
      try Mirage_crypto_pk.Z_extra.to_cstruct_be (Z.of_string_base 16 x)
      with Invalid_argument _ -> failwith "invalid Q"
    in
    let dec2bin q =
      let z = Z.of_string q in
      let s0 =
        match Hex.of_cstruct (Mirage_crypto_pk.Z_extra.to_cstruct_be z) with
        | `Hex "" -> "000"
        | `Hex h -> h
      in
      let s =
        match s0.[0] with
        | '0' -> String.sub s0 1 (String.length s0 - 1)
        | _ -> s0
      in
      hex2bin s
    in
    let qbuf = create 128 in
    memset qbuf 0 ;
    let y =
      match fst di.q with
      | `A -> of_string q
      | `N -> dec2bin q
      | `H -> hex2bin q
    in
    blit y 0 qbuf 0 (len y) ;
    qbuf
  in
  (* P, optional *)
  let fp =
    match (di.p, p) with
    | None, None -> create 0
    | Some dgst, Some (`Digest y) when Mirage_crypto.Hash.digest_size dgst = len y -> y
    | Some _, Some (`Digest _) -> failwith "P length conflicts suite"
    | Some dgst, Some (`String s) -> Mirage_crypto.Hash.digest dgst (Cstruct.of_string s)
    | None, Some _ -> failwith "no P in suite"
    | Some _, None -> failwith "suite requires P"
  in
  (* S, optional *)
  let fs =
    match (di.s, s) with
    | None, None -> create 0
    | Some n, Some y when len y = n -> y
    | Some _, Some _ -> failwith "S length conflicts suite"
    | None, Some _ -> failwith "no S in suite"
    | Some _, None -> failwith "suite requires S"
  in
  (* T, optional *)
  let ft =
    match (di.t, t, time) with
    | None, None, _ -> create 0
    | Some _, Some (`Int64 i), _ -> cs_64 i
    | Some y, Some `Now, Some time ->
        let open Int64 in
        cs_64 (div time (of_int y))
    | Some _, Some `Now, None ->
        failwith
          "no timestamp provided, try: (Unix.time() |> Astring.String.of_float)"
    | None, Some _, _ -> failwith "no T in suite"
    | Some _, None, _ -> failwith "suite requires T"
  in
  let c_off = match c with None -> None | Some _ -> Some (len fss) in
  let t_off =
    match t with
    | None -> None
    | Some _ ->
        Some (List.fold_left (fun a y -> a + len y) 0 [fss; fc; fq; fp; fs])
  in
  (Cstruct.concat [fss; fc; fq; fp; fs; ft], (c_off, t_off))

let gen1 ?time ~c ~p ~s ~t ~key ~q suite =
  try
    let buf = fst (format_data_input ?time suite.di c q p s t) in
    Ok (crypto_function suite.cf key buf)
  with Failure f -> Error (DataInput f)

let gen ?time ?c ?p ?s ?t ~key ~q suite = gen1 ?time ~c ~p ~s ~t ~key ~q suite

let verify1 ?time ~c ~p ~s ~t ~cw ~tw ~key ~q ~a suite =
  try
    let buf0, (c_off, t_off) = format_data_input ?time suite.di c q p s t in
    let verify_c buf =
      match (c_off, c, cw) with
      | _, _, None -> Ok (crypto_function suite.cf key buf = a, None)
      | Some c_off, Some c, Some cw1 when cw1 > 0 ->
          let ce = Int64.add c (Int64.of_int cw1) in
          let rec loop next =
            match crypto_function suite.cf key buf = a with
            | true -> (true, Some next)
            | false when next = ce -> (false, None)
            | false ->
                Cstruct.BE.set_uint64 buf c_off next ;
                loop (Int64.add next 0x01L)
          in
          Ok (loop (Int64.add c 0x01L))
      | _ -> Error (Window "invalid counter window or no C in suite")
    in
    match (t_off, tw) with
    | _, None -> verify_c buf0
    | Some t_off, Some tw1 when tw1 > 0 ->
        let t_start, t_stop =
          let t = Cstruct.BE.get_uint64 buf0 t_off in
          let w = Int64.of_int tw1 in
          (Int64.sub t w, Int64.add t w)
        in
        let rec loop t_next =
          Cstruct.BE.set_uint64 buf0 t_off t_next ;
          match verify_c buf0 with
          | Ok (true, next_c) -> Ok (true, next_c)
          | Ok (false, None) when t_next = t_stop -> Ok (false, None)
          | Error e -> Error e
          | _ -> loop (Int64.add t_next 1L)
        in
        loop t_start
    | _ -> Error (Window "invalid timestamp window or no T in suite")
  with Failure f -> Error (DataInput f)

let verify ?time ?c ?p ?s ?t ?cw ?tw ~key ~q ~a suite =
  verify1 ?time ~c ~p ~s ~t ~cw ~tw ~key ~q ~a suite

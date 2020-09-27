type decode = [ `Data of string | `Await | `End | `CRLF | `Spaces of string ]

type decoder = {
  mutable i : bytes;
  mutable i_pos : int;
  mutable i_len : int;
  mutable has_cr : bool;
  b : Buffer.t;
  (* XXX(dinosaure): we should replace it by something else
     where it can be an entry point for an attack. *)
  mutable k : decoder -> decode;
}

let i_rem d = d.i_len - d.i_pos + 1

let end_of_input d =
  d.i <- Bytes.empty ;
  d.i_pos <- 0 ;
  d.i_len <- min_int

let src decoder source off len =
  if off < 0 || len < 0 || off + len > Bytes.length source
  then Fmt.invalid_arg "Invalids bounds"
  else if len = 0
  then end_of_input decoder
  else (
    decoder.i <- source ;
    decoder.i_pos <- off ;
    decoder.i_len <- off + len - 1)

let ret k v decoder =
  decoder.k <- k ;
  v

let rec t_end decoder =
  decoder.k <- t_end ;
  `End

and t_space j decoder =
  let idx = ref j in
  let chr = ref '\000' in

  while
    decoder.i_len - !idx >= 0
    &&
    (chr := Bytes.get decoder.i !idx ;
     !chr = ' ' || !chr = '\t')
  do
    incr idx
  done ;

  let i = decoder.i_pos in
  let s = Bytes.sub_string decoder.i i (j - i) in
  let s = if decoder.has_cr then "\r" ^ s else s in
  let n = Bytes.sub_string decoder.i j (!idx - j) in

  Buffer.add_string decoder.b n ;
  decoder.has_cr <- false ;
  decoder.i_pos <- !idx ;

  if String.length s = 0
  then t_decode decoder
  else
    ret
      (fun decoder ->
        let s = Buffer.contents decoder.b in
        (* XXX(dinosaure): in [t_spaces], we ensure that [decoder.b] as, at
            least, one space. *)
        Buffer.clear decoder.b ;
        ret t_decode (`Spaces s) decoder)
      (`Data s) decoder

and t_space_data_space j decoder =
  if Buffer.length decoder.b > 0 && j - decoder.i_pos > 0 && not decoder.has_cr
  then (
    let s = Buffer.contents decoder.b in
    Buffer.clear decoder.b ;
    ret (t_space j) (`Spaces s) decoder)
  else t_space j decoder

and t_space_or k v decoder =
  if Buffer.length decoder.b > 0
  then (
    let s = Buffer.contents decoder.b in
    Buffer.clear decoder.b ;
    ret (ret k v) (`Spaces s) decoder)
  else ret k v decoder

and t_decode decoder =
  let rem = i_rem decoder in
  if rem <= 0
  then
    if rem < 0
    then
      if decoder.has_cr
      then t_space_or t_end (`Data "\r") decoder
      else t_space_or t_end `End decoder
    else `Await
  else
    match decoder.has_cr with
    | true ->
        if Bytes.get decoder.i decoder.i_pos = '\n'
        then (
          decoder.i_pos <- decoder.i_pos + 1 ;
          decoder.has_cr <- false ;
          t_space_or t_decode `CRLF decoder)
        else
          let idx = ref decoder.i_pos in
          let chr = ref '\000' in

          while
            decoder.i_len - !idx >= 0
            &&
            (chr := Bytes.get decoder.i !idx ;
             !chr <> '\r' && !chr <> ' ' && !chr <> '\t')
          do
            incr idx
          done ;

          if !chr = '\r'
          then (
            let j = decoder.i_pos in
            decoder.i_pos <- !idx + 1 ;
            let s = Bytes.sub_string decoder.i j (!idx - j) in
            let s = "\r" ^ s in
            decoder.has_cr <- true ;
            t_space_or t_decode (`Data s) decoder)
          else if !chr = ' ' || !chr = '\t'
          then t_space_data_space !idx decoder
          else
            let j = decoder.i_pos in
            decoder.i_pos <- !idx + 1 ;
            let s = Bytes.sub_string decoder.i j (!idx + 1 - j) in
            let s = "\r" ^ s in
            decoder.has_cr <- false ;
            t_space_or t_decode (`Data s) decoder
    | false ->
        let idx = ref decoder.i_pos in
        let chr = ref '\000' in

        while
          decoder.i_len - !idx >= 0
          &&
          (chr := Bytes.get decoder.i !idx ;
           !chr <> '\r' && !chr <> ' ' && !chr <> '\t')
        do
          incr idx
        done ;

        if !chr = '\r'
        then (
          let j = decoder.i_pos in
          decoder.i_pos <- !idx + 1 ;
          let s = Bytes.sub_string decoder.i j (!idx - j) in
          decoder.has_cr <- true ;
          if s = ""
          then t_decode decoder
          else t_space_or t_decode (`Data s) decoder)
        else if !chr = ' ' || !chr = '\t'
        then t_space_data_space !idx decoder
        else
          let j = decoder.i_pos in
          decoder.i_pos <- !idx ;
          let s = Bytes.sub_string decoder.i j (!idx - j) in
          decoder.has_cr <- false ;
          if s = ""
          then t_decode decoder
          else t_space_or t_decode (`Data s) decoder

let decode decoder = decoder.k decoder

let decoder () =
  {
    i = Bytes.empty;
    i_pos = 1;
    i_len = 0;
    has_cr = false;
    b = Buffer.create 16;
    k = t_decode;
  }

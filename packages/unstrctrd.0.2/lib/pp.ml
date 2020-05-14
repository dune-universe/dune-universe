let cut ~sep s =
  let sep_len = String.length sep in
  if sep_len = 0 then invalid_arg "cut: empty separator" ;
  let s_len = String.length s in
  let max_sep_idx = sep_len - 1 in
  let max_s_idx = s_len - sep_len in
  let rec check_sep i k =
    if k > max_sep_idx
    then let r_start = i + sep_len in
         Some (String.sub s 0 i, String.sub s r_start (s_len - r_start))
    else 
      ( if s.[i + k] = sep.[k]
        then check_sep i (succ k)
        else scan (succ i) )
   and scan i =
     if i > max_s_idx
     then None
     else ( if s.[i] = sep.[0] then check_sep i 1 else scan (succ i) ) in
  scan 0

let pp_fws k (`FWS fws) =
  match cut ~sep:"\r\n" fws with
  | None -> k [ `WSP fws ]
  | Some ("", "") -> assert false (* k [ `CRLF ] *)
  | Some ("", wsp) -> k [ `FWS wsp ]
  | Some (wsp0, wsp1) -> k [ `WSP wsp0; `FWS wsp1 ]

let is_obs_no_ws_ctl = function
  | '\001'..'\008'
  | '\011'
  | '\012'
  | '\014'..'\031'
  | '\127' -> true
  | _ -> false

let pp_obs_utext k (`OBS_UTEXT (lf0, cr0, obs_utext)) =
  let lf0 = List.init lf0 (fun _ -> `LF) in
  let cr0 = List.init cr0 (fun _ -> `CR) in
  let folder acc i = function
    | `Malformed _ -> `Invalid_char obs_utext.[i] :: acc
    | `Uchar uchar -> match Uchar.to_int uchar with
      | 0 -> `d0 :: acc
      | 0x0D -> `CR :: acc
      | 0x0A -> `LF :: acc
      | _ ->
        if Uchar.is_char uchar && is_obs_no_ws_ctl (Uchar.to_char uchar)
        then `OBS_NO_WS_CTL (Uchar.to_char uchar) :: acc
        else `Uchar uchar :: acc in
  let res = Uutf.String.fold_utf_8 folder (cr0 @ lf0) obs_utext in k (List.rev res)

let pp_vchar k (`VCHAR vchar) =
  let folder acc i = function
    | `Malformed _ -> `Invalid_char vchar.[i] :: acc
    | `Uchar _ as uchar -> uchar :: acc in
  let res = Uutf.String.fold_utf_8 folder [] vchar in k (List.rev res)

type obs_utext = [ `OBS_UTEXT of (int * int * string) ]
type fws = [ `FWS of string ]
type vchar = [ `VCHAR of string ]
type wsp = [ `WSP of string ]
type invalid_char = [ `Invalid_char of char ]

let pp k lst =
  let rec go acc = function
    | [] -> k (List.concat (List.rev acc))
    | x :: r ->
      let k lst = go (lst :: acc) r in
      match x with
      | #invalid_char as v -> k [ v ]
      | #obs_utext as v -> pp_obs_utext k v
      | #fws as v -> pp_fws k v
      | #vchar as v -> pp_vchar k v
      | #wsp as v -> k [ v ] in
  go [] lst

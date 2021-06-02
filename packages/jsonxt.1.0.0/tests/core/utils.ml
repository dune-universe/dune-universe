module IO : sig
  type 'a t

  val return : 'a -> 'a t
  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
  val result : 'a t -> 'a
end = struct
  type 'a t = 'a

  let return v = v
  let (>>=) v f = f v
  let result t = t
end

module StringIO = struct
  type t = {
    buf : Buffer.t
  ; off : int ref
  }

  let create s =
    let iobuf = { buf = Buffer.create 32; off = ref 0 } in
    Buffer.add_string iobuf.buf s;
    iobuf

  let read t buf len =
    let buflen = Buffer.length t.buf in
    let noff = !(t.off) + len in
    let noff = if noff > buflen then buflen else noff in
    let clen = noff - !(t.off) in
    if clen <= 0 then begin Buffer.reset t.buf; t.off := 0; 0 end
    else begin
      Buffer.blit t.buf !(t.off) buf 0 clen;
      t.off := noff;
      clen
    end

  let write t s = Buffer.add_string t.buf s

  let contents t = Buffer.sub t.buf !(t.off) (Buffer.length t.buf - !(t.off))
end

(* Determine the size of an integer, handles 31bit, 63bit and Jsoo using 32bit ints *)
let int_bits =
  let rec log2 n = if n <= 1 then 0 else 1 + log2(n asr 1) in
  let bits n = log2 n + 1 in
  match bits max_int with
  | 30|31|32 -> 32
  | _ -> 64

let die msg = 
  Printf.fprintf stderr "\nERROR: %s\n" msg;
  exit 255

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s


(* String.split_on_char was introduced at 4.04 *)
let split_string c str =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1
    then String.sub str 0 last_pos :: acc
    else if Char.equal c str.[pos]
    then (
      let pos1 = pos + 1 in
      let sub_str = String.sub str pos1 (last_pos - pos1) in
      loop (sub_str :: acc) pos (pos - 1))
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)
;;

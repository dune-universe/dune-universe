(*
 * Copyright (c) 2021 Tarides <contact@tarides.com>
 * Copyright (c) 2021 Gabriel Belouze <gabriel.belouze@ens.psl.eu>
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
 *)

include Input_intf

type config = {
  key_sz : int;
  value_sz : int;
  fanout : int;
  cache_sz : int;
  page_sz : int;
  start_sz : int;
  n : int;
  debug : bool;
  version : int;
  sleep : bool;
}

module Make (Size : SIZE) = struct
  module Key = struct
    type t = string [@@deriving repr]

    let encode s = s

    let decode s = s

    let encoded_size = Size.key_sz
  end

  let pad len c s =
    if String.length s > len then failwith "Impossible padding"
    else String.make (len - String.length s) c ^ s

  let unpad c_pad s =
    let _, padd =
      Seq.fold_left
        (fun (found, off) c ->
          match found with
          | true -> (found, off)
          | false -> if c == c_pad then (false, off + 1) else (true, off))
        (false, 0) (String.to_seq s)
    in
    String.sub s padd (String.length s - padd)

  module Value = struct
    type t = int [@@deriving repr]

    let encoded_size = Size.value_sz

    let encode x = x |> Repr.to_string t |> pad encoded_size '\003'

    let decode s =
      s |> unpad '\003' |> Repr.of_string t |> function
      | Ok t -> t
      | Error (`Msg msg) -> failwith msg
  end

  module Size = Size

  let random_char () = char_of_int (33 + Random.int 94)

  let random_string string_size = String.init string_size (fun _i -> random_char ())

  let generate_key _seed = random_string Key.encoded_size

  let generate_value seed = seed mod 1_000_003 (* , seed mod 1000159, seed mod 1000253)*)

  let sorted_data n =
    (* if a bigger sorted data file is already present, don't generate a new one *)
    let dh = Unix.opendir "." in
    let rec aux () =
      let file = Unix.readdir dh in
      if String.length file >= 3 && String.sub file 0 3 = "out" then
        try
          let num = String.sub file 3 (String.length file - 3) |> int_of_string in
          if num >= n then file else aux ()
        with Failure _ -> aux () (* if int_of_string failed *)
      else aux ()
    in
    try Some (aux ()) with End_of_file -> None

  let batch_initialiser n =
    let entry_size = Key.encoded_size + Value.encoded_size in

    let module StringValue = (val Oracle.stringv ~encode_sz:entry_size) in
    let module Sorter = Oracle.Make (StringValue) (Oracle.Default) in
    let seed = ref 0 in
    let k i = i |> generate_key |> Key.encode in
    let v i = i |> generate_value |> Value.encode in
    let oracle () =
      incr seed;
      k !seed ^ v !seed
    in
    let out = match sorted_data n with Some s -> s | None -> Fmt.str "out%i" n in

    Sorter.sort ~with_prog:true ~oracle ~out n;
    let open Unix in
    let fd = openfile out [ O_RDONLY ] 0o655 in
    let buff = Bytes.create (entry_size * Size.fanout * 2) in
    let read m = read fd buff 0 (m * entry_size) |> Bytes.sub_string buff 0 in
    read
end

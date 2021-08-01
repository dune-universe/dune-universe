include Oracle_intf

module type BUCKET = sig
  exception Empty

  type t
  (** the type of a bucket with values of type 'a *)

  val v : tmp:string -> string array -> t

  val init : tmp:string -> int -> (int -> string) -> t

  val load : tmp:string -> int -> t

  val pop : t -> string
  (** [pop t] removes the next value from [t] and returns it. Raises [Empty] if [t] has no more
      values. *)

  val safe_peek : t -> string option

  val close : t -> unit

  val length : t -> int
end

let file_size fd = fd |> Unix.fstat |> fun x -> x.st_size

let file_name name =
  match String.rindex_opt name '/' with
  | None -> name
  | Some i -> String.sub name (i + 1) (String.length name - i - 1)

module Make (V : VALUE) (H : HYPER) = struct
  include H

  let buffsize = ram / kway

  module Bucket : BUCKET = struct
    open Unix

    let read fd buffer off length =
      (* repeat read until all bytes are read *)
      let rec aux buffer_offset len =
        let r = read fd buffer buffer_offset len in
        if r = 0 || r = len then buffer_offset + r - off (* end of file or everything is read *)
        else (aux [@tailcall]) (buffer_offset + r) (len - r)
      in
      aux off length

    let write_string fd s = write_substring fd s 0 (String.length s) |> ignore

    exception Empty

    type t = { fd : Unix.file_descr; buff : bytes; mutable off : int; mutable length : int }

    let v ~tmp arr =
      let fd = openfile tmp [ O_CREAT; O_TRUNC; O_WRONLY; O_APPEND ] 0o644 in
      Array.iter (write_string fd) arr;
      assert (file_size fd mod V.encode_sz = 0);
      close fd;
      let fd = openfile tmp [ O_RDONLY ] 0o644 in
      lseek fd 0 SEEK_SET |> ignore;
      {
        fd;
        buff = Bytes.make (V.encode_sz * buffsize) '\000';
        off = buffsize;
        length = Array.length arr;
      }

    let init ~tmp n f =
      let fd = openfile tmp [ O_CREAT; O_TRUNC; O_WRONLY; O_APPEND ] 0o644 in
      let last = ref "" in
      let buff = Bytes.make (V.encode_sz * buffsize) '\000' in
      for i = 0 to n - 1 do
        let next = f i in
        let offset = i mod buffsize in

        assert (!last < next);
        Bytes.blit_string next 0 buff (V.encode_sz * offset) V.encode_sz;
        if (i + 1) mod buffsize = 0 then
          let write_sz = write fd buff 0 (V.encode_sz * buffsize) in
          assert (write_sz = V.encode_sz * buffsize)
        else if i = n - 1 then
          let write_sz = write fd buff 0 (V.encode_sz * (offset + 1)) in
          assert (write_sz = V.encode_sz * (offset + 1))
      done;
      close fd;
      let fd = openfile tmp [ O_RDONLY ] 0o644 in
      lseek fd 0 SEEK_SET |> ignore;
      { fd; buff = Bytes.make (V.encode_sz * buffsize) '\000'; off = buffsize; length = n }

    let load ~tmp n =
      Log.debug (fun reporter -> reporter "Loading %s from cache" (tmp |> file_name));
      let fd = openfile tmp [ O_RDONLY ] 0o644 in
      assert (file_size fd mod V.encode_sz = 0);
      lseek fd 0 SEEK_SET |> ignore;
      { fd; buff = Bytes.make (V.encode_sz * buffsize) '\000'; off = buffsize; length = n }

    let fill t =
      match t.length with
      | 0 -> raise Empty
      | _ ->
          let rd_size = read t.fd t.buff 0 (buffsize * V.encode_sz) in
          assert (rd_size mod V.encode_sz = 0);
          if rd_size = 0 then (
            close t.fd;
            raise Empty)
          else if rd_size < buffsize * V.encode_sz then (
            t.off <- buffsize - (rd_size / V.encode_sz);
            Bytes.blit t.buff 0 t.buff (t.off * V.encode_sz) rd_size;
            close t.fd)
          else t.off <- 0

    let peek t =
      if t.off = buffsize then fill t;
      Bytes.sub_string t.buff (t.off * V.encode_sz) V.encode_sz

    let safe_peek t = try Some (peek t) with Empty -> None

    let pop t =
      let ret = peek t in
      t.off <- t.off + 1;
      t.length <- t.length - 1;
      ret

    let close t = close t.fd

    let length t = t.length
  end

  let argmin buckets =
    let ipeeks =
      buckets
      |> List.mapi (fun i bucket -> (i, Bucket.safe_peek bucket))
      |> List.filter (fun (_i, peek_opt) -> match peek_opt with None -> false | Some _ -> true)
      |> List.map (fun (i, peek_opt) ->
             match peek_opt with None -> assert false | Some peek -> (i, peek))
    in

    let argmini, _mini =
      match ipeeks with
      | [] -> failwith "No more bindings to read"
      | (i, peek) :: ipeeks ->
          List.fold_left
            (fun (argmini, mini) (i, peek) -> if peek < mini then (i, peek) else (argmini, mini))
            (i, peek) ipeeks
    in
    argmini

  let with_progress_bar ~message ~n ~unit =
    let open Progress in
    let w = if n = 0 then 1 else float_of_int n |> log10 |> floor |> int_of_float |> succ in
    let w_pp = Printer.int ~width:w in
    let bar =
      Line.(
        list
          [
            const message;
            count_to ~pp:w_pp n;
            const unit;
            elapsed ();
            bar ~style:`UTF8 ~color:(`magenta |> Color.ansi) n;
            eta n |> brackets;
          ])
    in
    Progress.with_reporter bar

  let no_op _ = ()

  let merge ~prog ~out buckets =
    let tot = buckets |> List.map Bucket.length |> List.fold_left ( + ) 0 in
    let bucket =
      Bucket.init ~tmp:out tot (fun i ->
          if (i + 1) mod (tot / 10_000) = 0 then prog (tot / 10_000);
          let argmini = buckets |> argmin in
          argmini |> List.nth buckets |> Bucket.pop)
    in
    prog (tot mod (tot / 10_000));
    bucket

  let rec sort ~prog ~oracle ~out n =
    if Sys.file_exists out then (
      prog 1;
      Bucket.load ~tmp:out n)
    else if n < ram then (
      let ret = Array.init n (fun _ -> oracle () |> V.encode) in
      Array.sort String.compare ret;
      prog n;
      Bucket.v ~tmp:out ret)
    else
      let ns = List.init kway (fun i -> if i = 0 then (n / kway) + (n mod kway) else n / kway) in

      let buckets = List.mapi (fun i n -> sort ~prog ~oracle ~out:(Fmt.str "%s_%i" out i) n) ns in
      let ret = merge ~prog ~out buckets in
      List.iteri (fun i _n -> Unix.unlink (Fmt.str "%s_%i" out i)) ns;
      ret

  let rec predict_ops ~out n =
    if Sys.file_exists out then 1
    else if n < ram then n
    else
      let ns = List.init kway (fun i -> if i = 0 then (n / kway) + (n mod kway) else n / kway) in
      let ns_ops = List.mapi (fun i n -> predict_ops ~out:(Fmt.str "%s_%i" out i) n) ns in
      n + List.fold_left ( + ) 0 ns_ops

  let sort ?(with_prog = false) ~oracle ~out n =
    (if with_prog then
     let n_ops = predict_ops ~out n in
     with_progress_bar ~message:"External merge-sort" ~n:n_ops ~unit:"values" @@ fun prog ->
     sort ~prog ~oracle ~out n
    else sort ~prog:no_op ~oracle ~out n)
    |> Bucket.close
end

module Default = struct
  let ram = 1_000_000

  let kway = 15
end

let stringv ~encode_sz =
  (module struct
    type t = string

    let encode_sz = encode_sz

    let encode s =
      assert (String.length s = encode_sz);
      s
  end : VALUE
    with type t = string)

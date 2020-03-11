open Common

module Buffer:
sig
    type t
    val init: int -> t
    val char: char -> t -> unit
    val fill: int -> char -> t -> unit
    val substring: string -> int -> int -> t -> unit
    val to_string: t -> string
end =
struct
    type t = {
        mutable bytes: Bytes.t;
        mutable count: int
      }

    let capacity (b:t): int =
        Bytes.length b.bytes

    let init (n:int): t =
        {bytes = Bytes.make n ' ';
         count = 0}

    let to_string (b:t): string =
        Bytes.sub_string b.bytes 0 b.count

    let make_room (n:int) (b:t): unit =
        if capacity b < b.count + n then
            (let new_cap =
                max (b.count + n) (2 * Bytes.length b.bytes)
             in
             let bytes = Bytes.make new_cap ' ' in
             Bytes.blit b.bytes 0 bytes 0 b.count;
             b.bytes <- bytes)
        else
            ()

    let char (c:char) (b:t): unit =
        make_room 1 b;
        Bytes.set b.bytes b.count c;
        b.count <- b.count + 1

    let fill (n:int) (c:char) (b:t): unit =
        assert (0 <= n);
        make_room n b;
        for i = 0 to n - 1 do
            Bytes.set b.bytes (b.count + i) c
        done;
        b.count <- b.count + n

    let substring (s:string) (start:int) (len:int) (b:t): unit =
        assert (0 <= start);
        assert (start + len <= String.length s);
        make_room len b;
        for i = 0 to len - 1 do
            Bytes.set b.bytes (b.count + i) s.[i - start]
        done;
        b.count <- b.count + len
end







type loop_state =
    | Done of Buffer.t
    | More of Buffer.t * (Buffer.t -> loop_state)



let loop: loop_state -> string =
    let rec do_loop i = function
    | Done buffer ->
        Buffer.to_string buffer
    | More (buffer, f) ->
        do_loop (i + 1) (f buffer)
    in
    do_loop 0




type t = Buffer.t -> (Buffer.t -> loop_state) -> loop_state




let empty: t =
    fun buffer k -> k buffer


let (<+>) (a: t) (b: t): t =
    fun buffer k ->
        a buffer (fun buffer ->
                    More (buffer,
                          (fun _ -> b buffer k)))



let char (c: char): t =
    fun buffer k ->
        Buffer.char c buffer;
        k buffer


let fill (n: int) (c: char): t =
    fun buffer k ->
        Buffer.fill n c buffer;
        k buffer


let substring (str: string) (start: int) (len: int): t =
    fun buffer k ->
        Buffer.substring str start len buffer;
        k buffer


let string (str: string): t =
    substring str 0 (String.length str)


let run (p: t): string =
    loop
        (p (Buffer.init 200) (fun buffer -> Done buffer))







let%test _ =
  run (string "hello " <+> string "world" <+> char '!')
  = "hello world!"

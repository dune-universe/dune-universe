module type WORD = sig
  type t

  type letter

  val letter_equal : letter -> letter -> bool

  val length : t -> int

  val unsafe_get : t -> int -> letter
end

module type S = sig
  type word

  type letter

  val find : word -> ?offset:int -> letter Seq.t -> int Seq.t
end

module Make (Word : WORD)
    : S with type word = Word.t and type letter = Word.letter =
struct
  type word = Word.t

  type letter = Word.letter

  let table w =
    let len = Word.length w in
    let t = Array.make (len + 1) (-1) in
    let rec fill pos cnd =
      if pos < len then
        let cnd =
          if Word.letter_equal (Word.unsafe_get w pos) (Word.unsafe_get w cnd) then
            begin
              t.(pos) <- t.(cnd);
              cnd
            end
          else
            begin
              t.(pos) <- cnd;
              let rec update cnd =
                if cnd >= 0 &&
                  not (Word.letter_equal (Word.unsafe_get w pos) (Word.unsafe_get w cnd)) then
                  update t.(cnd)
                else
                  cnd in
              update t.(cnd)
            end in
        fill (pos + 1) (cnd + 1)
      else
        cnd in
    t.(len) <- fill 1 0;
    t

  let find (w : Word.t) =
    let t = table w in
    fun ?(offset = 0) (s : Word.letter Seq.t) : int Seq.t->
      let rec find_next j k (s : Word.letter Seq.t) () : int Seq.node =
        match s () with
        | Nil -> Nil
        | Cons (hd, tl) ->
            let rec check k : int Seq.node =
              if Word.letter_equal hd (Word.unsafe_get w k) then
                let j = j + 1 in
                let k = k + 1 in
                if k = Word.length w then
                  Cons (j - k, find_next j t.(k) tl)
                else
                  find_next j k tl ()
              else
                let k = t.(k) in
                if k < 0 then
                  find_next (j + 1) (k + 1) tl ()
                else
                  check k in
            check k in
      find_next 0 0 s
end

module Word_string : WORD with type t = string and type letter = char = struct
  type t = string

  type letter = char

  let letter_equal : char -> char -> bool = ( = )

  let length = String.length

  let unsafe_get = String.unsafe_get
end

module On_string = Make (Word_string)

let seq_of_in_channel (chan : in_channel) : char Seq.t =
  let rec loop () : char Seq.node =
    match input_char chan with
    | c -> Cons (c, loop)
    | exception End_of_file -> Nil in
  loop

let test () =
  assert (List.of_seq (On_string.find "ababc" (String.to_seq "cabababcababc")) = [3; 8])

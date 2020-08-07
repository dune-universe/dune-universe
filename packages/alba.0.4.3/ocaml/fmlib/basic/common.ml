let identity (a:'a): 'a = a

module Void:
sig
  type t
end =
  struct
    type t = int
  end


module Unit:
sig
  type t = unit
end =
  struct
    type t = unit
  end


module Int =
  struct
    type t = int
    let compare = Stdlib.compare

    let iterate (n: t) (f: 'a -> 'a) (start: 'a): 'a =
      let rec iter n v =
        if n = 0 then
          v
        else
          iter (n - 1) (f v)
      in
      iter n start
  end

module Int_set = Set.Make (Int)
module Int_map = Finite_map.Make (Int)



module Either =
  struct
    type ('a,'b) t =
      | Left of 'a
      | Right of 'b
    let left a = Left a
    let right b = Right b
  end


module Char =
  struct
    include Char
    let is_lower (c:char): bool =
      'a' <= c && c <= 'z'
    let is_upper (c:char): bool =
      'A' <= c && c <= 'Z'
    let is_letter (c:char): bool =
      is_lower c || is_upper c
    let is_digit (c:char): bool =
      '0' <= c && c <= '9'
  end


module String =
  struct
    include String

    let one (c:char): string =
      String.make 1 c

    let is_prefix (a: string) (b:string): bool =
      let len_a = length a in
      len_a <= length b && a = sub b 0 len_a

    let is_suffix (a: string) (b:string): bool =
      let len_a = length a
      and len_b = length b
      in
      len_a <= len_b
      && a = sub b  (len_b - len_a) len_a

    let find (f:char -> bool) (start:int) (s:string): int =
      let len = String.length s in
      let rec find i =
        if i = len || f s.[i] then
          i
        else
          find (i+1)
      in
      find start


    let has (f: char -> bool) (start: int) (s: string): bool =
        find f start s
        <
        length s


    let find_bwd (f:char -> bool) (beyond:int) (s:string): int =
      assert (beyond <= String.length s);
      let rec find i =
        if i = 0 || f s.[i-1] then
          i-1
        else
          find (i-1)
      in
      find beyond

    let list (s:string): char list =
      let rec list cs i =
        if i = 0 then
          cs
        else
          let j = i - 1 in
          list (s.[j]::cs) j
      in
      list [] (length s)

    let of_list (cs:char list): string =
      let rec str cs i =
        match cs with
        | [] ->
           Bytes.create i
        | c::cs ->
           let bs = str cs (i+1) in
           Bytes.set bs i c;
           bs
      in
      let bs = str cs 0 in
      Bytes.unsafe_to_string bs


    let reverse (s: string): string =
        let len = length s in
        init len (fun i -> s.[len - 1 - i])
  end


module String_set = Set.Make(String)
module String_map = Finite_map.Make(String)


module Interval =
struct
    let find (p:int -> bool) (start:int) (beyond:int): int =
      let rec fnd i =
        if i = beyond || p i then
          i
        else
          fnd (i+1)
      in
      fnd start


    let exist (p: int -> bool) (start: int) (beyond: int): bool =
        find p start beyond <> beyond


    let forall (p: int -> bool) (start: int) (beyond: int): bool =
        let notp i = not (p i)
        in
        not (exist notp start beyond)


    let fold (a:'a) (f:int -> 'a -> 'a) (start:int) (beyond:int): 'a =
      assert (start <= beyond);
      let rec fold i a =
        if i = beyond then
          a

        else
          fold (i + 1) (f i a)
      in
      fold start a


    module Monadic (M: Module_types.MONAD) =
      struct
        let fold
              (f: int -> 'a -> 'a M.t)
              (start: int)
              (beyond: int)
              (a: 'a)
            : 'a M.t
          =
          assert (start <= beyond);
          let rec fold i a =
            if i = beyond then
              M.return a

            else
              M.(f i a >>= fold (i + 1))
          in

          fold start a
      end
end




module String_reader =
struct
    include Readable_printer.R

    let of_substring str start len =
        Readable_printer.(readable (substring str start len))


    let of_string str =
        Readable_printer.(readable (string str))
end



module Fill_reader =
  struct
    type t = {n:int; c:char}
    let has_more (r:t): bool =
      r.n > 0
    let peek (r:t): char =
      r.c
    let advance (r:t): t =
      {r with n = r.n - 1}
    let make (n:int) (c:char): t =
      {n;c}
  end




module Char_reader =
  struct
    type t = char option
    let make (c:char): t =
      Some c
    let has_more (cr:t): bool =
      cr <> None
    let peek (cr:t): char =
      match cr with
      | None -> assert false (* Illegal call! *)
      | Some c -> c
    let advance (_:t): t =
      None
  end

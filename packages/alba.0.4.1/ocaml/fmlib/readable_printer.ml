module R =
  struct
    type e =
      | String of string * int * int (* start, beyond *)
      | Char of char
      | Fill of int * char

    type t =
      | More of e * (t Lazy.t)
      | Done

    let has_more (r:t): bool =
      r <> Done

    let peek (r:t): char =
      match r with
      | Done ->
         assert false (* Illegal call! *)
      | More (e,_) ->
         match e with
         | String (s,pos,beyond) ->
            assert (pos < beyond);
            s.[pos]
         | Char c ->
            c
         | Fill (n,c) ->
            assert (0 < n);
            c

    let advance (r:t): t =
      match r with
      | Done ->
         assert false (* Illegal call! *)
      | More (e,f) ->
         match e with
         | String (s,pos,beyond) ->
            assert (pos < beyond);
            if pos + 1 = beyond then
              Lazy.force f
            else
              More (String (s,pos+1,beyond),f)
         | Char _ ->
            Lazy.force f
         | Fill (n,c) ->
            assert (0 < n);
            if n = 1 then
              Lazy.force f
            else
              More (Fill (n-1, c), f)


    let make_substring (s:string) (start:int) (len:int) (f:unit -> t): t =
      assert (0 <= start);
      assert (0 <= len);
      assert (start + len <= String.length s);
      if len = 0 then
        f ()
      else
        More (String (s,start,start+len), Lazy.from_fun f)

    let make_char (c:char) (f:unit -> t): t =
      More (Char c, Lazy.from_fun f)

    let make_fill (n:int) (c:char) (f:unit -> t): t =
      if n = 0 then
        f ()
      else
        More (Fill(n,c), Lazy.from_fun f)

    let make_empty: t =
      Done
  end


module M0 =
  struct
    type 'a t = ('a -> R.t) -> R.t
    let return (a:'a) (k:'a -> R.t): R.t =
      k a
    let (>>=) (m:'a t) (f:'a -> 'b t) (k:'b -> R.t): R.t =
      m (fun a -> f a k)
  end

module M = Monad.Of_sig_min (M0)


type t = unit M.t


let empty: t =
  M.return ()


let (<+>)  (p1:t) (p2:t): t =
  M.(p1 >>= fun _ -> p2)


let string (s:string) (k:unit -> R.t): R.t =
  R.make_substring s 0 (String.length s) k


let substring (s:string) (start:int) (len:int) (k:unit -> R.t): R.t =
  assert (0 <= start);
  assert (0 <= len);
  assert (start + len <= String.length s);
  R.make_substring s start len k


let fill (n:int) (c:char) (k:unit -> R.t): R.t =
  assert (0 <= n);
  R.make_fill n c k


let char (c:char) (k:unit -> R.t): R.t =
  R.make_char c k


let readable (m:unit M.t): R.t =
  m (fun () -> R.make_empty)

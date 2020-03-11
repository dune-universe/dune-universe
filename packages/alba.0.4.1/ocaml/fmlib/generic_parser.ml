open Module_types


module type ERROR =
  sig
    type t
    type semantic
    type expect
    val is_semantic: t -> bool
    val semantic: t -> semantic
    val expectations: t -> expect list
    val make_semantic: semantic -> t
    val make_expectations: expect list -> t
  end


module Error (Expect:ANY) (Semantic:ANY) =
  struct
    type expect = Expect.t
    type semantic = Semantic.t
    type t =
      | Syntax of Expect.t list
      | Semantic of Semantic.t

    let to_string
          (e: t)
          (f: Expect.t -> string)
          (g: Semantic.t -> string)
        : string =
      match e with
      | Syntax lst ->
         "["
         ^ String.concat
             ", "
             (List.rev_map f lst)
         ^ "]"
      | Semantic sem ->
         g sem

    let init: t =
      Syntax []

    let add_expected (exp: Expect.t) (e: t): t =
      match e with
      | Syntax lst ->
         Syntax (exp :: lst)
      | _ ->
         Syntax [exp]

    let make_semantic (sem: Semantic.t): t =
      Semantic sem

    let make_expectations (lst: Expect.t list): t =
      Syntax (List.rev lst)

    let is_semantic (e:t): bool =
      match e with
      | Syntax _ -> false
      | _        -> true

    let semantic (e:t): Semantic.t =
      match e with
      | Syntax _ ->
         assert false (* Illegal call! *)
      | Semantic sem ->
         sem

    let expectations (e:t): Expect.t list =
      match e with
      | Syntax es ->
         List.rev es
      | Semantic _ ->
         assert false (* Illegal call! *)
  end


module type COMBINATORS =
  sig
    type 'a t
    type expect
    type semantic
    val return: 'a -> 'a t
    val succeed: 'a -> 'a t
    val unexpected: expect -> 'a t
    val fail:    semantic -> 'a t
    val consumer: 'a t -> 'a t
    val map:     ('a -> 'b) -> 'a t -> 'b t
    val (>>=):   'a t -> ('a -> 'b t) -> 'b t
    val (<|>):   'a t -> 'a t -> 'a t
    val (<?>):   'a t -> expect -> 'a t
    val backtrackable: 'a t -> expect -> 'a t
    val not_followed_by: 'a t -> expect -> unit t

    val optional: 'a t -> 'a option t
    val one_of:   'a t list -> 'a t
    val zero_or_more: 'a t -> 'a list t
    val one_or_more:  'a t -> 'a list t
    val one_or_more_separated:  'a t -> _ t -> 'a list t
    val zero_or_more_separated: 'a t -> _ t -> 'a list t
    val skip_zero_or_more: 'a t -> int t
    val skip_one_or_more:  'a t -> int t

    val (|=): ('a -> 'b) t -> 'a t -> 'b t
    val (|.): 'a t -> _ t -> 'a t
  end



module Buffer (S:ANY) (T:ANY) (Expect:ANY) (Semantic:ANY) =
  struct
    type state = S.t
    type token = T.t
    module Error = Error (Expect) (Semantic)
    type error = Error.t

    type t = {
        state: state;
        has_consumed: bool;
        error: error;
        la_ptr: int;          (* position of first lookahead token *)
        is_buffering: bool;
        toks: token array
      }

    let init (st:state): t =
      {state = st;
       has_consumed = false;
       error = Error.init;
       la_ptr = 0;
       is_buffering = false;
       toks = [||]}

    let state (b:t): state =
      b.state

    let error (b:t): error =
      b.error

    let count_toks (b:t): int =
      Array.length b.toks

    let has_lookahead (b:t): bool =
      b.la_ptr < count_toks b

    let lookahead_toks (b:t): token array =
      let len = count_toks b - b.la_ptr
      in
      Array.sub b.toks b.la_ptr len

    let lookahead (b:t): token list =
      Array.(to_list (lookahead_toks b))

    let lookahead_token (b:t): token =
      assert (has_lookahead b);
      b.toks.(b.la_ptr)

    let push_token (t:token) (b:t): t =
      if not b.is_buffering
         && b.la_ptr = count_toks b
      then
        {b with la_ptr = 0;
                toks = [|t|]}
      else
        {b with toks = Array.push t b.toks}

    let update (f:state->state) (b:t): t =
      {b with state = f b.state}

    let add_expected (e: Expect.t) (b:t): t =
      {b with error = Error.add_expected e b.error}

    let put_error (e: Semantic.t) (b: t): t =
      {b with error = Error.make_semantic e}

    let clear_errors  (b:t): t =
      {b with error = Error.init}

    let consume (state:state) (b:t): t =
      assert (has_lookahead b);
      {b with state;
              has_consumed = true;
              error = Error.init;
              la_ptr = 1 + b.la_ptr}

    let reject (e: Expect.t) (b:t): t =
      add_expected e b

    let start_new_consumer (b:t): t =
      {b with has_consumed = false}

    let has_consumed (b:t): bool =
      b.has_consumed

    let end_new_consumer (b0:t) (b:t): t =
      {b with
        has_consumed = b0.has_consumed || b.has_consumed}


    let start_alternatives (b:t): t =
      {b with has_consumed = false}

    let end_failed_alternatives (e: Expect.t) (b0:t) (b:t): t =
      if b.has_consumed then
        b
      else
        {b with
          has_consumed = b0.has_consumed;
          error = Error.add_expected e b0.error}

    let end_succeeded_alternatives (b0:t) (b:t): t =
      if b.has_consumed then
        b
      else
        {b with
          has_consumed = b0.has_consumed;
          error = b0.error}


    let start_backtrack (b:t): t =
      {b with is_buffering = true}


    let end_backtrack_success (b0:t) (b:t): t =
      if b0.is_buffering then
        b
      else
        {b with is_buffering = false;
                toks = lookahead_toks b; (* only lookahead token *)
                la_ptr = 0}


    let end_backtrack_fail (e: Expect.t option) (b0: t) (b: t): t =
      {b0 with
        toks = b.toks;
        error =
          match e with
          | None ->
             b0.error
          | Some e ->
             Error.add_expected e b0.error}
  end






module Make
         (T:ANY)
         (S:ANY)
         (Expect: ANY)
         (Semantic: ANY)
         (F:ANY) =
  struct
    type token = T.t

    module Error = Error (Expect) (Semantic)

    type state = S.t

    type final = F.t

    type expect   = Expect.t
    type semantic = Semantic.t

    module B = Buffer (S) (T) (Expect) (Semantic)

    type parser =
      | More  of B.t * (B.t -> parser)
      | Final of B.t * final option

    let needs_more (p:parser): bool =
      match p with
      | More _ -> true | Final _ -> false

    let has_ended (p:parser): bool = not (needs_more p)


    let put_token (p:parser) (t:token): parser =
      let push_token t p =
        match p with
        | More (b, f) ->
           More (B.push_token t b, f)
        | Final (b, res) ->
           Final (B.push_token t b, res)
      in
      let rec process_lookahead p =
        match p with
        | More (b, f) when B.has_lookahead b ->
           process_lookahead (f b)
        | _ ->
           p

      in
      process_lookahead (push_token t p)


    let state (p:parser): state =
      match p with
      | More (b,_) | Final (b, _) -> B.state b

    let result (p:parser): final option =
      match p with
      | Final (_, r) ->
         r
      | _ -> assert false (* Illegal call! *)

    let error (p:parser): Error.t =
      match p with
      | Final (b, _) | More (b, _) ->
         B.error b

    let error_string
          (p:parser)
          (f: Expect.t -> string)
          (g: Semantic.t -> string)
        : string =
      Error.to_string (error p) f g

    let lookahead (p:parser): token list =
      match p with
      | Final (b, _) ->
         B.lookahead b
      | _ -> assert false (* Illegal call! *)

    let has_succeeded (p:parser): bool =
      match p with
      | Final (_, Some _) ->
         true
      | _ ->
         false

    let has_failed (p:parser): bool =
      not (has_succeeded p)


    type 'a cont = 'a option -> B.t -> parser
    type 'a t = B.t -> 'a cont -> parser

    let make_parser (s:state) (p:final t): parser =
      p (B.init s)
        (fun res b -> Final (b, res))

    let update (f:state->state) (b:B.t) (k:unit cont): parser =
      k (Some ()) (B.update f b)

    let get (b:B.t) (k:state cont): parser =
      k (Some (B.state b)) b

    let get_and_update
          (f:state->state) (b:B.t) (k:state cont): parser =
      let st = B.state b in
      k (Some st) (B.update f b)



    (* Basic Combinators *)

    let return (a:'a) (b:B.t) (k:'a cont): parser =
      k (Some a) b

    let succeed (a:'a) (b:B.t) (k:'a cont): parser =
      k (Some a) (B.clear_errors b)

    let fail (e: Semantic.t): 'a t =
      fun b k ->
      k None (B.put_error e b)

    let unexpected (exp: expect): 'a t =
      fun b k ->
      k None (B.add_expected exp b)

    let token
          (f:state -> token -> ('a*state, Expect.t) result)
          (b:B.t)
          (k:'a cont)
        : parser
      =
      More (b,
            fun b ->
            match f (B.state b) (B.lookahead_token b)  with
            | Ok (a, s1) ->
               k (Some a) (B.consume s1 b)
            | Error e ->
               k None (B.reject e b))

    let map (f:'a -> 'b) (p:'a t) (b:B.t) (k:'b cont): parser =
      p b
        (fun o b ->
          match o with
          | None -> k None b
          | Some a -> k (Some (f a)) b)


    let consumer (p:'a t): 'a t =
      fun b0 k ->
      p (B.start_new_consumer b0)
        (fun res b ->
          let consumed = B.has_consumed b in
          assert (res = None || consumed);
          k res (B.end_new_consumer b0 b))


    let (>>=) (p:'a t) (f:'a -> 'b t) (b:B.t) (k:'b cont): parser =
      p
        b
        (fun o b ->
          match o with
          | Some a ->
             f a b k
          | None ->
             k None b)

    let (<|>) (p:'a t) (q:'a t): 'a t =
      fun b0 k ->
      p (B.start_new_consumer b0)
        (fun res b ->
          let consumed = B.has_consumed b in
          let b = B.end_new_consumer b0 b in
          match res with
          | None when not consumed ->
             (* p failed and did not consume tokens *)
             q b k
          |  _ ->
             k res b)


    let (<?>) (p:'a t) (e: Expect.t): 'a t =
      fun b0 k ->
      p (B.start_alternatives b0)
        (fun res b ->
          match res with
          | None ->
             k None (B.end_failed_alternatives e b0 b)
          | Some a ->
             k (Some a) (B.end_succeeded_alternatives b0 b))


    let backtrackable (p: 'a t) (e: Expect.t): 'a t =
      fun b0 k ->
      p (B.start_backtrack b0)
        (fun res b ->
          k res
            (match res with
            | None   -> B.end_backtrack_fail (Some e) b0 b
            | Some _ -> B.end_backtrack_success b0 b))


    let not_followed_by (p: 'a t) (exp: expect): unit t =
      fun b0 k ->
      p (B.start_backtrack b0)
        (fun res b ->
          match res with
          | None ->
             k (Some ()) (B.end_backtrack_fail None b0 b)
          | Some _ ->
             k None (B.end_backtrack_fail (Some exp) b0 b))




    (* Advanced Combinators *)

    let (|=) (p: ('a -> 'b) t) (q: 'a t): 'b t =
      p >>= fun f -> map f q


    let (|.) (p: 'a t) (q: _ t): 'a t =
      p >>= fun a ->
      q >>= fun _ -> return a


    let optional (p: 'a t): 'a option t =
      (map (fun a -> Some a) p)
      <|> return None


    let rec one_of (l: 'a t list): 'a t =
      match l with
      | [] ->
         assert false (* Illegal call *)

      | [p] ->
         p

      | p :: ps ->
         p <|> one_of ps


    let zero_or_more (p: 'a t): 'a list t =
      let rec many l =
        (consumer p >>= fun a -> many (a :: l))
        <|> return (List.rev l)
      in
      many []


    let one_or_more (p: 'a t): 'a list t =
      p >>= fun a ->
      zero_or_more p >>= fun l ->
      return (a :: l)


    let skip_zero_or_more (p: 'a t): int t =
      let rec many i =
        (consumer p >>= fun _ -> many (i+1))
        <|> return i
      in
      many 0


    let skip_one_or_more (p: 'a t): int t =
      return (fun n -> n + 1)
      |. p
      |= skip_zero_or_more p


    let one_or_more_separated (p: 'a t) (sep: _ t): 'a list t =
      return (fun a l -> a :: l)
      |= p
      |= zero_or_more (sep >>= fun _ -> p)


    let zero_or_more_separated (p: 'a t) (sep: _ t): 'a list t =
      one_or_more_separated p sep
      <|> return []
  end (* Make *)

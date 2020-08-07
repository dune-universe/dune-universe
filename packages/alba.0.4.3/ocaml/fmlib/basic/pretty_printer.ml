open Common


module type PRINTER =
  sig
    type t
    val empty: t
    val (<+>): t -> t -> t
    val char: char -> t
    val substring: string -> int -> int -> t
    val fill: int -> char -> t
  end





module type SIG =
  sig
    type t
    val empty: t
    val substring: string -> int -> int -> t
    val string: string -> t
    val char: char -> t
    val fill: int -> char -> t
    val line: string -> t
    val cut: t
    val space: t
    val nest: int -> t -> t
    val nest_list: int -> t list -> t
    val nest_relative: int -> t -> t
    val group: t -> t
    val group_list: t list -> t
    val wrap_words: string -> t
    val fill_paragraph: string -> t
    val (<+>): t -> t -> t
    val chain: t list -> t
    val chain_separated: t list -> t -> t
    val list_separated: t -> t list -> t
  end




module Text =
  struct
    type t =
      | String of string * int * int
      | Fill of int * char
      | Char of char
    let string s i l =
      assert (0 <= i);
      assert (0 <= l);
      assert (i + l <= String.length s);
      String (s,i,l)
    let char c = Char c
    let fill n c = Fill (n,c)
    let length = function
      | String (_,_,l) -> l
      | Fill (n,_) -> n
      | Char _ -> 1
    let apply
          (f1:string -> int -> int -> 'a)
          (f2:int -> char -> 'a)
          (f3:char -> 'a)
        : t -> 'a =
      function
      | String (s,i,l) -> f1 s i l
      | Fill (n,c) -> f2 n c
      | Char c -> f3 c
  end




module Line =
  struct
    type t = {s:string; i:int}
    let make s i = {s;i}
    let text (l:t): string = l.s
    let length l = String.length l.s
    let indent l = l.i
  end






(* Gammar

   d ::= t* g* c*                   -- document

   g ::= [| g* c* |]                -- group, at least one LB, either direct or
                                    -- indirect

   c ::= l t* g*                    -- chunk
*)



type chunk = {line: Line.t;
              texts:  Text.t list;
              cgroups: group list}

and group = {len:int;
             groups: group list;
             chunks: chunk list}

module Chunk =
  struct
    let line (c:chunk): Line.t = c.line
    let groups (c:chunk): group list = c.cgroups
    let texts (c:chunk): Text.t list = c.texts
    let make (line:Line.t): chunk =
      {line; texts = []; cgroups = []}
    let add_text (t:Text.t) (c:chunk): chunk =
      assert (c.cgroups = []);
      {c with texts = t :: c.texts}
    let add_group (g:group) (c:chunk): chunk =
      {c with cgroups = g :: c.cgroups}
  end





module Group =
  struct
    let length (g:group): int =  g.len
    let empty = {len = 0; groups = []; chunks = []}
    let groups (g:group): group list = g.groups
    let chunks (g:group): chunk list = g.chunks
    let add_text (t:Text.t) (g:group): group =
      match g.chunks with
      | [] ->
         assert false (* Illegal call *)
      | c :: tl ->
         {g with len = g.len + Text.length t; chunks = Chunk.add_text t c :: tl}
    let add_line (l:Line.t) (g:group): group =
      {g with
        len = g.len + Line.length l;
        chunks = Chunk.make l :: g.chunks}
    let add_group (gi:group) (go:group): group =
      let len = go.len + gi.len
      in
      match go.chunks with
      | [] ->
         {go with len; groups = gi :: go.groups}
      | c :: cs ->
         {go with len; chunks = Chunk.add_group gi c :: cs}
  end




module Buffer =
  struct
    type t = {gs: group list;
              l:  int;  (* length *)
              o:  int   (* open groups*) }

    let is_empty (b:t): bool = (b.o = 0)
    let length (b:t): int = b.l
    let count (b:t): int = b.o
    let groups (b:t): group list = b.gs
    let empty: t =
      {gs = []; l = 0; o = 0;}
    let push (g:group) (b:t): t =
      {gs = g :: b.gs; l = Group.length g + b.l; o = b.o + 1}
    let add_text (t:Text.t) (b:t): t =
      let open Text in
      match b.gs with
      | [] ->
         assert false (* Illegal call *)
      | g :: tl ->
         {b with
           gs = Group.add_text t g :: tl;
           l  = b.l + length t}
    let add_line (l:Line.t) (b:t): t =
      match b.gs with
      | [] ->
         assert false (* Illegal call *)
      | g :: tl ->
         {b with
           gs = Group.add_line l g :: tl;
           l  = b.l + Line.length l}
    let open_groups (n:int) (b:t): t =
      assert (0 <= n);
      let rec ogs n gs =
        if n = 0 then
          gs
        else
          Group.empty :: ogs (n-1) gs
      in
      {b with o = b.o + n; gs = ogs n b.gs}
    let close_groups (n:int) (b:t): t =
      assert (0 <= n);
      assert (n < b.o);
      let rec close n gs =
        if n = 0 then
          gs
        else
          match gs with
          | gi :: go :: tl ->
             close
               (n-1)
               (Group.add_group gi go :: tl)
          | _ ->
             assert false (* Illegal call: cannot close group unless there is
                             one group to which it can be added. *)
      in
      {b with o = b.o - n; gs = close n b.gs}
  end


module State =
  struct
    type indent = {
        line_indent:int;     (* Indent of the current line *)
        current_indent:int;  (* Current indentation level *)
        pos: int             (* Position on the current line *)
      }
    type groups = {
        oe: int;  (* open effecitve groups *)
        oa: int;  (* open active groups *)
        o_r: int; (* open groups to the right of the last open
                     group in buffer *)
      }
    type params = {
        width: int;        (* desired maximal line width *)
        ribbon: int;       (* desired maximal ribbon width *)
      }
    type t = {
        params:params;
        indent:indent;
        groups:groups;
        buffer:Buffer.t
      }

    let init  (i:int) (width:int) (ribbon:int): t =
      {params = {width;ribbon};
       indent = {line_indent = i; current_indent = i; pos = i};
       groups = {oe = 0; oa = 0; o_r = 0};
       buffer = Buffer.empty}

    let normal (st:t): bool =
      Buffer.is_empty st.buffer

    let buffering (st:t): bool =
      not (Buffer.is_empty st.buffer)

    let position (st:t): int =
      st.indent.pos

    let current_indent (st:t): int =
      st.indent.current_indent

    let relative_position(st:t): int =
      st.indent.pos - st.indent.current_indent

    let fits_pos (p:int) (st:t): bool =
      (* Is position [p] allowed, i.e. is it within the line width and the
         ribbon width? *)
      p <= st.params.width
      && p - st.indent.line_indent <= st.params.ribbon

    let fits (len:int) (st:t): bool =
      (* Do [len] more characters after the buffer still fit on the line? *)
      fits_pos (st.indent.pos + (Buffer.length st.buffer) + len) st

    let buffer_fits (st:t): bool =
      (* Does the buffer fit on the line? *)
      fits 0 st

    let increment_position (i:int) (st:t): t =
      {st with indent = {st.indent with pos = st.indent.pos + i}}

    let add_text (t:Text.t) (st:t): t =
      {st with buffer = Buffer.add_text t st.buffer}

    let add_line (alternative_text:string) (st:t): t =
      assert (buffering st);
      assert (0 < st.groups.oa);
      let o = Buffer.count st.buffer in
      let buffer =
        if st.groups.oa <= o then
          Buffer.close_groups (o - st.groups.oa) st.buffer
        else
          st.buffer
      in
      {st with
        buffer =
          Buffer.open_groups
            (if o < st.groups.oa then
               st.groups.oa - o
             else
               st.groups.o_r)
            buffer
          |> (Buffer.add_line
              @@ Line.make alternative_text st.indent.current_indent)}


    let newline (indent:int) (st:t): t =
      assert (normal st);
      {st with
        indent = {
          st.indent with
          pos = indent;
          line_indent = indent
        }}

    let active_to_effective (st:t): t =
      (* Make all active groups effective *)
      assert (normal st);
      {st with
        groups = {
          st.groups with
          oa = 0;
          oe = st.groups.oe + st.groups.oa
      }}

    let one_active_to_effective (st:t): t =
      (* Make one active group effective *)
      assert (0 < st.groups.oa);
      {st with
        groups = {
          st.groups with
          oa = st.groups.oa - 1;
          oe = st.groups.oe + 1
      }}


    let right_to_active (st:t): t =
      {st with
        groups = {
          st.groups with
          oa = st.groups.oa + st.groups.o_r;
          o_r = 0}}


    let start_buffering (s:string) (st:t): t =
      assert (normal st);
      {st with
        buffer = Buffer.(
          open_groups st.groups.oa st.buffer
          |> add_line (Line.make s st.indent.current_indent));
        groups = {st.groups with o_r = 0}}

    let clear_buffer (st:t): t =
      {st with buffer = Buffer.empty}

    let push (g:group) (st:t): t =
      {st with buffer = Buffer.push g st.buffer}


    let open_group (st:t): t =
      {st with
          groups =
            if st.groups.oa < Buffer.count st.buffer then
              {st.groups with o_r = st.groups.o_r + 1}
            else
              {st.groups with oa = st.groups.oa + 1}}


    let close_group (st:t): t =
      {st with
        groups =
          if 0 < st.groups.o_r then
            (assert (st.groups.oa < Buffer.count st.buffer);
             {st.groups with
               o_r = st.groups.o_r - 1})
          else if 0 < st.groups.oa then
            {st.groups with
              oa = st.groups.oa - 1}
          else
            (assert (0 < st.groups.oe);
             {st.groups with
               oe = st.groups.oe - 1})}

    let increment_indent (i:int) (st:t) =
      let current_indent = st.indent.current_indent + i in
      assert (0 <= current_indent);
      {st with
         indent = {st.indent with current_indent}}
  end








module Pretty (P:PRINTER) =
  struct

    type state = State.t

    type loop_state =
      | More of (unit -> loop_state)
      | Done of P.t


    let loop: loop_state -> P.t =
        let rec do_loop i = function
        | Done p ->
            p
        | More f ->
            do_loop (i + 1) (f ())
        in
        do_loop 0

    type 'a cont =
      (* A continuation function takes a value, a state and the cumulated
         printing command and returns the remainder of the loop. *)
      'a -> state -> P.t -> loop_state

    module M =
      struct
        type 'a t =
          state
          -> P.t      (* printed up to now *)
          -> 'a cont  (* continuation *)
          -> loop_state

        let return (a:'a): 'a t =
          fun st p k -> k a st p

        let (>>=) (m:'a t) (f:'a -> 'b t): 'b t =
          fun st p k ->
          m
            st
            p
            (fun a st p -> More (fun _ -> f a st p k))
      end


    type t = unit M.t


    let empty: t =
      M.return ()

    let (<+>) (m1:t) (m2:t): t =
      M.(m1 >>= fun _ -> m2)


    let state: state M.t =
      fun st p k -> k st st p

    let put (st:state): unit M.t =
      fun _ p k -> k () st p


    let update (f:state->state): t =
      fun st p k -> k () (f st) p


    let get_and_update (f:state->state): state M.t =
      fun st p k -> k st (f st) p


    let relative_position:  int M.t =
      M.(state >>= fun st -> return @@ State.relative_position st)


    let out_string (s:string) (start:int) (len:int): t =
      fun st p k ->
      More
        (fun _ ->
          k
            ()
            (State.increment_position len st)
            P.(p <+> substring s start len))


    let out_char (c:char): t =
      fun st p k ->
      More
        (fun _ ->
          k
            ()
            (State.increment_position 1 st)
            P.(p <+> char c))


    let out_fill (n:int) (c:char): t =
      assert (0 <= n);
      fun st p k ->
      More
        (fun _ ->
          k
            ()
            (State.increment_position n st)
            P.(p <+> fill n c))


    let out_text (t:Text.t): t =
      Text.apply out_string out_fill out_char t

    let out_line (indent:int): t =
      fun st p k ->
      More
        (fun _ ->
          k
            ()
            (State.newline indent st)
            P.(p <+> char '\n' <+> fill indent ' '))


    let print_list (l:'a list) (f:'a -> t): t =
      M.(List.fold_right
           (fun a pr -> pr >>= fun _ -> f a)
           l (return ()))

    let out_texts (l:Text.t list): t =
      print_list
        l
        out_text

    let out_alternative_text (l:Line.t): t =
      let s = Line.text l in
      out_string s 0 (String.length s)

    let line_normal (s:string): t =
      M.(state >>= fun st ->
         if 0 < st.groups.oa && State.fits (String.length s) st then
           put (State.start_buffering s st)
         else
           put (State.active_to_effective st) >>= fun _ ->
           out_line st.indent.current_indent)



    let rec flush_flatten_group (g:group): t =
      (* Print the group [g] flattened. *)
      let open Group in
      M.(flush_flatten_groups (groups g) >>= fun _ ->
         flush_flatten_chunks (chunks g))

    and flush_flatten_groups (gs:group list): t =
      (* Print all groups in the list [gs] flattened. *)
      print_list
        gs
        flush_flatten_group

    and flush_flatten_chunks (cs:chunk list): t =
      (* Print all chunks in the list [cs] flattened. *)
      print_list
        cs
        flush_flatten_chunk

    and flush_flatten_chunk (c:chunk): t =
      (* Print the chunk [c] flattened. *)
      M.(out_alternative_text (Chunk.line c)
         >>= fun _ ->
         out_texts (Chunk.texts c)
         >>= fun _ ->
         flush_flatten_groups (Chunk.groups c))


    let flush_flatten: t =
      (* flush the complete buffer flattening it i.e. print all line breaks
         with their corresponding alternative text. *)
      let open M in
      state >>= fun st ->
      print_list
        (Buffer.groups st.buffer)
        flush_flatten_group
      >>= fun _ ->
      update State.clear_buffer


    let rec flush_group (g:group): t =
      (* Flush the complete group [g]. If it fits on the line, then flush it
         flattened. Otherwise print its break hints as line breaks. *)
      let open M in
      state >>= fun st ->
      if State.fits (Group.length g) st then
        flush_flatten_group g
      else
        flush_groups g.groups >>= fun _ ->
        flush_chunks g.chunks

    and flush_chunk (c:chunk): t =
      let open M in
      out_line (Line.indent (Chunk.line c))
      >>= fun _ ->
      out_texts c.texts
      >>= fun _ ->
      flush_groups c.cgroups

    and flush_groups (gs:group list): t =
      print_list gs flush_group

    and flush_chunks (cs:chunk list): t =
      print_list cs flush_chunk


    let flush_incomplete (is_last:bool) (g:group): t =
      (* Flush an incomplete group from the buffer *)
      let open M in
      update
        (fun st ->
          if 0 < st.groups.oa then
            State.one_active_to_effective st
          else if is_last then
            State.right_to_active st
          else
            st)
      >>= fun _ ->
      flush_groups g.groups
      >>= fun _ ->
      flush_chunks g.chunks

    let flush_effective: t =
      (* Flush open groups until buffer fits or is empty. *)
      let open M in
      let rec flush (remaining_len:int) (is_last:bool) = function
        | [] -> assert false (* Illegal call! *)
        | [g] ->
           flush_incomplete is_last g
        | g :: gs ->
           let len = Group.length g + remaining_len
           in
           flush len false gs >>= fun _ ->
           state >>= fun st ->
           if State.fits len st then
             put (State.push g st)
           else
             flush_incomplete is_last g
      in
      state >>= fun st ->
      put (State.clear_buffer st) >>= fun _ ->
      flush 0 true (Buffer.groups st.buffer)


    let text (t:Text.t): t =
      let open M in
      state >>= fun st ->
      if State.normal st then
        out_text t
      else
        let st = State.add_text t st in
        if State.buffer_fits st then
          put st
        else
          put st >>= fun _ -> flush_effective


    let substring (s:string) (start:int) (len:int): t =
      assert (0 <= start);
      assert (start+len <= String.length s);
      text (Text.string s start len)


    let string (s:string): t =
      substring s 0 (String.length s)

    let char (c:char): t =
      text (Text.char c)

    let fill (n:int) (c:char): t =
      assert (0 <= n);
      text (Text.fill n c)


    let rec line (alternative_text:string): t =
      let open M in
      state >>= fun st ->
      if State.normal st then
        line_normal alternative_text
      else if 0 < st.groups.oa then
        (* Still inside the active group *)
        let st = State.(right_to_active @@ add_line alternative_text st)
        in
        if State.buffer_fits st then
          put st
        else
          put st >>= fun _ -> flush_effective
      else
        (* Outside the active group. *)
        put (State.right_to_active st) >>= fun _ ->
        flush_flatten >>= fun _ ->
        line alternative_text


    let cut: t =
      line ""

    let space: t =
      line " "


    let rec chain (l: t list): t =
      let open M in
      match l with
      | [] ->
         return ()
      | hd :: tl ->
         hd >>= fun _ -> chain tl


    let list_separated (sep: t) (lst: t list): t =
      let rec chn = function
        | [] ->
           empty
        | [p] ->
           p
        | p :: tl ->
           p <+> sep <+> chn tl
      in
      chn lst


    let chain_separated (lst:t list) (sep:t): t =
       list_separated sep lst


    let group (m:t): t =
      let open M in
      update State.open_group >>= fun _ ->
      m >>= fun a ->
      update State.close_group >>= fun _ ->
      return a

    let nest (i:int) (m:t): 't =
      let open M in
      get_and_update (State.increment_indent i) >>= fun st ->
      let pos,ind = State.position st, State.current_indent st in
      let m =
        if pos = 0 (*pos < ind + i*) then
          fill (ind + i - pos) ' ' >>= fun _ -> m
        else
          m
      in
      m >>= fun a ->
      update (State.increment_indent (-i)) >>= fun _ ->
      return a

    let nest_list (i:int) (lst:t list): t =
      nest i (chain lst)

    let nest_relative (i:int) (m:t): t =
      let open M in
      relative_position >>= fun p ->
      nest (i+p) m


    let group_list (lst: t list): t =
      group @@ chain lst

    let wrap_words (s:string): t =
      let open M in
      let is_blank c = c = ' '
      and is_not_blank c = c <> ' '
      in
      let word_start i = String.find is_not_blank i s
      and word_end   i = String.find is_blank i s
      and len = String.length s
      in
      let rec fill p =
        let i = word_start p in
        (if p < i then
           group space
         else
           return ())
        >>= fun _ ->
        if i < len then
          let j = word_end i in
          substring s i (j-i) >>= fun _ ->
          fill j
        else
          return ()
      in
      fill 0

    let fill_paragraph = wrap_words

    let run (indent:int) (width:int) (ribbon:int) (p:unit M.t): P.t =
      loop
      @@ M.(p >>= fun _ -> flush_flatten)
           (State.init indent width ribbon)
           P.empty
           (fun () _ p -> Done p)
  end





















(*
  ------------
  Module Tests
  ------------
 *)

module PP = Pretty (Readable_printer)

include PP

let run (indent:int) (width:int) (ribbon:int) (m:t): Readable_printer.R.t =
  PP.run
    indent width ribbon
    m
  |> Readable_printer.readable


module R = Readable_printer.R

let compare (r:R.t) (s:string): bool =
  let len = String.length s in
  let rec comp r i =
    if i = len then
      true
    else
      let more = R.has_more r in
      if not more then
        false
      else
        s.[i] = R.peek r
        && comp (R.advance r) (i+1)
  in
  comp r 0

let print_readable (r:R.t): unit =
  let open Printf in
  let rec print r =
    if R.has_more r then
      (printf "%c" (R.peek r);
       print (R.advance r))
    else
      ()
  in
  print r



let test (w:int) (pflag:bool) (pp:t) (expected:string): bool =
  let res = compare (run 0 w w pp) expected in
  if pflag then
    print_readable (run 0 w w pp);
  res


let%test _ =
  let str = "01234 6789 0 2 4 6 8 01 34 6789 0"
  and res = "01234 6789\n\
             0 2 4 6 8\n\
             01 34 6789\n\
             0"
  in
  test 10 false (fill_paragraph str) res


let%test _ =
  test
    10 false
    (nest_list 4 [string "0123"; cut; string "456"]
     <+> cut <+> string "0123")
    "    0123\n\
     \    456\n\
     0123"


let%test _ =
  compare
    (run 0 20 20
       (group_list
          [
            (group_list
               [string "class";
                nest_list 4 [space; string "Natural"];
                space; string "create"]);
            nest_list 4
              [space;
               (group_list
                  [string "0"; line "; "; string "succ(Natural)"])
              ];
            chain [space; string "end"]
          ]
       ))
       "class Natural create\n\
        \    0; succ(Natural)\n\
        end"



let maybe =
  group (group (string "class"
                <+> nest 4 (space <+> string "Maybe(A)")
                <+> space
                <+> string "create")
         <+> nest 4 (space
                     <+> group
                           (string "nothing"
                            <+> line "; "
                            <+> string "just(A)"))
         <+> space
         <+> string "end")

let%test _ =
  compare
    (run 0 70 70 maybe)
    "class Maybe(A) create nothing; just(A) end"


let%test _ =
  compare
    (run 0 20 20 maybe)
    "class\
     \n    Maybe(A)\n\
     create\
     \n    nothing; just(A)\n\
     end"



let%test _ =
  compare
    (run 0 15 15 maybe)
    "class\
     \n    Maybe(A)\n\
     create\
     \n    nothing\
     \n    just(A)\n\
     end"





let plus =
  let gns p =
    group (nest 2 (space <+> p))
  in
  let insp =
    group (string "inspect"
           <+> nest
                 2
                 (space
                  <+> group (
                          string "a" <+> line "; "
                          <+> string "(_:Natural) := Natural"))
           <+> space <+> string "case"
      )
  and cases =
    group (
        string "0 :="
        <+> gns (string "b")
        <+> line "; "
        <+> string "n.successor :="
        <+> gns (string "n + b.successor")
      )
  in
  string "(+)(a:Natural,b:Natural): Natural :="
  <+> gns (
          group (
              insp
              <+> nest
                    2
                    (space <+> cases)
              <+> space <+> string "end")
        )



let%test _ =
  compare
    (run 0 40 40 plus)
    "(+)(a:Natural,b:Natural): Natural :=\
     \n  inspect a; (_:Natural) := Natural case\
     \n    0 := b\n    n.successor := n + b.successor\
     \n  end"



let%test _ =
  compare
    (run 0 39 39 plus)
    "(+)(a:Natural,b:Natural): Natural :=\
     \n  inspect\
     \n    a; (_:Natural) := Natural\
     \n  case\
     \n    0 := b\
     \n    n.successor := n + b.successor\
     \n  end"


let%test _ =
  compare
    (run 0 33 33 plus)
    "(+)(a:Natural,b:Natural): Natural :=\
     \n  inspect\n    a; (_:Natural) := Natural\
     \n  case\
     \n    0 := b\
     \n    n.successor :=\
     \n      n + b.successor\
     \n  end"

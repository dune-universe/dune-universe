let none : Earley.blank = fun buf pos ->
  (buf, pos)

let from_charset : Charset.t -> Earley.blank = fun cs ->
  let rec blank_cs buf pos =
    let (c, buf', pos') = Input.read buf pos in
    if Charset.mem cs c then blank_cs buf' pos' else (buf, pos)
  in blank_cs

let default_charset : Charset.t =
  List.fold_left Charset.add Charset.empty [' '; '\t'; '\n'; '\r']

let default : Earley.blank = from_charset default_charset

let list_of_string : string -> char list = fun s ->
  let l = ref [] in
  String.iter (fun c -> l := c :: !l) s;
  List.rev !l

let line_comments : ?blanks:Charset.t -> string -> Earley.blank =
  fun ?(blanks=default_charset) s ->
    let line_comments1 c1 =
      let blanks = Charset.add blanks c1 in
      let rec line_comments1 buf pos =
        let (c, buf', pos') = Input.read buf pos in
        if Charset.mem blanks c then line_comments1 buf' pos' else (buf, pos)
      in line_comments1
    in
    let line_comments2 c1 c2 =
      let rec line_comments2 buf pos =
        let (c, buf', pos') = Input.read buf pos in
        if Charset.mem blanks c then line_comments2 buf' pos' else
        if c = c1 && Input.get buf' pos' = c2 then
          let (buf, pos) = Input.normalize buf' (Input.line_length buf') in
          line_comments2 buf pos
        else
          (buf, pos)
      in line_comments2
    in
    let line_commentsn cc ccs =
      let rec line_commentsn state buf pos =
        let (c, buf', pos') = Input.read buf pos in
        match state with
        | None when Charset.mem blanks c ->
            line_commentsn None buf' pos'
        | None when c = cc               ->
            line_commentsn (Some((buf,pos),ccs)) buf' pos'
        | None                           ->
            (buf, pos)
        | Some(_,[])                     ->
            let (buf, pos) = Input.normalize buf' (Input.line_length buf') in
            line_commentsn None buf pos
        | Some(p,d::cs) when c = d       ->
            line_commentsn (Some(p,cs)) buf' pos'
        | Some(p,_)                      ->
            p
      in line_commentsn None
    in
    match list_of_string s with
    | []                             -> invalid_arg "empty delimiter"
    | c::_ when Charset.mem blanks c -> invalid_arg "invalid delimiter"
    | [c1]                           -> line_comments1 c1
    | [c1;c2]                        -> line_comments2 c1 c2
    | c::cs                          -> line_commentsn c cs

exception Bad_ocaml_comment of string * (Input.buffer * int)

let ocaml_blank : Earley.blank = fun buf pos ->
  let unclosed_comment_string p =
    raise (Bad_ocaml_comment("unclosed string", p))
  in
  let unclosed_comment p =
    raise (Bad_ocaml_comment("unclosed comment", p))
  in
  let rec fn state stack prev curr =
    let (buf, pos) = curr in
    let (c, buf', pos') = Input.read buf pos in
    let next = (buf', pos') in
    match (state, stack, c) with
    (* Basic blancs. *)
    | (`Ini      , []  , ' '     )
    | (`Ini      , []  , '\t'    )
    | (`Ini      , []  , '\r'    )
    | (`Ini      , []  , '\n'    ) -> fn `Ini stack curr next
    (* Comment opening. *)
    | (`Ini      , _   , '('     ) -> fn (`Opn(curr)) stack curr next
    | (`Ini      , []  , _       ) -> curr
    | (`Opn(p)   , []  , '*'     ) ->
        let (c, buf', pos') = Input.read buf' pos' in
        let (c',_,_) = Input.read buf' pos' in
        if c = '*' && c' <> '*' then fn `Cls (p::stack) curr (buf',pos')
        else fn `Ini (p::stack) curr next
    | (`Opn(p)   , _   , '*'     ) -> fn `Ini (p::stack) curr next
    | (`Opn(_)   , _::_, '"'     ) -> fn (`Str(curr)) stack curr next (*#*)
    | (`Opn(_)   , _::_, '{'     ) -> fn (`SOp([],curr)) stack curr next (*#*)
    | (`Opn(_)   , _::_, '('     ) -> fn (`Opn(curr)) stack curr next
    | (`Opn(_)   , []  , _       ) -> prev
    | (`Opn(_)   , _   , _       ) -> fn `Ini stack curr next
    (* String litteral in a comment (including the # rules). *)
    | (`Ini      , _::_, '"'     ) -> fn (`Str(curr)) stack curr next
    | (`Str(_)   , _::_, '"'     ) -> fn `Ini stack curr next
    | (`Str(p)   , _::_, '\\'    ) -> fn (`Esc(p)) stack curr next
    | (`Esc(p)   , _::_, _       ) -> fn (`Str(p)) stack curr next
    | (`Str(p)   , _::_, '\255'  ) -> unclosed_comment_string p
    | (`Str(_)   , _::_, _       ) -> fn state stack curr next
    | (`Str(_)   , []  , _       ) -> assert false (* Impossible. *)
    | (`Esc(_)   , []  , _       ) -> assert false (* Impossible. *)
    (* Delimited string litteral in a comment. *)
    | (`Ini      , _::_, '{'     ) -> fn (`SOp([],curr)) stack curr next
    | (`SOp(l,p) , _::_, 'a'..'z')
    | (`SOp(l,p) , _::_, '_'     ) -> fn (`SOp(c::l,p)) stack curr next
    | (`SOp(_,_) , p::_, '\255'  ) -> unclosed_comment p
    | (`SOp(l,p) , _::_, '|'     ) -> fn (`SIn(List.rev l,p)) stack curr next
    | (`SOp(_,_) , _::_, _       ) -> fn `Ini stack curr next
    | (`SIn(l,p) , _::_, '|'     ) -> fn (`SCl(l,(l,p))) stack curr next
    | (`SIn(_,p) , _::_, '\255'  ) -> unclosed_comment_string p
    | (`SIn(_,_) , _::_, _       ) -> fn state stack curr next
    | (`SCl([],b), _::_, '}'     ) -> fn `Ini stack curr next
    | (`SCl([],b), _::_, '\255'  ) -> unclosed_comment_string (snd b)
    | (`SCl([],b), _::_, _       ) -> fn (`SIn(b)) stack curr next
    | (`SCl(l,b) , _::_, c       ) ->
        if c = List.hd l then fn (`SCl(List.tl l, b)) stack curr next
        else fn (`SIn(b)) stack curr next
    | (`SOp(_,_) , []  , _       ) -> assert false (* Impossible. *)
    | (`SIn(_,_) , []  , _       ) -> assert false (* Impossible. *)
    | (`SCl(_,_) , []  , _       ) -> assert false (* Impossible. *)
    (* Comment closing. *)
    | (`Ini      , _::_, '*'     ) -> fn `Cls stack curr next
    | (`Cls      , _::_, '*'     ) -> fn `Cls stack curr next
    | (`Cls      , _::_, '"'     ) -> fn (`Str(curr)) stack curr next (*#*)
    | (`Cls      , _::_, '{'     ) -> fn (`SOp([],curr)) stack curr next (*#*)
    | (`Cls      , p::s, ')'     ) -> fn `Ini s curr next
    | (`Cls      , _::_, _       ) -> fn `Ini stack curr next
    | (`Cls      , []  , _       ) -> assert false (* Impossible. *)
    (* Comment contents (excluding string litterals). *)
    | (`Ini     , p::_, '\255'  ) -> unclosed_comment p
    | (`Ini     , _::_, _       ) -> fn `Ini stack curr next
  in
  fn `Ini [] (buf, pos) (buf, pos)



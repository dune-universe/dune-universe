open Identifiers

module Id = NumId.Make(Int64)

type byte = char
type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type buffer = | Bytes of bytes | Bigstr of bigstring | Bufset of t list
and  t = { 
  id : Id.t;
  mutable buffer : buffer;
  mutable offset : int; 
  mutable capacity : int; 
  grow : int;
}

let compare a b = Id.compare a.id b.id 

let equal a b = Id.equal a.id b.id


let from_bigstring ?(grow=0) bs =
  { 
    id = Id.next_id ();
    buffer = Bigstr bs;
    offset = 0;
    capacity = Bigstringaf.length bs;
    grow;
  }

let from_bytes ?(grow=0) bs =
  { 
    id = Id.next_id ();
    buffer = Bytes bs;
    offset = 0;
    capacity = Bytes.length bs;
    grow;
  }

let create_bigstring ?(grow=0) len = from_bigstring ~grow (Bigstringaf.create len)

let create_bytes ?(grow=0) len = from_bytes ~grow (Bytes.create len)

let create ?(grow=0) len = 
  if len < 2048 
  then create_bytes ~grow len
  else create_bigstring ~grow len

let duplicate bs = 
  { 
    id = bs.id;
    buffer = bs.buffer;
    offset = bs.offset;
    capacity = bs.capacity;
    grow = bs.grow;
  }

let capacity bs = bs.capacity

let wrap ?(grow=0) bslist = 
  { 
    id = Id.next_id ();
    buffer = Bufset bslist;
    offset = 0;
    capacity = List.fold_left (fun accu bs -> accu + capacity bs) 0 bslist;
    grow;
  }

let slice from len bs = 
  if from >= 0 && len >= 0 && (from + len) <= bs.capacity  then
    { 
      id = Id.next_id ();
      buffer = bs.buffer;
      offset = bs.offset + from;
      capacity = len;
      grow = 0;
    }
  else raise @@ Atypes.Exception (`OutOfBounds (`Msg (
    Printf.sprintf "Abytes.slice")))

let expand n bs = 
  match bs.buffer with 
  | Bytes _ -> 
    let hd = duplicate bs in 
    bs.buffer <- Bufset [hd; create_bytes n];
    bs.capacity <- bs.capacity + n
  | Bigstr _ -> 
    let hd = duplicate bs in 
    bs.buffer <- Bufset [hd; create_bigstring n];
    bs.capacity <- bs.capacity + n
  | Bufset b -> 
    bs.buffer <- Bufset (List.append b [create n]);
    bs.capacity <- bs.capacity + n


let rec blit_from_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= Bytes.length src && dst_idx >= 0 
  then 
    if dst_idx + len <= capacity dst 
    then
      let dst_idx = (dst.offset + dst_idx) in
      match dst.buffer with 
      | Bytes b -> (Bytes.blit src src_idx b dst_idx len)
      | Bigstr b -> (Bigstringaf.blit_from_bytes src ~src_off:src_idx b ~dst_off:dst_idx ~len)
      | Bufset b -> 
        let rec blit_from_bytes_to_set ~src ~src_idx ~dst ~dst_idx ~len = 
          match dst with 
          | [] -> ()
          | hd :: tl -> 
            if capacity hd > dst_idx 
            then 
              let hd_writable = capacity hd - dst_idx in 
              if hd_writable >= len
              then blit_from_bytes ~src ~src_idx ~dst:hd ~dst_idx ~len
              else 
                begin 
                  blit_from_bytes ~src ~src_idx ~dst:hd ~dst_idx ~len:hd_writable ;                   
                  blit_from_bytes_to_set ~src ~src_idx:(src_idx + hd_writable) ~dst:tl ~dst_idx:0 ~len:(len - hd_writable)
                end
            else 
              blit_from_bytes_to_set ~src ~src_idx ~dst:tl ~dst_idx:(dst_idx - capacity hd) ~len
        in 
        blit_from_bytes_to_set ~src ~src_idx ~dst:b ~dst_idx ~len
    else
      match dst.grow with 
      | 0 -> raise @@ Atypes.Exception (`OutOfBounds (`Msg "Abytes.blit_from_bytes"))
      | n -> expand n dst; blit_from_bytes ~src ~src_idx ~dst ~dst_idx ~len
  else raise @@ Atypes.Exception (`OutOfBounds (`Msg "Abytes.blit_from_bytes"))
  
let rec blit_to_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= capacity src 
  then
    let src_idx = (src.offset + src_idx) in
    match src.buffer with 
    | Bytes b -> (Bytes.blit b src_idx dst dst_idx len)
    | Bigstr b -> (Bigstringaf.blit_to_bytes b ~src_off:src_idx dst ~dst_off:dst_idx ~len)
    | Bufset b -> 
      let rec blit_set_to_bytes ~src ~src_idx ~dst ~dst_idx ~len = 
        match src with 
        | [] ->  ()
        | hd :: tl -> 
          if capacity hd > src_idx 
          then 
            let hd_readable = capacity hd - src_idx in
            if hd_readable >= len
            then blit_to_bytes ~src:hd ~src_idx ~dst ~dst_idx ~len
            else 
              begin 
                blit_to_bytes ~src:hd ~src_idx ~dst ~dst_idx ~len:hd_readable ;
                blit_set_to_bytes ~src:tl ~src_idx:0 ~dst ~dst_idx:(dst_idx + hd_readable) ~len:(len - hd_readable)
              end               
          else 
            blit_set_to_bytes ~src:tl ~src_idx:(src_idx - capacity hd) ~dst ~dst_idx ~len
      in 
      blit_set_to_bytes ~src:b ~src_idx ~dst ~dst_idx ~len
  else 
    raise @@ Atypes.Exception (`OutOfBounds (`Msg "Abytes.blit_to_bytes")) 

let rec blit_from_bigstring ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= Bigstringaf.length src && dst_idx >= 0 
  then 
    if dst_idx + len <= capacity dst 
    then
      let dst_idx = (dst.offset + dst_idx) in
      match dst.buffer with 
      | Bytes b -> (Bigstringaf.blit_to_bytes src ~src_off:src_idx b ~dst_off:dst_idx ~len)
      | Bigstr b -> (Bigstringaf.blit src ~src_off:src_idx b ~dst_off:dst_idx ~len)
      | Bufset b -> 
        let rec blit_from_bigstring_to_set ~src ~src_idx ~dst ~dst_idx ~len = 
          match dst with 
          | [] -> ()
          | hd :: tl -> 
            if capacity hd > dst_idx 
            then 
              let hd_writable = capacity hd - dst_idx in 
              if hd_writable >= len
              then blit_from_bigstring ~src ~src_idx ~dst:hd ~dst_idx ~len
              else 
                begin 
                  blit_from_bigstring ~src ~src_idx ~dst:hd ~dst_idx ~len:hd_writable ;
                  blit_from_bigstring_to_set ~src ~src_idx:(src_idx + hd_writable) ~dst:tl ~dst_idx:0 ~len:(len - hd_writable)
                end
            else 
              blit_from_bigstring_to_set ~src ~src_idx ~dst:tl ~dst_idx:(dst_idx - capacity hd) ~len
        in 
        blit_from_bigstring_to_set ~src ~src_idx ~dst:b ~dst_idx ~len
    else
      match dst.grow with 
      | 0 -> raise @@ Atypes.Exception (`OutOfBounds (`Msg "Abytes.blit_from_bigstring"))
      | n -> expand n dst; blit_from_bigstring ~src ~src_idx ~dst ~dst_idx ~len
  else 
    raise @@ Atypes.Exception (`OutOfBounds (`Msg "Abytes.blit_from_bigstring"))
  
let rec blit_to_bigstring ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= capacity src 
  then
    let src_idx = (src.offset + src_idx) in
    match src.buffer with 
    | Bytes b -> (Bigstringaf.blit_from_bytes b ~src_off:src_idx dst ~dst_off:dst_idx ~len)
    | Bigstr b -> (Bigstringaf.blit b ~src_off:src_idx dst ~dst_off:dst_idx ~len)
    | Bufset b -> 
      let rec blit_set_to_bigstring ~src ~src_idx ~dst ~dst_idx ~len = 
        match src with 
        | [] -> ()
        | hd :: tl -> 
          if capacity hd > src_idx 
          then 
            let hd_readable = capacity hd - src_idx in
            if hd_readable >= len
            then blit_to_bigstring ~src:hd ~src_idx ~dst ~dst_idx ~len:len
            else
              begin 
                blit_to_bigstring ~src:hd ~src_idx ~dst ~dst_idx ~len:hd_readable ;
                blit_set_to_bigstring ~src:tl ~src_idx:0 ~dst ~dst_idx:(dst_idx + hd_readable) ~len:(len - hd_readable)
              end
          else 
            blit_set_to_bigstring ~src:tl ~src_idx:(src_idx - capacity hd) ~dst ~dst_idx ~len
      in 
      blit_set_to_bigstring ~src:b ~src_idx ~dst ~dst_idx ~len
  else 
    raise @@ Atypes.Exception (`OutOfBounds (`Msg "Abytes.blit_to_bigstring")) 

let rec blit ~src ~src_idx ~dst ~dst_idx ~len = 
  if src_idx >= 0 && len >= 0 && src_idx + len <= capacity src && dst_idx >= 0 
  then 
    let src_idx = (src.offset + src_idx) in
    if dst_idx + len <= capacity dst 
    then
      match src.buffer with 
      | Bytes b -> blit_from_bytes ~src:b ~src_idx ~dst ~dst_idx ~len
      | Bigstr b -> blit_from_bigstring ~src:b ~src_idx ~dst ~dst_idx ~len
      | Bufset b -> 
        let rec blit_fromto_set ~src ~src_idx ~dst ~dst_idx ~len = 
          match src with 
          | [] -> ()
          | hd :: tl -> 
            if capacity hd > src_idx 
            then 
              let hd_readable = capacity hd - src_idx in
              if hd_readable >= len
              then blit ~src:hd ~src_idx ~dst ~dst_idx ~len:len
              else 
                begin 
                  blit ~src:hd ~src_idx ~dst ~dst_idx ~len:hd_readable ;                
                  blit_fromto_set ~src:tl ~src_idx:0 ~dst ~dst_idx:(dst_idx + hd_readable) ~len:(len - hd_readable)
                end
            else 
              blit_fromto_set ~src:tl ~src_idx:(src_idx - capacity hd) ~dst ~dst_idx ~len
        in
        blit_fromto_set ~src:b ~src_idx ~dst ~dst_idx ~len
    else
      match dst.grow with 
      | 0 -> raise @@ Atypes.Exception (`OutOfBounds (`Msg "Abytes.blit"))
      | n -> expand n dst; blit ~src ~src_idx ~dst ~dst_idx ~len
  else raise @@ Atypes.Exception (`OutOfBounds (`Msg "Abytes.blit"))


let rec get_byte ~at bs =
  if at >= 0 && at + 1 <= capacity bs then
    begin
      let at = bs.offset + at in
      (match bs.buffer with 
      | Bytes b -> (Bytes.get b at)
      | Bigstr b -> (Bigstringaf.get b at)
      | Bufset b -> 
        let rec get_byte_from_set at set = 
          match set with 
          | [] -> raise @@ Atypes.Exception (`OutOfBounds (`Msg "Abytes.get_byte"))
          | hd :: tl -> 
            if capacity hd > at 
            then get_byte ~at hd
            else get_byte_from_set (at - capacity hd) tl in 
        get_byte_from_set at b)
    end
  else 
    raise @@ Atypes.Exception (`OutOfBounds (`Msg "Abytes.get_byte"))

let get_bytes ~at len bs = 
  let dst = Bytes.create len in
  blit_to_bytes ~src:bs ~src_idx:at ~dst ~dst_idx:0 ~len ;
  dst 

let get_bigstring ~at len bs = 
  let dst = Bigstringaf.create len in
  blit_to_bigstring ~src:bs ~src_idx:at ~dst ~dst_idx:0 ~len ; dst 

let get_abytes ~at len bs = 
  let dst = create len in
  blit ~src:bs ~src_idx:at ~dst ~dst_idx:0 ~len ; dst 


let rec set_byte c ~at bs = 
  if at >= 0 then 
    begin
      if at + 1 <= capacity bs then
        begin
          match bs.buffer with 
          | Bytes b -> Bytes.set b (bs.offset + at) c
          | Bigstr b -> Bigstringaf.set b (bs.offset + at) c
          | Bufset b -> 
            let rec set_byte_to_set at set = 
              match set with 
              | [] -> raise @@ Atypes.Exception (`OutOfBounds (`Msg "Abytes.set_byte"))
              | hd :: tl -> 
                if capacity hd > at 
                then set_byte c ~at hd
                else set_byte_to_set (at - capacity hd) tl in 
            set_byte_to_set at b
        end
      else
        match bs.grow with 
        | 0 -> raise @@ Atypes.Exception (`OutOfBounds (`Msg "Abytes.set_byte"))
        | n -> expand n bs; set_byte ~at c bs
    end
  else raise @@ Atypes.Exception (`OutOfBounds (`Msg "Abytes.set_byte"))

let set_bytes src ~at bs = 
  blit_from_bytes ~src ~src_idx:0 ~dst:bs ~dst_idx:at ~len:(Bytes.length src)

let set_bigstring src ~at bs = 
  blit_from_bigstring ~src ~src_idx:0 ~dst:bs ~dst_idx:at ~len:(Bigstringaf.length src)

let set_abytes src ~at bs = 
  blit ~src ~src_idx:0 ~dst:bs ~dst_idx:at ~len:(capacity src) 

let rec to_io_vecs ~idx ~len ~append_bytes ~append_bigarray io_vecs bs = 
  if capacity bs >= idx + len 
  then 
    let idx = bs.offset + idx in
    match bs.buffer with 
    | Bytes b -> append_bytes io_vecs b idx len 
    | Bigstr b -> append_bigarray io_vecs b idx len 
    | Bufset b -> 
      let rec set_to_io_vecs ~idx ~len ~append_bytes ~append_bigarray io_vecs b = match b with 
      | [] -> ()
      | hd :: tl -> 
        if capacity hd >= idx
        then 
          let fst_len = min (len) (capacity hd - idx) in
          to_io_vecs ~idx ~len:fst_len  ~append_bytes ~append_bigarray io_vecs hd; 
          set_to_io_vecs ~idx:0 ~len:(len - fst_len) ~append_bytes ~append_bigarray io_vecs tl; 
        else 
          set_to_io_vecs ~idx:(idx - capacity hd) ~len ~append_bytes ~append_bigarray io_vecs tl in 
      set_to_io_vecs  ~idx ~len ~append_bytes ~append_bigarray io_vecs b
  else 
    match bs.grow with 
    | 0 -> raise @@ Atypes.Exception (`OutOfBounds (`Msg "Abytes.to_io_vecs"))
    | n -> expand n bs; to_io_vecs ~idx ~len ~append_bytes ~append_bigarray io_vecs bs
    


let hexdump ?separator:(sep="") bs =
  let rec hexdump bs idx =
    if idx < bs.capacity then 
    (Printf.sprintf "%02x%s" (get_byte ~at:idx bs |> int_of_char ) sep ) ^ (hexdump bs (idx+1))
    else "" in 
  hexdump bs 0
    
let to_string bs =
  "(capacity: " ^ (string_of_int bs.capacity) ^ " content: " ^ (hexdump bs ~separator:":") ^ ")"

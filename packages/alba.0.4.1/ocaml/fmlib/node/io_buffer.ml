open Fmlib
open Module_types
open Js_of_ocaml

type position = int

class type dataview =
  object
    method getUint8: position -> char Js.meth
    method setUint8: position -> char -> unit Js.meth
  end

class type buf =
  object
    method length: int Js.readonly_prop
    method copy: buf Js.t -> int -> int -> int -> unit Js.meth
    method rp: int Js.prop
    method wp: int Js.prop
  end



type js_buffer = buf Js.t

type t = {buffer: buf Js.t; view: dataview Js.t}

let js_buffer (b:t): js_buffer =
  b.buffer

let is_empty (b:t): bool =
  b.buffer##.rp = b.buffer##.wp

let is_full (b:t): bool =
  b.buffer##.wp = b.buffer##.length

let read_pointer (b:t): int =
  b.buffer##.rp

let write_pointer (b:t): int =
  b.buffer##.rp

let capacity (b:t): int =
  b.buffer##.length

let length (b:t): int =
  b.buffer##.wp - b.buffer##.rp

let reset (b:t): unit =
  b.buffer##.rp := 0;
  b.buffer##.wp := 0


let set_read_pointer (b:t) (rp:int): unit =
  assert (rp <= b.buffer##.wp);
  b.buffer##.rp := rp

let set_write_pointer (b:t) (wp:int): unit =
  assert (b.buffer##.rp <= wp);
  assert (wp <= b.buffer##.length);
  b.buffer##.wp := wp


let getc (b:t): char option =
  let rp = b.buffer##.rp in
  if rp < b.buffer##.wp then
    (let c = b.view##getUint8 rp in
     b.buffer##.rp := rp + 1;
     Some c)
  else
    None



let putc (b:t) (c:char): unit option =
  let wp = b.buffer##.wp in
  if wp < b.buffer##.length then
    (b.view##setUint8 wp c;
     b.buffer##.wp := wp + 1;
     Some ())
  else
    None


let copy (src:t) (s0:int) (s1:int) (dst:t) (d0:int): unit =
  assert (s0 <= s1);
  assert (d0 + s1 - s0 <= capacity dst);
  src.buffer##copy dst.buffer d0 s0 s1


let alloc (size:int): t =
  let buffer  = (Js.Unsafe.global##.Buffer)##allocUnsafe size in
  buffer##.rp := 0;
  buffer##.wp := 0;
  let view = (* Workaround: I don't know how to call [buffer[i]] from
                ocaml. Therefore I define a view for the buffer and manipulate
                the data in the buffer via the view. *)
    let constr = Js.Unsafe.global##.DataView in
    new%js constr buffer##.buffer
  in
  {buffer; view}



module Read (W:WRITABLE) =
  struct
    let read (b:t) (w:W.t): W.t =
      let rec next w =
        if W.needs_more w then
          match getc b with
          | None ->
             w
          | Some c ->
             next @@ W.putc w c
        else
          w
      in
     next w
  end

module Write (R:READABLE) =
  struct
    let write (b:t) (r:R.t): R.t =
      let r = ref r in
      while R.has_more !r && not (is_full b) do
        let o = putc b (R.peek !r) in
        assert (o <> None);
        r := R.advance !r
      done;
      !r
  end

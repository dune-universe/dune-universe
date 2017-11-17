
type bitarray = bytes ref

type t = bitarray

let create n = ref (Bytes.make ((n+7) / 8) '\000')

let get_offset_and_mask n = 
  let offset = n lsr 3
  and mask_offset = n mod 8 in
  let mask = 1 lsl mask_offset in
  (offset, mask)

let get sr i =
  if i >= 8 * Bytes.length !sr then false
  else
    let (offset, mask) = get_offset_and_mask i in
    0 <> Char.code (Bytes.get !sr offset) land mask

let set sr i value =
  let len = Bytes.length !sr in
  if i >= 8 * len then
    begin
      let newstring = Bytes.make (max (1 + i/8) (2 * len)) '\000' in
(*      Printf.printf "blit : %d 0 %d 0 %d\n%!" (String.length !sr) (String.length newstring) len ; *)
      Bytes.blit !sr 0 newstring 0 len ;
      sr := newstring ;
    end ;

  let (offset, mask) = get_offset_and_mask i in
  
  let cell = Char.code (Bytes.get !sr offset) in
  
  let newcell =
    if value then cell lor mask
    else cell land (mask lxor 0xFF)
  in

  Bytes.set !sr offset (Char.chr newcell)

(* Fold over bits of a byte *)
let rec fold_bits pos acu f byte =
  if byte = 0 then acu
  else
    let acu' = if byte land 1 = 1 then f pos acu else acu in
    fold_bits (pos + 1) acu' f (byte lsr 1)

(* Fold over bits of a string *)
let rec fold_aux s acu f len pos =
  if pos >= len then acu
  else
    let acu' = fold_bits (pos lsl 3) acu f (Char.code (Bytes.get s pos)) in
    fold_aux s acu' f len (pos + 1)
  
let fold sr acu f = fold_aux !sr acu f (Bytes.length !sr) 0

let iter sr f = fold sr () (fun n () -> f n)

let count sr = fold sr 0 (fun _ c -> c + 1)

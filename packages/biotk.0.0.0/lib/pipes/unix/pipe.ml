include Biotk_pipes.Pipe.Make(struct
    type 'a t = 'a
    let return x = x
    let bind x f = f x
  end)

open Monad_infix

let input_string len =
  let buf = Bytes.make len '\000' in
  fun ic ->
    let n = input ic buf 0 len in
    if n = 0 then None
    else Some (Bytes.sub_string buf 0 n)

let from_file ?(buffer_size = 64 * 1024) fn =
  bracket
    (fun () -> open_in fn)
    close_in
    (fun ic ->
       let rec loop () =
         match input_string buffer_size ic with
         | Some i -> yield i >>= loop
         | None -> Done ()
       in
       loop ())

let to_file fn =
  bracket
    (fun () -> open_out fn)
    close_out
    (fun oc ->
       let rec loop () =
         await () >>= function
         | None -> Done ()
         | Some v ->
           output_string oc v ;
           loop ()
       in
       loop ())

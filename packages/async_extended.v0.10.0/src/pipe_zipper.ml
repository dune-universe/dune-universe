open Core
open Async

type 'a t =
  { mutable pipe : 'a Pipe.Reader.t option
  ; left  : 'a Doubly_linked.t
  ; right : 'a Doubly_linked.t
  ; create_pipe : (unit -> 'a Pipe.Reader.t Deferred.t)
  ; max_history : int
  }

let push ~max_history stack a =
  ignore (Doubly_linked.insert_first stack a);
  if Doubly_linked.length stack > max_history then
    ignore (Doubly_linked.remove_last stack : 'a option)

let pop = Doubly_linked.remove_first

let create ?(max_history=100) create_pipe =
  { pipe = None
  ; left = Doubly_linked.create ()
  ; right = Doubly_linked.create ()
  ; create_pipe
  ; max_history
  }

let current t = Doubly_linked.first t.right

let next t =
  match pop t.right with
  | Some a ->
    push ~max_history:t.max_history t.left a;
    return (Ok a)
  | None ->
    begin match t.pipe with
    | None -> t.create_pipe () >>| fun pipe -> t.pipe <- Some pipe; pipe
    | Some pipe -> return pipe
    end
    >>= fun pipe ->
    Monitor.try_with (fun () -> Pipe.read pipe) >>| function
    | Error exn -> Or_error.of_exn (Monitor.extract_exn exn)
    | Ok `Eof -> Or_error.error_string "end of file"
    | Ok (`Ok a) ->
      push ~max_history:t.max_history t.left a;
      Ok a

let prev t =
  match pop t.left with
  | Some a ->
    push ~max_history:t.max_history t.right a;
    return (Ok a)
  | None ->
    return (Or_error.error_string "pipe_zipper: no more history available")

let zip t dir n =
  let next = match dir with
    | `next -> next
    | `prev -> prev
  in
  next t >>= function
  | Error _ as error -> return error
  | Ok a ->
    let rec loop a n =
      if n <= 0 then return (Ok a)
      else
        next t >>= function
        | Error _ -> return (Ok a)
        | Ok a -> loop a (n - 1)
    in
    loop a (n - 1)

let reset t =
  Doubly_linked.clear t.left;
  Doubly_linked.clear t.right;
  Option.iter t.pipe ~f:Pipe.close_read;
  t.create_pipe () >>| fun pipe -> t.pipe <- Some pipe

let rec find t ~f =
  next t >>= function
  | Error _ as error -> return error
  | Ok a -> if f a then return (Ok a) else find t ~f

let rec find_rev t ~f =
  prev t >>= function
  | Error _ as error -> return error
  | Ok a -> if f a then return (Ok a) else find_rev t ~f

let find_first_larger_or_equal t ~compare_with_target =
  begin match current t with
  | None -> next t
  | Some a -> return (Ok a)
  end
  >>= function
  | Error _ as error -> return error
  | Ok a ->
    match compare_with_target a |> Ordering.of_int with
    | Equal -> return (Ok a)
    | Less -> find t ~f:(fun a -> compare_with_target a >= 0)
    | Greater ->
      find_rev t ~f:(fun a -> compare_with_target a <= 0) >>= function
      | Ok _ as ok -> return ok
      | Error _ ->
        reset t >>= fun () -> find t ~f:(fun a -> compare_with_target a >= 0)

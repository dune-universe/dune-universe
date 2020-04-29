module Queue = struct
  include Queue

  let peek_opt queue =
    if Queue.is_empty queue then None else Some (Queue.peek queue)
end

type ('a, 'b) t = {
  in_stream_push: 'a option -> unit;
  out_stream: 'b Lwt_stream.t;
  queue: unit Lwt_condition.t Queue.t;
  queue_lock: Lwt_mutex.t;
  wakeup_lock: Lwt_mutex.t;
}

let create in_stream_push out_stream =
  let queue = Queue.create () in
  let queue_lock = Lwt_mutex.create () in
  let wakeup_lock = Lwt_mutex.create () in
  { in_stream_push; out_stream; queue; queue_lock; wakeup_lock }

let process { in_stream_push; out_stream; queue; queue_lock; wakeup_lock } value
    =
  (* High-level description of algorithm:
      * Submit data to be processed to input stream
      * Join queue of threads waiting for outputs
      * If there is a thread ahead of us:
          * Sleep and wait for it to wake us up when it is our turn
      * Get response from output stream
      * Remove us from queue
      * If there is a thread behind us:
          * Wake it up
      * Return response

   * Notes:
      * [queue_lock] is taken before performing operations that rely on or change the length of the queue, to stop various obvious race conditions
      * [wakeup_lock] is used to stop the following race:
          * Thread 1 adds its condition variable to the queue, behind thread 2
          * Thread 2 pops itself from the queue and signals thread 1
          * Thread 1 waits on the condition variable,
          * but the signal has already happened so it will never wake
   *)
  (* Submit to input stream and add to queue *)
  let%lwt () = Lwt_mutex.lock queue_lock in
  let () = in_stream_push (Some value) in
  let ready_to_await_response = Lwt_condition.create () in
  let first_in_queue = 0 = Queue.length queue in
  let%lwt () =
    if not first_in_queue then Lwt_mutex.lock wakeup_lock else () |> Lwt.return
  in
  let () = Queue.push ready_to_await_response queue in
  let () = Lwt_mutex.unlock queue_lock in

  (* Wait for threads in front to finish *)
  let%lwt () =
    if not first_in_queue then
      let%lwt () =
        Lwt_condition.wait ~mutex:wakeup_lock ready_to_await_response
      in
      Lwt_mutex.unlock wakeup_lock |> Lwt.return
    else () |> Lwt.return
  in
  (* Wait for data from output stream *)
  let%lwt output = Lwt_stream.next out_stream in

  (* Remove from queue and wait next thread *)
  let%lwt () = Lwt_mutex.lock queue_lock in
  let _output_ready = Queue.pop queue in
  let () = Lwt_mutex.unlock queue_lock in
  let%lwt () =
    match Queue.peek_opt queue with
    | Some next_output_ready ->
        let%lwt () = Lwt_mutex.lock wakeup_lock in
        Lwt_condition.signal next_output_ready ();
        Lwt_mutex.unlock wakeup_lock |> Lwt.return
    | None -> () |> Lwt.return
    (* No more inputs waiting *)
  in
  output |> Lwt.return

let close { in_stream_push; _ } = in_stream_push None

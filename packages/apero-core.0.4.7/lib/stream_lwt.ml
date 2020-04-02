module Stream = struct   
  
  type 'a q = 
    { stream : 'a Lwt_stream.t
    ; bounded_push : 'a Lwt_stream.bounded_push 
    ; pusher: 'a -> unit Lwt.t }
  
  let create len = 
    let (stream, p) = Lwt_stream.create_bounded len in 
    { stream
    ; bounded_push = p
    ; pusher = fun e -> p#push e } 

  let push e s = s.pusher e 

  let get s = Lwt_stream.get s.stream  

  let close  s = s.bounded_push#close    

  let count s = s.bounded_push#count

  let blocked s = s.bounded_push#blocked

  let closed s = s.bounded_push#closed
end


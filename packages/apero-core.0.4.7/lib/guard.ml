type 'a t = {mutable self: 'a;  mutex: Lwt_mutex.t}

let create v = 
    { self = v
    ; mutex = Lwt_mutex.create () }

let get g = g.self

let acquire g = 
    let open Lwt.Infix in 
    Lwt_mutex.lock g.mutex 
    >>= fun () -> Lwt.return g.self

let release g v = 
    g.self <- v;
    Lwt_mutex.unlock g.mutex
    
let set v g = 
    let open Lwt.Infix in 
    Lwt_mutex.lock g.mutex 
    >>= fun () -> Lwt.return (g.self <- v) 
    >>= fun () -> Lwt.return @@ Lwt_mutex.unlock g.mutex

let guarded g f = 
    let open Lwt.Infix in 
    Lwt_mutex.lock g.mutex >>=
    fun () -> 
        Lwt.try_bind 
            (fun () -> f g.self)
            (fun (s, r) -> 
                g.self <- s;
                Lwt_mutex.unlock g.mutex
                ; r)
            (fun e -> Lwt_mutex.unlock g.mutex ; Lwt.fail e)        
    
let return v s = Lwt.return (s, Lwt.return v)

let return_lwt v s = Lwt.return (s, v)
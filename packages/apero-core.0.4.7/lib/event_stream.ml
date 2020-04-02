module EventStream = struct 
  
  module type S = sig 
    type 'a t          
    
    module Sink : sig
      type 'a s             
      val of_stream : 'a t -> 'a s
      val push : 'a -> 'a s -> unit Lwt.t    
      val count : 'a s -> int 
      val close : 'a s -> unit
      val closed : 'a s -> bool
      val blocked : 'a s -> bool 
    end

    module Source : sig             
      type 'a s 
      val of_stream : 'a t -> 'a s
      val get : 'a s -> 'a option Lwt.t      
      val count : 'a s -> int 
      val closed : 'a s -> bool
      val blocked : 'a s -> bool      
    end

    val create : int -> 'a Source.s * 'a Sink.s 
  end  
  

   module Make (I : sig 
                    type 'a q 
                    val create : int -> 'a q
                    val push : 'a -> 'a q -> unit Lwt.t  
                    val get : 'a q -> 'a option Lwt.t
                    val count : 'a q -> int 
                    val close : 'a q -> unit                    
                    val closed : 'a q -> bool
                    val blocked : 'a q -> bool       
                  end ) : S with type 'a t = 'a I.q = struct 

      type 'a t = 'a I.q
             
    module Sink = struct
      type 'a s  = 'a I.q
      
      let of_stream s = s
      let push = I.push 
      let count = I.count
      let close = I.close
      let closed = I.closed
      let blocked = I.blocked
    end

    module  Source = struct
      type 'a s = 'a I.q 
      
      let of_stream s = s
      let get = I.get 
      let count = I.count      
      let closed = I.closed
      let blocked = I.blocked
    end  

    let create len = 
      let s = I.create len in 
      (Source.of_stream s, Sink.of_stream s)           
    end
end

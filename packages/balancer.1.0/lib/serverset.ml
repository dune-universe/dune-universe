
open Lwt.Infix
open Util


module LoadedNode = struct
  type t = {node: Node.t; load: Counter64.t}
             
  let node t = t.node
  let load t = Counter64.get t.load

  let of_node node =
    let load = Counter64.zero () in
    {node; load}

  let make node i =
    let load = Counter64.create i in
    {node; load}

  let compare l r = Node.compare l.node r.node

                                 
                             
end



module NodeSet = Set.Make(Node) 
module LoadedSet = Set.Make(LoadedNode)

(** Maintains information to perform balancing decisions and membership *)
module type S = sig
  type elt 
  type t 

  (** A function to maintain server set retrieved from an external source*)
  val update: t -> NodeSet.t -> elt Lwt.t 

  (** Use a reference cell to maintain the ServerSet using update when an event is triggered*)
  val from_src: t -> NodeSet.t React.S.t -> t
  val of_nodes: Node.t list -> t




  val add_node: t -> Node.t -> elt Lwt.t 
  val rm_node: t -> Node.t -> elt Lwt.t

  val nodes: t -> Node.t list Lwt.t
                       
                                 
end

                  
                           


module LoadedNodes  = struct
  open LoadedNode
  type elt = LoadedSet.t 
      
  type t = LoadedSet.t SyncVar.t

  let of_list x = LoadedSet.of_list x |> SyncVar.create
                              
  let of_nodes x =
    List.map (fun n -> LoadedNode.of_node n) x |> LoadedSet.of_list |> SyncVar.create

  let to_nodes ls =
    let elements = LoadedSet.elements ls in
    List.fold_left (fun acc x -> LoadedSet.add x acc) LoadedSet.empty elements 




  let update t nodes =
    let nodes0 =
      NodeSet.elements nodes |> List.map (fun n -> LoadedNode.of_node n)  |> LoadedSet.of_list
    in
    
    let f s =                   
      let e = to_nodes s in
      
      let rm_set = LoadedSet.diff s nodes0 in
      let add_set = LoadedSet.diff nodes0 s in
      

      let v = LoadedSet.filter (fun x -> (LoadedSet.mem x rm_set) <> true) s in
      LoadedSet.union v add_set
    in
    
    SyncVar.update t f 

           
  let from_src t src =
    let e = React.S.changes src in
    React.E.map (fun nodes -> update t nodes ) e;
    t

      
 
  let add_node t node =
    let ln = LoadedNode.of_node node in
    SyncVar.update t (LoadedSet.add ln)

  let rm_node t node =
    let ln = LoadedNode.of_node node in
    SyncVar.update t (LoadedSet.remove ln)

  let nodes t =
    SyncVar.read t >|= fun e -> LoadedSet.elements e |> List.map (fun x -> x.node)

end


                       

module Nodes = struct
  type elt = NodeSet.t
  type t = NodeSet.t SyncVar.t

  let of_nodes l = NodeSet.of_list l |> SyncVar.create 

  let update t x =
    SyncVar.become t x >|= fun () -> x

  let from_src t src =
    let e = React.S.changes src in
    React.E.map (fun nodes -> update t nodes ) e;
    t



  let add_node t node =
    SyncVar.update t (NodeSet.add node)

  let rm_node t node =
    SyncVar.update t (NodeSet.remove node)

  let nodes t =
    SyncVar.read t >|= fun e -> NodeSet.elements e
    
end





                 

module RRQueue = struct

  type elt = {queue: Node.t Queue.t; mutable nodes: NodeSet.t}
  type t = elt SyncVar.t

  open SyncVar
         


  (*
  let get_nodes t = read t |> (fun x -> x.nodes)
  let get_queue t = read t |> (fun x -> x.queue) 
              
  *)              
  let of_nodes hosts =

    let nodes = NodeSet.of_list hosts in 
    let queue = Queue.create () in
    
    List.iter (fun x -> Queue.add x queue) hosts;
    SyncVar.create {queue; nodes}

                   
  let take t =
    SyncVar.sync t (fun () ->
        let q = t.value.queue in 
        Queue.take q |> Lwt.return
      )
    

  let add t n =
    SyncVar.sync t (fun () ->

        let (nodes, q) =
          let s = SyncVar.value t in
          s.nodes, s.queue
        in
        
        
        if (NodeSet.mem n nodes) then 
          Queue.add n q |> Lwt.return

        else
          Lwt.return_unit 
      )

                 
  let update t x =

    sync t (
        fun () ->
        t.value.nodes <- x;
        Lwt.return t.value
      )

         
  let from_src t src =
    let e = React.S.changes src in    
    React.E.map (update t) e;
    t
                

  let add_node t node =
    let f () = sync t (fun () ->
        let v = t.value in
        v.nodes <- NodeSet.add node t.value.nodes;
        Queue.add node v.queue; 
        Lwt.return v                      
    ) in

    if (NodeSet.mem node t.value.nodes <> true) then f ()
    else Lwt.return t.value
                    


  let rm_node t node =
    sync t (fun () ->
        let v = t.value in
        v.nodes <- NodeSet.remove node t.value.nodes;
        Lwt.return v                      
    )


  let nodes t  =
    SyncVar.read t >|= fun e ->
    NodeSet.elements e.nodes
end

                  
            



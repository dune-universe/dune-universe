# Balancer

A collection of load balancing algorithms implemented in pure Ocaml

The supported Algorithms are Power of two choices, P2C with Partial Key Grouping (Kafka uses this) , Round Robin, Consistent Hashing, and Consistent Hashing + Least loaded.


Each loadbalancer supports state updates via react (making it easier to work with service discovery utilities), along with manually removing and deleting nodes.

Also included are utilities for thread safe shared state under Balancer.Util.



Here are some examples on how to perform common tasks. 

To dispatch work

```ocaml
module P2C = Balancers.P2C

let make_node port =
  let host = "localhost" in 
  Node.make ~host ~port ()

in

let rec range i j = if i > j then [] else i :: (range (i+1) j)
let nodes = range 3000 3005 |> List.map (fun x -> make_node x) in 
let t = P2C.of_nodes nodes in

P2C.use t (fun x -> Node.to_string x |> print_endline |> Lwt.return) 

```




To manage it's state
```ocaml


let make_node_set b e =
  range b e |> List.map (fun x -> make_node x) |> Balancer.Serverset.NodeSet.of_list

let node = make_node 9000 in
P2C.add_node t node >>= fun _ ->
P2C.rm_node t node >>= fun _ ->

let new_endpts = make_node_set 2000 2006 in
(* update removes all entries not found in new_endpts since it's to be used to resolve hosts from an external source *)
P2C.update t new_endpts >>= fun _ ->


let (src, v) = React.S.create () in
let endpts_n = make_node_set 8000 8003 in
v endpts_n;

P2C.from_src t src 

```
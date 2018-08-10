open Balancer
open Lwt.Infix
       

open Serverset
open Util
       
module CH = Chash
              
let rec range i j = if i > j then [] else i :: (range (i+1) j)
                                                 
let string_m =
  let open Alcotest in list string 


let make_node port =
  let host = "localhost" in 
  Node.make ~host ~port ()

let make_loaded port i =
  make_node port |> (fun n -> LoadedNode.make n i)



let node =
  let pp = Fmt.of_to_string Node.to_string in
  let eq l r = (Node.compare l r) = 0 in 
  Alcotest.testable pp eq


let loaded_node =
  let pp = Fmt.of_to_string (fun ln -> LoadedNode.node ln |> Node.to_string ) in
  let eq l r = LoadedNode.compare l r = 0 in
  Alcotest.testable pp eq

let node_list x =
  let open Alcotest in list node

                            
let free () = Lwt.return_unit
                
let use_switch switch =
  Lwt_switch.add_hook (Some switch) free




                               
let random_host_port () =

  let host = "localhost" in
  let (min, max) = 1024, 65535 in 
  let port = min + ( Random.int (max - min) ) in

  host, port 


          
let random_node () =
  let (host, port) = random_host_port () in
  Node.make ~host ~port () 


            
                               
let make_nodes n =
  let rec aux i l =
    if (i > 0) then

      let n = random_node () in
      aux (i - 1) (l @ [n])

    else
      l

  in
  aux n []
        
                

                      
let test_p2c switch () =
  use_switch switch;
  
  let (v1, v2) = (make_loaded 4000 0L), (make_loaded 4001 1L) in
  
  let srv =  LoadedNodes.of_list [v1; v2] in 
  Distributor.P2C.pick srv () >|= fun got -> 
  (Alcotest.check loaded_node) "node is uneqal" v1 got 










                               
let test_chash () =
  let srvs = ["a"; "b"; "c"; "d"; "e" ] in
  let got = CH.slice srvs 3 3 in
 
  (Alcotest.check string_m) "test slice" ["d"; "e"; "a"] got


      
let test_rr switch () =
  use_switch switch;

  let peers = make_nodes 5 in
  let expected = List.hd peers in

  let ss = RRQueue.of_nodes peers in 
  RoundRobin.pick ss () >>= fun got -> 
  Alcotest.check node "Nodes didn't match" expected got;

  let expect_n = List.nth peers 1 in

  RoundRobin.pick ss () >|= fun got -> 
  
  Alcotest.check node "Doesn't exibit FIFO properties" expect_n got



                

let test_counter s () =
  let ctr = Util.Counter64.zero () in
  let l = [1; 2; 3; 4;] in
  
  let expected = 4L in
  
  Lwt_list.iter_p (fun _ ->
      Util.Counter64.incr ctr;
      Lwt.return_unit
    ) l >>= fun () ->

  let got = Counter64.get ctr in

  Alcotest.check
    Alcotest.int64
    "counter isn't equal to expected result"
    expected
    got
  |> Lwt.return





let timeout delay t =
  let tmout =
    Lwt_unix.sleep delay >|= fun () ->
    print_endline "triggered"
  in

  Lwt.pick [
      (tmout >|= fun () -> true);
      (t >|= fun v -> false);
    ]


           
let test_syncvar s () =
  let open SyncVar in

  let var = SyncVar.create "homie" in 


  let s () =
    sync var (fun () -> print_endline "hoe" |> Lwt.return)
  in

  let v () = read var >>= fun _ -> Lwt.return_unit in
    
  
  
  let tasks = [s; s; s; s; v;] in

  Lwt_list.iter_p (fun f -> f () ) tasks >>= fun _ -> 

  
  update var (fun x -> "wass good " ^ x) >>= fun e ->
  read var >>= fun g ->

  Alcotest.check Alcotest.string "update wasn't seen" e g;

  let exp = "hello friend" in 
  become var exp >>= fun () ->
  read var >|= fun got1 -> 
  
  Alcotest.check Alcotest.string "State change wasn't seen" exp got1           
           
open Serverset
open Alcotest
       
       
let test_serverset (type a) (module SS: Serverset.S with type elt = a) s () =
  let t =
    range 3000 3005 |> List.map (fun x -> make_node x) |> SS.of_nodes
  in


  let get_set () =
    Lwt_main.run ( SS.nodes t >|= fun x ->  ( NodeSet.of_list x) )
  in
  
  SS.add_node t (make_node 3006) >>= fun _ ->
  let got = NodeSet.mem (make_node 3006) ( get_set () ) in

  Alcotest.check bool "node is in set" true got;

  let n = make_node 3006 in
  SS.rm_node t n >>= fun _ ->

  let got1 = NodeSet.mem n (get_set ()) in
  Alcotest.check bool "node is not in set" false got1;

  let s1 =
    range 3007 3010 |> List.map (fun x -> make_node x) |> NodeSet.of_list
  in

  SS.update t s1 >|= fun _ ->
  
  let got2 =
    let s = get_set () in
    NodeSet.equal s1 s
  in

  Alcotest.check bool "sets are equal" true got2
  
  
  
  
  

  
let distributors = [
    "Chash", `Quick, test_chash;
    Alcotest_lwt.test_case "P2C" `Quick test_p2c;
    Alcotest_lwt.test_case "Round Robin" `Quick test_rr; 
  ]




                     
let utils = [
    Alcotest_lwt.test_case "Counter Test" `Quick test_counter;
    Alcotest_lwt.test_case "SyncVar Test" `Quick test_syncvar
  ]



let serversets =

  [
    Alcotest_lwt.test_case "RRQueue" `Quick (test_serverset (module RRQueue) );

    Alcotest_lwt.test_case "LoadedNodes" `Quick (test_serverset (module LoadedNodes) );

    Alcotest_lwt.test_case "Nodes" `Quick (test_serverset (module Nodes) )
  ]
                

let () =


  Alcotest.run "Testing Balancer" [
      "Distributor Suite", distributors;
      "Synchronization Util Suite", utils;
      "Server Sets Suite", serversets
  ]

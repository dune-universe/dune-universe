open UtilsLib.SharedForest.SharedForest
       
let init_f_list l = Forest ([],l)
			   
let tree0=
  Node ("tree0.(0,1)",
	[
		init_f_list 
		  [
		    Node ("tree0.(0,1).(1,1)",[]);
		    Node ("tree0.(0,1).(1,2)",[]);
		    Node ("tree0.(0,1).(1,3)",[]);
		  ];
		init_f_list 
		  [
		    Node ("tree0.(0,1).(2,1)",[]);
		    Node ("tree0.(0,1).(2,2)",[]);
		    Node ("tree0.(0,1).(2,3)",[]);
		  ];
	]
       )
       
       
let tree = 
  Node ("(0,1)",
	[
	  init_f_list 
	    [
	      Node ("(0,1).(1,1)",[]);
	      Node ("(0,1).(1,2)",[]);
	      Node ("(0,1).(1,3)",[]);
	    ];
	  init_f_list
	    [
	      Node ("(0,1).(2,1)",
		    [
		      init_f_list
			[
			  Node ("(0,1).(2,1),(1,1)",[]);
			  Node ("(0,1).(2,1),(1,2)",[]);
			];
		      init_f_list
			[
			  Node ("(0,1).(2,1),(2,1)",[]);
			];
		      Link_to (2,[(1,4)]); 
		   (*				  Link_to (2,[(1,2)]);  *)
		   ]);
	      Node ("(0,1).(2,2)",[]);
	    ];
	  init_f_list
	    [
	      Node ("(0,1).(3,1)",[]);
	      Node ("(0,1).(3,2)",[]);
	      Node ("(0,1).(3,3)",[]);
	      Node ("(0,1).(3,4)",[]);
	    ];
	  init_f_list
	    [
	      Node ("(0,1).(4,1)",[]);
	      tree0;
	    ];
	]
       )
       
       
let tree1 =
  [
    
    Node ("2",
	  [
	    Link_to (1,[]);
	 ]);
    
    
    Node ("1",
	  [
	    init_f_list
	      [
		Node ("2.1",[]);
	      ];
	 ]);
    
  ]
    
let rec print_tree prefix tree =
  match tree with
  | SimpleTree (v,[]) ->
     Logs.app (fun m -> m "%s -- %s\n" prefix v)
  | SimpleTree (v,a::children) ->
     let () = print_tree (Printf.sprintf "%s -- %s" prefix v) a in
     List.iter
       (fun child -> print_tree (String.make (4+String.length prefix + String.length v) ' ') child)
       children
       
       
(*
let trees= build_trees [tree0] in
let buff=Buffer.create 80 in
let () = Printf.bprintf buff "Found %d trees:\n" (List.length trees) in
let () = 
  List.iter
    (fun t ->
      let () = print_tree "" buff t in
      Printf.bprintf buff "\n\n")
    trees in
Printf.printf "%s" (Buffer.contents buff)
 *)
let output_tree t = 
  let buff=Buffer.create 80 in
  let () = print_tree "" t in
  Printf.printf "%s" (Buffer.contents buff)
		
		
type inputs =
  | Stop
  | Next
  | All
      
let return_input s =
  match String.lowercase_ascii (String.trim s) with
  | "y" | "yes"-> Some Next
  | "n" | "no" -> Some Stop
  | "a" | "all" -> Some All
  | "" -> Some Next
  | _ -> None
	   
	   
let interact_aux get_input =
  get_input (read_line ())
	    
	    
let rec interact message get_input =
  let () = Printf.printf "%s %!" message in
  match interact_aux get_input with
  | Some v -> v
  | None -> interact message get_input
		     
let rec ask_for_next_parse ?(interactive = true) f param =
  let rec all_results l_par =
    match f l_par with
    | None -> Printf.printf "No other returned value\n"
    | Some new_par -> all_results new_par in
  match interactive with
  | true -> 
     let msg = Printf.sprintf "Do you want to look for another solution?\n\ty/yes\n\tn/no\n\ta/all\n(Default: yes):" in
     (match interact msg return_input with
      | Next -> 
         let () = Printf.printf "Going to get a term\n%!" in
         (match f param with
          | None -> Printf.printf "No other returned value\n"
          | Some new_param -> ask_for_next_parse f new_param)
      | All -> all_results param
      | Stop -> ())
  | false when (snd param)<=3 -> 
     let () = Printf.printf "Going to get a term\n%!" in
     (match f param with
      | None -> Printf.printf "No other returned value\n"
      | Some new_param -> ask_for_next_parse ~interactive f new_param)
  | false -> ()

let () =
  let () = UtilsLib.Log.set_level Logs.Warning in
  let resume = init tree1 in
  let () = Logs.app (fun m -> m  "**********************************") in
  ask_for_next_parse ~interactive:false
    (fun (res,i) -> 
      match resumption res with
      | None,_ -> None
      | Some t,resume -> 
	 let () = Logs.app (fun m -> m  "Got result %i" i) in
	 let () = output_tree t in
	 Some (resume,i+1))
    (resume,1)
    
    

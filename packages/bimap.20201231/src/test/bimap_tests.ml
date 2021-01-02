(*===Tests 1 - 5 target the bimap implemented as a class with single values per key.
     Tests 6 - 10 target the bimap implemented as a class with multiple values per key.
     Tests 11 -15 target the bimap implemented as a module with single values per key.
     Tests 16  20 target the bimap implemented as a module with multiple values per key.
*)
module Bimap=Bimap
open OUnit2
module Bimap_tests = struct

  let oc = Core.Out_channel.stdout;;    
  let print_n_flush s =
    Core.Out_channel.output_string oc s;
    Core.Out_channel.flush oc;;
  
  let rec print_n_flush_alist ~sep ~to_string_func l =
    match l with
    | [] -> ()
    | h :: t ->
       let s = to_string_func h in 
       let () = print_n_flush (s ^ sep) in
       print_n_flush_alist ~sep ~to_string_func t;;

  let rec print_n_flush_alistlist ~sep ~to_string_func l =
    match l with
    | [] -> ()
    | h :: t ->
       let () = print_n_flush " <|> " in 
       let () = print_n_flush_alist ~sep ~to_string_func h in
       print_n_flush_alistlist ~sep ~to_string_func t;;
(*
  let rec list_list_mem l v =
    match l with
    | [] -> false
    | h::t ->
       if Core.List.mem ~equal:(fun x y -> Core.String.equal x y) h v then true else
	 list_list_mem t v

  let rec key_list_list_mem l v =
    match l with
    | [] -> false
    | h::t ->
       if Core.List.mem ~equal:(fun x y -> Core.String.equal x y) h v then true else
	 list_list_mem t v
 *)    
  let test1 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module Bimap_single_class = Bimap.Bimap_single_class(Core.Int)(Core.String) in
    let bim = new Bimap_single_class.bimap_single_class int_map string_map in
    begin
      bim#set ~key:1 ~data:"one";
      bim#set ~key:2 ~data:"two";
      assert_equal 2 (bim#length);
      assert_equal false (bim#is_empty);
      assert_equal "one" (bim#find_exn ~key:1);
      assert_equal "two" (bim#find_exn ~key:2);
      assert_equal 1 (bim#find_exn_reverse ~key:"one");
      assert_equal 2 (bim#find_exn_reverse ~key:"two");

      bim#set_reverse ~key:"three" ~data:3;

      assert_equal "three" (bim#find_exn ~key:3);
      assert_equal 3 (bim#find_exn_reverse ~key:"three");
      assert_equal [(1,"one");(2,"two");(3,"three")] (bim#to_alist ());
      assert_equal [(3,"three");(2,"two");(1,"one")] (bim#to_alist ~key_order:`Decreasing ());
      
      bim#change ~key:3 ~f:(fun _x -> (Some "tri"));

      assert_equal "tri" (bim#find_exn ~key:3);
      assert_equal 3 (bim#find_exn_reverse ~key:"tri");      
      bim#change_reverse ~key:"tri" ~f:(fun _x -> (Some 4));
      assert_equal 4 (bim#find_exn_reverse ~key:"tri");
      assert_equal "tri" (bim#find_exn ~key:4);
      bim#remove ~key:1;
      assert_equal 2 (bim#length);
      bim#remove_reverse ~key:"two";
      assert_equal 1 (bim#length);
      bim#remove ~key:4;
      assert_equal 0 (bim#length);     
    end

  let test2 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module Bimap_single_class = Bimap.Bimap_single_class(Core.Int)(Core.String) in
    let bim = new Bimap_single_class.bimap_single_class int_map string_map in 
    begin
      bim#set ~key:1 ~data:"one";
      bim#set ~key:2 ~data:"two";
      assert_equal 2 (bim#length);
      assert_equal false (bim#is_empty);
      assert_equal (Some "one") (bim#find ~key:1);
      assert_equal (Some "two") (bim#find ~key:2);
      assert_equal (Some 1) (bim#find_reverse ~key:"one");
      assert_equal (Some 2) (bim#find_reverse ~key:"two");
      assert_equal None (bim#find ~key:3);
      assert_equal None (bim#find_reverse ~key:"three");
      bim#set_reverse ~key:"three" ~data:3;
      assert_equal (Some "three") (bim#find ~key:3);
      assert_equal (Some 3) (bim#find_reverse ~key:"three");

      bim#change ~key:3 ~f:(fun _x -> (Some "tri"));

      assert_equal 3 (bim#find_exn_reverse ~key:"tri");

      bim#change_reverse ~key:"tri" ~f:(fun _x -> (Some 4));

      assert_equal (Some 4) (bim#find_reverse ~key:"tri");
      assert_equal (Some "tri") (bim#find ~key:4);
      assert_equal true (bim#mem 1);
      assert_equal true (bim#mem 2);
      assert_equal true (bim#mem_reverse "tri");
      assert_equal (Some (1,"one")) (bim#min_elt);
      assert_equal (Some (4,"tri")) (bim#max_elt);
      assert_equal (1,"one") (bim#min_elt_exn);
      assert_equal (4,"tri") (bim#max_elt_exn);
      assert_equal (Some ("one",1)) (bim#min_elt_reverse);
      assert_equal (Some ("two",2)) (bim#max_elt_reverse);
      assert_equal ("one",1) (bim#min_elt_exn_reverse);
      assert_equal ("two",2) (bim#max_elt_exn_reverse);
      assert_equal (Some (2,"two")) (bim#nth 1);
      assert_equal (Some (4,"tri")) (bim#nth 2);
      assert_equal (Some ("one",1)) (bim#nth_reverse 0);
      assert_equal (Some ("tri",4)) (bim#nth_reverse 1);
      assert_equal (Some ("two",2)) (bim#nth_reverse 2);
      assert_equal None (bim#nth_reverse 3);
    end

  let test3 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module Bimap_single_class = Bimap.Bimap_single_class(Core.Int)(Core.String) in
    let bim = new Bimap_single_class.bimap_single_class int_map string_map in   
    begin
      bim#set ~key:1 ~data:"single";
      bim#set ~key:2 ~data:"double";
      bim#set ~key:3 ~data:"triple";
      assert_equal 3 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#length);
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 2 (bim#counti ~f:(fun ~key ~data:_data -> if key <= 2 then true else false));
      assert_equal ["single";"double";"triple"] (bim#data);
      assert_equal true (List.mem 1 (bim#data_reverse));
      assert_equal true (List.mem 2 (bim#data_reverse));
      assert_equal true (List.mem 3 (bim#data_reverse));
      assert_equal true (bim#exists ~f:(fun x -> x = "double"));
      assert_equal true (bim#exists ~f:(fun x -> x = "triple"));
      assert_equal false (bim#exists ~f:(fun x -> x = "quadruple"));
      assert_equal true (bim#exists_reverse ~f:(fun x -> x = 2));
      assert_equal false (bim#exists_reverse ~f:(fun x -> x = 4));
      assert_equal true (bim#existsi ~f:(fun ~key ~data -> key = 3 && data = "triple"));
      assert_equal true (bim#existsi_reverse ~f:(fun ~key ~data -> key = "double" && data = 2));
      bim#empty ();
      assert_equal 0 (bim#length);
      assert_equal 0 (bim#count ~f:(fun _x -> true));
      assert_equal true (bim#is_empty);

      bim#set ~key:1 ~data:"one";
      bim#set ~key:2 ~data:"two";
      bim#set ~key:3 ~data:"three";
      bim#update ~key:1 ~f:(fun x -> match x with
				  Some s -> Core.String.rev s
				 | None -> "newentry");
      assert_equal "eno" (bim#find_exn ~key:1);
    end 

  let test3b _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module Bimap_single_class = Bimap.Bimap_single_class(Core.Int)(Core.String) in
    let bim = new Bimap_single_class.bimap_single_class int_map string_map in
    begin
      bim#set ~key:4 ~data:"quadruple";
      bim#set ~key:5 ~data:"pentuple";
      bim#set ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#length);
      assert_equal 3 (bim#count ~f:(fun _x -> true));
      bim#filter ~f:(fun v -> String.length v > 8);
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 1 (bim#count_reverse ~f:(fun _x -> true));

      bim#set ~key:5 ~data:"pentuple";
      bim#set ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      bim#filter_reverse ~f:(fun v -> v > 4);
      assert_equal 2 (bim#count ~f:(fun _x -> true));
      assert_equal 2 (bim#count_reverse ~f:(fun _x -> true));
      bim#set ~key:4 ~data:"quadruple";

      assert_equal 3 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      bim#filter_keys ~f:(fun k -> k < 5);
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 1 (bim#count_reverse ~f:(fun _x -> true));

      bim#set ~key:5 ~data:"pentuple";
      bim#set ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      bim#filter_keys_reverse ~f:(fun v -> String.length v > 8);
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 1 (bim#count_reverse ~f:(fun _x -> true));

      bim#set ~key:4 ~data:"quadruple";
      bim#set ~key:5 ~data:"pentuple";
      bim#set ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      bim#filteri ~f:(fun ~key ~data -> String.length data > 8 || key < 6);
      assert_equal 2 (bim#count ~f:(fun _x -> true));
      assert_equal (Some "quadruple") (bim#find ~key:4);
      assert_equal (Some "pentuple") (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      
      bim#set ~key:4 ~data:"quadruple";
      bim#set ~key:5 ~data:"pentuple";
      bim#set ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      bim#filteri_reverse ~f:(fun ~key ~data -> String.length key > 8 || data < 6);

      assert_equal 2 (bim#count ~f:(fun _x -> true));
      assert_equal 2 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal (Some "quadruple") (bim#find ~key:4);
      assert_equal (Some "pentuple") (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      assert_equal (Some 4) (bim#find_reverse ~key:"quadruple");
      assert_equal (Some 5) (bim#find_reverse ~key:"pentuple");
      assert_equal None (bim#find_reverse ~key:"sextuple");

      bim#set ~key:4 ~data:"quadruple";
      bim#set ~key:5 ~data:"pentuple";
      bim#set ~key:6 ~data:"sextuple";
      bim#filter_map ~f:(fun v -> if String.length v > 8 then (Some "survivor") else None);
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 1 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal (Some "survivor") (bim#find ~key:4);
      assert_equal None (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      assert_equal (Some 4) (bim#find_reverse ~key:"survivor");
      assert_equal None (bim#find_reverse ~key:"pentuple");
      assert_equal None (bim#find_reverse ~key:"sextuple");

      bim#set ~key:4 ~data:"quadruple";
      bim#set ~key:5 ~data:"pentuple";
      bim#set ~key:6 ~data:"sextuple";
      bim#filter_map_reverse ~f:(fun v -> if v > 5 then (Some (v * 2)) else None);
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 1 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal (Some 12) (bim#find_reverse ~key:"sextuple");
      assert_equal None (bim#find_reverse ~key:"quadruple");
      assert_equal None (bim#find_reverse ~key:"pentuple");
      assert_equal None (bim#find ~key:4);
      assert_equal None (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      assert_equal (Some "sextuple") (bim#find ~key:12);      
    end

  let test4 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module Bimap_single_class = Bimap.Bimap_single_class(Core.Int)(Core.String) in
    let bim = new Bimap_single_class.bimap_single_class int_map string_map in
    begin
      bim#set ~key:1 ~data:"single";
      bim#set ~key:2 ~data:"double";
      bim#set ~key:3 ~data:"triple";
      let concat = bim#fold ~init:"" ~f:(fun ~key ~data init -> if key < 3 then (init ^ data) else init) in
      assert_equal "singledouble" concat;
      let sum = bim#fold_reverse
		      ~init:0
		      ~f:(fun ~key ~data init ->
			  if key.[0] = 't' || key.[0] = 'd'
			  then (data + init) else init) in
      assert_equal 5 sum;
      let dividend = bim#fold_reverse
		       ~init:1
		       ~f:(fun ~key ~data init ->
			   if key.[0] = 't' || key.[0] = 'd'
			   then (data / init) else init) in
      assert_equal 1 dividend;
      let concat2 =
	bim#fold_right
	      ~init:"" ~f:(fun ~key ~data init ->
			   if key < 3 then (init ^ data) else init) in
      assert_equal "doublesingle" concat2;
      let dividend2 =
	bim#fold_right_reverse
			 ~init:1 ~f:(fun ~key ~data init ->
				     if key.[0] ='t' || key.[0]='d'
				     then (data / init) else init) in
      assert_equal 0 dividend2;
      assert_equal true (bim#for_all ~f:(fun v -> if (String.length v) > 0 then true else false));
    end

  let test5 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module Bimap_single_class = Bimap.Bimap_single_class(Core.Int)(Core.String) in
    let bim = new Bimap_single_class.bimap_single_class int_map string_map in
    begin
      bim#set ~key:1 ~data:"uno";
      bim#set ~key:2 ~data:"dos";
      bim#set ~key:3 ~data:"tres";
      assert_equal true (bim#for_all ~f:(fun v -> (String.length v) > 1));
      assert_equal false (bim#for_all ~f:(fun v -> (String.length v) > 4));
      assert_equal true (bim#for_all_reverse ~f:(fun v -> v < 4));
      assert_equal false (bim#for_all_reverse ~f:(fun v -> v > 3));
      assert_equal [1;2;3] (bim#keys);
      assert_equal ["uno";"dos";"tres"] (bim#data);
      bim#map ~f:(fun v -> Core.String.concat [v;v;]);
      assert_equal ["unouno";"dosdos";"trestres"] (bim#data);
      bim#mapi ~f:(fun ~key ~data -> Core.String.concat [(Core.Int.to_string key);"->";data]);
      assert_equal ["1->unouno";"2->dosdos";"3->trestres"] (bim#data);
      assert_equal "1->unouno" (bim#find_exn ~key:1);
      assert_equal "2->dosdos" (bim#find_exn ~key:2);
      assert_equal 1 (bim#find_exn_reverse ~key:"1->unouno");
      assert_equal 2 (bim#find_exn_reverse ~key:"2->dosdos");

      bim#map_reverse ~f:(fun x -> x*2);
      assert_equal [2;4;6] (bim#keys);
      bim#mapi_reverse ~f:(fun ~key ~data -> if key.[0]='1' then
					       data*1
					     else
					       if key.[0]='2' then
						 data*2
					       else
						 data*3);
      assert_equal [2;8;18] (bim#keys);
    end

  let test6 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module BimapMulti_class = Bimap.Bimap_multi_class(Core.Int)(Core.String) in
    let bim = new BimapMulti_class.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";
      bim#add_multi_reverse ~key:"tri" ~data:3;
      bim#add_multi_reverse ~key:"iii" ~data:3;
      bim#add_multi_reverse ~key:"four" ~data:4;

      assert_equal 4 (bim#count ~f:(fun _l -> true));
      assert_equal 4 (bim#counti ~f:(fun ~key:_key ~data:_data -> true));
      assert_equal 1 (bim#counti ~f:(fun ~key ~data:_data -> key > 3));
      assert_equal 9 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 4 (bim#length);
      assert_equal 1 (Core.List.length (bim#find_exn ~key:1));
      assert_equal 2 (Core.List.length (bim#find_exn ~key:2));
      assert_equal 5 (Core.List.length (bim#find_exn ~key:3));
      assert_equal 1 (Core.List.length (bim#find_exn ~key:4));
      assert_equal true (List.mem "one" (bim#find_exn ~key:1));
      assert_equal true (List.mem "two" (bim#find_exn ~key:2));
      assert_equal true (List.mem "dos" (bim#find_exn ~key:2));
      assert_equal true (List.mem "three" (bim#find_exn ~key:3));
      assert_equal true (List.mem "tres" (bim#find_exn ~key:3));
      assert_equal true (List.mem "triple" (bim#find_exn ~key:3));
      assert_equal true (List.mem "tri" (bim#find_exn ~key:3));
      assert_equal true (List.mem "iii" (bim#find_exn ~key:3));
      
      assert_equal [1] (bim#find_exn_reverse ~key:"one");
      assert_equal [2] (bim#find_exn_reverse ~key:"two");
      assert_equal [2] (bim#find_exn_reverse ~key:"dos");
      assert_equal [3] (bim#find_exn_reverse ~key:"three");
      assert_equal [3] (bim#find_exn_reverse ~key:"tres");
      assert_equal [3] (bim#find_exn_reverse ~key:"triple");
      assert_equal [3] (bim#find_exn_reverse ~key:"tri");
      assert_equal [3] (bim#find_exn_reverse ~key:"iii");

      assert_equal (Some [1]) (bim#find_reverse ~key:"one");
      assert_equal (Some [2]) (bim#find_reverse ~key:"two");
      assert_equal (Some [2]) (bim#find_reverse ~key:"dos");
      assert_equal (Some [3]) (bim#find_reverse ~key:"three");
      assert_equal (Some [3]) (bim#find_reverse ~key:"tres");
      assert_equal (Some [3]) (bim#find_reverse ~key:"triple");
      assert_equal [3] (bim#find_exn_reverse ~key:"tri");
      
      (*print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> x) (bim#data);*)
      assert_equal ([["one"];["dos";"two"];["iii";"tri";"triple";"tres";"three"];["four"]]) (bim#data);
      (*print_n_flush_alist ~sep:" | " ~to_string_func:(fun x -> Core.Int.to_string x) (bim#data_reverse);*)
      assert_equal 1 (Core.List.length (Core.List.filter (bim#data_reverse) ~f:(fun x -> if x = [1] then true else false)));
      assert_equal 2 (Core.List.length (Core.List.filter (bim#data_reverse) ~f:(fun x -> if x = [2] then true else false)));
      assert_equal 5 (Core.List.length (Core.List.filter (bim#data_reverse) ~f:(fun x -> if x = [3] then true else false)));
      assert_equal 1 (Core.List.length (Core.List.filter (bim#data_reverse) ~f:(fun x -> if x = [4] then true else false)));

      bim#empty ();
      assert_equal 0 (bim#count ~f:(fun _l -> true));
      assert_equal 0 (bim#counti ~f:(fun ~key:_key ~data:_data -> true));
      assert_equal 0 (bim#counti ~f:(fun ~key ~data:_data -> key > 3));
      assert_equal 0 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 0 (bim#length);

      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";
      bim#add_multi_reverse ~key:"tri" ~data:3;

      assert_equal true (bim#exists ~f:(fun x -> Core.List.length x = 1 && Core.String.equal (Core.List.nth_exn x 0) "one"));
      assert_equal true (bim#exists_reverse ~f:(fun x -> x = [2]));
      assert_equal true (bim#existsi ~f:(fun ~key ~data -> Core.List.length data = 2 && key = 2 && List.mem "two" data));
      assert_equal false (bim#existsi ~f:(fun ~key ~data -> List.mem "one" data && key = 2));
      assert_equal true (bim#existsi_reverse ~f:(fun ~key ~data -> key = "tri" && data = [3]));
 
      bim#change ~key:3 ~f:(fun _x -> (Some ["iii";"tri";"triple";"tres";"three"]));
      assert_equal (Some [3]) (bim#find_reverse ~key:"three");
      assert_equal (Some [3]) (bim#find_reverse ~key:"tres");
      assert_equal (Some [3]) (bim#find_reverse ~key:"tri");
      assert_equal [3] (bim#find_exn_reverse ~key:"triple");
      assert_equal [3] (bim#find_exn_reverse ~key:"iii");
      assert_equal 5 (Core.List.length (bim#find_exn ~key:3));

      bim#change_reverse ~key:"two" ~f:(fun _x -> (Some [4]));
      (*print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> x) (bim#data);
      print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> Core.Int.to_string x) (bim#data_reverse);*)
      assert_equal 1 (Core.List.length (bim#find_exn ~key:4));
      assert_equal true (List.mem "two" (bim#find_exn ~key:4)); 
      assert_equal false (List.mem "dos" (bim#find_exn ~key:4));
      assert_equal [4] (bim#find_exn_reverse ~key:"two");
      assert_equal [2] (bim#find_exn_reverse ~key:"dos");
      assert_equal ["dos"] (bim#find_exn ~key:2);

      bim#change_reverse ~key:"two" ~f:(fun _x -> (Some [2;4]));
      (*print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> x) (bim#data);
      print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> Core.Int.to_string x) (bim#data_reverse);*)
      assert_equal 1 (Core.List.length (bim#find_exn ~key:4));
      assert_equal true (List.mem "two" (bim#find_exn ~key:4));
      assert_equal true (List.mem "two" (bim#find_exn ~key:2));
      assert_equal false (List.mem "dos" (bim#find_exn ~key:4));
      assert_equal true (List.mem 2 (bim#find_exn_reverse ~key:"two"));
      assert_equal true (List.mem 4 (bim#find_exn_reverse ~key:"two"));
      assert_equal [2] (bim#find_exn_reverse ~key:"dos");
      assert_equal true (List.mem "two" (bim#find_exn ~key:2));
      assert_equal true (List.mem "dos" (bim#find_exn ~key:2));
    end 

  let test7 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module BimapMulti_class = Bimap.Bimap_multi_class(Core.Int)(Core.String) in
    let bim = new BimapMulti_class.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter ~f:(fun v -> (Core.List.length v) < 3) ();
      assert_equal 2 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 2 (bim#length);

      bim#filter_reverse ~f:(fun kl ->
          let max = Core.List.fold ~init:0 ~f:(fun accum v -> if v > accum then v else accum) kl in
          max < 2
        ) ();
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 1 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 1 (bim#length);
    end

  let test8 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module BimapMulti_class = Bimap.Bimap_multi_class(Core.Int)(Core.String) in
    let bim = new BimapMulti_class.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filteri ~f:(fun ~key ~data -> key < 3 && (Core.List.length data) > 1) ();
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 2 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filteri_reverse ~f:(fun ~key ~data ->
          let max = Core.List.fold ~init:0 ~f:(fun accum v -> if v > accum then v else accum) data in
          key.[0] = 't' && max < 3) ();
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 1 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_keys ~f:(fun k -> k > 2) ();
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_keys_reverse ~f:(fun k -> (String.length k) > 3) ();
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_map ~f:(fun vlist -> if Core.List.length vlist < 3 then Some vlist else None) ();
      assert_equal 2 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 2 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_map_reverse
        ~f:(fun invk ->
          let max = Core.List.fold ~init:0 ~f:(fun accum v -> if v > accum then v else accum) invk in
          if max < 3 then
            let newlist = Core.List.map invk ~f:(fun k -> k+1) in 
            Some (newlist)
          else
            None
        ) ();
      assert_equal 2 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 2 (bim#length);
      assert_equal 1 (Core.List.length (bim#find_exn ~key:2));
      assert_equal 2 (Core.List.length (bim#find_exn ~key:3));
      assert_equal true (List.mem "one" (bim#find_exn ~key:2));
      assert_equal true (List.mem "two" (bim#find_exn ~key:3)); 
      assert_equal true (List.mem "dos" (bim#find_exn ~key:3));
      assert_equal [2] (bim#find_exn_reverse ~key:"one");
      assert_equal [3] (bim#find_exn_reverse ~key:"two");
      assert_equal [3] (bim#find_exn_reverse ~key:"dos");
      assert_equal None (bim#find ~key:1);
    end 


  let test9 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module BimapMulti_class = Bimap.Bimap_multi_class(Core.Int)(Core.String) in
    let bim = new BimapMulti_class.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      let total_chars =
	bim#fold ~init:0 ~f:(fun ~key:_key ~data accum ->
			     (Core.List.fold data ~init:accum
					     ~f:(fun accum data -> (String.length data) + accum))) in
      assert_equal 24 total_chars;
      let keys_times_num_values =
	bim#fold_reverse ~init:0 ~f:(fun ~key:_key ~data accum ->
            let subtotal = Core.List.fold data ~init:0 ~f:(fun accum x -> accum + x) in 
	    accum + subtotal) in
      assert_equal 14 keys_times_num_values;
      (*fold over keys in decreasing order instead of increasing--in this case reverse alphabetical order*)
      let folded_right =
	bim#fold_right ~init:None
		       ~f:(fun ~key ~data:_data accum ->
			   match accum with
			   | None -> Some key
			   | Some i -> Some (i - key)
			  ) in
      assert_equal (Some 0) folded_right;
      let folded_right_reverse = bim#fold_right_reverse
				       ~init:[]
				       ~f:(fun ~key ~data:_data accum ->
					   key::accum
					  ) in
      (*print_n_flush_alist ~sep:"|" ~to_string_func:(fun x -> x) folded_right_reverse;*)
      assert_equal "two" (Core.List.nth_exn (Core.List.rev folded_right_reverse) 0);
      let forall = bim#for_all
			 ~f:(fun l ->
			     (Core.List.for_all l ~f:(fun x -> Core.String.length x > 3))
			    ) in
      assert_equal forall false;
      assert_equal "two" (Core.List.nth_exn (Core.List.rev folded_right_reverse) 0);
      let forall = bim#for_all
			 ~f:(fun l ->
			     (Core.List.for_all l ~f:(fun x -> Core.String.length x > 2))
			    ) in
      assert_equal forall true;
      let forallreverse =
        bim#for_all_reverse
          ~f:(fun x ->
            let max = Core.List.fold x ~init:0 ~f:(fun accum x -> if x > accum then x else accum) in
            max < 4) in
      assert_equal forallreverse true;
      let forallreverse =
        bim#for_all_reverse
          ~f:(fun x ->
            let max = Core.List.fold x ~init:0 ~f:(fun accum x -> if x > accum then x else accum) in
            max < 3) in
      assert_equal forallreverse false;
      assert_equal false (bim#is_empty);
      assert_equal [1;2;3] (bim#keys);
      (*let () print_n_flush_alist ~sep:"," ~to_string_func:(fun x -> x) reverse_keys in *)
      assert_equal true (List.mem "dos" (bim#keys_reverse));
      assert_equal 1 (Core.List.length (Core.List.filter (bim#keys_reverse) ~f:(fun x -> String.equal x "dos")));
      assert_equal true (List.mem "one" (bim#keys_reverse));
      assert_equal true (List.mem "three" (bim#keys_reverse));
      assert_equal true (List.mem "tres" (bim#keys_reverse));
      assert_equal true (List.mem "triple" (bim#keys_reverse));
      assert_equal true (List.mem "two" (bim#keys_reverse));

      assert_equal 3 (bim#length);

      bim#map ~f:(fun l -> let len = Core.List.length l in
			   match len with
			   | 1 -> ["1"]@l
			   | 2 -> ["2"]@l
			   | 3 -> ["3"]@l
			   | _ -> ["unexpectedlen"]@l);
      assert_equal 2 (Core.List.length (bim#find_exn ~key:1));
      assert_equal true (List.mem "1" (bim#find_exn ~key:1));
      assert_equal 3 (Core.List.length (bim#find_exn ~key:2));
      assert_equal true (List.mem "2" (bim#find_exn ~key:2));
      assert_equal 4 (Core.List.length (bim#find_exn ~key:3));
      assert_equal true (List.mem "3" (bim#find_exn ~key:3));
      assert_equal [1] (bim#find_exn_reverse ~key:"1");
      assert_equal [2] (bim#find_exn_reverse ~key:"2");
      assert_equal [3] (bim#find_exn_reverse ~key:"3");
      assert_equal [1] (bim#find_exn_reverse ~key:"one");
      assert_equal [2] (bim#find_exn_reverse ~key:"two");
      assert_equal [3] (bim#find_exn_reverse ~key:"three");
    end

  let test10 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module BimapMulti_class = Bimap.Bimap_multi_class(Core.Int)(Core.String) in
    let bim = new BimapMulti_class.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      assert_equal true (bim#mem 1);
      assert_equal true (bim#mem 2);
      assert_equal true (bim#mem 3);
      assert_equal true (bim#mem_reverse "one");
      assert_equal true (bim#mem_reverse "two");
      assert_equal true (bim#mem_reverse "three");
      
      bim#map_reverse ~f:(fun x ->
          Core.List.map x ~f:(fun v -> v * 10));
      assert_equal [10] (bim#find_exn_reverse ~key:"one");
      assert_equal [20] (bim#find_exn_reverse ~key:"two");
      assert_equal [30] (bim#find_exn_reverse ~key:"three");
      assert_equal None (bim#find ~key:1);
      assert_equal None (bim#find ~key:2);
      assert_equal None (bim#find ~key:3);
      assert_equal true (List.mem "one" (bim#find_exn ~key:10));
      assert_equal true (List.mem "two" (bim#find_exn ~key:20));
      assert_equal true (List.mem "three" (bim#find_exn ~key:30));

      assert_equal (Some (10, ["one"])) (bim#min_elt);
      assert_equal (10, ["one"]) (bim#min_elt_exn);
      assert_equal (Some (30, ["triple";"tres";"three"])) (bim#max_elt);
      assert_equal (30,["triple";"tres";"three"]) (bim#max_elt_exn);

      assert_equal (Some ("dos", [20])) (bim#min_elt_reverse);
      assert_equal ("dos", [20]) (bim#min_elt_exn_reverse);
      assert_equal (Some ("two", [20])) (bim#max_elt_reverse);
      assert_equal ("two",[20]) (bim#max_elt_exn_reverse);

      assert_equal (Some (10, ["one"])) (bim#nth 0);
      assert_equal (Some (20, ["two";"dos"])) (bim#nth 1);
      assert_equal (Some (30, ["triple";"tres";"three"])) (bim#nth 2);
      assert_equal (Some ("dos",[20])) (bim#nth_reverse 0);
      assert_equal (Some ("one",[10])) (bim#nth_reverse 1);
      assert_equal (Some ("three",[30])) (bim#nth_reverse 2);
      assert_equal (Some ("tres",[30])) (bim#nth_reverse 3);
      assert_equal (Some ("triple",[30])) (bim#nth_reverse 4);
      assert_equal (Some ("two",[20])) (bim#nth_reverse 5);

      bim#remove ~key:20;
      assert_equal None (bim#find ~key:20);
      assert_equal None (bim#find_reverse ~key:"two");

      bim#remove_reverse ~key:"triple";
      assert_equal false (List.mem "triple" (bim#find_exn ~key:30));
      assert_equal true (List.mem "three" (bim#find_exn ~key:30));
      assert_equal true (List.mem "tres" (bim#find_exn ~key:30));

      bim#remove_multi ~key:30;
      assert_equal false (List.mem "triple" (bim#find_exn ~key:30));
      assert_equal false (List.mem "tres" (bim#find_exn ~key:30));
      assert_equal true (List.mem "three" (bim#find_exn ~key:30));
      assert_equal 1 (Core.List.length (bim#find_exn ~key:30));
      assert_equal None (bim#find_reverse ~key:"tres");
      assert_equal (Some [30]) (bim#find_reverse ~key:"three");

      bim#update ~key:30 ~f:(fun l ->
			match l with
			| Some elems ->
			   let elems2 = if not (List.mem "three" elems) then
					  (*let () = print_n_flush "\nAdding three..." in *)
					  ("three" :: elems)
					else elems in
			   let elems3 = if not (List.mem "tres" elems2) then
					  (*let () = print_n_flush "\nAdding tres..." in *)
					  ("tres" :: elems2)
					else elems2 in
			   let elems4 = if not (List.mem "triple" elems3) then
					  (*let () = print_n_flush "\nAdding triple..." in*)
					  ("triple" :: elems3)
					else elems3 in
			   elems4
			| None -> ["three";"tres";"triple"]
		       );
      assert_equal true (List.mem "triple" (bim#find_exn ~key:30));
      assert_equal true (List.mem "tres" (bim#find_exn ~key:30));
      assert_equal true (List.mem "three" (bim#find_exn ~key:30));
      assert_equal (Some [30]) (bim#find_reverse ~key:"tres");
      assert_equal (Some [30]) (bim#find_reverse ~key:"triple");
      assert_equal (Some [30]) (bim#find_reverse ~key:"three");
    end 

  let test11 _text_ctx =
    let module Bimap_single_module = Bimap.Bimap_single_module(Core.Int)(Core.String) in
    let t = Bimap_single_module.empty in 
    let t1 = Bimap_single_module.set t ~key:1 ~data:"one" in 
    let t2 = Bimap_single_module.set t1 ~key:2 ~data:"two" in 
    let () = assert_equal 2 (Bimap_single_module.length t2) in 
    let () = assert_equal false (Bimap_single_module.is_empty t2) in 
    let () = assert_equal "one" (Bimap_single_module.find_exn t2 ~key:1) in 
    let () = assert_equal "two" (Bimap_single_module.find_exn t2 ~key:2) in
    let () = assert_equal 1 (Bimap_single_module.find_exn_reverse t2 ~key:"one") in
    let () = assert_equal 2 (Bimap_single_module.find_exn_reverse t2 ~key:"two") in 
    let t3 = Bimap_single_module.set_reverse t2 ~key:"three" ~data:3 in 
    let () = assert_equal "three" (Bimap_single_module.find_exn t3 ~key:3) in
    let () = assert_equal 3 (Bimap_single_module.find_exn_reverse t3 ~key:"three") in
    let () = assert_equal [(1,"one");(2,"two");(3,"three")] (Bimap_single_module.to_alist t3) in 
    let () = assert_equal [(3,"three");(2,"two");(1,"one")] (Bimap_single_module.to_alist t3 ~key_order:`Decreasing) in
    let t4 = Bimap_single_module.change t3 ~key:3 ~f:(fun _x -> (Some "tri")) in
    let () = assert_equal "tri" (Bimap_single_module.find_exn t4 ~key:3) in 
    let () = assert_equal 3 (Bimap_single_module.find_exn_reverse t4 ~key:"tri") in
    let t5 = Bimap_single_module.change_reverse t4 ~key:"tri" ~f:(fun _x -> (Some 4)) in
    let () = assert_equal 4 (Bimap_single_module.find_exn_reverse t5 ~key:"tri") in 
    let () = assert_equal "tri" (Bimap_single_module.find_exn t5 ~key:4) in
    let t6 = Bimap_single_module.remove t5 ~key:1 in
    let () = assert_equal 2 (Bimap_single_module.length t6) in 
    let t7 = Bimap_single_module.remove_reverse t6 ~key:"two" in 
    let () = assert_equal 1 (Bimap_single_module.length t7) in
    let t8 = Bimap_single_module.remove t7 ~key:4 in
    assert_equal 0 (Bimap_single_module.length t8);;

  let test12 _text_ctx =
    let module Bimap_single_module = Bimap.Bimap_single_module(Core.Int)(Core.String) in
    let t = Bimap_single_module.empty in
    let t2 = Bimap_single_module.set t ~key:1 ~data:"one" in 
    let t3 = Bimap_single_module.set t2 ~key:2 ~data:"two" in
    let () = assert_equal 2 (Bimap_single_module.length t3) in
    let () = assert_equal false (Bimap_single_module.is_empty t3) in
    let () = assert_equal (Some "one") (Bimap_single_module.find t3 ~key:1) in
    let () = assert_equal (Some "two") (Bimap_single_module.find t3 ~key:2) in
    let () = assert_equal (Some 1) (Bimap_single_module.find_reverse t3 ~key:"one") in
    let () = assert_equal (Some 2) (Bimap_single_module.find_reverse t3 ~key:"two") in
    let () = assert_equal None (Bimap_single_module.find t3 ~key:3) in
    let () = assert_equal None (Bimap_single_module.find_reverse t3 ~key:"three") in
    let t4 = Bimap_single_module.set_reverse t3 ~key:"three" ~data:3 in
    let () = assert_equal (Some "three") (Bimap_single_module.find t4 ~key:3) in
    let () = assert_equal (Some 3) (Bimap_single_module.find_reverse t4 ~key:"three") in 
    let t5 = Bimap_single_module.change t4 ~key:3 ~f:(fun _x -> (Some "tri")) in 
    let () = assert_equal 3 (Bimap_single_module.find_exn_reverse t5 ~key:"tri") in
    let t6 = Bimap_single_module.change_reverse t5 ~key:"tri" ~f:(fun _x -> (Some 4)) in 
    let () = assert_equal (Some 4) (Bimap_single_module.find_reverse t6 ~key:"tri") in 
    let () = assert_equal (Some "tri") (Bimap_single_module.find t6 ~key:4) in 
    let () = assert_equal true (Bimap_single_module.mem t6 ~key:1) in 
    let () = assert_equal true (Bimap_single_module.mem t6 ~key:2) in 
    let () = assert_equal true (Bimap_single_module.mem_reverse t6 ~key:"tri") in
    let () = assert_equal (Some (1,"one")) (Bimap_single_module.min_elt t6) in
    let () = assert_equal (Some (4,"tri")) (Bimap_single_module.max_elt t6) in
    let () = assert_equal (1,"one") (Bimap_single_module.min_elt_exn t6) in
    let () = assert_equal (4,"tri") (Bimap_single_module.max_elt_exn t6) in
    let () = assert_equal (Some ("one",1)) (Bimap_single_module.min_elt_reverse t6) in
    let () = assert_equal (Some ("two",2)) (Bimap_single_module.max_elt_reverse t6) in
    let () = assert_equal ("one",1) (Bimap_single_module.min_elt_exn_reverse t6) in
    let () = assert_equal ("two",2) (Bimap_single_module.max_elt_exn_reverse t6) in
    let () = assert_equal (Some (2,"two")) (Bimap_single_module.nth t6 ~int:1) in
    let () = assert_equal (Some (4,"tri")) (Bimap_single_module.nth t6 ~int:2) in
    let () = assert_equal (Some ("one",1)) (Bimap_single_module.nth_reverse t6 ~int:0) in
    let () = assert_equal (Some ("tri",4)) (Bimap_single_module.nth_reverse t6 ~int:1) in 
    let () = assert_equal (Some ("two",2)) (Bimap_single_module.nth_reverse t6 ~int:2) in
    assert_equal None (Bimap_single_module.nth_reverse t6 ~int:3);;


  let test13 _text_ctx =
    let module Bimap_single_module = Bimap.Bimap_single_module(Core.Int)(Core.String) in
    let t = Bimap_single_module.empty in
    let t2 = Bimap_single_module.set t ~key:1 ~data:"single" in 
    let t3 = Bimap_single_module.set t2 ~key:2 ~data:"double" in 
    let t4 = Bimap_single_module.set t3 ~key:3 ~data:"triple" in
    let () = assert_equal 3 (Bimap_single_module.count t4 ~f:(fun _x -> true)) in 
    let () = assert_equal 3 (Bimap_single_module.length t4) in 
    let () = assert_equal 3 (Bimap_single_module.count_reverse t4 ~f:(fun _x -> true)) in 
    let () = assert_equal 2 (Bimap_single_module.counti t4 ~f:(fun ~key ~data:_data -> if key <= 2 then true else false)) in
    let () = assert_equal ["single";"double";"triple"] (Bimap_single_module.data t4) in 
    let () = assert_equal true (List.mem 1 (Bimap_single_module.data_reverse t4)) in
    let () = assert_equal true (List.mem 2 (Bimap_single_module.data_reverse t4)) in 
    let () = assert_equal true (List.mem 3 (Bimap_single_module.data_reverse t4)) in
    let () = assert_equal true (Bimap_single_module.exists t4 ~f:(fun x -> x = "double")) in 
    let () = assert_equal true (Bimap_single_module.exists t4 ~f:(fun x -> x = "triple")) in
    let () = assert_equal false (Bimap_single_module.exists t4 ~f:(fun x -> x = "quadruple")) in
    let () = assert_equal true (Bimap_single_module.exists_reverse t4 ~f:(fun x -> x = 2)) in 
    let () = assert_equal false (Bimap_single_module.exists_reverse t4 ~f:(fun x -> x = 4)) in 
    let () = assert_equal true (Bimap_single_module.existsi t4 ~f:(fun ~key ~data -> key = 3 && data = "triple")) in 
    let () = assert_equal true (Bimap_single_module.existsi_reverse t4 ~f:(fun ~key ~data -> key = "double" && data = 2)) in 
    let t5 = Bimap_single_module.empty in
    let () = assert_equal 0 (Bimap_single_module.length t5) in 
    let () = assert_equal 0 (Bimap_single_module.count t5 ~f:(fun _x -> true)) in 
    let () = assert_equal true (Bimap_single_module.is_empty t5) in 
    let t6 = Bimap_single_module.set t5 ~key:1 ~data:"one" in 
    let t7 = Bimap_single_module.set t6 ~key:2 ~data:"two" in 
    let t8 = Bimap_single_module.set t7 ~key:3 ~data:"three" in
    let t9 = Bimap_single_module.update t8 ~key:1
               ~f:(fun x -> match x with
			      Some s -> Core.String.rev s
			    | None -> "newentry") in 
    assert_equal "eno" (Bimap_single_module.find_exn t9 ~key:1);;

  let test13b _text_ctx =
    let module Bimap_single_module = Bimap.Bimap_single_module(Core.Int)(Core.String) in
    let t = Bimap_single_module.empty in 
    let t1 = Bimap_single_module.set t ~key:4 ~data:"quadruple" in
    let t2 = Bimap_single_module.set t1 ~key:5 ~data:"pentuple" in
    let t3 = Bimap_single_module.set t2 ~key:6 ~data:"sextuple" in
    let () = assert_equal 3 (Bimap_single_module.length t3) in 
    let () = assert_equal 3 (Bimap_single_module.count t3 ~f:(fun _x -> true)) in
    let t4 = Bimap_single_module.filter t3 ~f:(fun v -> String.length v > 8) in 
    let () = assert_equal 1 (Bimap_single_module.length t4) in
    let () = assert_equal 1 (Bimap_single_module.count_reverse t4 ~f:(fun _x -> true)) in
    let t5 = Bimap_single_module.set t4 ~key:5 ~data:"pentuple" in 
    let t6 = Bimap_single_module.set t5 ~key:6 ~data:"sextuple" in 
    let () = assert_equal 3 (Bimap_single_module.length t6) in 
    let t7 = Bimap_single_module.filter_reverse t6 ~f:(fun v -> v > 4) in 
    let () = assert_equal 2 (Bimap_single_module.count t7 ~f:(fun _x -> true)) in 
    let t8 = Bimap_single_module.set t7 ~key:4 ~data:"quadruple" in 
    let () = assert_equal 3 (Bimap_single_module.length t8) in 
    let t9 = Bimap_single_module.filter_keys t8 ~f:(fun k -> k < 5) in 
    let () = assert_equal 1 (Bimap_single_module.length t9) in

    let t10 = Bimap_single_module.set t9 ~key:5 ~data:"pentuple" in 
    let t11 = Bimap_single_module.set t10 ~key:6 ~data:"sextuple" in 
    let () = assert_equal 3 (Bimap_single_module.length t11) in 
    let t12 = Bimap_single_module.filter_keys_reverse t11 ~f:(fun v -> String.length v > 8) in
    let () = assert_equal 1 (Bimap_single_module.length t12) in
    let () = assert_equal 1 (Bimap_single_module.count_reverse t12 ~f:(fun _x -> true)) in 

    let t13 = Bimap_single_module.set t ~key:4 ~data:"quadruple" in
    let t14 = Bimap_single_module.set t13 ~key:5 ~data:"pentuple" in 
    let t15 = Bimap_single_module.set t14 ~key:6 ~data:"sextuple" in 
    let () = assert_equal 3 (Bimap_single_module.length t15) in 
    let t16 = Bimap_single_module.filteri t15 ~f:(fun ~key ~data -> String.length data > 8 || key < 6) in
    let () = assert_equal 2 (Bimap_single_module.length t16) in
    let () = assert_equal (Some "quadruple") (Bimap_single_module.find t15 ~key:4) in
    let () = assert_equal (Some "pentuple") (Bimap_single_module.find t15 ~key:5) in 
    let () = assert_equal None (Bimap_single_module.find t16 ~key:6) in

    let t17 = Bimap_single_module.set t ~key:4 ~data:"quadruple" in 
    let t18 = Bimap_single_module.set t17 ~key:5 ~data:"pentuple" in 
    let t19 = Bimap_single_module.set t18 ~key:6 ~data:"sextuple" in 
    let t20 = Bimap_single_module.filteri_reverse t19 ~f:(fun ~key ~data -> String.length key > 8 || data < 6) in 
    let () = assert_equal 2 (Bimap_single_module.length t20) in
    let () = assert_equal 2 (Bimap_single_module.count_reverse t20 ~f:(fun _x -> true)) in 
    let () = assert_equal (Some "quadruple") (Bimap_single_module.find t20 ~key:4) in 
    let () = assert_equal (Some "pentuple") (Bimap_single_module.find t20 ~key:5) in 
    let () = assert_equal None (Bimap_single_module.find t20 ~key:6) in 
    let () = assert_equal (Some 4) (Bimap_single_module.find_reverse t20 ~key:"quadruple") in 
    let () = assert_equal (Some 5) (Bimap_single_module.find_reverse t20 ~key:"pentuple") in
    let () = assert_equal None (Bimap_single_module.find_reverse t20 ~key:"sextuple") in

    let t21 = Bimap_single_module.set t20 ~key:4 ~data:"quadruple" in 
    let t22 = Bimap_single_module.set t21 ~key:5 ~data:"pentuple" in 
    let t23 = Bimap_single_module.set t22 ~key:6 ~data:"sextuple" in
    let t24 = Bimap_single_module.filter_map t23 ~f:(fun v -> if String.length v > 8 then (Some "survivor") else None) in 
    let () = assert_equal 1 (Bimap_single_module.length t24) in 
    let () = assert_equal 1 (Bimap_single_module.count_reverse t24 ~f:(fun _x -> true)) in 
    let () = assert_equal (Some "survivor") (Bimap_single_module.find t24 ~key:4) in 
    let () = assert_equal None (Bimap_single_module.find t24 ~key:5) in 
    let () = assert_equal None (Bimap_single_module.find t24 ~key:6) in 
    let () = assert_equal (Some 4) (Bimap_single_module.find_reverse t24 ~key:"survivor") in 
    let () = assert_equal None (Bimap_single_module.find_reverse t24 ~key:"pentuple") in 
    let () = assert_equal None (Bimap_single_module.find_reverse t24 ~key:"sextuple") in 

    let t25 = Bimap_single_module.set t24 ~key:4 ~data:"quadruple" in 
    let t26 = Bimap_single_module.set t25 ~key:5 ~data:"pentuple" in
    let t27 = Bimap_single_module.set t26 ~key:6 ~data:"sextuple" in
    let t28 = Bimap_single_module.filter_map_reverse t27 ~f:(fun v -> if v > 5 then (Some (v * 2)) else None) in 
    let () = assert_equal 1 (Bimap_single_module.length t28) in
    let () = assert_equal 1 (Bimap_single_module.count_reverse t28 ~f:(fun _x -> true)) in 
    let () = assert_equal (Some 12) (Bimap_single_module.find_reverse t28 ~key:"sextuple") in 
    let () = assert_equal None (Bimap_single_module.find_reverse t28 ~key:"quadruple") in
    let () = assert_equal None (Bimap_single_module.find_reverse t28 ~key:"pentuple") in 
    let () = assert_equal None (Bimap_single_module.find t28 ~key:4) in 
    let () = assert_equal None (Bimap_single_module.find t28 ~key:5) in 
    let () = assert_equal None (Bimap_single_module.find t28 ~key:6) in
    assert_equal (Some "sextuple") (Bimap_single_module.find t28 ~key:12);;


  let test14 _text_ctx =
    let module Bimap_single_module = Bimap.Bimap_single_module(Core.Int)(Core.String) in
    let t = Bimap_single_module.empty in 
    let t1 = Bimap_single_module.set t ~key:1 ~data:"single" in 
    let t2 = Bimap_single_module.set t1 ~key:2 ~data:"double" in 
    let t3 = Bimap_single_module.set t2 ~key:3 ~data:"triple" in 
    let concat = Bimap_single_module.fold ~init:"" t3 ~f:(fun ~key ~data init -> if key < 3 then (init ^ data) else init) in
    let () = assert_equal "singledouble" concat in 
    let sum = Bimap_single_module.fold_reverse
                t3
		~init:0
		~f:(fun ~key ~data init ->
		  if key.[0] = 't' || key.[0] = 'd'
		  then (data + init) else init) in
    let () = assert_equal 5 sum in 
    let dividend = Bimap_single_module.fold_reverse
                     t3
		     ~init:1
		     ~f:(fun ~key ~data init ->
		       if key.[0] = 't' || key.[0] = 'd'
		       then (data / init) else init) in
     let () = assert_equal 1 dividend in
     let concat2 =
        Bimap_single_module.fold_right t3
	  ~init:"" ~f:(fun ~key ~data init ->
	    if key < 3 then (init ^ data) else init) in
      let () = assert_equal "doublesingle" concat2 in 
      let dividend2 =
        Bimap_single_module.fold_right_reverse t3
	  ~init:1 ~f:(fun ~key ~data init ->
	    if key.[0] ='t' || key.[0]='d'
	    then (data / init) else init) in
      let () = assert_equal 0 dividend2 in 
      assert_equal true
        (Bimap_single_module.for_all t3
           ~f:(fun v -> if (String.length v) > 0 then true else false));;

  let test15 _text_ctx =
    let module Bimap_single_module = Bimap.Bimap_single_module(Core.Int)(Core.String) in
    let t = Bimap_single_module.empty in 
    let t1 = Bimap_single_module.set t ~key:1 ~data:"uno" in 
    let t2 = Bimap_single_module.set t1 ~key:2 ~data:"dos" in
    let t3 = Bimap_single_module.set t2 ~key:3 ~data:"tres" in 
    let () = assert_equal true (Bimap_single_module.for_all t3 ~f:(fun v -> (String.length v) > 1)) in
    let () = assert_equal false (Bimap_single_module.for_all t3 ~f:(fun v -> (String.length v) > 4)) in 
    let () = assert_equal true (Bimap_single_module.for_all_reverse t3 ~f:(fun v -> v < 4)) in 
    let () = assert_equal false (Bimap_single_module.for_all_reverse t3 ~f:(fun v -> v > 3)) in 
    let () = assert_equal [1;2;3] (Bimap_single_module.keys t3) in 
    let () = assert_equal ["uno";"dos";"tres"] (Bimap_single_module.data t3) in 
    let t4 = Bimap_single_module.map t3 ~f:(fun v -> Core.String.concat [v;v;]) in 
    let () = assert_equal ["unouno";"dosdos";"trestres"] (Bimap_single_module.data t4) in 
    let t5 = Bimap_single_module.mapi t4 ~f:(fun ~key ~data -> Core.String.concat [(Core.Int.to_string key);"->";data]) in 
    let () = assert_equal ["1->unouno";"2->dosdos";"3->trestres"] (Bimap_single_module.data t5) in 
    let () = assert_equal "1->unouno" (Bimap_single_module.find_exn t5 ~key:1) in 
    let () = assert_equal "2->dosdos" (Bimap_single_module.find_exn t5 ~key:2) in 
    let () = assert_equal 1 (Bimap_single_module.find_exn_reverse t5 ~key:"1->unouno") in 
    let () = assert_equal 2 (Bimap_single_module.find_exn_reverse t5 ~key:"2->dosdos") in

    let t6 = Bimap_single_module.map_reverse t5 ~f:(fun x -> x*2) in 
    let () = assert_equal [2;4;6] (Bimap_single_module.keys t6) in 
    let t7 = Bimap_single_module.mapi_reverse t6
               ~f:(fun ~key ~data -> if key.[0]='1' then
				       data*1
				     else
				       if key.[0]='2' then
					 data*2
				       else
					 data*3) in 
    assert_equal [2;8;18] (Bimap_single_module.keys t7);;

  let test16 _text_ctx =
    let module Bimap_multi_module = Bimap.Bimap_multi_module(Core.Int)(Core.String) in
    let t = Bimap_multi_module.empty in 
    let t1 = Bimap_multi_module.add_multi t ~key:1 ~data:"one" in 
    let t2 = Bimap_multi_module.add_multi t1 ~key:2 ~data:"two" in
    let t3 = Bimap_multi_module.add_multi t2 ~key:3 ~data:"three" in 
    let t4 = Bimap_multi_module.add_multi t3 ~key:2 ~data:"dos" in 
    let t5 = Bimap_multi_module.add_multi t4 ~key:3 ~data:"tres" in 
    let t6 = Bimap_multi_module.add_multi t5 ~key:3 ~data:"triple" in
    let t7 = Bimap_multi_module.add_multi_reverse t6 ~key:"tri" ~data:3 in 
    let t8 = Bimap_multi_module.add_multi_reverse t7 ~key:"iii" ~data:3 in 
    let t9 = Bimap_multi_module.add_multi_reverse t8 ~key:"four" ~data:4 in 

    let () = assert_equal 4 (Bimap_multi_module.count t9 ~f:(fun _l -> true)) in 
    let () = assert_equal 4 (Bimap_multi_module.counti t9 ~f:(fun ~key:_key ~data:_data -> true)) in 
    let () = assert_equal 1 (Bimap_multi_module.counti t9 ~f:(fun ~key ~data:_data -> key > 3)) in 
    let () = assert_equal 9 (Bimap_multi_module.count_reverse t9 ~f:(fun _x -> true)) in
    let () = assert_equal 4 (Bimap_multi_module.length t9) in
    let () = assert_equal 1 (Core.List.length (Bimap_multi_module.find_exn t9 ~key:1)) in 
    let () = assert_equal 2 (Core.List.length (Bimap_multi_module.find_exn t9 ~key:2)) in
    let () = assert_equal 5 (Core.List.length (Bimap_multi_module.find_exn t9 ~key:3)) in
    let () = assert_equal 1 (Core.List.length (Bimap_multi_module.find_exn t9 ~key:4)) in 
    let () = assert_equal true (List.mem "one" (Bimap_multi_module.find_exn t9 ~key:1)) in 
    let () = assert_equal true (List.mem "two" (Bimap_multi_module.find_exn t9 ~key:2)) in 
    let () = assert_equal true (List.mem "dos" (Bimap_multi_module.find_exn t9 ~key:2)) in 
    let () = assert_equal true (List.mem "three" (Bimap_multi_module.find_exn t9 ~key:3)) in 
    let () = assert_equal true (List.mem "tres" (Bimap_multi_module.find_exn t9 ~key:3)) in 
    let () = assert_equal true (List.mem "triple" (Bimap_multi_module.find_exn t9 ~key:3)) in 
    let () = assert_equal true (List.mem "tri" (Bimap_multi_module.find_exn t9 ~key:3)) in
    let () = assert_equal true (List.mem "iii" (Bimap_multi_module.find_exn t9 ~key:3)) in 
      
    let () = assert_equal [1] (Bimap_multi_module.find_exn_reverse t9 ~key:"one") in 
    let () = assert_equal [2] (Bimap_multi_module.find_exn_reverse t9 ~key:"two") in 
    let () = assert_equal [2] (Bimap_multi_module.find_exn_reverse t9 ~key:"dos") in 
    let () = assert_equal [3] (Bimap_multi_module.find_exn_reverse t9 ~key:"three") in 
    let () = assert_equal [3] (Bimap_multi_module.find_exn_reverse t9 ~key:"tres") in 
    let () = assert_equal [3] (Bimap_multi_module.find_exn_reverse t9 ~key:"triple") in 
    let () = assert_equal [3] (Bimap_multi_module.find_exn_reverse t9 ~key:"tri") in 
    let () = assert_equal [3] (Bimap_multi_module.find_exn_reverse t9 ~key:"iii") in 

    let () = assert_equal (Some [1]) (Bimap_multi_module.find_reverse t9 ~key:"one") in 
    let () = assert_equal (Some [2]) (Bimap_multi_module.find_reverse t9 ~key:"two") in 
    let () = assert_equal (Some [2]) (Bimap_multi_module.find_reverse t9 ~key:"dos") in 
    let () = assert_equal (Some [3]) (Bimap_multi_module.find_reverse t9 ~key:"three") in 
    let () = assert_equal (Some [3]) (Bimap_multi_module.find_reverse t9 ~key:"tres") in 
    let () = assert_equal (Some [3]) (Bimap_multi_module.find_reverse t9 ~key:"triple") in 
    let () = assert_equal [3] (Bimap_multi_module.find_exn_reverse t9 ~key:"tri") in 
      
      (*print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> x) (bim#data);*)
    let () = assert_equal ([["one"];["dos";"two"];["iii";"tri";"triple";"tres";"three"];["four"]]) (Bimap_multi_module.data t9) in 
      (*print_n_flush_alist ~sep:" | " ~to_string_func:(fun x -> Core.Int.to_string x) (bim#data_reverse);*)
    let () = assert_equal 1 (Core.List.length (Core.List.filter (Bimap_multi_module.data_reverse t9) ~f:(fun x -> if x = [1] then true else false))) in 
    let () = assert_equal 2 (Core.List.length (Core.List.filter (Bimap_multi_module.data_reverse t9) ~f:(fun x -> if x = [2] then true else false))) in 
    let () = assert_equal 5 (Core.List.length (Core.List.filter (Bimap_multi_module.data_reverse t9) ~f:(fun x -> if x = [3] then true else false))) in 
    let () = assert_equal 1 (Core.List.length (Core.List.filter (Bimap_multi_module.data_reverse t9) ~f:(fun x -> if x = [4] then true else false))) in

    let t9 = Bimap_multi_module.empty in 
    let () = assert_equal 0 (Bimap_multi_module.count t9 ~f:(fun _l -> true)) in 
    let () = assert_equal 0 (Bimap_multi_module.counti t9 ~f:(fun ~key:_key ~data:_data -> true)) in 
    let () = assert_equal 0 (Bimap_multi_module.counti t9 ~f:(fun ~key ~data:_data -> key > 3)) in 
    let () = assert_equal 0 (Bimap_multi_module.count_reverse t9 ~f:(fun _x -> true)) in 
    let () = assert_equal 0 (Bimap_multi_module.length t9) in 

    let t10 = Bimap_multi_module.add_multi t9 ~key:1 ~data:"one" in 
    let t11 = Bimap_multi_module.add_multi t10 ~key:2 ~data:"two" in
    let t12 = Bimap_multi_module.add_multi t11 ~key:3 ~data:"three" in 
    let t13 = Bimap_multi_module.add_multi t12 ~key:2 ~data:"dos" in 
    let t14 = Bimap_multi_module.add_multi t13 ~key:3 ~data:"tres" in
    let t15 = Bimap_multi_module.add_multi t14 ~key:3 ~data:"triple" in 
    let t16 = Bimap_multi_module.add_multi_reverse t15 ~key:"tri" ~data:3 in 

    let () = assert_equal true (Bimap_multi_module.exists t16 ~f:(fun x -> Core.List.length x = 1 && Core.String.equal (Core.List.nth_exn x 0) "one")) in 
    let () = assert_equal true (Bimap_multi_module.exists_reverse t16 ~f:(fun x -> x = [2])) in 
    let () = assert_equal true (Bimap_multi_module.existsi t16 ~f:(fun ~key ~data -> Core.List.length data = 2 && key = 2 && List.mem "two" data)) in 
    let () = assert_equal false (Bimap_multi_module.existsi t16 ~f:(fun ~key ~data -> List.mem "one" data && key = 2)) in 
    let () = assert_equal true (Bimap_multi_module.existsi_reverse t16 ~f:(fun ~key ~data -> key = "tri" && data = [3])) in 
 
    let t17 = Bimap_multi_module.change t16 ~key:3 ~f:(fun _x -> (Some ["iii";"tri";"triple";"tres";"three"])) in 
    let () = assert_equal (Some [3]) (Bimap_multi_module.find_reverse t17 ~key:"three") in 
    let () = assert_equal (Some [3]) (Bimap_multi_module.find_reverse t17 ~key:"tres") in 
    let () = assert_equal (Some [3]) (Bimap_multi_module.find_reverse t17 ~key:"tri") in 
    let () = assert_equal [3] (Bimap_multi_module.find_exn_reverse t17 ~key:"triple") in 
    let () = assert_equal [3] (Bimap_multi_module.find_exn_reverse t17 ~key:"iii") in 
    let () = assert_equal 5 (Core.List.length (Bimap_multi_module.find_exn t17 ~key:3)) in

    let t18 = Bimap_multi_module.change_reverse t17 ~key:"two" ~f:(fun _x -> (Some [4])) in 
      (*print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> x) (bim#data);
      print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> Core.Int.to_string x) (bim#data_reverse);*)
    let () = assert_equal 1 (Core.List.length (Bimap_multi_module.find_exn t18 ~key:4)) in 
    let () = assert_equal true (List.mem "two" (Bimap_multi_module.find_exn t18 ~key:4)) in
    let () = assert_equal false (List.mem "dos" (Bimap_multi_module.find_exn t18 ~key:4)) in 
    let () = assert_equal [4] (Bimap_multi_module.find_exn_reverse t18 ~key:"two") in 
    let () = assert_equal [2] (Bimap_multi_module.find_exn_reverse t18 ~key:"dos") in 
    let () = assert_equal ["dos"] (Bimap_multi_module.find_exn t18 ~key:2) in

    let t19 = Bimap_multi_module.change_reverse t18 ~key:"two" ~f:(fun _x -> (Some [2;4])) in 
      (*print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> x) (bim#data);
      print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> Core.Int.to_string x) (bim#data_reverse);*)
    let () = assert_equal 1 (Core.List.length (Bimap_multi_module.find_exn t19 ~key:4)) in 
    let () = assert_equal true (List.mem "two" (Bimap_multi_module.find_exn t19 ~key:4)) in 
    let () = assert_equal true (List.mem "two" (Bimap_multi_module.find_exn t19 ~key:2)) in 
    let () = assert_equal false (List.mem "dos" (Bimap_multi_module.find_exn t19 ~key:4)) in 
    let () = assert_equal true (List.mem 2 (Bimap_multi_module.find_exn_reverse t19 ~key:"two")) in 
    let () = assert_equal true (List.mem 4 (Bimap_multi_module.find_exn_reverse t19 ~key:"two")) in 
    let () = assert_equal [2] (Bimap_multi_module.find_exn_reverse t19 ~key:"dos") in 
    let () = assert_equal true (List.mem "two" (Bimap_multi_module.find_exn t19 ~key:2)) in 
    assert_equal true (List.mem "dos" (Bimap_multi_module.find_exn t19 ~key:2));;

  let test17 _text_ctx =
    let module Bimap_multi_module = Bimap.Bimap_multi_module(Core.Int)(Core.String) in
    let t = Bimap_multi_module.empty in
    let t1 = Bimap_multi_module.add_multi t ~key:1 ~data:"one" in
    let t2 = Bimap_multi_module.add_multi t1 ~key:2 ~data:"two" in 
    let t3 = Bimap_multi_module.add_multi t2 ~key:3 ~data:"three" in 
    let t4 = Bimap_multi_module.add_multi t3 ~key:2 ~data:"dos" in 
    let t5 = Bimap_multi_module.add_multi t4 ~key:3 ~data:"tres" in 
    let t6 = Bimap_multi_module.add_multi t5 ~key:3 ~data:"triple" in 

    let t7 = Bimap_multi_module.filter t6 ~f:(fun v -> (Core.List.length v) < 3) in 
    let () = assert_equal 2 (Bimap_multi_module.count t7 ~f:(fun _x -> true)) in 
    let () = assert_equal 3 (Bimap_multi_module.count_reverse t7 ~f:(fun _x -> true)) in 
    let () = assert_equal 2 (Bimap_multi_module.length t7) in 

    let t8 = Bimap_multi_module.filter_reverse t7 ~f:(fun kl ->
                 let max = Core.List.fold ~init:0 ~f:(fun accum v -> if v > accum then v else accum) kl in
                 max < 2
               ) in 
    let () = assert_equal 1 (Bimap_multi_module.count t8 ~f:(fun _x -> true)) in 
    let () = assert_equal 1 (Bimap_multi_module.count_reverse t8 ~f:(fun _x -> true)) in 
    assert_equal 1 (Bimap_multi_module.length t8);;

  let test18 _text_ctx =
    let module Bimap_multi_module = Bimap.Bimap_multi_module(Core.Int)(Core.String) in
    let t = Bimap_multi_module.empty in
    let t1 = Bimap_multi_module.add_multi t ~key:1 ~data:"one" in 
    let t2 = Bimap_multi_module.add_multi t1 ~key:2 ~data:"two" in 
    let t3 = Bimap_multi_module.add_multi t2 ~key:3 ~data:"three" in 
    let t4 = Bimap_multi_module.add_multi t3 ~key:2 ~data:"dos" in
    let t5 = Bimap_multi_module.add_multi t4 ~key:3 ~data:"tres" in 
    let t6 = Bimap_multi_module.add_multi t5 ~key:3 ~data:"triple" in 

    let t7 = Bimap_multi_module.filteri t6 ~f:(fun ~key ~data -> key < 3 && (Core.List.length data) > 1) in 
    let () = assert_equal 1 (Bimap_multi_module.count t7 ~f:(fun _x -> true)) in 
    let () = assert_equal 2 (Bimap_multi_module.count_reverse t7 ~f:(fun _x -> true)) in 
    let () = assert_equal 1 (Bimap_multi_module.length t7) in 

    let t8 = Bimap_multi_module.empty in
    let t9 = Bimap_multi_module.add_multi t8 ~key:1 ~data:"one" in 
    let t10 = Bimap_multi_module.add_multi t9 ~key:2 ~data:"two" in
    let t11 = Bimap_multi_module.add_multi t10 ~key:3 ~data:"three" in 
    let t12 = Bimap_multi_module.add_multi t11 ~key:2 ~data:"dos" in 
    let t13 = Bimap_multi_module.add_multi t12 ~key:3 ~data:"tres" in
    let t14 = Bimap_multi_module.add_multi t13 ~key:3 ~data:"triple" in

    let t15 = Bimap_multi_module.filteri_reverse t14 ~f:(fun ~key ~data ->
          let max = Core.List.fold ~init:0 ~f:(fun accum v -> if v > accum then v else accum) data in
          key.[0] = 't' && max < 3) in
    let () = assert_equal 1 (Bimap_multi_module.count t15 ~f:(fun _x -> true)) in
    let () = assert_equal 1 (Bimap_multi_module.count_reverse t15 ~f:(fun _x -> true)) in 
    let () = assert_equal 1 (Bimap_multi_module.length t15) in 

    let t16 = Bimap_multi_module.empty in
    let t17 = Bimap_multi_module.add_multi t16 ~key:1 ~data:"one" in 
    let t18 = Bimap_multi_module.add_multi t17 ~key:2 ~data:"two" in 
    let t19 = Bimap_multi_module.add_multi t18 ~key:3 ~data:"three" in 
    let t20 = Bimap_multi_module.add_multi t19 ~key:2 ~data:"dos" in 
    let t21 = Bimap_multi_module.add_multi t20 ~key:3 ~data:"tres" in
    let t22 = Bimap_multi_module.add_multi t21 ~key:3 ~data:"triple" in 

    let t23 = Bimap_multi_module.filter_keys t22 ~f:(fun k -> k > 2) in
    let () = assert_equal 1 (Bimap_multi_module.count t23 ~f:(fun _x -> true)) in 
    let () = assert_equal 3 (Bimap_multi_module.count_reverse t23 ~f:(fun _x -> true)) in
    let () = assert_equal 1 (Bimap_multi_module.length t23) in 

    let t24 = Bimap_multi_module.empty in
    let t25 = Bimap_multi_module.add_multi t24 ~key:1 ~data:"one" in 
    let t26 = Bimap_multi_module.add_multi t25 ~key:2 ~data:"two" in
    let t27 = Bimap_multi_module.add_multi t26 ~key:3 ~data:"three" in 
    let t28 = Bimap_multi_module.add_multi t27 ~key:2 ~data:"dos" in
    let t29 = Bimap_multi_module.add_multi t28 ~key:3 ~data:"tres" in 
    let t30 = Bimap_multi_module.add_multi t29 ~key:3 ~data:"triple" in

    let t31 = Bimap_multi_module.filter_keys_reverse t30 ~f:(fun k -> (String.length k) > 3) in
    let () = assert_equal 1 (Bimap_multi_module.count t31 ~f:(fun _x -> true)) in 
    let () = assert_equal 3 (Bimap_multi_module.count_reverse t31 ~f:(fun _x -> true)) in 
    let () = assert_equal 1 (Bimap_multi_module.length t31) in 

    let t32 = Bimap_multi_module.empty in
    let t33 = Bimap_multi_module.add_multi t32 ~key:1 ~data:"one" in 
    let t34 = Bimap_multi_module.add_multi t33 ~key:2 ~data:"two" in 
    let t35 = Bimap_multi_module.add_multi t34 ~key:3 ~data:"three" in 
    let t36 = Bimap_multi_module.add_multi t35 ~key:2 ~data:"dos" in 
    let t37 = Bimap_multi_module.add_multi t36 ~key:3 ~data:"tres" in 
    let t38 = Bimap_multi_module.add_multi t37 ~key:3 ~data:"triple" in

    let t39 = Bimap_multi_module.filter_map t38 ~f:(fun vlist -> if Core.List.length vlist < 3 then Some vlist else None) in
    let () = assert_equal 2 (Bimap_multi_module.count t39 ~f:(fun _x -> true)) in 
    let () = assert_equal 3 (Bimap_multi_module.count_reverse t39 ~f:(fun _x -> true)) in 
    let () = assert_equal 2 (Bimap_multi_module.length t39) in 

    let t40 = Bimap_multi_module.empty in 
    let t41 = Bimap_multi_module.add_multi t40 ~key:1 ~data:"one" in 
    let t42 = Bimap_multi_module.add_multi t41 ~key:2 ~data:"two" in 
    let t43 = Bimap_multi_module.add_multi t42 ~key:3 ~data:"three" in 
    let t44 = Bimap_multi_module.add_multi t43 ~key:2 ~data:"dos" in 
    let t45 = Bimap_multi_module.add_multi t44 ~key:3 ~data:"tres" in 
    let t46 = Bimap_multi_module.add_multi t45 ~key:3 ~data:"triple" in 

    let t47 = Bimap_multi_module.filter_map_reverse t46
        ~f:(fun invk ->
          let max = Core.List.fold ~init:0 ~f:(fun accum v -> if v > accum then v else accum) invk in
          if max < 3 then
            let newlist = Core.List.map invk ~f:(fun k -> k+1) in 
            Some (newlist)
          else
            None
        ) in
    let () = assert_equal 2 (Bimap_multi_module.count t47 ~f:(fun _x -> true)) in 
    let () = assert_equal 3 (Bimap_multi_module.count_reverse t47 ~f:(fun _x -> true)) in 
    let () = assert_equal 2 (Bimap_multi_module.length t47) in 
    let () = assert_equal 1 (Core.List.length (Bimap_multi_module.find_exn t47 ~key:2)) in 
    let () = assert_equal 2 (Core.List.length (Bimap_multi_module.find_exn t47 ~key:3)) in 
    let () = assert_equal true (List.mem "one" (Bimap_multi_module.find_exn t47 ~key:2)) in 
    let () = assert_equal true (List.mem "two" (Bimap_multi_module.find_exn t47 ~key:3)) in 
    let () = assert_equal true (List.mem "dos" (Bimap_multi_module.find_exn t47 ~key:3)) in 
    let () = assert_equal [2] (Bimap_multi_module.find_exn_reverse t47 ~key:"one") in 
    let () = assert_equal [3] (Bimap_multi_module.find_exn_reverse t47 ~key:"two") in 
    let () = assert_equal [3] (Bimap_multi_module.find_exn_reverse t47 ~key:"dos") in 
    assert_equal None (Bimap_multi_module.find t47 ~key:1)

  let test19 _text_ctx =
    let module Bimap_multi_module = Bimap.Bimap_multi_module(Core.Int)(Core.String) in
    let t = Bimap_multi_module.empty in
    let t1 = Bimap_multi_module.add_multi t ~key:1 ~data:"one" in 
    let t2 = Bimap_multi_module.add_multi t1 ~key:2 ~data:"two" in 
    let t3 = Bimap_multi_module.add_multi t2 ~key:3 ~data:"three" in 
    let t4 = Bimap_multi_module.add_multi t3 ~key:2 ~data:"dos" in 
    let t5 = Bimap_multi_module.add_multi t4 ~key:3 ~data:"tres" in 
    let t6 = Bimap_multi_module.add_multi t5 ~key:3 ~data:"triple" in 
    let total_chars =
      Bimap_multi_module.fold t6 ~init:0 ~f:(fun ~key:_key ~data accum ->
	  (Core.List.fold data ~init:accum
	     ~f:(fun accum data -> (String.length data) + accum))) in
    let () = assert_equal 24 total_chars in 
    let keys_times_num_values =
      Bimap_multi_module.fold_reverse t6 ~init:0 ~f:(fun ~key:_key ~data accum ->
          let subtotal = Core.List.fold data ~init:0 ~f:(fun accum x -> accum + x) in 
	  accum + subtotal) in
    let () = assert_equal 14 keys_times_num_values in
    (*fold over keys in decreasing order instead of increasing--in this case reverse alphabetical order*)
    let folded_right =
      Bimap_multi_module.fold_right t6 ~init:None
	~f:(fun ~key ~data:_data accum ->
	  match accum with
	  | None -> Some key
	  | Some i -> Some (i - key)
	) in
    let () = assert_equal (Some 0) folded_right in
    let folded_right_reverse =
      Bimap_multi_module.fold_right_reverse t6
	~init:[]
	~f:(fun ~key ~data:_data accum ->
	  key::accum
	) in
    (*print_n_flush_alist ~sep:"|" ~to_string_func:(fun x -> x) folded_right_reverse;*)
    let () = assert_equal "two" (Core.List.nth_exn (Core.List.rev folded_right_reverse) 0) in
    let forall = Bimap_multi_module.for_all t6
		   ~f:(fun l ->
		     (Core.List.for_all l ~f:(fun x -> Core.String.length x > 3))
		   ) in
    let () = assert_equal forall false in 
    let () = assert_equal "two" (Core.List.nth_exn (Core.List.rev folded_right_reverse) 0) in
    let forall = Bimap_multi_module.for_all t6
		   ~f:(fun l ->
		     (Core.List.for_all l ~f:(fun x -> Core.String.length x > 2))
		   ) in
      let () = assert_equal forall true in 
      let forallreverse =
        Bimap_multi_module.for_all_reverse t6
          ~f:(fun x ->
            let max = Core.List.fold x ~init:0 ~f:(fun accum x -> if x > accum then x else accum) in
            max < 4) in
      let () = assert_equal forallreverse true in 
      let forallreverse =
        Bimap_multi_module.for_all_reverse t6
          ~f:(fun x ->
            let max = Core.List.fold x ~init:0 ~f:(fun accum x -> if x > accum then x else accum) in
            max < 3) in
      let () = assert_equal forallreverse false in 
      let () = assert_equal false (Bimap_multi_module.is_empty t6) in 
      let () = assert_equal [1;2;3] (Bimap_multi_module.keys t6) in 
      (*let () print_n_flush_alist ~sep:"," ~to_string_func:(fun x -> x) reverse_keys in *)
      let () = assert_equal true (List.mem "dos" (Bimap_multi_module.keys_reverse t6)) in 
      let () = assert_equal 1
                 (Core.List.length
                    (Core.List.filter
                       (Bimap_multi_module.keys_reverse t6) ~f:(fun x -> String.equal x "dos"))) in 
      let () = assert_equal true (List.mem "one" (Bimap_multi_module.keys_reverse t6)) in 
      let () = assert_equal true (List.mem "three" (Bimap_multi_module.keys_reverse t6)) in 
      let () = assert_equal true (List.mem "tres" (Bimap_multi_module.keys_reverse t6)) in
      let () = assert_equal true (List.mem "triple" (Bimap_multi_module.keys_reverse t6)) in
      let () = assert_equal true (List.mem "two" (Bimap_multi_module.keys_reverse t6)) in 
      let () = assert_equal 3 (Bimap_multi_module.length t6) in 

      let t7 = Bimap_multi_module.map t6
                 ~f:(fun l -> let len = Core.List.length l in
		              match len with
		              | 1 -> ["1"]@l
		              | 2 -> ["2"]@l
		              | 3 -> ["3"]@l
		              | _ -> ["unexpectedlen"]@l) in 
      let () = assert_equal 2 (Core.List.length (Bimap_multi_module.find_exn t7 ~key:1)) in 
      let () = assert_equal true (List.mem "1" (Bimap_multi_module.find_exn t7 ~key:1)) in 
      let () = assert_equal 3 (Core.List.length (Bimap_multi_module.find_exn t7 ~key:2)) in
      let () = assert_equal true (List.mem "2" (Bimap_multi_module.find_exn t7 ~key:2)) in 
      let () = assert_equal 4 (Core.List.length (Bimap_multi_module.find_exn t7 ~key:3)) in 
      let () = assert_equal true (List.mem "3" (Bimap_multi_module.find_exn t7 ~key:3)) in 
      let () = assert_equal [1] (Bimap_multi_module.find_exn_reverse t7 ~key:"1") in
      let () = assert_equal [2] (Bimap_multi_module.find_exn_reverse t7 ~key:"2") in
      let () = assert_equal [3] (Bimap_multi_module.find_exn_reverse t7 ~key:"3") in
      let () = assert_equal [1] (Bimap_multi_module.find_exn_reverse t7 ~key:"one") in 
      let () = assert_equal [2] (Bimap_multi_module.find_exn_reverse t7 ~key:"two") in 
      assert_equal [3] (Bimap_multi_module.find_exn_reverse t7 ~key:"three")

  let test20 _text_ctx =
    let module Bimap_multi_module = Bimap.Bimap_multi_module(Core.Int)(Core.String) in
    let t = Bimap_multi_module.empty in
    let t1 = Bimap_multi_module.add_multi t ~key:1 ~data:"one" in 
    let t2 = Bimap_multi_module.add_multi t1 ~key:2 ~data:"two" in 
    let t3 = Bimap_multi_module.add_multi t2 ~key:3 ~data:"three" in 
    let t4 = Bimap_multi_module.add_multi t3 ~key:2 ~data:"dos" in 
    let t5 = Bimap_multi_module.add_multi t4 ~key:3 ~data:"tres" in 
    let t6 = Bimap_multi_module.add_multi t5 ~key:3 ~data:"triple" in 

    let () = assert_equal true (Bimap_multi_module.mem t6 ~key:1) in 
    let () = assert_equal true (Bimap_multi_module.mem t6 ~key:2) in 
    let () = assert_equal true (Bimap_multi_module.mem t6 ~key:3) in 
    let () = assert_equal true (Bimap_multi_module.mem_reverse t6 ~key:"one") in 
    let () = assert_equal true (Bimap_multi_module.mem_reverse t6 ~key:"two") in 
    let () = assert_equal true (Bimap_multi_module.mem_reverse t6 ~key:"three") in
      
    let t7 = Bimap_multi_module.map_reverse t6 ~f:(fun x ->
                 Core.List.map x ~f:(fun v -> v * 10)) in 
    let () = assert_equal [10] (Bimap_multi_module.find_exn_reverse t7 ~key:"one") in 
    let () = assert_equal [20] (Bimap_multi_module.find_exn_reverse t7 ~key:"two") in 
    let ()=  assert_equal [30] (Bimap_multi_module.find_exn_reverse t7 ~key:"three") in 
    let () = assert_equal None (Bimap_multi_module.find t7 ~key:1) in 
    let () = assert_equal None (Bimap_multi_module.find t7 ~key:2) in 
    let () = assert_equal None (Bimap_multi_module.find t7 ~key:3) in 
    let () = assert_equal true (List.mem "one" (Bimap_multi_module.find_exn t7 ~key:10)) in 
    let () = assert_equal true (List.mem "two" (Bimap_multi_module.find_exn t7 ~key:20)) in 
    let () = assert_equal true (List.mem "three" (Bimap_multi_module.find_exn t7 ~key:30)) in 

    let () = assert_equal (Some (10, ["one"])) (Bimap_multi_module.min_elt t7) in 
    let () = assert_equal (10, ["one"]) (Bimap_multi_module.min_elt_exn t7) in 
    let () = assert_equal (Some (30, ["triple";"tres";"three"])) (Bimap_multi_module.max_elt t7) in 
    let () = assert_equal (30,["triple";"tres";"three"]) (Bimap_multi_module.max_elt_exn t7) in 

    let () = assert_equal (Some ("dos", [20])) (Bimap_multi_module.min_elt_reverse t7) in 
    let () = assert_equal ("dos", [20]) (Bimap_multi_module.min_elt_exn_reverse t7) in 
    let () = assert_equal (Some ("two", [20])) (Bimap_multi_module.max_elt_reverse t7) in 
    let () = assert_equal ("two",[20]) (Bimap_multi_module.max_elt_exn_reverse t7) in 

    let () = assert_equal (Some (10, ["one"])) (Bimap_multi_module.nth t7 0) in 
    let () = assert_equal (Some (20, ["two";"dos"])) (Bimap_multi_module.nth t7 1) in 
    let () = assert_equal (Some (30, ["triple";"tres";"three"])) (Bimap_multi_module.nth t7 2) in 
    let () = assert_equal (Some ("dos",[20])) (Bimap_multi_module.nth_reverse t7 0) in 
    let () = assert_equal (Some ("one",[10])) (Bimap_multi_module.nth_reverse t7 1) in 
    let () = assert_equal (Some ("three",[30])) (Bimap_multi_module.nth_reverse t7 2) in
    let () = assert_equal (Some ("tres",[30])) (Bimap_multi_module.nth_reverse t7 3) in 
    let () = assert_equal (Some ("triple",[30])) (Bimap_multi_module.nth_reverse t7 4) in 
    let () = assert_equal (Some ("two",[20])) (Bimap_multi_module.nth_reverse t7 5) in 

    let t8 = Bimap_multi_module.remove t7 ~key:20 in 
    let () = assert_equal None (Bimap_multi_module.find t8 ~key:20) in 
    let () = assert_equal None (Bimap_multi_module.find_reverse t8 ~key:"two") in 

    let t9 = Bimap_multi_module.remove_reverse t8 ~key:"triple" in 
    let () = assert_equal false (List.mem "triple" (Bimap_multi_module.find_exn t9 ~key:30)) in 
    let () = assert_equal true (List.mem "three" (Bimap_multi_module.find_exn t9 ~key:30)) in 
    let () = assert_equal true (List.mem "tres" (Bimap_multi_module.find_exn t9 ~key:30)) in 

    let t10 = Bimap_multi_module.remove_multi t9 ~key:30 in 
    let () = assert_equal false (List.mem "triple" (Bimap_multi_module.find_exn t10 ~key:30)) in 
    let () = assert_equal false (List.mem "tres" (Bimap_multi_module.find_exn t10 ~key:30)) in 
    let () = assert_equal true (List.mem "three" (Bimap_multi_module.find_exn t10 ~key:30)) in 
    let () = assert_equal 1 (Core.List.length (Bimap_multi_module.find_exn t10 ~key:30)) in 
    let () = assert_equal None (Bimap_multi_module.find_reverse t10 ~key:"tres") in 
    let () = assert_equal (Some [30]) (Bimap_multi_module.find_reverse t10 ~key:"three") in 

    let t11 = Bimap_multi_module.update t10 ~key:30 ~f:(fun l ->
		  match l with
		  | Some elems ->
		     let elems2 = if not (List.mem "three" elems) then
				    (*let () = print_n_flush "\nAdding three..." in *)
				    ("three" :: elems)
				  else elems in
		     let elems3 = if not (List.mem "tres" elems2) then
				    (*let () = print_n_flush "\nAdding tres..." in *)
				    ("tres" :: elems2)
				  else elems2 in
		     let elems4 = if not (List.mem "triple" elems3) then
				    (*let () = print_n_flush "\nAdding triple..." in*)
				    ("triple" :: elems3)
				  else elems3 in
		     elems4
		  | None -> ["three";"tres";"triple"]
		) in 
      let () = assert_equal true (List.mem "triple" (Bimap_multi_module.find_exn t11 ~key:30)) in 
      let () = assert_equal true (List.mem "tres" (Bimap_multi_module.find_exn t11 ~key:30)) in 
      let () = assert_equal true (List.mem "three" (Bimap_multi_module.find_exn t11 ~key:30)) in 
      let () = assert_equal (Some [30]) (Bimap_multi_module.find_reverse t11 ~key:"tres") in 
      let () = assert_equal (Some [30]) (Bimap_multi_module.find_reverse t11 ~key:"triple") in 
      assert_equal (Some [30]) (Bimap_multi_module.find_reverse t11 ~key:"three");;

                   
  let suite =
    "suite">:::
      ["test-1">:: test1;
       "test-2">:: test2;
       "test-3">:: test3;
       "test-3b">:: test3b;
       "test-4">:: test4;
       "test-5">:: test5;
       "test-6">:: test6;
       "test-7">:: test7;
       "test-8">:: test8;
       "test-9">:: test9;
       "test-10">:: test10;
       "test-11">:: test11;
       "test-12">:: test12;
       "test-13">:: test13;
       "test-13b">:: test13b;
       "test-14">:: test14;
       "test-15">:: test15;
       "test-16">:: test16;
       "test-17">:: test17;
       "test-18">:: test18;
       "test-19">:: test19;
       "test-20">:: test20;
      ];;

  let () =
    run_test_tt_main suite
	
end 

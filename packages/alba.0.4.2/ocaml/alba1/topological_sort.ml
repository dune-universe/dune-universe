(* Copyright (C) 2017 Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)


open Container

module type Application_data =
  sig
    type node
    type graph
    val compare: node -> node -> int
    val dependencies: node -> graph -> node list
    val string_of_node: node -> string
  end



module type S =
    sig
      type node
      type graph
      val print_cycle: Format.formatter -> node list -> unit
      val sort: node list -> graph -> (node list, node list) result
    end



module Make(AD: Application_data) =
  struct
    type node = AD.node
    type graph = AD.graph

    module EMap = Map.Make(struct
      let compare = AD.compare
      type t = AD.node
    end)

    type state = Work | Onstack | Done

    type t = {
        graph: graph;
        mutable work: node list;
        mutable stack: (node * node list) list;
        mutable sorted: node list;
        mutable map: state EMap.t
      }

    exception Cycle of node list


    let add_new_elements
        (lst:node list)
        (map: state EMap.t)
        : state EMap.t =
      List.fold_left
        (fun map e ->
          if EMap.mem e map then
            map
          else
            EMap.add e Work map
        )
        map
        lst



    let make (w: node list) (graph:graph): t =
      let map = add_new_elements w EMap.empty in
      { graph  = graph;
        work   = w;
        stack  = [];
        sorted = [];
        map    = map}


    let is_in_work (e:node) (d:t): bool =
      try
        match EMap.find e d.map with
          Work ->
            true
        | _ ->
            false
      with Not_found ->
        assert false (* cannot happen *)


    let top_stack (d:t): node * node list =
      assert (d.stack <> []);
      List.hd d.stack


    let pop (d:t): unit =
      assert (d.stack <> []);
      d.stack <- List.tl d.stack



    let get_cycle (first:node) (d:t): node list =
      let rec get lst stack =
        match stack with
          [] ->
            assert false (* cannot happen, the first element must be
                            on the stack *)
        | (e,_) :: tail ->
            if AD.compare first e = 0 then
              first :: lst
            else
              get (e :: lst) tail
      in
      get [] d.stack




    let rec inner_loop (d:t): unit =
      match d.stack with
        [] ->
          ()
      | (e,[]) :: rest_stack ->
          d.stack <- rest_stack;
          d.sorted <- e :: d.sorted;
          d.map <- EMap.add e Done d.map;
          inner_loop d
      | (e, u :: rest_dependencies) :: rest_stack ->
          begin
            try
              match EMap.find u d.map with
                Work ->
                  let u_dependencies = AD.dependencies u d.graph
                  in
                  d.map <-
                    add_new_elements u_dependencies (EMap.add u Onstack d.map);
                  d.stack <-
                    (u,u_dependencies) :: (e,rest_dependencies) :: rest_stack;
                  inner_loop d
              | Onstack ->
                  raise (Cycle (get_cycle u d))
              | Done ->
                  d.stack <- (e,rest_dependencies) :: rest_stack;
                  inner_loop d
            with Not_found ->
              assert false  (* Cannot happen, all elements are in the map *)
          end



    let rec outer_loop (d:t): unit =
      assert (d.stack = []);
      match d.work with
        [] ->
          ()
      | e :: rest ->
          d.work <- rest;
          if is_in_work e d then
            begin
              let dependencies = AD.dependencies e d.graph in
              d.map <- add_new_elements dependencies d.map;
              d.stack <- [e,dependencies];
              d.map <- EMap.add e Onstack d.map;
              inner_loop d;
              assert (d.stack = []);
              outer_loop d
            end
          else
            outer_loop d


    let print_cycle (f:Format.formatter) (cycle: node list): unit =
      Format.fprintf f "@[<v>";
      List.iteri
        (fun i n ->
          let start =
            if i = 0 then
              "+-> "
            else
              "|   "
          in
          Format.fprintf
            f
            "%s%s@,"
            start
            (AD.string_of_node n)
        )
        cycle;
      Format.fprintf f "+ <-+@,"


    let sort (w:node list) (graph:graph): (node list, node list) result =
      let d = make w graph in
      try
        ignore(outer_loop d);
        Ok (List.rev d.sorted)
      with Cycle lst ->
        Error lst

  end

type order =
  | Asc
  | Desc

module type Comparable = Comparable.S

exception Invalid_interval = Interval.Invalid_interval

module Make (Bound_compare : Comparable) = struct
  module Bound = Bound.Make (Bound_compare)
  module Interval = Interval.Make (Bound)

  module Tree = struct
    type 'v node =
      { interval : Interval.t
      ; left : 'v t
      ; right : 'v t
      ; height : int
      ; max : Bound.t
      ; min : Bound.t
      ; values : 'v list
      }

    and 'v t = 'v node option

    let height node = match node with None -> 0 | Some n -> n.height

    let max_bound (interval : Interval.t) left right =
      let mid = interval.high in
      match left, right with
      | None, None ->
        mid
      | None, Some r ->
        Bound.max_upper mid r.max
      | Some l, None ->
        Bound.max_upper mid l.max
      | Some l, Some r ->
        Bound.max_upper mid (Bound.max_upper l.max r.max)

    let min_bound (interval : Interval.t) left right =
      let mid = interval.low in
      match left, right with
      | None, None ->
        mid
      | None, Some r ->
        Bound.min_lower mid r.min
      | Some l, None ->
        Bound.min_lower mid l.min
      | Some l, Some r ->
        Bound.min_lower mid (Bound.min_lower l.min r.min)

    let create interval left right values =
      let height = max (height left) (height right) + 1 in
      let max = max_bound interval left right in
      let min = min_bound interval left right in
      { interval; left; right; height; max; min; values }

    let leaf interval values = Some (create interval None None values)

    let size = function
      | None ->
        0
      | Some node ->
        let rec go { left; right; values; _ } size =
          let size = size + List.length values in
          match left, right with
          | None, None ->
            size
          | Some lt, None ->
            go lt size
          | None, Some rt ->
            go rt size
          | Some lf, Some rt ->
            let size = go lf size in
            go rt size
        in
        go node 0

    let rec find interval = function
      | None ->
        None
      | Some node ->
        let ivl_cmp = Interval.compare interval node.interval in
        if ivl_cmp = 0 then
          Some node
        else if ivl_cmp < 0 then
          find interval node.left
        else
          find interval node.right

    let balance_factor { left; right; _ } = height left - height right

    let replace_left node new_left =
      create node.interval new_left node.right node.values

    let replace_right node new_right =
      create node.interval node.left new_right node.values

    let rotate_right node =
      let pivot = Option.get node.left in
      let new_right = replace_left node pivot.right in
      replace_right pivot (Some new_right)

    let rotate_left node =
      let pivot = Option.get node.right in
      let new_left = replace_right node pivot.left in
      replace_left pivot (Some new_left)

    let balance node =
      let bf = balance_factor node in
      if bf < -1 then
        let right = Option.get node.right in
        if balance_factor right > 0 then
          replace_right node (Some (rotate_right right)) |> rotate_left
        else
          rotate_left node
      else if bf > 1 then
        let left = Option.get node.left in
        if balance_factor left < 0 then
          replace_left node (Some (rotate_left left)) |> rotate_right
        else
          rotate_right node
      else
        node

    let rec add interval value = function
      | None ->
        leaf interval [ value ]
      | Some node ->
        let ivl_cmp = Interval.compare interval node.interval in
        let res =
          if ivl_cmp < 0 then
            let insert_left =
              match node.left with
              | None ->
                leaf interval [ value ]
              | lt ->
                add interval value lt
            in
            replace_left node insert_left
          else if ivl_cmp > 0 then
            let insert_right =
              match node.right with
              | None ->
                leaf interval [ value ]
              | rt ->
                add interval value rt
            in
            replace_right node insert_right
          else
            { node with values = value :: node.values }
        in
        Some (balance res)

    let rec add_replace_all interval values = function
      | None ->
        leaf interval values
      | Some node ->
        let ivl_cmp = Interval.compare interval node.interval in
        let res =
          if ivl_cmp < 0 then
            let insert_left =
              match node.left with
              | None ->
                leaf interval values
              | lt ->
                add_replace_all interval values lt
            in
            replace_left node insert_left
          else if ivl_cmp > 0 then
            let insert_right =
              match node.right with
              | None ->
                leaf interval values
              | rt ->
                add_replace_all interval values rt
            in
            replace_right node insert_right
          else
            { node with values }
        in
        Some (balance res)

    let rec min_interval node =
      match node.left with None -> node.interval | Some lt -> min_interval lt

    let remove_by interval value_rm_fn = function
      | None ->
        None
      | Some node ->
        let rec remove node =
          let ivl_cmp = Interval.compare interval node.interval in
          let res =
            if ivl_cmp = 0 then
              let values =
                List.filter (fun v -> not (value_rm_fn v)) node.values
              in
              match values, node.left, node.right with
              | _ :: _, _, _ ->
                Some { node with values }
              | _, None, None ->
                None
              | _, Some lt, None ->
                Some lt
              | _, None, Some rt ->
                Some rt
              | _, Some _, Some rt ->
                let successor = min_interval rt in
                let rt = remove rt in
                let node = create successor node.left rt values in
                Some node
            else if ivl_cmp < 0 then
              match node.left with
              | None ->
                Some node
              | Some lt ->
                let lt = remove lt in
                Some (replace_left node lt)
            else
              match node.right with
              | None ->
                Some node
              | Some rt ->
                let rt = remove rt in
                Some (replace_right node rt)
          in
          match res with None -> None | Some n -> Some (balance n)
        in
        remove node

    let fold_bf fn acc = function
      | None ->
        acc
      | Some node ->
        let queue = Queue.create () in
        Queue.add node queue;
        let rec iter acc =
          match Queue.take_opt queue with
          | None ->
            acc
          | Some node ->
            Option.iter (fun lt -> Queue.add lt queue) node.left;
            Option.iter (fun rt -> Queue.add rt queue) node.right;
            let acc = fn acc node in
            iter acc
        in
        iter acc
  end

  module Gen = struct
    open Tree

    type 'v t =
      { node : 'v node option
      ; stack : 'v node list
      ; order : order
      ; go_left : 'v node -> bool
      ; go_right : 'v node -> bool
      ; emit : 'v node -> bool
      }

    let go _node = true

    let create ?(go_left = go) ?(go_right = go) ?(emit = go) order node =
      { node; stack = []; order; go_left; go_right; emit }

    let filter_option fn = function
      | Some v as opt ->
        if fn v then opt else None
      | None ->
        None

    let rec next { node; stack; order; go_left; go_right; emit } =
      match node, stack with
      | Some node, stack ->
        let stack = node :: stack in
        let next_node =
          match order with
          | Asc ->
            filter_option go_left node.left
          | Desc ->
            filter_option go_right node.right
        in
        next { node = next_node; stack; order; go_left; go_right; emit }
      | None, node :: stack ->
        let next_node =
          match order with
          | Asc ->
            filter_option go_right node.right
          | Desc ->
            filter_option go_left node.left
        in
        if emit node then
          Some
            ( (node.interval, node.values)
            , { node = next_node; stack; order; go_left; go_right; emit } )
        else
          next { node = next_node; stack; order; go_left; go_right; emit }
      | None, [] ->
        None

    let fold fn acc gen =
      let rec iter acc = function
        | Some ((ivl, values), gen) ->
          let acc = fn acc ivl values in
          iter acc (next gen)
        | None ->
          acc
      in
      iter acc (next gen)
  end

  type 'v t = 'v Tree.t

  let empty = None

  let size = Tree.size

  let cardinal = size

  let add = Tree.add

  let find_opt interval map =
    let open Tree in
    Tree.find interval map |> Option.map (fun n -> n.values)

  let find interval map =
    match find_opt interval map with None -> raise Not_found | Some v -> v

  let mem interval map = Tree.find interval map |> Option.is_some

  let query_interval ?(order = Asc) query =
    let open Interval in
    let open Tree in
    let go_left left =
      match left.max, query.low with
      | Included max, Included low ->
        max >= low
      | Included max, Excluded low
      | Excluded max, Included low
      | Excluded max, Excluded low ->
        max > low
      | _ ->
        true
    in
    let go_right right =
      match right.min, query.high with
      | Included min, Included high ->
        min <= high
      | Included min, Excluded high
      | Excluded min, Included high
      | Excluded min, Excluded high ->
        min < high
      | _ ->
        true
    in
    let emit node = Interval.overlaps query node.interval in
    Gen.create ~go_left ~go_right ~emit order

  let query_interval_list query map =
    query_interval ~order:Desc query map
    |> Gen.fold (fun xs ivl values -> (ivl, values) :: xs) []

  let remove_by = Tree.remove_by

  let remove_interval interval = remove_by interval (fun _ -> true)

  let generator ?(order = Asc) map = Gen.create order map

  let fold fn map acc =
    Gen.create Asc map |> Gen.fold (fun acc ivl values -> fn ivl values acc) acc

  let mapi fn =
    Tree.fold_bf
      (fun map node ->
        Tree.add_replace_all
          node.interval
          Tree.(fn node.interval node.values)
          map)
      empty

  let map fn = mapi (fun _ values -> fn values)

  let iteri fn map =
    Gen.create Asc map |> Gen.fold (fun () ivl values -> fn ivl values) ()

  let iter fn = iteri (fun _ivl values -> fn values)

  let to_list map =
    Gen.create Desc map
    |> Gen.fold (fun xs ivl values -> (ivl, values) :: xs) []

  let rec seq_unfold f u () =
    let open Seq in
    match f u with None -> Nil | Some (x, u) -> Cons (x, seq_unfold f u)

  let to_seq map = Gen.create Asc map |> seq_unfold Gen.next
end

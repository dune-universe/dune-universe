type 'a t = {
    length : int;
    list : 'a list;
  }

let empty = { length = 0; list = [] }

let add value { length; list } =
  length, { length = succ length; list = value :: list }

let length { length; _ } =
  length

let to_array ({ length; list } : 'a t) : 'a array =
  match list with
  | [] -> [||]
  | hd :: tl ->
      let result = Array.make length hd in
      let rec fill i list =
        match list with
        | [] -> ()
        | hd :: tl ->
            result.(i) <- hd;
            fill (pred i) tl in
      fill (length - 2) tl;
      result

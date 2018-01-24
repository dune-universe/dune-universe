module Option = struct
  let bind e f = match e with
    | Some v -> f v
    | None -> None
  let return v = Some v
end

(* pattern guard => do_ *)
let t = match () with
  | () when Some x <-- Some 1 -> do_;
      Option.return x
  | _ -> assert false

(* do_ => pattern guard *)
let t2 = Option.do_;
  match () with
  | () when Some x <-- Some 1 -> return x
  | _ -> assert false

(* do_ => do_ *)
let t3 = Option.do_;
  x <-- (Option.do_;
         return 1);
  return x

(* pattern guard => pattern guard *)
let t4 =
  match () with
  | () when Some x <-- begin match () with
                       | () when Some x <-- Some 1 -> Some x
                       | _ -> assert false
                       end ->
      begin match () with
      | () when Some y <-- Some 1 -> x + y
      | _ -> assert false
      end
  | _ -> assert false

(* list comp => list comp *)
let t5 = [%comp x || x <-- [%comp x || x <-- [1;2;3]]]

(* pattern guard => list comp *)
let t6 = match () with
  | () when Some x <-- Some [%comp x || x <-- [1; 2]] -> x
  | _ -> assert false

(* list comp => pattern guard *)
let t7 = [%comp (match x with 2 when Some x <-- Some 1 -> x
                 | _ -> assert false)
         || x <-- (match () with () when Some x <-- Some 2 -> [x]
                   | _ -> assert false);
            match x with n when m <-- n; m = 2 -> true | _ -> false
         ]
                    
(* do => list comp *)
let t8 = 
  let bind x f = f x 
  and return x = x in do_;
  x <-- return [%comp x || x <-- [1;2]];
  return x

(* list comp => do *)
let t9 = [%comp x || x <-- [ ( Option.do_; (* we need parens!! *)
                               x <-- return 2;
                               return [x] ) ] ]

(** Reverses a string. eg, "kayak" becomes "kayak" *)
let string_rev s =
  let len = String.length s in
  String.init len (fun i -> s.[len - 1 - i])

(** string_fold f a "c1c2 ... cn" is f (... (f (f a c1) c2) ...) cn. *)
let string_fold (f:'a -> char -> 'a) acc str =
  let n = String.length str - 1 in
  let rec aux acc i =
    if i > n then acc
    else aux (f acc str.[i]) (i+1)
  in aux acc 0

(** generalization of String.split_on_char, that takes a predicates
   over chars instead of a single char *)
let split_on str (p:char -> bool) =
  let str,_ = string_fold (fun (acc,(i,j)) c ->
                  if p c then
                    if i = j then (acc,(i+1,j+1))
                    else let str' = String.sub str i (j-i) in
                         (str'::acc),(j+1,j+1)
                  else if j = String.length str - 1 then
                    let str' = String.sub str i (j-i +1) in
                         (str'::acc),(j+1,j+1)
                  else acc,(i,j+1)
                ) ([],(0,0)) str
  in List.rev str

(** removes beginning zeros of a string *)
let trail_beginning_zeros str =
  let cpt = ref 0 in
  String.iter (function '0' -> incr cpt; | _ -> ()) str;
  String.sub str !cpt (String.length str - !cpt)

(** removes ending zeros of a string *)
let trail_ending_zeros str =
  str |> string_rev |> trail_beginning_zeros |> string_rev

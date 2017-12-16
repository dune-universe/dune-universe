module Focused_list =
struct
  type 'a t= List_zip of ('a list*'a*'a list)
  (** This type aims to implement a zipper for lists. The context is
      the first parameter. It represents the list of the elements {e
      in reverse order} that has been traversed to reach the focused
      element (the second parameter). The last element is the
      remaining elements of the list. *)

  exception Empty_list
  exception End_of_list


  let init = function
    | [] -> raise Empty_list
    | h::tl -> List_zip([],h,tl)


  let forward = function
    | List_zip (c,f,h::tl) -> List_zip(f::c,h,tl)
    | List_zip (_,_,[]) -> raise End_of_list


  let backward = function
    | List_zip (h::tl,f,r) -> List_zip(tl,h,f::r)
    | List_zip ([],_,_) -> raise End_of_list


  let rec fold f acc = function
    | List_zip ((c,a,[]) as focus) ->  f acc focus
    | List_zip ((c,a,h::tl) as focus) as lst -> fold f (f acc focus) (forward lst)
end

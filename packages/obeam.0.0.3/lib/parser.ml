(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

(**)
module Combinator = struct
  let rec repeat p ctx =
    match p ctx with
    | Ok ((result, rest) as c) ->
       repeat p c
    | Error _ ->
       Ok ctx

  (* or *)
  let (/) pa pb ctx =
    match pa ctx with
    | Ok ((result, rest) as c) ->
       Ok c
    | Error _ ->
       begin
         match pb ctx with
         | Ok ((result, rest) as c) ->
            Ok c
         | Error _ ->
            Error ()
       end

  (* and *)
  let (>>) pa pb ctx =
    match pa ctx with
    | Ok ((result, rest) as c) ->
       pb c
    | Error _ as err ->
       err

  (* bind *)
  let (>>=) pa pb ctx =
    match pa ctx with
    | Ok ((result, rest) as c) ->
       pb result c
    | Error _ as err ->
       err

  (* return *)
  let return a (_, rest) =
    Ok (a, rest)

  (**)
  let act p action ctx =
    match p ctx with
    | Ok (result, rest) ->
       let (prev, _) = ctx in
       let new_result = action result prev in
       Ok (new_result, rest)
    | Error _ as err ->
       err

  (**)
  let list p n buf =
    let rec fold (xs, sub_buf) c =
      match c = n with
      | true ->
         Ok (xs, sub_buf)
      | _ ->
         begin
           match p (xs, sub_buf) with
           | Ok (v, rest) ->
              fold (v :: xs, rest) (c+1)
           | Error _ as err ->
              err
         end
    in
    match fold ([], buf) 0 with
    | Ok (rev_list, buf) ->
       Ok (List.rev rev_list, buf)
    | Error _ as err ->
       err

  (**)
  let map f res =
    match res with
    | Ok (v, rest) -> Ok (f v, rest)
    | Error _ as err -> err
end

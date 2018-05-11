(********************************************************************************)
(*  Prelude.ml
    Copyright (c) 2018 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the ISC license.
    See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(** {1 Signatures}                                                              *)
(********************************************************************************)

module type TESTABLE =
sig
    type t

    val pp: Format.formatter -> t -> unit
    val equal: t -> t -> bool
end


(********************************************************************************)
(** {1 Modules}                                                                 *)
(********************************************************************************)

module Array =
struct
    include Array

    let binary_search comp el arr =
        let rec loop low high =
            if low > high
            then
                None
            else
                let mid = (high + low) / 2 in
                match comp el arr.(mid) with
                    | 0            -> Some mid
                    | x when x < 0 -> loop low (mid - 1)
                    | _            -> loop (mid + 1) high in
        loop 0 (Array.length arr - 1)
end

module String =
struct
    include String

    let edit_distance a b =
        let min2 (x: int) (y: int) = if x < y then x else y in
        let min3 x y z = min2 (min2 x y) z in
        let na = String.length a in
        let nb = String.length b in
        let m = Array.make_matrix (na + 1) (nb + 1) 0 in
        for i = 0 to na do m.(i).(0) <- i done;
        for j = 0 to nb do m.(0).(j) <- j done;
        for i = 1 to na do
            for j = 1 to nb do
                let cost = if a.[i-1] = b.[j-1] then 0 else 1 in
                m.(i).(j) <- min3 (m.(i-1).(j) + 1) (m.(i).(j-1) + 1) (m.(i-1).(j-1) + cost);
                if i > 2 && j > 2 && a.[i-1] = b.[j-2] && a.[i-2] = b.[j-1]
                then m.(i).(j) <- min2 m.(i-1).(j-1) (m.(i-2).(j-2) + cost)
            done
        done;
        m.(na).(nb)
end

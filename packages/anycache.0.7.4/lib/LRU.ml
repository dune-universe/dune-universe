(******************************************************************************)
(* Copyright (c) 2017 Török Edwin <edwin@etorok.net>                          *)
(* Copyright (c) 2014-2016 Skylable Ltd. <info-copyright@skylable.com>        *)
(*                                                                            *)
(* Permission to use, copy, modify, and/or distribute this software for any   *)
(* purpose with or without fee is hereby granted, provided that the above     *)
(* copyright notice and this permission notice appear in all copies.          *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES   *)
(* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF           *)
(* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR    *)
(* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES     *)
(* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN      *)
(* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF    *)
(* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.             *)
(******************************************************************************)

(* 2Q: A Low Overhead High Performance Buffer Management Replacement Algorithm
 * Theodore Johnson, Dennis Shasha
 * 1994
*)

module Make(Key:Map.OrderedType) = struct
  module type S = sig
    type v
    val find : Key.t -> v option
    val replace : Key.t -> v -> unit
  end

  type 'a cache = (module S with type v = 'a)

  let const_1 _ = 1

  let create (type a) ?(weight=const_1) n : a cache =
    (module struct
      type v = a
      module V = struct
        type t = a
        let weight = weight
      end
      module L = Lru.F.Make(Key)(V)

      let amain = ref (L.empty (n/2))
      let a1in = ref (L.empty (n/4))
      let a1out = ref (L.empty (n/4))
      let total_size = n

      let has_room () =
        L.size !a1in +
        L.size !a1out +
        L.size !amain <
        total_size

      let add ?trim k v lru = lru := L.add ?trim k v !lru

      let add_a1out k v = add ~trim:false k v a1out

      let add_a1in k v = add ~trim:false k v a1in

      let add_main k v =
        (* do not put it on A1out, it hasn't been accessed for a while *)
        add ~trim:true k v amain

      let pop_if_full t =
        let lru = !t in
        if L.size lru > L.capacity lru then
          match L.pop_lru lru with
          | None -> assert false
          | Some (kv, t') ->
              t := t';
              Some kv
        else None

      let reclaim () =
        begin match pop_if_full a1in with
        | Some (ykey, yval) ->
            add_a1out ykey yval
        | None -> ()
        end;
        if not (has_room ()) then
          ignore (pop_if_full a1out)

      let find_update key lru =
        match L.find key !lru with
        | Some (data, t) ->
            lru := t;
            Some data
        | None -> None

      let find key =
        match find_update key amain with
        | Some _ as result -> result
        | None ->
            match L.find ~promote:false key !a1in with
            | Some (data, _) -> Some data
            | None ->
                match L.find ~promote:false key !a1out with
                | Some (data, _) ->
                    a1out := L.remove key !a1out;
                    add_main key data;
                    Some data
                | None -> None

      let find_and_replace key data lru =
        let found = L.mem key !lru in
        if found then
          lru := L.add ~trim:false key data !lru;
        found

      let replace key data =
        if not (find_and_replace key data amain) then
          if not (find_and_replace key data a1in) then
            if L.mem key !a1out then
              (*BISECT-IGNORE-BEGIN*)
              begin
                a1out := L.remove key !a1out;
                add_main key data;
              end
              (*BISECT-IGNORE-END*)
            else begin
              reclaim ();
              add_a1in key data
            end
    end : S with type v = a)

  let find (type a) (module Cache : S with type v = a) = Cache.find

  let replace (type a) (module Cache : S with type v = a) = Cache.replace
end

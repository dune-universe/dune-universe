open Core
open Util

module type PartialOrdered = sig
  include Data
  val po : t -> t -> int option
end

module DataToPO(D:Data) = struct
  include D
  let po = Option.some %% D.compare
end

module MaxForestOf(PO:PartialOrdered) = struct
  type t = PO.t list

  let empty = []

  let rec insert
      (mf:t)
      (x:PO.t)
    : t =
    begin match mf with
      | [] -> [x]
      | h::t ->
        begin match PO.po x h with
          | None -> h::(insert t x)
          | Some c ->
            if c > 0 then
              insert t x
            else
              mf
        end
    end

  let insert_list
      (mf:t)
      (xs:PO.t list)
    : t =
    List.fold
      ~f:insert
      ~init:mf
      xs

  let of_list
      (xs:PO.t list)
    : t =
    insert_list
      empty
      xs

  let singleton
      (x:PO.t)
    : t =
    [x]

  let to_list
      (mf:t)
    : PO.t list =
    mf

  let merge
      (mf1:t)
      (mf2:t)
    : t =
    insert_list mf1 mf2

  let multi_merge
      (mfs:t list)
    : t =
    List.fold
      ~f:merge
      ~init:empty
      mfs

  let cartesian_map
      ~(f:PO.t -> PO.t -> PO.t)
      (mf1:t)
      (mf2:t)
    : t =
    of_list
      (cartesian_map
         ~f
         mf1
         mf2)

  let map
      ~(f:PO.t -> PO.t)
      (mf:t)
    : t =
    List.map
      ~f
      mf
end

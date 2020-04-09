module type ENV = sig
  type dict

  type env = {
    dict : dict;
    vars : Vars.t;
    op : Types.fn
  }
end

module Make (Dict : sig type t end) : ENV with type dict = Dict.t

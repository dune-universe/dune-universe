(* @todo [@@autodoc.inline] *)

module type MT = sig
  (** Doc for include *)
  include sig
    type t
  end
  (** Doc' for include *)

  (** Doc for hidden include *)
  include sig
    type u
  end [@@autodoc.hide]
  (** Doc' for hidden include *)

  (** Doc for ModuleType *)
  module type ModuleType = sig
    (** Doc for ModuleType.t *)
    type t
    (** Doc' for ModuleType.t *)

    (** Floating doc in ModuleType *)
  end [@@autodoc.hide]
  (** Doc' for ModuleType *)

  module A: sig
    (** Doc for include *)
    include ModuleType
    (** Doc' for include *)
  end

  (** Doc for ModuleTypeAlias *)
  module type ModuleTypeAlias = ModuleType [@@autodoc.hide]
  (** Doc' for ModuleTypeAlias *)

  module B: sig
    (** Doc for indirect include *)
    include ModuleTypeAlias
    (** Doc' for indirect include *)
  end
end

include MT

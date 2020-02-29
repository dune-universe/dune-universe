module type UnaryS = sig
  type 'a t
end

module type UnaryMakeS = sig
  type 'a t

  val make : unit -> 'a t
end

module type MetapointsS = sig
  type 'a x

  type t = {
      expr : Parsetree.expression x;
      pat : Parsetree.pattern x;
      typ : Parsetree.core_type x;
      class_type : Parsetree.class_type x;
      class_type_field : Parsetree.class_type_field x;
      class_expr : Parsetree.class_expr x;
      class_field : Parsetree.class_field x;
      module_type : Parsetree.module_type x;
      module_expr : Parsetree.module_expr x;
      signature_item : Parsetree.signature_item x;
      structure_item : Parsetree.structure_item x;
    }
end

module type QuotationsS = sig
  type 'a x

  type t = {
      expr : Parsetree.expression x;
      pat : Parsetree.pattern x;
      typ : Parsetree.core_type x;
      signature : Parsetree.signature x;
      signature_item : Parsetree.signature_item x;
      structure : Parsetree.structure x;
      structure_item : Parsetree.structure_item x;
    }
end

module type MetapointS = sig
  include Metapp_preutils.ExtensibleS

  module MetapointAccessor (Collector : MetapointsS) : sig
    val get : Collector.t -> t Collector.x

    val set : t Collector.x -> Collector.t -> Collector.t
  end
end

module type QuotationS = sig
  include Metapp_preutils.VisitableS

  val of_payload : Parsetree.payload -> t

  module QuotationAccessor (Collector : QuotationsS) : sig
    val get : Collector.t -> t Collector.x

    val set : t Collector.x -> Collector.t -> Collector.t
  end
end

module Exp = struct
  include Metapp_preutils.Exp

  module MetapointAccessor (Collector : MetapointsS) = struct
    let get (c : Collector.t) : t Collector.x = c.expr

    let set (x : t Collector.x) (c : Collector.t) =
      { c with expr = x }
  end

  module QuotationAccessor (Collector : QuotationsS) = struct
    let get (c : Collector.t) : t Collector.x = c.expr

    let set (x : t Collector.x) (c : Collector.t) =
      { c with expr = x }
  end
end

module Pat = struct
  include Metapp_preutils.Pat

  module MetapointAccessor (Collector : MetapointsS) = struct
    let get (c : Collector.t) : t Collector.x = c.pat

    let set (x : t Collector.x) (c : Collector.t) =
      { c with pat = x }
  end

  module QuotationAccessor (Collector : QuotationsS) = struct
    let get (c : Collector.t) : t Collector.x = c.pat

    let set (x : t Collector.x) (c : Collector.t) =
      { c with pat = x }
  end
end

module Typ = struct
  include Metapp_preutils.Typ

  module MetapointAccessor (Collector : MetapointsS) = struct
    let get (c : Collector.t) : t Collector.x = c.typ

    let set (x : t Collector.x) (c : Collector.t) =
      { c with typ = x }
  end

  module QuotationAccessor (Collector : QuotationsS) = struct
    let get (c : Collector.t) : t Collector.x = c.typ

    let set (x : t Collector.x) (c : Collector.t) =
      { c with typ = x }
  end
end

module Cty = struct
  include Metapp_preutils.Cty

  module MetapointAccessor (Collector : MetapointsS) = struct
    let get (c : Collector.t) : t Collector.x = c.class_type

    let set (x : t Collector.x) (c : Collector.t) =
      { c with class_type = x }
  end
end

module Ctf = struct
  include Metapp_preutils.Ctf

  module MetapointAccessor (Collector : MetapointsS) = struct
    let get (c : Collector.t) : t Collector.x = c.class_type_field

    let set (x : t Collector.x) (c : Collector.t) =
      { c with class_type_field = x }
  end
end

module Cl = struct
  include Metapp_preutils.Cl

  module MetapointAccessor (Collector : MetapointsS) = struct
    let get (c : Collector.t) : t Collector.x = c.class_expr

    let set (x : t Collector.x) (c : Collector.t) =
      { c with class_expr = x }
  end
end

module Cf = struct
  include Metapp_preutils.Cf

  module MetapointAccessor (Collector : MetapointsS) = struct
    let get (c : Collector.t) : t Collector.x = c.class_field

    let set (x : t Collector.x) (c : Collector.t) =
      { c with class_field = x }
  end
end

module Mty = struct
  include Metapp_preutils.Mty

  module MetapointAccessor (Collector : MetapointsS) = struct
    let get (c : Collector.t) : t Collector.x = c.module_type

    let set (x : t Collector.x) (c : Collector.t) =
      { c with module_type = x }
  end
end

module Mod = struct
  include Metapp_preutils.Mod

  module MetapointAccessor (Collector : MetapointsS) = struct
    let get (c : Collector.t) : t Collector.x = c.module_expr

    let set (x : t Collector.x) (c : Collector.t) =
      { c with module_expr = x }
  end
end

module Sig = struct
  include Metapp_preutils.Sig

  module QuotationAccessor (Collector : QuotationsS) = struct
    let get (c : Collector.t) : t Collector.x = c.signature

    let set (x : t Collector.x) (c : Collector.t) =
      { c with signature = x }
  end
end

module Sigi = struct
  include Metapp_preutils.Sigi

  module MetapointAccessor (Collector : MetapointsS) = struct
    let get (c : Collector.t) : t Collector.x = c.signature_item

    let set (x : t Collector.x) (c : Collector.t) =
      { c with signature_item = x }
  end

  module QuotationAccessor (Collector : QuotationsS) = struct
    let get (c : Collector.t) : t Collector.x = c.signature_item

    let set (x : t Collector.x) (c : Collector.t) =
      { c with signature_item = x }
  end
end

module Str = struct
  include Metapp_preutils.Str

  module QuotationAccessor (Collector : QuotationsS) = struct
    let get (c : Collector.t) : t Collector.x = c.structure

    let set (x : t Collector.x) (c : Collector.t) =
      { c with structure = x }
  end
end

module Stri = struct
  include Metapp_preutils.Stri

  module MetapointAccessor (Collector : MetapointsS) = struct
    let get (c : Collector.t) : t Collector.x = c.structure_item

    let set (x : t Collector.x) (c : Collector.t) =
      { c with structure_item = x }
  end

  module QuotationAccessor (Collector : QuotationsS) = struct
    let get (c : Collector.t) : t Collector.x = c.structure_item

    let set (x : t Collector.x) (c : Collector.t) =
      { c with structure_item = x }
  end
end

module type MetapointsMakeS = sig
  include MetapointsS

  module Make (X : UnaryMakeS with type 'a t = 'a x) : sig
    val make : unit -> t
  end
end

module type MetapointsWithMakeS = sig
  include MetapointsS

  val make : unit -> t
end

module type QuotationsMakeS = sig
  include QuotationsS

  module Make (X : UnaryMakeS with type 'a t = 'a x) : sig
    val make : unit -> t
  end
end

module type QuotationsWithMakeS = sig
  include QuotationsS

  val make : unit -> t
end

module Metapoints (X : UnaryS)
    : MetapointsMakeS with type 'a x = 'a X.t = struct
  module rec Sub : sig
    include MetapointsS with type 'a x = 'a X.t
  end = struct
    include Sub
  end

  include Sub

  module Make (X : UnaryMakeS with type 'a t = 'a X.t) = struct
    let make () = {
      expr = X.make ();
      pat = X.make ();
      typ = X.make ();
      class_type = X.make ();
      class_type_field = X.make ();
      class_expr = X.make ();
      class_field = X.make ();
      module_type = X.make ();
      module_expr = X.make ();
      signature_item = X.make ();
      structure_item = X.make ();
    }
  end
end

module Quotations (X : UnaryS)
    : QuotationsMakeS with type 'a x = 'a X.t = struct
  module rec Sub : sig
    include QuotationsS with type 'a x = 'a X.t
  end = struct
    include Sub
  end

  include Sub

  module Make (X : UnaryMakeS with type 'a t = 'a X.t) = struct
    let make () = {
      expr = X.make ();
      pat = X.make ();
      typ = X.make ();
      signature = X.make ();
      signature_item = X.make ();
      structure = X.make ();
      structure_item = X.make ();
    }
  end
end

module MetapointName = Metapoints (struct type 'a t = string end)

let metapoint_name : MetapointName.t =
  {
    expr = "expr";
    pat = "pat";
    typ = "typ";
    class_type = "class_type";
    class_type_field = "class_type_field";
    class_expr = "class_expr";
    class_field = "class_field";
    module_type = "module_type";
    module_expr = "module_expr";
    signature_item = "signature_item";
    structure_item = "structure_item";
  }

module QuotationName = Quotations (struct type 'a t = string end)

let quotation_name : QuotationName.t =
  {
    expr = "expr";
    pat = "pat";
    typ = "typ";
    signature = "signature";
    signature_item = "signature_item";
    structure = "structure";
    structure_item = "structure_item";
  }

module MetapointsWithMake (X : UnaryMakeS) : MetapointsWithMakeS
with type 'a x = 'a X.t = struct
  include Metapoints (X)

  include Make (X)
end

module QuotationsWithMake (X : UnaryMakeS) : QuotationsWithMakeS
with type 'a x = 'a X.t = struct
  include Quotations (X)

  include Make (X)
end

module type Map = sig
  type 'a x

  type 'a y

  val map : 'a x -> 'a y
end

module MetapointMap (X : MetapointsS) (Y : MetapointsS)
    (M : Map with type 'a x = 'a X.x and type 'a y = 'a Y.x) = struct
  let map (x : X.t) : Y.t = {
    expr = M.map x.expr;
    pat = M.map x.pat;
    typ = M.map x.typ;
    class_type = M.map x.class_type;
    class_type_field = M.map x.class_type_field;
    class_expr = M.map x.class_expr;
    class_field = M.map x.class_field;
    module_type = M.map x.module_type;
    module_expr = M.map x.module_expr;
    signature_item = M.map x.signature_item;
    structure_item = M.map x.structure_item;
  }
end

module QuotationMap (X : QuotationsS) (Y : QuotationsS)
    (M : Map with type 'a x = 'a X.x and type 'a y = 'a Y.x) = struct
  let map (x : X.t) : Y.t = {
    expr = M.map x.expr;
    pat = M.map x.pat;
    typ = M.map x.typ;
    signature = M.map x.signature;
    signature_item = M.map x.signature_item;
    structure = M.map x.structure;
    structure_item = M.map x.structure_item;
  }
end

module OptionArray = struct
  type 'a t = 'a option array
end

module OptionArrayMetapoints = Metapoints (OptionArray)

module LocationArray = struct
  type _ t = Location.t array
end

module MetapointsLocation = Metapoints (LocationArray)

module rec ArrayQuotation : sig
  type context = {
      metapoints : OptionArrayMetapoints.t;
      loc : MetapointsLocation.t;
      subquotations : ArrayQuotations.t;
    }

  type 'a quotation = {
      context : context;
      fill : unit -> 'a;
    }

  type 'a t = 'a quotation array
end = struct
  include ArrayQuotation
end
and ArrayQuotations : QuotationsS with type 'a x = 'a ArrayQuotation.t =
    Quotations (ArrayQuotation)

type context = ArrayQuotation.context

let top_context : context option ref =
  ref None

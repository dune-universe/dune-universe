(***********************************************************************)
(*                                                                     *)
(*                           FaCiLe                                    *)
(*                 A Functional Constraint Library                     *)
(*                                                                     *)
(*            Nicolas Barnier, Pascal Brisset, LOG, CENA               *)
(*                                                                     *)
(* Copyright 2004 CENA. All rights reserved. This file is distributed  *)
(* under the terms of the GNU Lesser General Public License.           *)
(***********************************************************************)
(* $Id: facile.ml,v 1.22 2004/08/09 14:45:41 barnier Exp $ *)

module Data = Fcl_data
module Cstr = Fcl_cstr
module Genesis = Fcl_genesis
module Reify = Fcl_reify
module Alldiff = Fcl_alldiff
module Debug = Fcl_debug
module Goals = struct
  module GlArray = struct (* Deprecated, for backward compatibility *)
    include Fcl_goals.Array
    let iteri lab_one a = foralli (fun i x -> lab_one i x) a
    let iter f a = iteri (fun _i -> f) a
    let iter_hi h lab_one a = foralli ~select:h (fun i x -> lab_one i x) a
    let iter_h select lab_one a = iter_hi select (fun _ -> lab_one) a
    let iter2 f a b = iteri (fun i ai -> f ai b.(i)) a
    let labeling = forall Fcl_goals.indomain
  end
  module GlList = struct (* Deprecated, for backward compatibility *)
    include Fcl_goals.List
    let iter f l = forall f l
    let iter_h h = forall ~select:h
    let labeling = forall Fcl_goals.indomain
  end
  include Fcl_goals
end
module Sorting = Fcl_sorting
module Boolean = Fcl_boolean
module Expr = Fcl_expr
module Arith = Fcl_arith
module Domain = Fcl_domain
module Interval = Fcl_interval
module Stak = Fcl_stak
module FdArray = Fcl_fdArray
module Misc = Fcl_misc
module Var = Fcl_var
module Gcc = Fcl_gcc
module Opti = Fcl_opti
module Conjunto = Fcl_conjunto
module SetDomain = Fcl_setDomain
module Float = Fcl_float
module Invariant = Fcl_invariant
module Easy = struct
  let i2e = Arith.i2e
  let fd2e = Arith.fd2e
  let ( +~ ) = Arith.( +~ )
  let ( *~ ) = Arith.( *~ )
  let ( -~ ) = Arith.( -~ )
  let ( /~ ) = Arith.( /~ )
  let ( **~ ) = Arith.( **~ )
  let ( %~ ) = Arith.( %~ )
  let ( <=~ ) = Arith.( <=~ )
  let ( <~ ) = Arith.( <~ )
  let ( >~ )  = Arith.( >~ ) 
  let ( =~ )  = Arith.( =~ ) 
  let ( <>~ ) = Arith.( <>~ )
  let ( >=~ ) = Arith.( >=~ )
  let ( <=~~ ) = Arith.( <=~~ )
  let ( <~~ )  = Arith.( <~~ ) 
  let ( >~~ )  = Arith.( >~~ ) 
  let ( =~~ )  = Arith.( =~~ ) 
  let ( <>~~ ) = Arith.( <>~~ )
  let ( >=~~ ) = Arith.( >=~~ )
  let (&&~~) = Reify.(&&~~)
  let (||~~) = Reify.(||~~)
  let (=>~~) = Reify.(=>~~)
  let (<=>~~) = Reify.(<=>~~)
  let ( &&~ ) = Goals.( &&~ )
  let ( ||~ ) = Goals.( ||~ )
  module Fd = Var.Fd
  type ('a, 'b) concrete' = ('a, 'b) Var.concrete = Unk of 'a | Val of 'b
  type concrete_fd = (Fd.attr, Fd.elt) concrete'
end

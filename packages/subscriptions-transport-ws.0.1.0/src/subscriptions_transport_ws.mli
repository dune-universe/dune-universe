(*----------------------------------------------------------------------------
 *  Copyright (c) 2018-2020 António Nuno Monteiro
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its
 *  contributors may be used to endorse or promote products derived from this
 *  software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

module type SubscriptionsManager = sig
  type t

  val add : t -> string -> (unit -> unit) -> unit

  val create : int -> t

  val remove : t -> string -> unit

  val mem : t -> string -> bool

  val find_opt : t -> string -> (unit -> unit) option

  val iter : (string -> (unit -> unit) -> unit) -> t -> unit

  val clear : t -> unit
end

module SubscriptionsManager : sig
  type 't t

  val make : (module SubscriptionsManager with type t = 't) -> 't -> 't t
end

type 'a handlers =
  { schedule :
      'a
      -> on_recv:((Yojson.Basic.t, Yojson.Basic.t) result -> unit)
      -> on_close:(unit -> unit)
      -> unit
  ; destroy : 'a -> unit
  }

val on_recv
  :  't SubscriptionsManager.t
  -> subscribe:
       (variables:Graphql.Schema.variables
        -> ?operation_name:string
        -> string
        -> (( [< `Response of Yojson.Basic.t | `Stream of 'a ]
            , Yojson.Basic.t )
            result
            -> unit)
        -> unit)
  -> 'a handlers
  -> Websocketaf.Wsd.t
  -> opcode:Websocketaf.Websocket.Opcode.t
  -> is_fin:bool
  -> Bigstringaf.t
  -> off:int
  -> len:int
  -> unit

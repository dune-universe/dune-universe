module type ErrorType = sig
  type t
  val of_exn : exn -> t
  (* val of_string : string -> t *)
end

module type S = sig
  include Monad.S
  type +'a u
  module E : ErrorType
  val fail : E.t -> 'a t
  val lift : 'a u -> 'a t
  val lift_opt : 'a option -> 'a t -> 'a t
end

module Make(M : Monad.S)(E : ErrorType) : S with type +'a t = ('a, E.t) result M.t and module E = E and type +'a u = 'a M.t = struct
  module E = E
  type +'a u = 'a M.t
  let fail x = Error x |> M.return
  let lift_opt x em = match x with
    | None -> em
    | Some y -> Ok y |> M.return
  let lift bm = M.bind bm (fun x -> Ok x |> M.return)
  include Monad.Make(struct
    type +'a t = ('a, E.t) result M.t
    let return x = Ok x |> M.return
    let bind m f = M.bind m (function
      | Ok x ->
        (try
          f x
        with e ->
          e |> E.of_exn |> fail)
      | Error x -> fail x)
  end)
end

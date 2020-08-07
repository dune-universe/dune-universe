open Module_types


module type SIG_MIN =
  sig
    type _ t
    val return: 'a -> 'a t
    val (>>=): 'a t -> ('a -> 'b t) -> 'b t
  end

module type SIG_WITH_MAP =
  sig
    include SIG_MIN
    val map: ('a -> 'b) -> 'a t -> 'b t
  end





module type RESULT =
  sig
    include MONAD
    type error
    val throw: error -> 'a t
    val catch: 'a t -> (error -> 'a t) -> 'a t
    val continue: 'a t -> ('a -> 'z) -> (error -> 'z) -> 'z
  end




module Of_sig_with_map (M:SIG_WITH_MAP): MONAD with type 'a t = 'a M.t =
  struct
    include M

    let (>=>) f g a =
      f a >>= g

    let (<*>) mf m =
      mf >>= fun f -> map f m

    let join mm =
      mm >>= fun m -> m
  end


module Of_sig_min (M:SIG_MIN): MONAD with type 'a t = 'a M.t =
  struct
    include M

    let (>=>) f g a =
      f a >>= g

    let map (f:'a -> 'b) (m:'a t): 'b t =
      m >>= fun a -> return (f a)

    let (<*>) mf m =
      mf >>= fun f -> map f m

    let join mm =
      mm >>= fun m -> m
  end




module Identity =
struct
    include
        Of_sig_min (
        struct
            type 'a t = 'a
            let return (a: 'a): 'a t = a
            let (>>=) (m: 'a t) (f: 'a -> 'b t): 'b t =
                f m
        end
        )
    let eval (m: 'a t): 'a =
        m
end




module Result (E: ANY) =
  struct
    type error = E.t

    include
      Of_sig_min(
          struct
            type 'a t = ('a,error) result
            let return (a:'a): 'a t = Ok a
            let (>>=) (m:'a t) (f:'a -> 'b t): 'b t =
              match m with
              | Ok a -> f a
              | Error e -> Error e
          end
        )

    let throw (e:error): 'a t =
      Error e

    let catch (m:'a t) (f:error->'a t): 'a t =
      match m with
      | Ok _ -> m
      | Error e -> f e

    let continue (m:'a t) (f1:'a->'r) (f2:error->'r): 'r =
      match m with
      | Ok a ->
         f1 a
      | Error e ->
         f2 e
  end (* Result *)

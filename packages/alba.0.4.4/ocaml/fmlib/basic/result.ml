open Module_types



type ('a, 'e) t = ('a, 'e) result


let (>>=)
    (m: ('a, 'e) t)
    (f: 'a -> ('b, 'e) t)
    : ('b, 'e) t
    =
    match m with
    | Ok a ->
        f a
    | Error e ->
        Error e



let map (f: 'a -> 'b) (m: ('a, 'e) t): ('b, 'e) t =
    match m with
    | Ok a ->
        Ok (f a)
    | Error e ->
        Error e



let map_error (f: 'e1 -> 'e2) (m: ('a,'e1) t): ('a, 'e2) t =
    match m with
    | Ok a ->
        Ok a
    | Error e ->
        Error (f e)


let throw (e: 'e): ('a, 'e) t =
    Error e


let catch (m: ('a, 'e) t) (f: 'e -> ('a, 'e) t): ('a, 'e) t =
    match m with
    | Ok a ->
        Ok a
    | Error e ->
        f e




module Make (Error: ANY) =
struct
    type error = Error.t

    include
        Monad.Of_sig_min (
            struct
                type 'a t = ('a, error) result

                let return (a: 'a): 'a t =
                    Ok a

                let (>>=) = (>>=)
            end
        )

    let throw = throw

    let catch = catch

    let continue m f g =
        match m with
        | Ok a ->
            f a
        | Error e ->
            g e
end

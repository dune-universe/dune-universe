(**
  Sugar module signatures.
 *)


(**
  Signatures for the dependencies used in Sugar's module builders.
*)
module Params = struct


(**
  A generic signature describing a monad.
*)
module type Monad = sig

  type 'a t
  (** A parametric type representing any OCaml value. *)

  val return: 'a -> 'a t
  (** Creates a constant value in this monad.  *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Waits for the conclusion of the monad in the left,
      and then, apply the unwrapped value to the function in the right.
   *)

end



(**
  Conventional module type to define the errors inside a project.
  This is how Sugar understands the error handling layer of a project.

  Like:
  {[
    module MyError = struct
      type error = Not_found | Invalid_arg of string
    end
  ]}

  This module might be used to create blocking or asynchronous error handling
  layers, using the Sugar functors, like:
  {[
    module MyResult = Sugar.Result.Make (MyError)

    module MyResult2 = Sugar.Promise.Make (Lwt) (MyError)
    module MyResult2 = MyResult.For (Lwt)
  ]}
*)
module type Error = sig

  type t
  (**
    This type describes the errors of your project. It's one of the main requirements
    to create a result monad.

    If you don't want to specify your errors upfront, you can still use something like [unit] or
    [string] as error type.
  *)

end



(**
  This signature describes an [Error] module that has some control over unexpected exceptions.

  If you want to handle unexpected exceptions as they appear, you should probably define
  an error case with the type [exn], like in the code below:
  {[
  module Error = struct
    type t =
      | Because_reasons
      | Unexpected of exn

    let panic e = Unexpected e
  end
  ]}
 *)
module type Strict_error = sig
  include Error

  val panic : exn -> t
  (**
    When an exception is detected, this module can either terminate the process with a proper
    message or chose convert the error to the type {!t}.
  *)

end


(**
  A monad that provides some awareness about unexpected exceptions.

  This module is related to {{!Sugar.S.Params.Strict_error} Strict_error}.
*)
module type Strict_monad = sig
  include Monad

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  (**
    Checks if the monad returned by the thunk raised an exception, and applies
    the given error handler if necessary.

    This function has intentionally the
    exact signature as [Lwt.catch]. This means the [Lwt] module is already a [Strict_monad]:
    {[
    let _ =
      (module Lwt: Sugar.Params.Strict_monad)
    ]}
   *)
end


end

open Params



module type Promise = sig

  type error
  (** Error definition imported from your project *)

  type 'a value = ('a, error) Result.result
  (** An alias for [Pervasives.result] that can only work with errors of your project. *)

  type 'a monad
  (**
    This type is an alias to the underlining monad used to create your [Promise] module.
    In other words, it points to the main type of your threading library ([Lwt.t] for Lwt,
    or [Async.Std.Deferred.t] for Async).
   *)

  type 'a result = 'a value monad
  (**
    This type describes a result monad inside a project.

    For example, if the concrete module is built on top of Lwt,
    a value of type [unit result] will be translated as [(unit, error) Pervasives.result Lwt.t].
  *)

  val (>>=): 'a result -> ('a -> 'b result) -> 'b result
  (**
    This combinator is also called [bind].

    It can be used to chain sequential operations with the current monad.
    If the computation in the left failed, the operator will propagate the error,
    skipping the function completely.
  *)

  val bind:  'a result -> ('a -> 'b result) -> 'b result
  (**
     Similar to {{!Sugar__S.Result.bind} S.Result.bind}
   *)

   val bind_unless: 'a result -> (error -> 'a result) -> 'a result
  (**
     Similar to {{!Sugar__S.Result.bind_unless} S.Result.bind_unless}
   *)

   val map:  'a result -> ('a -> 'b) -> 'b result
   (**
      Similar to {{!Sugar__S.Result.map} S.Result.map}
    *)

    val return: 'a -> 'a result
    (**
       Similar to {{!Sugar__S.Result.return} S.Result.return}
    *)


  val throw: error -> 'a result
  (**
    Similar to {{!Sugar__S.Result.throw} S.Result.throw}
  *)

  module Infix : sig

    val (>>|): 'a result -> ('a -> 'b) -> 'b result
    (**
      {{!Sugar.S.Result.Infix.(>>|)} More...}
    *)

    val (<$>): ('a -> 'b) -> 'a result -> 'b result
    (** Applicative combinator for map *)

    val (<*>): ('a -> 'b) result -> 'a result -> 'b result
    (** Applicative combinator for parallel execution of function and operand *)

    val (>>>=): 'a monad -> ('a -> 'b monad) -> 'b monad
    (** An alias for UserMonad.(>>=)

        This combinator provides direct access to the monad used to create
        this module. *)

    val (>---------): 'a result -> (error -> 'a result) -> 'a result
    (**
      "Broom" combinator

      This is an alias for the function {!bind_unless}. It provides syntatic
      sugar to create error handlers in a more intuitive way.

      There's a secret message behind the form of this combinator.
      It has the same number of characters sufficient for the whole block
      in the next line. For example:

      {[
      let program1 () =
        do_something ()
        >---------
        ( fun e ->
          return ()
        )

      let program2 () =
        do_something ()
        >---------
        ( function
          e -> return ()
        )
      ]}
    *)

    val ( >> ) : unit result -> 'b result Lazy.t -> 'b result
    (**
      {{!Sugar.S.Result.Infix.(>>)} More...}
    *)

    val (>>>): 'a result -> 'b result Lazy.t -> 'b result
    (**
      {{!Sugar.S.Result.Infix.(>>>)} More...}
    *)

  end

  val unwrap: 'a value monad -> 'a monad
  (**
    Unwraps the successful value as a normal value in the threading monad.
    If the value is not successful, it will raise an [Invalid_arg] exception.
  *)

  val unwrap_or: (error -> 'a monad) -> 'a value monad -> 'a monad
  (**
    Unwraps the successful value as a value in the threading monad.
    Different from [unwrap], you can assign an error handler to be
    executed if the computation failed.
  *)

  val expect: 'a value monad -> string -> 'a monad
  (**
    Extracts a successful value from an computation, or raises and [Invalid_arg]
    exception with a customized error message.
  *)

end

(**
  This interface specifies an error handling layer for monadic computations.

  Sugar value modules work with any monad.

  {[
    module MyMonad = struct
      type 'a monad = 'a Lwt.t
      let return = Lwt.return
      let (>>=) = Lwt.bind
    end
    module MyResult = Sugar.Promise.Make (MyMonad) (MyError)
  ]}
*)
module type Strict_promise = sig
  include Promise

  (**
    Disable exception handling. Open this module with you don't want to catch exceptions.
  *)
  module NoExceptions : Promise
    with type error := error
    and type 'a monad := 'a monad

end


(**
  Common definitions for the default result monad.
*)
module type Result_partials = sig

  type error
  (** Error definition from your project *)


  type 'a result = ('a, error) Result.result
  (** An alias for the result type in the stdlib *)

  val bind:  'a result -> ('a -> 'b result) -> 'b result
  (** Apply the binding only if the computation was successful.
      You can use the operator {{!(>>=)} >>=} instead of this function for syntatic sugar *)

  val bind_unless: 'a result -> (error -> 'a result) -> 'a result
  (** Apply the binding only if the computation failed.

      Notice that an error handler must be provided, and this handler
      must throw an error or provide an equivalent for the result type of the
      previous computation.

      You can use the operator {{!Infix.(>---------)} >---------} instead of this function for syntatic sugar *)

  val map:  'a result -> ('a -> 'b) -> 'b result
  (**
    Apply a function to the result of a successful computation. This function
    makes it ease to work with non error aware functions.

    Example:
    {[
    open Sugar.Option

    let twenty =
     map (Some 10) (fun n -> n + n)
    ]}

    You could also use the combinator {{!Infix.(>>|)} >>|} for syntatic sugar. *)

  val return: 'a -> 'a result
  (** Return a value in a successful computation.
      This function should be used with its counterpart, [throw] *)


  val throw: error -> 'a result
  (**
    Return an error as the result of a computation.

    Like the [return] function, [throw] helps you hide the internals of your
    result type and keep a clean code.

    If you are still at the beginning of your project, and don't have your
    errors defined yet, this function still is a great help. For example,
    the code bellow have the same usage as the function [failwith], but is a lot
    safer.

    {[
     module MyResult = Sugar.MakeResult (struct error = string end)
     open MyResult
     let run (): int result =
       if true then
         return 10
       else
         throw "something bad happend"
    ]}

    You could also not describe your errors at all for some time, and
    use the {!Sugar.Option} module to create error aware computations, like:

    {[
     open Sugar.Option
     let run (): string result =
       if true then
         return "hello world"
       else
         throw ()
    ]} *)

  module Infix : sig

    val (>---------): 'a result -> (error -> 'a result) -> 'a result
    (**
      Combinator used to introduce an error handler block to "clean errors".

      There's a secret message behind the form of this combinator.
      It has the same number of characters sufficient for the whole block
      in the next line. For example:

      {[
      let program1 () =
       do_something ()
       >---------
       ( fun e ->
         return ()
       )

      let program2 () =
       do_something ()
       >---------
       ( function
         e -> return ()
       )
      ]}

      So beyond the clean aesthetics similar to markdown, we are
      implying that a developer should never handle errors in an open
      anonymous function. *)


    val (>>|): 'a result -> ('a -> 'b) -> 'b result
    (**
      Combinator for map with semantic similar to bind

      As its name sugests, this is an alias for the function {{!map} map}.
      Its intended use is to help integrate with functions that are not error
      aware.

      For example, considere the function [let double x = x + x] in the code
      fragments bellow:

      {[
       open Sugar.Option

       let twenty =
         match Some 10 with
         | None -> None
         | Some n -> Some (double n)

        let using_bind_combinator =
         Some 10
         >>=
         ( fun n ->
           return (double n)
         )

       let using_map_combinator =
         Some 10
         >>| double
      ]}
      *)


    val (<$>): ('a -> 'b) -> 'a result -> 'b result
    val (<*>): ('a -> 'b) result -> 'a result -> 'b result


    val (>>>): 'a result -> 'b result Lazy.t -> 'b result
    (**
      Ignore operator.

      This is a type of semicolon combinator that can be used to ignore any
      result. It is supposed to be used with the keyword lazy as in [>>>lazy].
      It can be used to replace the creation of an anonymous function to discard
      the previous value, like:

      {[
      (* instead of writing *)
      do_something ()
      >>=
      ( fun _ ->
        do_something_else ()
      )

      (* you can write *)
      ( do_something ()      ) >>>lazy
      ( do_something_else () )
      ]}

      For more information, look at {!(>>)}.
    *)

    val (>>): unit result -> 'b result Lazy.t -> 'b result
    (**
      A sequential semicolon combinator.

      This operator is supposed to be used with the keyword lazy, as [>>lazy].
      To reduce cognitive load, it's interesting to treat [>>lazy] as one word.

      To chain monadic expressions that are not related to each other.
      Instead of writing code like:

      {[
      puts "hello"
      >>=
      ( fun () ->
        puts "monadic"
      )
      >>=
      ( fun () ->
        puts "world"
      )
      ]}

      You can use these form:
      {[
      ( puts "hello"   ) >>lazy
      ( puts "monadic" ) >>lazy
      ( puts "world"   )
      ]}

      Or this alternative form that has exactly the structure as the example above:
      {[
      ( puts "hello"   )>>lazy(
        puts "monadic" )>>lazy(
        puts "world"
      )
      ]}

      Notice that the expression on the left must be of type [unit result]. If you wish to
      ignore other type of [result]s, use {{!(>>>)} >>>lazy}
    *)

  end


  val (>>=): 'a result -> ('a -> 'b result) -> 'b result
  (**
    Bind combinator

    If the computation in the left is successful, the operator will
    Take the inner value and feed it to the function in the right. This is an
    alias for the function [bind].

    If the computation in the left failed, the operator will propagate the error,
    skipping the function completely.
  *)


  val unwrap: 'a result -> 'a
  (**
    Unwraps the successful result as a normal value in the threading monad.
    If the value is not successful, it will raise an Invalid_arg exception.
  *)


  val unwrap_or: (error -> 'a) -> 'a result -> 'a
  (**
    Unwraps the successful result as a value in the threading monad.
    Different from [unwrap], you can assign an error handler to be
    executed if the computation failed. Example:
    {[
    let run () =
      get_data ()
      |> unwrap_or (fun _ -> "default")
    ]}
  *)


  val expect: 'a result -> string -> 'a
  (**
    Extracts a successful value from an computation, or raises and Invalid_arg
    exception with the defined parameter.
  *)

end


(**
  The signature for the default result monad.
*)
module type Result = sig
  include Result_partials

  (**
    Create a new result module based on the current one, but wrapped around a monad.
  *)
  module For : functor (UserMonad:Monad) -> Promise
    with type error := error
    and type 'a monad := 'a UserMonad.t
end


(**
  The signature for a result monad that has some awareness about unexpected exceptions.
*)
module type Strict_result = sig
  include Result_partials

  (**
    Create a new result module based on the current one, but wrapped around a monad.
  *)
  module For : functor (UserMonad:Params.Strict_monad) -> Strict_promise
    with type error := error
    and type 'a monad := 'a UserMonad.t


  (**
    Disable exception handling
  *)
  module NoExceptions : Result
    with type error := error
end

(**
  Sugar is a small monadic library that provides syntactic sugar to help you manipulate error aware expressions.
*)

(** <h2>Submodules</h2> *)

module S = S

(** Use to create a result monad on top of an arbitrary monad.  *)
module Promise = Promise_builder

(** Create a result monad for synchronous processing. *)
module Result = Result_builder

module Option = Option

module Strict = struct
  module Result = Strict_result_builder
  module Promise = Strict_promise_builder
end


(**
  <h2>Introduction</h2>

  The first thing you'll notice if you look at the code examples using Sugar is that monads are used everywhere. They are not hidden with compiler extensions, nor look like anything other than monads.

  But threre's a few conventions to style your code. These conventions are not mandatory - they just make it easier to undertand what is cool (and whats not) when writing monadic code in OCaml.

  For example, a code like this:

  {[
  return [1; 2; 3]
  >>= fun list ->
  return (List.length list)
  >>= fun _len ->
  do_something ()
  >>= fun () ->
  return "hello world"
  ]}


  Could be refactored to look like this:

  {[
  ( List.length <$> return [1; 2; 3] )
  >>=
  ( fun _len ->
    do_something ()
  )
  >>lazy
  ( return "Hello World"  )
  ]}


    - If a function is non-monadic, maybe you should just use a combinator that supports non-monadic functions, and make code cleaner, like {{!Sugar.S.Promise.Infix.(<$>)} <$>} or {{!Sugar.S.Promise.Infix.(>>|)} >>|}.

    - It's important to use blocks. They make code easier to read and avoid ambiguity. Using one space to separate the parenthesis from its content is a good practice.

    - We recommend treating {{!Sugar.S.Promise.Infix.(>>)} >>lazy} as one word, with the same meaning as the sequential semicolon. It can only be used if the expression in the left resolves to [unit result]. If you want to ignore any value without writing an anonymous function, you can use {{!Sugar.S.Promise.Infix.(>>>)} >>>lazy}.

      ​

  <h2>Your own result monad</h2>

  Sugar is easy to use and customize. At the beginning, you create a result monad specific to your project. For that, you describe things like the errors of your project and if an underlining monad should be used for async. After that, you can basically ignore the fact that your result monad was customized at all. You get the same clean interface for all error aware computations.

  It's interesting to say that describing your errors at an early development state is a good practice. The correct usage of this monad is enforced by Sugar interfaces. The most basic ones are:

  - {{!Sugar.S.Result.(>>=)} >>=}: Also called the [bind] combinator. It resolves the expression in the left, and if successful, applies the returned value to the function in the right. If case of failures, it just returns the error.

  - {{!Sugar.S.Result.return} return}: Create a value in your result monad

  - {{!Sugar.S.Result.throw} throw}: Throws a project error using your result monad

    ​
  These constructs abstract away things like the usage of async code and the concrete result type, making code easier to read.


  <h2>Exception awareness</h2>

  It would be great if exceptions would not be used as openly as they are in libraries like the Stdlib and Lwt. But they are, and you might want to use then nonetheless. You can still have some control over exceptions while using Sugar.

  Sugar has some *strict* modules builders that take exception handling into account before creating a result monad. That is reflected in small changes in the [Error] and [Monad] modules provided as dependencies.

  When you use a strict result monad, Sugar will handle the [try-with] blocks for you and ask your [Error] module to decide if the detected exception should be converted into one of your project errors, or some other action should be taken, like logging the error and kiling the process.



  <h2>Expressive error handling</h2>

  One common problem in the transition for writing heavily monadic code is that you need to think about how to handle errors in an expressive way.

  Here's a code fragment that relies merely on exception and [try-with] blocks to handle errors.

  {[
  try
    delete_user "john"
  with
    Not_found -> ()
  ]}

  And here is the same example, now using error aware expressions and a result monad to handle failures:

  {[
  delete_user "john"
  >---------
  ( function
    | Not_found -> return ()
    | e -> throw e
  )
  ]}

  As you can see, both versions are easy to write and read, but the latter is much safer, since it does not hide the fact that some errors can still be returned to the caller.


  <h2>Interacting with threading libraries</h2>

  If you use an async library with Sugar, chances are you will need to convert monads back and fourth to access functionality outside your project. Fear not, you will have access to the underlining monad with the operator {{!Sugar.S.Promise.Infix.(>>>=)} >>>=}.

  The code bellow mixes both usage of the underlining bind, the result bind, and exception handling. Take into account that the code raising the exception just exemplifies an interaction with Lwt code that already raises exceptions. You should avoid writing this kind of code.

  {[
  module MyError = struct
    type t = Unexpected of exn

    let panic e = Unexpected e
  end

  module MyResult = Sugar.Strict.Promise.Make (MyError) (Lwt)

  open MyResult
  open MyResult.Infix
  open MyError

  let get_message () =
    Lwt_unix.file_exists "README.md"{{: string }  text }
    >>>=
    ( fun exists ->
      if exists then
        return "file exists"
      else
        Lwt.fail (Failure "file does not exists")
    )
    >---------
    ( function
      Unexpected e -> return ("Unexpected: " ^ (Printexc.to_string e))
    )
  ]}
*)

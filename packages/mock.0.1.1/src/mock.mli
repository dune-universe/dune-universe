(**
   A mock is something that can be used in place of function.

   [('args, 'ret) t] is similar to ['args -> 'ret] except that:
   - its return value (or effect) can be configured
   - it is possible to determine whether it has been called
*)
type ('args, 'ret) t

(**
   Build an unconfigured mock.
   Calling an unconfigured mock will raise an exception that contains the name
   of the mock.
*)
val make: name:string -> ('args, 'ret) t

(** Retrieve the name of a mock. *)
val name: ('args, 'ret) t -> string

(** What a mock can evaluate to. *)
type 'a side_effect

(**
   Evaluation is not possible.
   [call] will raise [Mock_non_configured].
*)
val not_configured: 'a side_effect

(** Return a value. *)
val return: 'a -> 'a side_effect

(** Raise an exception. *)
val raise: exn -> 'a side_effect

(** Define what a mock should return. *)
val configure: ('args, 'ret) t -> 'ret side_effect -> unit

(**
   Call the mock:
   - record its arguments
   - perform its side effects
*)
val call: ('args, 'ret) t -> 'args -> 'ret

(** [call] will raise this exception if the mock has not been configured. *)
exception Mock_not_configured of string

(**
   Return the list of arguments this mock has received.
   The list is in the same order as the calls.
*)
val recorded_calls: ('args, 'ret) t -> 'args list

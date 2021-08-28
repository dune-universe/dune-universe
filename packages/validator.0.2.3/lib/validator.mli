(** Create a record validator via composable sub-validators. The input and the output can be different types.

{[
type input_form = {
  name: string;
  email: string option;
  age: int;
}

type valid_form = {
  name: string;
  email: string;
  age: int;
}

let build_valid_form name email age =
  { name; email; age }

let validator_name =
  let open Validator in
  string_is_not_empty "Empty"
  |> compose
    (string_has_min_length 3 "Name is too short")

let validator_email =
  let open Validator in
  option_is_some "Missing email"
  |> compose (string_is_email "Not an email")

let validator_age =
  let open Validator in
  int_min 13 "Must be 13"

let validator (input: input_form) =
  let open Validator in
  build build_valid_form
  |> validate input.name validator_name
  |> validate input.email validator_email
  |> validate input.age validator_age

validator { name = "Sam"; email = Some "sam@sample.com"; age = 20}

==>

Ok { name = "Sam"; email = "sam@sample.com"; age = 20}
]}
*)

type 'err errors = 'err * 'err list
(** Validator errors are a tuple of (error, error list)

The first element in the tuple is the first error.
The second element is a list of all errors (including the first one)
*)

type ('out, 'err) validator_result =
  ('out, 'err errors) result
(** A validator returns a result of:

- Ok output or
- Error (err, err list)  *)

type ('input, 'output, 'err) validator =
  'input -> ('output, 'err) validator_result

(** A validator takes an input an returns a validator result  *)

type ('input, 'output, 'err) validator_builder =
  'err -> ('input, 'output, 'err) validator

(** A validator builder takes an en error and returns a validator *)

val int_min : int -> (int, int, 'err) validator_builder
(** Validate min permitted integeger
{[
let validator input =
  let open Validator in
  build build_valid
  |> validate input.age (int_min 13 "Must be 13")
]}
*)

val int_max : int -> (int, int, 'err) validator_builder
(** Validate max permitted integeger
{[
let validator input =
  let open Validator in
  build build_valid
  |> validate input.age (int_max 6 "Must be 6 or younger")
]}
*)

val list_is_not_empty :
  ('a list, 'a list, 'err) validator_builder
(** Validate that a list is not empty
{[
let validator input =
  let open Validator in
  build build_valid
  |> validate input.hobbies (list_is_not_empty "Must have a hobby")
]}
*)

val list_has_max_length :
  int -> ('a list, 'a list, 'err) validator_builder
(** Validate max length of a list
{[
let validator input =
  let open Validator in
  build build_valid
  |> validate input.hobbies (list_has_max_length 12 "Max 12 hobbies")
]}
*)

val list_has_min_length :
  int -> ('a list, 'a list, 'err) validator_builder
(** Validate min length of a list
{[
let validator input =
  let open Validator in
  build build_valid
  |> validate input.hobbies (list_has_min_length 2 "Min 2 hobbies")
]}
*)

val list_every :
  ('i, 'o, 'err) validator ->
  ('i list, 'o list, 'err) validator
(** Validate a list of items. Run the given validator for each item returning all the errors.
{[
let hobbie_validator =
  let open Validator in
  string_is_not_empty "Must not be empty"

let validator input =
  let open Validator in
  build build_valid
  |> validate input.hobbies (list_every hobbie_validator)
]}
*)

val option_is_some : ('a option, 'a, 'err) validator_builder
(** Validate that a value is not None.
   Returns the value if Some. 
{[
let validator input =
  let open Validator in
  build build_valid
  |> validate input.name (option_is_some "Must be present")
]}
*)

val string_is_not_empty :
  (string, string, 'err) validator_builder
(** Validate if a string is not empty
{[
let validator input =
  let open Validator in
  build build_valid
  |> validate input.name (string_is_not_empty "Must not be blank")
]}
*)

val string_is_int : (string, int, 'err) validator_builder
(** Validate if a string parses to an Int. Returns the Int if so
{[
let validator input =
  let open Validator in
  build build_valid
  |> validate input.age (string_is_int "Must be a number")
]}
*)

val string_has_min_length :
  int -> (string, string, 'err) validator_builder
(** Validate the min length of a string
{[
let validator input =
  let open Validator in
  build build_valid
  |> validate input.password (string_has_min_length 3 "Min 3 chars")
]}
*)

val string_has_max_length :
  int -> (string, string, 'err) validator_builder
(** Validate the max length of a string
{[
let validator input =
  let open Validator in
  build build_valid
  |> validate input.password (string_has_max_length 100 "Max 100 chars")
]}
*)

val string_is_email :
  (string, string, 'err) validator_builder
(** Validate if a string is an email.
   This checks if a string follows a simple pattern `_@_`.
{[
let validator input =
  let open Validator in
  build build_valid
  |> validate input.email (string_is_email "Not an email")
]}
*)

val optional :
  ('i, 'o, 'err) validator ->
  'i option ->
  ('i option, 'err errors) result
(** Validate an optional value.
   Run the validator only if the value is Some.
   If the value is None then just return None back. 
{[
let message_validator =
  Validator.string_is_not_empty "Must not be blank"

let validator input =
  let open Validator in
  build build_valid
  |> validate input.message (optional message_validator)
]}
*)

val keep :
  'a ->
  ('a -> 'next_acc, 'e errors) result ->
  ('next_acc, 'e errors) result
(** Keep a value as is.
{[
let validator input =
  let open Validator in
  build build_valid
  |> keep input.message
]}
*)

val build :
  ('a -> 'final) -> ('a -> 'final, 'e errors) result
(** Start the build pipeline for a validator
{[
let validator input =
  let open Validator in
  build build_valid
  |> validate input.age (int_min 13 "Must be 13")
  |> validate input.email (string_is_email "Must be email")
  |> keep input.message
]}
*)

val validate :
  'input ->
  ('input, 'output, 'err) validator ->
  ('output -> 'next_acc, 'err errors) result ->
  ('next_acc, 'err errors) result
(** Chain validators
{[
let validator input =
  let open Validator in
  build build_valid
  |> validate input.age (int_min 13 "Must be 13")
  |> validate input.email (string_is_email "Must be email")
  |> keep input.message
]}
*)

val compose :
  ('mid, 'o, 'err) validator ->
  ('i, 'mid, 'err) validator ->
  ('i, 'o, 'err) validator
(** Compose validators
Run the first validator and if successful then the second.
Only returns the first error.
{[
let name_validator =
  open Validator in
  string_is_not_empty "Empty"
  |> compose (string_has_min_length 3 "Too short")

let validator input =
  let open Validator in
  build build_valid
  |> validate input.name name_validator
]}
*)

val all :
  ('io, 'io, 'err) validator list ->
  ('io, 'io, 'err) validator
(** Validate a value using a list of validators.
This runs all the validators in the list.

The initial input is passed to all validators.
All these validators must have the same input and output types.

Returns Ok when all validators pass.
Returns Error when any validator fails. Error will have all failures.
{[
let validators =
  let open Validator in
  [
    string_is_not_empty "Empty";
    string_has_min_length 4 "Min";
    string_has_max_length 20 "Max";
    string_is_email "Email";
  ]

let validator input =
  let open Validator in
  build build_valid
  |> validate input.email (all validators)
]}
*)

val whole :
  ('whole -> ('whole, 'err) result) ->
  ('whole, 'err) validator_result ->
  ('whole, 'err) validator_result
(** Validate a structure as a whole.

Sometimes we need to validate a property in relation to another.

This is just a function that takes the output and return a result.
{[
let validate_whole person =
  if person.name == "Sam" then
    Ok person
  else
    Error "Not Sam"

let validator input =
  let open Validator in
  build build_valid
  |> validate input.email (...)
  |> whole validate_whole
]}
*)
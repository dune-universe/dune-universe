(** Data structure for clients. *)
type t = {
  api_key: string;
  configuration: Configuration.t
}

(** Creates a new client from its API key.

    By default, uses [Configuration.create ()] as its configuration.
*)
val create : ?configuration:Configuration.t -> string -> t

(** Fetches information about the client's TextRazor account. *)
val account : t -> (Account.t, string) result

(** Analyzes a text using TextRazor.

    Use [(`Text "text")] to send TextRazor raw text, or [(`Uri uri)] to send a
    publicly available URL containing the document to be analyzed.

    By default, uses [Analysis.Options.default] as analysis options.
*)
val analyze : [`Text of string | `Uri of Uri.t] -> ?options:Analysis.Options.t -> t
  -> (Analysis.t, string) result
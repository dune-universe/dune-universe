type header = string list

type item = {
  description : string;
  sequence : string;
}
[@@ deriving sexp]


val from_string :
  string ->
  (header * item list, string) result

val from_file :
  string ->
  (header * item list, string) result

val from_file_exn :
  string ->
  header * item list

val sequences_from_file_exn :
  string ->
  string list

val to_file :
  string ->
  item list ->
  unit

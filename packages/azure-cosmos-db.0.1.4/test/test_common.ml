open Cosmos
open Databases

let master_key_env = "AZURE_COSMOS_KEY"
let endpoint_env = "AZURE_COSMOS_ENDPOINT"

module MyAuthKeys : Auth_key = struct
  let getenv s =
    match Sys.getenv_opt s with
    | None -> ""
    | Some x -> x
  let master_key = getenv master_key_env
  let endpoint = getenv endpoint_env
end

module D = Database(MyAuthKeys)

let dbname = "test"
let collection_name = "testCollection"
let dbname_partition = "testPartition"
let collection_name_partition = "testPartition"
let document_id = "document_id"

let should_run () =
  Option.is_some @@ Sys.getenv_opt master_key_env
  && Option.is_some @@ Sys.getenv_opt endpoint_env

open Encoding
module Encoding = Encoding

module V1 = struct
  let version = `V1
end

module Conf = struct
  let entries = 32
  let stable_hash = 256
end

module Store =
  Irmin_pack.Make_ext (V1) (Conf) (Node) (Commit) (Metadata) (Contents) (Path)
    (Branch)
    (Hash)

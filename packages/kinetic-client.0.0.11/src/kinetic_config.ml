(*
  Copyright (C) iNuron - info@openvstorage.com
  This file is part of Open vStorage. For license information, see <LICENSE.txt>
 *)

open Kinetic_util

module Config = struct
    type t = {
        vendor: string;
        model:string;
        serial_number: bytes;
        world_wide_name: bytes;
        version: string;
        ipv4_addresses : bytes list;
        max_key_size: int;
        max_value_size: int;
        max_version_size: int;
        max_tag_size: int;
        max_connections: int;
        max_outstanding_read_requests: int;
        max_outstanding_write_requests: int;
        max_message_size: int;
        max_key_range_count: int;
        max_operation_count_per_batch: int option;
        max_batch_size : int option;
        max_deletes_per_batch : int option;
        (* max_batch_count_per_device: int; *)
        timeout : float;
      } [@@deriving show {with_path = false}]

    let make ~vendor ~world_wide_name ~model
             ~serial_number
             ~version
             ~ipv4_addresses
             ~max_key_size
             ~max_value_size
             ~max_version_size
             ~max_tag_size
             ~max_connections
             ~max_outstanding_read_requests
             ~max_outstanding_write_requests
             ~max_message_size
             ~max_key_range_count
             ~max_operation_count_per_batch
             ~timeout
             ~max_batch_size
             ~max_deletes_per_batch
             (* ~max_batch_count_per_device *)

      = {
        vendor;
        model;
        serial_number;
        world_wide_name;
        version;
        ipv4_addresses;
        max_key_size;
        max_value_size;
        max_version_size;
        max_tag_size;
        max_connections;
        max_outstanding_read_requests;
        max_outstanding_write_requests;
        max_message_size;
        max_key_range_count;
        max_operation_count_per_batch;
        (* max_batch_count_per_device; *)
        max_batch_size;
        max_deletes_per_batch;
        timeout;
      }

end

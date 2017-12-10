(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

external shortest_string_of_float: float -> string = "flow_shortest_string_of_float"
external ecma_string_of_float: float -> string = "flow_ecma_string_of_float"
external g_fmt: float -> string = "flow_g_fmt"

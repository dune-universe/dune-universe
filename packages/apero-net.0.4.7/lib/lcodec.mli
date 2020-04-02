open Locator


val decode_locator : Abuf.t -> Locator.t option
val encode_locator : Locator.t -> Abuf.t -> unit

val decode_locators : Abuf.t -> Locators.t
val encode_locators : Locators.t -> Abuf.t -> unit

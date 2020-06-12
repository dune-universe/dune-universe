open! Base
open! Import

let staged = [%accessor Accessor.isomorphism ~get:Staged.stage ~construct:Staged.unstage]
let unstaged = [%accessor Accessor.invert staged]

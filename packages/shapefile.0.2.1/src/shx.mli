(* Copyright 2018 Cyril Allignol
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. *)

(** .shx files utilities. *)

type index = {
    offset: int; (** postion of the shape in the .shp file in 16-bits words *)
    length: int  (** length of the shape description in 16-bits words *)
  } (** Type of an index record. *)

val read: string -> Common.header * index list
(** [Shx.read file] parses the [file] and returns a list of indexes,
    i.e. position and size of shape descriptions found in the corresponding
    .shp file. *)

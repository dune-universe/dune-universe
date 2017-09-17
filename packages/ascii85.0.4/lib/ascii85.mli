(** This module implements the Ascii85 Encoding as specified by 
    Adobe's "PostScript LANGUAGE REFERENCE third edition". For the details
    of the format, see sections "ASCII Base-85 Strings" and "ASCII85Encode
    Filter".    

    @author Christian Lindig 

    Copyright (c) 2015, Christian Lindig <lindig\@gmail.com>
    All rights reserved. See LICENSE.md.
*)

val encode: string -> in_channel -> out_channel -> unit
(** [encode head ic oc] writes [head] to [oc] and then reads a byte stream
    from [ic] until the end and emits its Ascii85 encoding to [oc]. *)

val encode_file: string -> string -> unit
(** [encode_file head path] emits [head] and then reads a named file [path]
    and emits its Ascii85 encoding to standard output.  Opening and closing
    file [path] is handled by [encode_file]. *)

(*
 * Copyright (c) 2015 Unikernel Systems
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** Allow an application to be managed by launchd.

*)

open Launchd

val activate_socket: string -> (Lwt_unix.file_descr list, error) result Lwt.t
(** [activate_socket name]: retrieve the file descriptors for the sockets
    associated with the given [name] in the .plist file. This should be
    called once per invocation; subsequent calls will fail with [`Ealready].

    For example, in the following .plist, [name] should be ["Listener"]:
    {[

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
        <key>Label</key>
        <string>org.recoil.dave.anotherd</string>
        <key>ProgramArguments</key>
        <array>
                <string>/path/to/binary</string>
        </array>
        <key>Sockets</key>
        <dict>

                <key>Listener</key>
                <dict>
                        <key>SockServiceName</key>
                        <string>8081</string>
                        <key>SockType</key>
                        <string>stream</string>
                        <key>SockFamily</key>
                        <string>IPv4</string>
                </dict>
        </dict>
</dict>
</plist>
    ]}
    Please note that launchd may bind multiple sockets (e.g. one for IPv4
    and another for IPv6) and so the application must handle all the sockets
    returned by the call.

    Please note that this call is blocking.

    This call fails with:
    {ul
    {li  [`Enoent name] if [name] could not be found in the .plist.}
    {li [`Esrch] if this process is not being managed by launchd
    (for example if it is running from a terminal).}
    {li [`Ealready] if the socket has already been activated.}
    }
*)

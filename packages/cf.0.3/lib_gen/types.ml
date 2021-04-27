(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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

module C (F : Cstubs.Types.TYPE) = struct
  module StringEncoding = struct
    (* typedef UInt32 CFStringEncoding; *)
    let t = F.uint32_t

    let mac_roman = F.constant "kCFStringEncodingMacRoman" t

    let windows_latin1 = F.constant "kCFStringEncodingWindowsLatin1" t

    let iso_latin1 = F.constant "kCFStringEncodingISOLatin1" t

    let nextstep_latin = F.constant "kCFStringEncodingNextStepLatin" t

    let ascii = F.constant "kCFStringEncodingASCII" t

    let unicode = F.constant "kCFStringEncodingUnicode" t

    let utf8 = F.constant "kCFStringEncodingUTF8" t

    let nonlossy_ascii = F.constant "kCFStringEncodingNonLossyASCII" t

    let utf16 = F.constant "kCFStringEncodingUTF16" t

    let utf16be = F.constant "kCFStringEncodingUTF16BE" t

    let utf16le = F.constant "kCFStringEncodingUTF16LE" t

    let utf32 = F.constant "kCFStringEncodingUTF32" t

    let utf32be = F.constant "kCFStringEncodingUTF32BE" t

    let utf32le = F.constant "kCFStringEncodingUTF32LE" t
  end

  module RunLoopActivity = struct
    let t = F.ullong

    let entry = F.constant "kCFRunLoopEntry" t

    let before_timers = F.constant "kCFRunLoopBeforeTimers" t

    let before_sources = F.constant "kCFRunLoopBeforeSources" t

    let before_waiting = F.constant "kCFRunLoopBeforeWaiting" t

    let after_waiting = F.constant "kCFRunLoopAfterWaiting" t

    let exit = F.constant "kCFRunLoopExit" t

    let all_activities = F.constant "kCFRunLoopAllActivities" t
  end

  module RunLoopRunResult = struct
    let t = F.int32_t

    let finished = F.constant "kCFRunLoopRunFinished" t

    let stopped = F.constant "kCFRunLoopRunStopped" t

    let timed_out = F.constant "kCFRunLoopRunTimedOut" t

    let handled_source = F.constant "kCFRunLoopRunHandledSource" t
  end
end

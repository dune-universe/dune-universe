(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2012 Citrix Systems, Inc
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
 *)

open Ring

module Ring = struct
	type t = buf
	let of_buf t = t
	module Layout = struct
		(* memory layout from the frontend's point of view *)
		[%%cstruct
		type ring = {
			output: uint8_t [@len 1024];
			input: uint8_t [@len 1024];
			(* (* unsafe to use these as they produce multi byte load/stores *)
			output_cons: uint32_t;
			output_prod: uint32_t;
			input_cons: uint32_t;
			input_prod: uint32_t;
			*)
		} [@@little_endian]
		]
		let _output_cons = 2048
		let _output_prod = _output_cons + 4
		let _input_cons  = _output_prod + 4
		let _input_prod  = _input_cons  + 4
		let get_ring_output_cons c = Int32.of_int (unsafe_load_uint32 c _output_cons)
		let get_ring_output_prod c = Int32.of_int (unsafe_load_uint32 c _output_prod)
		let get_ring_input_cons  c = Int32.of_int (unsafe_load_uint32 c _input_cons)
		let get_ring_input_prod  c = Int32.of_int (unsafe_load_uint32 c _input_prod)
		let set_ring_output_cons c x = unsafe_save_uint32 c _output_cons (Int32.to_int x)
		let set_ring_output_prod c x = unsafe_save_uint32 c _output_prod (Int32.to_int x)
		let set_ring_input_cons  c x = unsafe_save_uint32 c _input_cons (Int32.to_int x)
		let set_ring_input_prod  c x = unsafe_save_uint32 c _input_prod (Int32.to_int x)
	end
	let init = zero
	let to_debug_map t = [
		"input-cons", Int32.to_string (Layout.get_ring_input_cons t);
		"input-prod", Int32.to_string (Layout.get_ring_input_prod t);
		"output-cons", Int32.to_string (Layout.get_ring_output_cons t);
		"output-prod", Int32.to_string (Layout.get_ring_output_prod t);
	]
	module Front = Pipe(Layout)
	module Back = Pipe(Reverse(Layout))
end

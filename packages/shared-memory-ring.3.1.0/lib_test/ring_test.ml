(*
 * Copyright (C) Citrix Systems Inc.
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

[@@@warning "-27"]
open OUnit

let ( |> ) a b = b a
let id x = x

let alloc_page () =
	Bigarray.Array1.create Bigarray.char Bigarray.c_layout 4096

let length t = Cstruct.len t

let compare_bufs a b =
	assert_equal ~printer:string_of_int (Bigarray.Array1.dim a) (Bigarray.Array1.dim b);
	for i = 0 to Bigarray.Array1.dim a - 1 do
		let x = Bigarray.Array1.unsafe_get a i in
		let y = Bigarray.Array1.unsafe_get b i in
		assert_equal ~printer:(fun c -> Printf.sprintf "%02x" (int_of_char c)) x y
	done

let bigarray_to_string a =
	let s = Bytes.make (Bigarray.Array1.dim a) '\000' in
	for i = 0 to Bigarray.Array1.dim a - 1 do
		Bytes.set s i (Bigarray.Array1.unsafe_get a i)
	done;
	s

let with_xenstores f =
	let b1 = alloc_page () in
	let b2 = alloc_page () in
	let a = Cstruct.of_bigarray b1 in
	let b = Old_ring.C_Xenstore.of_buf b2 in
	Xenstore_ring.Ring.init a;
	Old_ring.C_Xenstore.zero b;
	f b1 b2 a b

let xenstore_init () =
	with_xenstores
		(fun b1 b2 _ _ ->
			compare_bufs b1 b2
		)

let xenstore_hello () =
	let msg = "hello" in
	let msg' = Bytes.of_string msg in
	let buf = String.make 16 '\000' in
	let buf' = Bytes.make 16 '\000' in
	with_xenstores
		(fun b1 b2 a b ->
			let x = Xenstore_ring.Ring.Front.unsafe_write a msg' 0 (Bytes.length msg') in
			let y = Old_ring.C_Xenstore.unsafe_write b msg (String.length msg) in
			assert_equal ~printer:string_of_int x y;
			compare_bufs b1 b2;
			let x = Xenstore_ring.Ring.Back.unsafe_read a buf' 0 (Bytes.length buf') in
			assert_equal ~printer:string_of_int x (String.length msg);
			assert_equal msg' (Bytes.sub buf' 0 x);
			let x = Old_ring.C_Xenstore.Back.unsafe_read b buf (String.length buf) in
			assert_equal ~printer:string_of_int x (String.length msg);
			assert_equal msg (String.sub buf 0 x);
			()
		)

[%%cstruct
type ring = {
	output: uint8_t [@len 1024];
	input: uint8_t [@len 1024];
	output_cons: uint32_t;
	output_prod: uint32_t;
	input_cons: uint32_t;
	input_prod: uint32_t;
} [@@little_endian]
]

let check_signed_unsigned_write () =
	(* Check for errors performing comparison across int32 max_int *)
	let msg = "this is a test" in
	let msg' = Bytes.of_string msg in
	let ofs = Int32.(succ (succ max_int)) in
	with_xenstores
		(fun b1 b2 a b ->
			set_ring_output_cons a ofs;
			set_ring_output_prod a ofs;
			set_ring_output_cons (Cstruct.of_bigarray b2) ofs;
			set_ring_output_prod (Cstruct.of_bigarray b2) ofs;
			let x = Xenstore_ring.Ring.Front.unsafe_write a msg' 0 (Bytes.length msg') in
			let y = Old_ring.C_Xenstore.unsafe_write b msg (String.length msg) in
			assert_equal ~printer:string_of_int x y;
			compare_bufs b1 b2;
		)

let check_signed_unsigned_read () =
	let msg = "this is a test" in
	let buf = String.make (String.length msg) '\000' in
	let buf' = Bytes.make (String.length msg) '\000' in
	with_xenstores
		(fun b1 b2 a b ->
			set_ring_output_cons a (Int32.(pred (pred max_int)));
			set_ring_output_prod a (Int32.(succ (succ max_int)));
			set_ring_output_cons (Cstruct.of_bigarray b2) (Int32.(pred (pred max_int)));
 			set_ring_output_prod (Cstruct.of_bigarray b2) (Int32.(succ (succ max_int)));
			let x' = Xenstore_ring.Ring.Back.unsafe_read a buf' 0 (Bytes.length buf') in
			let y' = Old_ring.C_Xenstore.Back.unsafe_read b buf (String.length buf) in
			assert_equal ~printer:string_of_int x' y';
			compare_bufs b1 b2;
		)

let with_consoles f =
	let b1 = alloc_page () in
	let b2 = alloc_page () in
	let a = Cstruct.of_bigarray b1 in
	let b = Old_ring.C_Console.of_buf b2 in
	Console_ring.Ring.init a;
	Old_ring.C_Console.zero b;
	f b1 b2 a b

let console_init () =
	with_consoles
		(fun b1 b2 _ _ ->
			compare_bufs b1 b2
		)

let console_hello () =
	let msg = "hello" in
	let msg' = Bytes.of_string msg in
	let buf = String.make 16 '\000' in
	let buf' = Bytes.make 16 '\000' in
	with_consoles
		(fun b1 b2 a b ->
			let x = Console_ring.Ring.Front.unsafe_write a msg' 0 (Bytes.length msg') in
			let y = Old_ring.C_Console.unsafe_write b msg (String.length msg) in
			assert_equal ~printer:string_of_int x y;
			compare_bufs b1 b2;
			let x = Console_ring.Ring.Back.unsafe_read a buf' 0 (Bytes.length buf') in
			assert_equal ~printer:string_of_int x (Bytes.length msg');
			assert_equal msg (Bytes.to_string @@ Bytes.sub buf' 0 x);
			let x = Old_ring.C_Console.Back.unsafe_read b buf (String.length buf) in
			assert_equal ~printer:string_of_int x (String.length msg);
			assert_equal msg (String.sub buf 0 x);
			()
		)

let block' =
	let buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout (15 * 1024 * 1024) in
	let counter = ref 0l in
	let c = Cstruct.of_bigarray buf in
	for i = 0 to Bigarray.Array1.dim buf / 4 - 1 do
		Cstruct.LE.set_uint32 c (i * 4) !counter;
		counter := Int32.add 1l !counter;
	done;
	buf

let throughput_test ~use_ocaml ~write_chunk_size ~read_chunk_size ~verify () =
	with_consoles
		(fun b1 b2 a b ->
			let read_chunk = String.make read_chunk_size '\000' in
			let input = bigarray_to_string block' in
			let length = Bytes.length input in
			let output = Bytes.make length 'X' in
			let producer = ref 0 in
			let consumer = ref 0 in
			let start = Unix.gettimeofday () in
			let end_of_transfer = ref false in
			while not(!end_of_transfer) do
				let remaining = length - !producer in
				let can_write = min write_chunk_size remaining in
				let written =
					if use_ocaml
					then Console_ring.Ring.Front.unsafe_write a input !producer can_write
					else Old_ring.C_Console.unsafe_write b (Bytes.to_string @@ Bytes.sub input !producer can_write) can_write in
				producer := !producer + written;
				let remaining = length - !consumer in
				let can_read = min read_chunk_size remaining in
				let read =
					if use_ocaml
					then Console_ring.Ring.Back.unsafe_read a output !consumer can_read
					else begin
						let n = Old_ring.C_Console.Back.unsafe_read b read_chunk can_read in
						begin
							try
								String.blit read_chunk 0 output !consumer n
							with e ->
								Printf.fprintf stderr "String.blit consumed=%d n=%d\n%!" !consumer n;
								raise e
						end;
						n
					end in
				(* verify *)
				if verify then begin
					let originally_written = Bytes.sub input !consumer read in
					let block_read = Bytes.sub output !consumer read in
					if originally_written <> block_read then begin
						Printf.fprintf stderr "producer = %d\nconsumer = %d\nwritten = %d\nread = %d\n%!" !producer !consumer written read;
						assert_equal ~msg:"transfer" ~printer:Bytes.to_string originally_written block_read
					end;
				end;

				consumer := !consumer + read;
				end_of_transfer := (written = 0) && (read = 0)
			done;
			let duration = Unix.gettimeofday () -. start in
			assert_equal ~msg:"consumer" ~printer:string_of_int length !consumer;
			assert_equal ~msg:"producer" ~printer:string_of_int length !producer;
			if not verify
			then Printf.fprintf stderr "%s read(%d) write(%d): %.2f MiB/sec\n"
				(if use_ocaml then "OCaml" else "C")
				read_chunk_size write_chunk_size
				(float_of_int !consumer /. duration /. (1024. *. 1024.))
		)

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test shared memory ring code";

  let suite = "ring" >:::
    [
		"xenstore_init" >:: xenstore_init;
(* XXX need to diagnose the ARM failure:
		"check_signed_unsigned_read" >:: check_signed_unsigned_read;
		"check_signed_unsigned_write" >:: check_signed_unsigned_write;
*)
                "xenstore_hello" >:: xenstore_hello;
		"console_init" >:: console_init;
		"console_hello" >:: console_hello;
		"ocaml throughput_test1" >:: throughput_test ~use_ocaml:true ~read_chunk_size:1024 ~write_chunk_size:1024 ~verify:false;
		"C throughput_test1" >:: throughput_test ~use_ocaml:false ~read_chunk_size:1024 ~write_chunk_size:1024 ~verify:false;
		"ocaml correctness_test2" >:: throughput_test ~use_ocaml:true ~read_chunk_size:1024 ~write_chunk_size:1024 ~verify:true;
		"C correctness_test2" >:: throughput_test ~use_ocaml:false ~read_chunk_size:1024 ~write_chunk_size:1024 ~verify:true;
		"ocaml correctness_test3" >:: throughput_test ~use_ocaml:true ~read_chunk_size:1023 ~write_chunk_size:1024 ~verify:true;
		"C correctness_test3" >:: throughput_test ~use_ocaml:false ~read_chunk_size:1023 ~write_chunk_size:1024 ~verify:true;
		"ocaml correctness_test4" >:: throughput_test ~use_ocaml:true ~read_chunk_size:1024 ~write_chunk_size:1023 ~verify:true;
		"C correctness_test4" >:: throughput_test ~use_ocaml:false ~read_chunk_size:1024 ~write_chunk_size:1023 ~verify:true;
    ] in
  run_test_tt ~verbose:!verbose suite

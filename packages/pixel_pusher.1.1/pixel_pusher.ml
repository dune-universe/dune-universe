open! Core
open! Async

let discovery_port = 7331
let command_port = 9897

(* magic numbers to reset the pixel pusher *)
let _reset_command = [| 0x40; 0x09; 0x2d; 0xa6; 0x15; 0xa5; 0xdd; 0xe5; 0x6a; 0x9d; 0x4d; 0x5a; 0xcf; 0x09; 0xaf; 0x50; 0x01 |]

module Color = Pp_color

module Beacon = struct
  module Strip_info = struct
    type t =
	{ rgbow         : bool (* high CRI strip *)
	; widepixels    : bool (* 48 bit/pixel RGBrgb *)
	; logarithmic   : bool (* LED has logarithmic response *)
	; motion        : bool (* a motion controller *)
	; notidempotent : bool (* a motion controller with side-effects *)
	}
    [@@deriving sexp]
    let of_wire bs =
      match%bitstring bs with
	| {| rgbow : 1
	   ; widepixels : 1
	   ; logarithmic : 1
	   ; motion : 1
	   ; notidempotent : 1
	   ; _unused : 3 |} ->
	  { rgbow; widepixels; logarithmic; motion; notidempotent }
  end
  type t =
      { mac_address : string
      ; ip_address  : string
      ; device_type : int
      ; protocol_version : int
      ; vendor_id : int
      ; product_id : int
      ; hw_revision : int
      ; sw_revision : int
      ; link_speed : int (* in bits per second *)
      ; strips_attached : int
      ; max_strips_per_packet : int
      ; pixels_per_strip : int
      ; update_period : Time.Span.t
      ; power_total : int (* in PWM units *)
      ; delta_sequence : int (* diff between received and expected sequence numbers *)
      ; controller_ordinal : int
      ; group_ordinal : int
      ; my_port : int
      ; strip_info : Strip_info.t Array.t
      ; protected : bool (* require qualified registry.getStrips() call (???) *)
      ; fixed_size : bool (* requires every datagram same size *)
      (* last host and port to drive this PP *)
      ; last_driven_ip : string
      ; last_driven_port : int
      }
  [@@deriving sexp]

  let of_wire s =
    match%bitstring Bitstring.bitstring_of_string s with
      | {| mac_address : 48 : bitstring
	; ip_address  : 32
	; device_type : 8
	; protocol_version : 8
	; vendor_id : 16 : littleendian
	; product_id : 16 : littleendian
	; hw_revision : 16 : littleendian
	; sw_revision : 16 : littleendian
	; link_speed : 32 : littleendian
	; strips_attached : 8
	; max_strips_per_packet : 8
	; pixels_per_strip : 16 : littleendian
	; update_period : 32 : littleendian
	; power_total : 32 : littleendian
	; delta_sequence : 32 : littleendian
	; controller_ordinal : 32 : littleendian
	; group_ordinal : 32 : littleendian
	; _artnet_universe : 16 : littleendian
	; _artnet_channel : 16 : littleendian
	; my_port : 16 : littleendian
	; strip_info : 64 : bitstring
	; _padding : 16
	; protected : 1
	; fixed_size : 1
	; _unused_flags : 30 : bitstring (* flags for the whole pusher *)
	; _segments : 32 : littleendian (* number of segments in each strip *)
	; _power_domain : 32 : littleendian
	; last_driven_ip : 32 : littleendian
	; last_driven_port : 16 : littleendian |} ->

	if device_type <> 2 then failwithf "Unsupported device type: %d" device_type ();
	let mac_address =
	  match%bitstring mac_address with
	    | {| a : 8; b : 8; c : 8; d : 8; e : 8; f : 8 |} ->
	      sprintf "%02x:%02x:%02x:%02x:%02x:%02x" a b c d e f
	in
	let strip_info =
	  match%bitstring strip_info with
	    | {| a : 8 : bitstring; b : 8 : bitstring
	      ; c : 8 : bitstring; d : 8 : bitstring
	      ; e : 8 : bitstring; f : 8 : bitstring
	      ; g : 8 : bitstring; h : 8 : bitstring |} ->
	      let z = Strip_info.of_wire in
	      [| z a; z b; z c; z d; z e; z f; z g; z h |]
	in
	let to_int a = Option.value_exn (Int32.to_int a) in
	let ip_address = Unix.Inet_addr.inet4_addr_of_int32 ip_address |> Unix.Inet_addr.to_string in
	let last_driven_ip = Unix.Inet_addr.inet4_addr_of_int32 last_driven_ip |> Unix.Inet_addr.to_string in
	let update_period =
	  (* comes over the wire in microseconds *)
	  Time.Span.of_ms (Int32.to_float update_period /. 1000.0)
	in
	{ mac_address; ip_address; device_type; protocol_version; vendor_id; product_id
	; hw_revision; sw_revision; link_speed = to_int link_speed; strips_attached; max_strips_per_packet
	; pixels_per_strip; update_period; power_total = to_int power_total
	; delta_sequence = to_int delta_sequence ; controller_ordinal = to_int controller_ordinal
	; group_ordinal = to_int group_ordinal; my_port; strip_info; protected; fixed_size
	; last_driven_ip; last_driven_port }
end

module Controller_report = struct
  type t =
      { controller_id : int
      ; group_id : int
      ; update_period : Time.Span.t
      ; last_beacon : Time.t }
end

module Strip = struct
  type t =
      { strip_number: int
      ; strip_length : int
      ; controller_id : int
      ; group_id : int
      ; matrix : Color.t Array.t }
  let set_pixel t ~color ~index =
    (* TODO: fix me *)
    if index > t.strip_length then ()
    else
     (* failwithf "Strip.set_pixel: exceded strip length for %d|%d: %d > %d"
       t.controller_id t.strip_number index t.strip_length (); *)
      t.matrix.(t.strip_number * t.strip_length + index) <- color
end

module Pusher_state = struct
  type t =
      { beacon_time : Time.t
      ; beacon      : Beacon.t
      ; mutable seq : int
      ; matrix      : Color.t Array.t
      ; mutable last_command : Time.t
      ; socket      : Core.Unix.File_descr.t }
  let known_pushers = String.Table.create ()
  let strips = ref []
  let strips_map = ref Map.Poly.empty
  let update () =
    let strips' =
      Hashtbl.fold known_pushers ~init:[] ~f:(fun ~key:_ ~data acc ->
	let beacon = data.beacon in
	List.fold_left (List.range 0 beacon.Beacon.strips_attached) ~init:acc ~f:(fun acc i ->
	  { Strip.strip_number = i
	  ; strip_length = beacon.Beacon.pixels_per_strip
	  ; controller_id = beacon.Beacon.controller_ordinal
	  ; group_id = beacon.Beacon.group_ordinal
	  ; matrix = data.matrix } :: acc))
    in
    let strips_map' =
      List.fold_left strips' ~init:Map.Poly.empty ~f:(fun map strip ->
	let controller_id = strip.Strip.controller_id in
	let strip_id = strip.Strip.strip_number in
	let key = (controller_id, strip_id) in
	Map.set map ~key ~data:strip)
    in
    strips := strips';
    strips_map := strips_map'

  let drop_missing_pushers () =
    let threshold = sec 60. in
    let now = Time.now () in
    let drop =
      Hashtbl.fold known_pushers ~init:[] ~f:(fun ~key ~data acc ->
	let timestamp = data.beacon_time in
	if Time.Span.(>) (Time.diff now timestamp) threshold
	then (key, data.socket) :: acc
	else acc)
    in
    (*
    List.iter drop ~f:(fun (key, socket) ->
      printf "*** Forgetting about Pixel Pusher %s, hasn't been seen in awhile (>%s)\n%!"
	key (Time.Span.to_string threshold);
      (* Wait 10s to close the socket, just in case we have packets queued *)
      don't_wait_for (Clock.after (sec 10.) >>| fun () -> Core.Unix.close socket);
      Hashtbl.remove known_pushers key);
    *)
    if List.length drop > 0 then
      update ()
end

let send_now_or_soon pusher sendfun =
  (*let beacon = pusher.Pusher_state.beacon in*)
  (*let ip = beacon.Beacon.ip_address in*)
  let update_period = pusher.Pusher_state.beacon.Beacon.update_period in
  let run_at = Time.add pusher.Pusher_state.last_command update_period in
  if Time.(<) run_at (Time.now ()) then begin
    pusher.Pusher_state.last_command <- Time.now ();
    sendfun ()
  end else begin
    (*
    let overrun_span = sec 0.1 in
    if Time.Span.(>) (Time.diff run_at (Time.now ())) overrun_span then
      printf "*** PP %s next command scheduled for >%s in the future %s vs %s: update_period: %s; dropping\n"
	ip (Time.Span.to_string overrun_span) (Time.now () |> Time.to_string) (Time.to_string run_at)
	(Time.Span.to_string update_period)
    else begin
    *)
      pusher.Pusher_state.last_command <- run_at;
      don't_wait_for (Clock.at run_at >>| sendfun)
    (*end*)
  end

let send_pixels_to_pushers () =
  Hashtbl.iteri Pusher_state.known_pushers ~f:(fun ~key:ip ~data:pusher ->
    let beacon = pusher.Pusher_state.beacon in
    let socket = pusher.Pusher_state.socket in
    let addr = Unix.ADDR_INET (Unix.Inet_addr.of_string ip, command_port) in
    let pixels_per_strip = beacon.Beacon.pixels_per_strip in
    let strips_attached = beacon.Beacon.strips_attached in
    let max_strips_per_packet = beacon.Beacon.max_strips_per_packet in
    (* TODO: only send strips that have changed *)
    let stripss =
      List.groupi (List.range 0 strips_attached)
	~break:(fun i _x _y -> i mod max_strips_per_packet = 0)
    in
    let matrix = pusher.Pusher_state.matrix in
    List.iteri stripss ~f:(fun seq_index strips ->
      let packet_size =
	let num_strips = List.length strips in
	assert (num_strips <= max_strips_per_packet);
	  4 (* 32-bit sequence *)
	+ 1*num_strips (* 8-bit strip indices *)
	+ 3*num_strips*pixels_per_strip (* 24 bit rgb data *)
      in
      let buf = Bytes.create packet_size in
      let seq = pusher.Pusher_state.seq + seq_index in
      let char = Char.of_int_exn in
      let set b i c = Bytes.set b i c in
      (*
      buf.[0] <- char ((seq lsr 24) land 0xFF);
      buf.[1] <- char ((seq lsr 16) land 0xFF);
      buf.[2] <- char ((seq lsr  8) land 0xFF);
      buf.[3] <- char (seq land 0xFF);
      *)
      set buf 0 (char ((seq lsr 24) land 0xFF));
      set buf 1 (char ((seq lsr 16) land 0xFF));
      set buf 2 (char ((seq lsr  8) land 0xFF));
      set buf 3 (char (seq land 0xFF));

      List.iteri strips ~f:(fun strip_index strip_num ->
	let strip_base = 4 + strip_index*(1+pixels_per_strip*3) in
        set buf strip_base (char strip_num);
	(*buf.[strip_base] <- char strip_num;*)
	let pixels_base = strip_base+1 in
	for pixel_num=0 to pixels_per_strip-1; do
	  let pixel = matrix.(strip_num*pixels_per_strip + pixel_num) in
          set buf (pixels_base + pixel_num*3    ) (char (Color.r pixel));
	  set buf (pixels_base + pixel_num*3 + 1) (char (Color.g pixel));
	  set buf (pixels_base + pixel_num*3 + 2) (char (Color.b pixel))
          (*
	  buf.[pixels_base + pixel_num*3    ] <- char (Color.r pixel);
	  buf.[pixels_base + pixel_num*3 + 1] <- char (Color.g pixel);
	  buf.[pixels_base + pixel_num*3 + 2] <- char (Color.b pixel)
          *)
	done);
      send_now_or_soon pusher (fun () ->
	let bytes_sent = Core.Unix.sendto socket ~buf ~pos:0 ~len:packet_size ~mode:[] ~addr in
	if bytes_sent < packet_size then
	  failwithf "Failed to send %d bytes to %s (%d bytes short)"
	    bytes_sent ip (Bytes.length buf - bytes_sent) ()));
    pusher.Pusher_state.seq <- pusher.Pusher_state.seq + (List.length stripss))

let setup_refresh_loop_for_non_async read_fd =
  let rec wait_for_refresh fd =
    Reader.read_char fd >>= fun event ->
    match event with
      | `Eof -> failwithf "wait_for_refresh: unexpected eof!" ()
      | `Ok 'r' ->
	send_pixels_to_pushers ();
	wait_for_refresh fd
      | `Ok char ->
	failwithf "wait_for_refresh: unexpected char: %c" char ()
  in
  let fd = Fd.create Fd.Kind.Fifo read_fd (Info.of_string "nurple") in
  Fd.clear_nonblock fd;
  wait_for_refresh (Reader.create fd)

type non_async_token = Core.Unix.File_descr.t

let send_updates () =
  send_pixels_to_pushers ();
  return ()

let send_updates_from_non_async_thread (fd : non_async_token) =
  (* TODO: add logic to go boom if this is called from an Async thread *)
  let buf = Bytes.of_string "r" in
  if Core.Unix.write fd ~buf ~pos:0 ~len:1 <> 1 then
    failwithf "couldn't write one char to update fd" ()

let get_controllers () =
  List.map (Hashtbl.data Pusher_state.known_pushers) ~f:(fun data ->
    let beacon = data.Pusher_state.beacon in
    { Controller_report.
      controller_id = beacon.Beacon.controller_ordinal
    ; group_id = beacon.Beacon.group_ordinal
    ; update_period = beacon.Beacon.update_period
    ; last_beacon = data.Pusher_state.beacon_time })

let get_strips () =
  !Pusher_state.strips

let get_strips_as_map () =
  !Pusher_state.strips_map

let start_discovery_listener () =
  (*printf "*** Starting Pixel Pusher listener on port %d...\n%!" discovery_port;*)
  let addr = `Inet (Unix.Inet_addr.bind_any, discovery_port) in
  let socket = Async_udp.bind addr in
  let fd = Socket.fd socket in
  Async_udp.recvfrom_loop fd
    (fun buf _addr ->
      (*let addr_s = Socket.Address.Inet.to_string addr in*)
      let beacon = Iobuf.to_string buf |> Beacon.of_wire in
      let key = beacon.Beacon.ip_address in
      let my_port = beacon.Beacon.my_port in
      if my_port <> command_port then
	failwithf "*** PP %s's command port is %d, not %d" key my_port command_port ();
      let num_pixels = Beacon.(beacon.pixels_per_strip * beacon.strips_attached) in
      match Hashtbl.find Pusher_state.known_pushers key with
	| Some state ->
	  let mlen = Array.length state.Pusher_state.matrix in
	  if mlen <> num_pixels then
	    failwithf "*** PP %s's dimensions changed from %d to %d pixels" key mlen num_pixels ();
	  let data = { state with Pusher_state.beacon_time = Time.now (); beacon } in
	  Hashtbl.set Pusher_state.known_pushers ~key ~data
	| None ->
	  (*printf "*** Discovered new Pixel Pusher: %s\n" addr_s;
	  printf "%s\n" (Beacon.sexp_of_t beacon |> Sexp.to_string_hum ~indent:2);*)
	  let matrix = Array.init num_pixels ~f:(fun _ -> Color.black) in
	  Hashtbl.add_exn Pusher_state.known_pushers ~key
	    ~data:{ Pusher_state.beacon_time = Time.now ()
		  ; beacon = beacon
		  ; seq = 0
		  ; matrix = matrix
		  ; last_command = Time.epoch
		  ; socket = Caml.Unix.socket Caml.Unix.PF_INET Caml.Unix.SOCK_DGRAM 0 };
	  Pusher_state.update ())

let start () =
  let read_fd, write_fd = Core.Unix.pipe () in
  don't_wait_for (setup_refresh_loop_for_non_async read_fd);
  don't_wait_for (
    start_discovery_listener () >>= fun loop_result ->
    match loop_result with
    | Async_udp.Loop_result.Stopped
    | Async_udp.Loop_result.Closed ->
      return ());
  Clock.every (sec 1.) Pusher_state.drop_missing_pushers;
  return write_fd

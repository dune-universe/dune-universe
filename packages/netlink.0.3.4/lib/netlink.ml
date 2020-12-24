open Ctypes
open Foreign

(* --- to be upstreamed to ctypes --- *)
let castp typ p = from_voidp typ (to_voidp p)

let read_nullable t p =
  if p = null then None
    else Some !@(castp t (allocate (ptr void) p))

let write_nullable t = function
  | None -> null
  | Some f -> !@(castp (ptr void) (allocate t f))

let nullable_view t =
  let read = read_nullable t
    and write = write_nullable t in
  view ~read ~write (ptr void)

let string_opt = nullable_view string
(* --- *)

(* The library names vary by distribution, so use a search list *)

let libnl_names = [
	"libnl-3.so";
	"libnl-3.so.200"; (* Debian/Ubuntu *)
]

let libnl_route_names = [
	"libnl-route-3.so";
	"libnl-route-3.so.200"; (* Debian/Ubuntu *)
]

let dlopen ~filenames ~flags =
	let rec loop = function
	| [] -> failwith (Printf.sprintf "Failed to open any of these libraries: [ %s ] (is the package missing?)" (String.concat ", " filenames))
	| n :: ns ->
		try
			Dl.dlopen ~filename:n ~flags
		with _ ->
			loop ns in
	loop filenames

let libnl = dlopen ~filenames:libnl_names ~flags:[Dl.RTLD_LAZY]
let libnl_route = dlopen ~filenames:libnl_route_names ~flags:[Dl.RTLD_LAZY]

module Socket = struct
	type t
	let t : t structure typ = structure "nl_sock"

	type protocol = NETLINK_ROUTE

	let int_of_protocol = function
		| NETLINK_ROUTE -> 0

	let protocol_of_int = function
		| 0 -> NETLINK_ROUTE
		| _ -> invalid_arg "protocol"

	let protocol = view ~read:protocol_of_int ~write:int_of_protocol int

	let alloc = foreign ~from:libnl "nl_socket_alloc" (void @-> returning (ptr t))
	let free = foreign ~from:libnl "nl_socket_free" (ptr t @-> returning void)

	exception Connect_failed

	let connect' = foreign ~from:libnl "nl_connect" (ptr t @-> protocol @-> returning int)
	let connect s p =
		let ret = connect' s p in
		if ret = 0 then
			()
		else
			raise Connect_failed

	let close = foreign ~from:libnl "nl_close" (ptr t @-> returning void)
end

module Cache = struct
	let t = ptr void

	let free' = foreign ~from:libnl "nl_cache_free"
		(t @-> returning void)
	let free cache = free' (!@ cache)

	let iter f cache ty =
		let callback_t = ptr ty @-> ptr void @-> returning void in
		let foreach = foreign ~from:libnl "nl_cache_foreach"
			(t @-> funptr callback_t @-> ptr void @-> returning void) in
		let f' x _ = f x in
		foreach (!@ cache) f' null

	let to_list cache ty =
		let get_first = foreign ~from:libnl "nl_cache_get_first" (t @-> returning (ptr ty)) in
		let get_prev = foreign ~from:libnl "nl_cache_get_prev" (ptr ty @-> returning (ptr ty)) in
		let get_last = foreign ~from:libnl "nl_cache_get_last" (t @-> returning (ptr ty)) in

		let first = get_first (!@ cache) in
		let rec loop obj ac =
			if obj = first then
				obj :: ac
			else
				loop (get_prev obj) (obj :: ac)
		in
		loop (get_last (!@ cache)) []
end

type addr
let addr : addr structure typ = structure "nl_addr"

let addr_to_string' = foreign ~from:libnl "nl_addr2str"
	(ptr addr @-> string @-> returning string)

let addr_to_string addr =
	let buf = String.make 128 ' ' in
	addr_to_string' addr buf

module Link = struct
	type t

	type stat_id = RX_PACKETS | TX_PACKETS | RX_BYTES | TX_BYTES | RX_ERRORS | TX_ERRORS

	exception AllocCacheError

	let int_of_stat_id = function
		| RX_PACKETS -> 0
		| TX_PACKETS -> 1
		| RX_BYTES -> 2
		| TX_BYTES -> 3
		| RX_ERRORS -> 4
		| TX_ERRORS -> 5

	let stat_id_of_int = function
		| 0 -> RX_PACKETS
		| 1 -> TX_PACKETS
		| 2 -> RX_BYTES
		| 3 -> TX_BYTES
		| 4 -> RX_ERRORS
		| 5 -> TX_ERRORS
		| _ -> invalid_arg "stat_id"

	let stat_id = view ~read:stat_id_of_int ~write:int_of_stat_id int

	let t : t structure typ = structure "rtnl_link"

	let alloc_cache' = foreign ~from:libnl_route "rtnl_link_alloc_cache"
		(ptr Socket.t @-> int @-> ptr Cache.t @-> returning int)

	let cache_alloc s =
		let cache = allocate Cache.t null in
		let ret = alloc_cache' s 0 cache in
		if ret = 0 then cache else raise AllocCacheError

	let cache_iter f cache =
		Cache.iter f cache t

	let cache_to_list cache =
		Cache.to_list cache t

	let get_by_name = foreign ~from:libnl_route "rtnl_link_get_by_name"
		(Cache.t @-> string @-> returning (ptr t))

	let put = foreign ~from:libnl_route "rtnl_link_put"
		(ptr t @-> returning void)

	let get_ifindex = foreign ~from:libnl_route "rtnl_link_get_ifindex"
		(ptr t @-> returning int)

	let get_name = foreign ~from:libnl_route "rtnl_link_get_name"
		(ptr t @-> returning string)

	let get_mtu = foreign ~from:libnl_route "rtnl_link_get_mtu"
		(ptr t @-> returning int)

	let get_stat = foreign ~from:libnl_route "rtnl_link_get_stat"
		(ptr t @-> stat_id @-> returning uint64_t)

	let get_addr = foreign ~from:libnl_route "rtnl_link_get_addr"
		(ptr t @-> returning (ptr addr))
end

module Address = struct
	type t
	let t : t structure typ = structure "rtnl_addr"

	let alloc_cache' = foreign ~from:libnl_route "rtnl_addr_alloc_cache"
		(ptr Socket.t @-> ptr Cache.t @-> returning int)

	let cache_alloc s =
		let cache = allocate Cache.t null in
		let _ = alloc_cache' s cache in
		cache

	let cache_iter f cache =
		Cache.iter f cache t

	let cache_to_list cache =
		Cache.to_list cache t

	let get_ifindex = foreign ~from:libnl_route "rtnl_addr_get_ifindex"
		(ptr t @-> returning int)

	let get_label = foreign ~from:libnl_route "rtnl_addr_get_label"
		(ptr t @-> returning string_opt)

	let get_local = foreign ~from:libnl_route "rtnl_addr_get_local"
		(ptr t @-> returning (ptr addr))
end

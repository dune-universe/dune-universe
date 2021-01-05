exception PortAudio_exn of Portaudio_ffi.PaError.t * string

module C_ffi = Portaudio_ffi

module PaError = C_ffi.PaError

let global_terminate_on_exn = ref true

let throw e = 
    let s = C_ffi.get_error_text e in
    raise (PortAudio_exn (e, s))

let check_error = function
    | C_ffi.PaError.NoError -> ()
    | err -> 
        if !global_terminate_on_exn then (
            try 
                let err = C_ffi.terminate() in
                match err with
                | C_ffi.PaError.NoError -> ()
                | err -> print_endline (C_ffi.get_error_text err)
            with e ->
                print_endline Printexc.(to_string e)
        );
        throw err
;;

module SampleFormat = struct
    type float32
    type int32
    type int24
    type int16
    type int8
    type custom

    type interleaved
    type non_interleaved

    type ('a, 'b, 'c) t = I_Float32 : (float, float32, interleaved) t
                        | I_Int32 : (Signed.Int32.t, int32, interleaved) t
                        | I_Int24 : (int, int24, interleaved) t
                        | I_Int16 : (int, int16, interleaved) t
                        | I_Int8  : (int, int8, interleaved) t
                        | I_Custom : (unit, custom, interleaved) t

                        | N_Float32 : (float, float32, non_interleaved) t
                        | N_Int32 : (Signed.Int32.t, int32, non_interleaved) t
                        | N_Int24 : (int, int24, non_interleaved) t
                        | N_Int16 : (int, int16, non_interleaved) t
                        | N_Int8  : (int, int8, non_interleaved) t
                        | N_Custom : (unit, custom, non_interleaved) t

    let is_interleaved : type a b c. (a, b, c) t -> bool = function
        | I_Float32
        | I_Int32
        | I_Int24
        | I_Int16
        | I_Int8
        | I_Custom -> true

        | N_Float32
        | N_Int32
        | N_Int24
        | N_Int16
        | N_Int8
        | N_Custom -> false

    let non_interleave t =
        Unsigned.ULong.logor t C_ffi.SampleFormat.non_interleaved

    let format_to_cffi (type a) (type b) (type c) : (a, b, c) t -> C_ffi.SampleFormat.t = function
        | I_Float32 -> C_ffi.SampleFormat.float32
        | I_Int32 -> C_ffi.SampleFormat.int32
        | I_Int24 -> C_ffi.SampleFormat.int24
        | I_Int16 -> C_ffi.SampleFormat.int16
        | I_Int8 -> C_ffi.SampleFormat.int8
        | I_Custom -> C_ffi.SampleFormat.custom_format

        | N_Float32 -> C_ffi.SampleFormat.float32 |> non_interleave
        | N_Int32 -> C_ffi.SampleFormat.int32 |> non_interleave
        | N_Int24 -> C_ffi.SampleFormat.int24 |> non_interleave
        | N_Int16 -> C_ffi.SampleFormat.int16 |> non_interleave
        | N_Int8 -> C_ffi.SampleFormat.int8 |> non_interleave
        | N_Custom -> C_ffi.SampleFormat.custom_format |> non_interleave
    ;;
end

type device_index = int
type pa_time = float

open Ctypes

module StreamParameters = struct
    type ('a, 'b, 'c) t = {
        device : device_index;
        channel_count : int;
        sample_format : ('a, 'b, 'c) SampleFormat.t;
        suggested_latency : pa_time;
    }

    let to_cffi inp =
      let open C_ffi.StreamParameters in
      let params = allocate_n C_ffi.StreamParameters.t ~count:1 in
      let format = SampleFormat.format_to_cffi inp.sample_format in
      setf (!@params) device inp.device;
      setf (!@params) channel_count inp.channel_count;
      setf (!@params) sample_format format;
      setf (!@params) suggested_latency inp.suggested_latency;
      params 
end

module View = struct
    type ('a, 'b) t = {
        arr : 'a CArray.t;
        set : 'a CArray.t -> int -> 'a -> unit;
        get : 'a CArray.t -> int -> 'a;
        length : int;
    }
    let set t = t.set t.arr
    let get t = t.get t.arr
    let length t = t.length
    let buffer t = t.arr

    (* Probably not that safe, do we need to keep the root alive? *)
    let slice (orig : ('a, 'b) t) ~start ~len = 
        assert (start < orig.length);
        assert (start + len < orig.length);
        {
            arr = CArray.from_ptr (CArray.start orig.arr +@ start) len;
            length = len;
            set = orig.set;
            get = orig.get;
        }

    let create_basic arr = {
        arr;
        set = CArray.set;
        get = CArray.get;
        length = CArray.length arr;
    }

    let create typ ~len =
        create_basic (CArray.make typ len)

    let of_views (views : ('a, 'b) t array) =
        let typ = CArray.element_type views.(0).arr in
        let len = Array.length views in
        let arr = CArray.make (ptr typ) len in
        for i=0 to len-1 do
            CArray.set arr i (CArray.start views.(i).arr)
        done;
        create_basic arr
end

module Stream = struct
    module Flags = struct
        type t = C_ffi.StreamFlags.t
    end
    

    module Callback = struct
        module StatusFlags = C_ffi.StreamCallbackFlags
        module Result = C_ffi.StreamCallbackResult

        type time_info = {
            input_buffer_adc_time : pa_time;
            current_time : pa_time;
            output_buffer_dac_time : pa_time;
        }

        let time_info_of_cffi ptr : time_info =
            let open C_ffi.StreamCallbackTimeInfo in
            let input_buffer_adc_time = getf (!@ptr) input_buffer_adc_time in
            let current_time = getf (!@ptr) current_time in
            let output_buffer_dac_time = getf (!@ptr) output_buffer_dac_time in
            { input_buffer_adc_time; current_time; output_buffer_dac_time }
        ;;

        type cb_result = C_ffi.StreamCallbackResult.t

        type ('a, 'b, 'c, 'd) t = 
            ('a, 'b) View.t array -> ('c, 'd) View.t array -> time_info:time_info -> status:StatusFlags.t -> cb_result
    end

    type ('inp_fmt, 'inp_inter, 'out_fmt', 'out_inter) stream = {
        stream : unit ptr ptr;
        callback : C_ffi.pa_stream_callback option;
    }

    let mk_interleaved typ count = 
        (fun buf len ->
         let arr = CArray.from_ptr (from_voidp typ buf) (len*count) in
         Array.init count (fun i ->
             View.{
                 arr;
                 set = (fun arr idx v -> CArray.set arr (idx*count+i) v);
                 get = (fun arr idx -> CArray.get arr (idx*count+i));
                 length = len;
             }
         )
    );;

    let mk_non_interleaved typ count = 
        (fun buf len ->
         let buf = CArray.from_ptr (from_voidp (ptr typ ) buf) count in
         Array.init count (fun i ->
            CArray.from_ptr (CArray.get buf i) len |> View.create_basic
         )
    );;

    let set_sample_24 arr offset value =
         let value = if value < 0 then value + 0x1000000 else value in
         CArray.set arr (offset + 0) value;
         CArray.set arr (offset + 1) (value lsr 8);
         CArray.set arr (offset + 2) (value lsr 16);
    ;;
    

    let _24_bit_mask = ~-1 - 0xffffff
    let get_sample_24 arr offset =
         let v1 = CArray.get arr (offset + 0) land 0xff in
         let v2 = CArray.get arr (offset + 1) land 0xff in
         let v3 = CArray.get arr (offset + 2) land 0xff in
         let num = (v3 lsl 16) lor (v2 lsl 8) lor v1 in
         if v3 land 0x80 > 0 then (
             num lor _24_bit_mask
         ) else num
    ;;

    let mk_24bit_interleaved typ count = 
        (fun buf len ->
         let arr = CArray.from_ptr (from_voidp typ buf) (len*count*3) in
         Array.init count (fun i ->
             View.{
                 arr;
                 set = (fun arr idx v -> 
                     let offset = count*idx*3 + i*3 in
                     set_sample_24 arr offset v
                 );
                 get = (fun arr idx -> 
                     let offset = count*idx*3 + i*3 in
                     get_sample_24 arr offset
                 );
                 length = len;
             }
         )
    );;

    let mk_24bit_non_interleaved typ count = 
        (fun buf len ->
         let buf = CArray.from_ptr (from_voidp (ptr typ) buf) count in
         Array.init count (fun i ->
            let arr = CArray.from_ptr (CArray.get buf i) (len*3) in
             View.{
                 arr;
                 set = (fun arr idx v -> 
                     let offset = idx*3 in
                     set_sample_24 arr offset v
                 );
                 get = (fun arr idx -> 
                     let offset = idx*3 in
                     get_sample_24 arr offset
                 );
                 length = len;
             }
         )
    );;

    let wrap_callback : type a b c d e f. 
        (int * (a, b, c) SampleFormat.t) option
        -> (int * (d, e, f) SampleFormat.t) option
        -> (a, c, d, f) Callback.t 
        -> C_ffi.pa_stream_callback
    = fun format_in format_out callback ->
        let staged specialized_inp specialized_out =
            (fun inp out frames time_info status _ -> 
                let frames = Unsigned.ULong.to_int frames in
                let inp = specialized_inp inp frames in
                let out = specialized_out out frames in
                let time_info = Callback.time_info_of_cffi time_info in
                callback inp out ~time_info ~status
            )
        in
        let get_build_fn : type a b c. 
            (a, b, c) SampleFormat.t -> (int -> unit ptr -> int -> (a, c) View.t array) = function
            | I_Float32 -> mk_interleaved float
            | I_Int32 -> mk_interleaved int32_t
            | I_Int24 -> mk_24bit_interleaved int8_t
            | I_Int16 -> mk_interleaved int16_t
            | I_Int8 -> mk_interleaved int8_t
            | I_Custom -> mk_interleaved void

            | N_Float32 -> mk_non_interleaved float
            | N_Int32 -> mk_non_interleaved int32_t
            | N_Int24 -> mk_24bit_non_interleaved int8_t
            | N_Int16 -> mk_non_interleaved int16_t
            | N_Int8 -> mk_non_interleaved int8_t
            | N_Custom -> mk_non_interleaved void
        in
        let in_fun = 
            match format_in with
            | Some (ch_count, fmt) -> get_build_fn fmt ch_count
            | None -> (fun _ _ -> [||])
        in
        let out_fun = 
            match format_out with
            | Some (ch_count, fmt) -> get_build_fn fmt ch_count
            | None -> (fun _ _ -> [||])
        in
        staged in_fun out_fun
    ;;

    let get_format_from_stream_params : 
        type a b c. (a, b, c) StreamParameters.t option -> (int * (a, b, c) SampleFormat.t) option
        = function
        | None -> None
        | Some params ->
            if params.channel_count > 0 then (
                Some (params.channel_count, params.sample_format)
            ) else None
    ;;

    let open_stream 
        (type a) (type b) (type c) (type d) (type e) (type f)
        ?input_params
        ?output_params
        ~sample_rate 
        ~frames_per_buffer 
        ~stream_flags
        ?callback
        ()
        : (a, c, d, f) stream =
            let stream = allocate (ptr void) null in
            let input = match input_params with
                      | None -> from_voidp (C_ffi.StreamParameters.t) null
                      | Some (inp : (a, b, c) StreamParameters.t) -> StreamParameters.to_cffi inp
            in
            let output = match output_params with
                       | None -> from_voidp (C_ffi.StreamParameters.t) null
                       | Some (out : (d, e, f) StreamParameters.t)-> StreamParameters.to_cffi out
            in 
            let frames = Unsigned.ULong.of_int frames_per_buffer in
            let pa_callback = match callback with
                         | None -> None
                         | Some (cb : (a, c, d, f) Callback.t) ->
                            let format_in = get_format_from_stream_params input_params in
                            let format_out = get_format_from_stream_params output_params in
                            Some (wrap_callback 
                                format_in
                                format_out
                                cb)
            in
            C_ffi.open_stream stream input output sample_rate frames stream_flags pa_callback null
             |> check_error;
             {stream; callback=pa_callback}
    ;;

    let open_default_stream
        (type a) (type b)
        ~num_input_channels 
        ~num_output_channels 
        ~format 
        ~sample_rate
        ~frames_per_buffer
        ?callback
        ()
        : (a, b, a, b) stream =
        let stream = allocate (ptr void) null in
        let ffi_format = SampleFormat.format_to_cffi format in
        let pa_callback = match callback with
                     | None -> None
                     | Some (cb : (a, b, a, b) Callback.t) -> 
                         let inp = if num_input_channels > 0 then
                             Some (num_input_channels, format)
                             else None
                         in
                         let out = if num_output_channels > 0 then 
                             Some (num_output_channels, format)
                             else None
                         in
                         Some (wrap_callback inp out cb)
        in
        let frames_per_buffer = Unsigned.ULong.of_int frames_per_buffer in
        C_ffi.open_default_stream 
            stream 
            num_input_channels 
            num_output_channels 
            ffi_format 
            sample_rate 
            frames_per_buffer
            pa_callback
            null
        |> check_error;
         {stream; callback=pa_callback}
    ;;

    type stream_info = {
        version : int;
        input_latency : pa_time;
        output_latency : pa_time;
        sample_rate : float;
    }

    let start t = C_ffi.start_stream !@(t.stream) |> check_error
    let stop t = C_ffi.stop_stream !@(t.stream) |> check_error
    let close t = C_ffi.close_stream !@(t.stream) |> check_error
    let abort t = C_ffi.abort_stream !@(t.stream) |> check_error

    let is_stopped t = 
        match C_ffi.is_stream_stopped !@(t.stream) with
        | Other 1 -> true
        | NoError -> false
        | value -> throw value
    ;;

    let is_active t =
        match C_ffi.is_stream_active !@(t.stream) with
        | Other 1 -> true
        | NoError -> false
        | value -> throw value
    ;;

    let get_info t =
        let info = C_ffi.get_stream_info !@(t.stream) in
        let open C_ffi.StreamInfo in
        let version = getf (!@info) struct_version in
        let input_latency = getf (!@info) input_latency in
        let output_latency = getf (!@info) output_latency in
        let sample_rate = getf (!@info) sample_rate in
        { version; input_latency; output_latency; sample_rate }
    ;;

    let time t = C_ffi.get_stream_time !@(t.stream)
    let cpu_load t = C_ffi.get_stream_cpu_load !@(t.stream)

    let do_read_write t fn buf len =
        let buf = CArray.start buf |> to_voidp in
        let len = Unsigned.ULong.of_int len in
        fn !@(t.stream) buf len |> check_error
    ;;

    let read_interleaved t (view : ('a, SampleFormat.interleaved) View.t) =
         do_read_write t C_ffi.read_stream view.arr view.length
    ;;

    let read_non_interleaved t (views : ('a, SampleFormat.non_interleaved) View.t array) =
        let arrs = View.of_views views in
        let len = View.length views.(0) in
        do_read_write t C_ffi.read_stream arrs.arr len
    ;;

    let write_interleaved t (view : ('a , SampleFormat.interleaved) View.t) =
        do_read_write t C_ffi.write_stream view.arr view.length
    ;;

    let write_non_interleaved t (views : ('a, SampleFormat.non_interleaved) View.t array) =
        let arrs = View.of_views views in
        let len = View.length views.(0) in
        do_read_write t C_ffi.write_stream arrs.arr len
    ;;

    let read_available t = 
        C_ffi.get_stream_read_available !@(t.stream) |> Signed.Long.to_int

    let write_available t =
        C_ffi.get_stream_write_available !@(t.stream) |> Signed.Long.to_int

    let set_finished_callback t callback =
        let callback = match callback with
                     | None -> None
                     | Some cb -> Some (fun _ -> cb())
        in
        C_ffi.set_stream_finished_callback !@(t.stream) callback |> check_error
end

let initialize ?(terminate_on_exn=true) () = 
    global_terminate_on_exn := terminate_on_exn; 
    C_ffi.initialize() |> check_error

let terminate () = C_ffi.terminate() |> check_error
let sleep ms = C_ffi.sleep Signed.Long.(of_int ms)
let sample_size format =
    let fmt = SampleFormat.format_to_cffi format in
    let error = C_ffi.get_sample_size fmt in
    match error with
    | NoError -> 0
    | Other v -> v
    | e -> throw e
;;

module VersionInfo = struct
    type t = {
        major : int;
        minor : int;
        sub_minor : int;
        version_control_revision : string;
        version_text : string;
    }
end

module HostApiTypeId = C_ffi.HostApiTypeId

module HostApiInfo = struct
    type t = {
        version : int;
        type_ : HostApiTypeId.t;
        name : string;
        device_count : int;
        default_input_device : device_index;
        default_output_device : device_index;
    }
end

module DeviceInfo = struct
    type t = {
        version : int;
        name : string;
        host_api_index : int;
        max_input_channels : int;
        max_output_channels : int;
        default_low_input_latency : pa_time;
        default_low_output_latency : pa_time;
        default_high_input_latency : pa_time;
        default_high_output_latency : pa_time;
        default_sample_rate : float;
    }
end

let get_version () = C_ffi.get_version()
let get_version_text () = C_ffi.get_version_text()
let get_version_info () =
    let open C_ffi.VersionInfo in
    let ptr = C_ffi.get_version_info() in
    let t = !@ptr in
    let major = getf t version_major in
    let minor = getf t version_minor in
    let sub_minor = getf t version_sub_minor in
    let version_control_revision = getf t version_control_revision in
    let version_text = getf t version_text in
    VersionInfo.{
        major;
        minor;
        sub_minor; 
        version_control_revision;
        version_text;
    }
;;

let get_error_text = C_ffi.get_error_text
let get_host_api_count = C_ffi.get_host_api_count
let get_default_host_api = C_ffi.get_default_host_api
let get_host_api_info index =
    let info = C_ffi.get_host_api_info index in
    if Ctypes.is_null info then None
    else (
        let open C_ffi.HostApiInfo in
        let t = !@info in
        let version = getf t struct_version in 
        let type_ = getf t type_ in
        let name = getf t name in
        let device_count = getf t device_count in
        let default_input_device = getf t default_input_device in
        let default_output_device = getf t default_output_device in
        Some (HostApiInfo.{
            version;
            type_;
            name;
            device_count;
            default_input_device;
            default_output_device;
        })
    )
;;

let host_api_type_id_to_host_api_index id =
    let result = C_ffi.host_api_type_id_to_host_api_index id in
    if result < 0 then (
        None
    ) else (
        Some result
    )
;;

let get_device_count = C_ffi.get_device_count
let get_default_input_device = C_ffi.get_default_input_device
let get_default_output_device = C_ffi.get_default_output_device
let get_device_index index =
    if index < 0 then None
    else (
        let count = get_device_count() in
        if index >= count then None
        else Some index
    )

let get_device_info idx =
    let ptr = C_ffi.get_device_info idx in
    let open C_ffi.DeviceInfo in
    let t = !@ptr in
    let version = getf t struct_version in
    let name = getf t name in
    let host_api_index = getf t host_api_index in
    let max_input_channels = getf t max_input_channels in
    let max_output_channels = getf t max_output_channels in
    let default_low_input_latency = getf t default_low_input_latency in
    let default_low_output_latency = getf t default_low_output_latency in
    let default_high_input_latency = getf t default_high_input_latency in
    let default_high_output_latency = getf t default_high_output_latency in
    let default_sample_rate = getf t default_sample_rate in
    DeviceInfo.{
        version;
        name;
        host_api_index;
        max_input_channels;
        max_output_channels;
        default_low_input_latency;
        default_low_output_latency;
        default_high_input_latency;
        default_high_output_latency;
        default_sample_rate;
    }
;;

let is_format_supported input output ~sample_rate =
    let input = StreamParameters.to_cffi input in
    let output = StreamParameters.to_cffi output in
    let result = C_ffi.is_format_supported input output sample_rate in
    match result with	
    | NoError -> Result.ok ()
    | err -> Result.error err

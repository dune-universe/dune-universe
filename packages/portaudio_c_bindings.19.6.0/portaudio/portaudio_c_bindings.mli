(**
 Most functions in this module follow from {{: http://files.portaudio.com/docs/v19-doxydocs/} PortAudio Doxygen}
 as they are thin wrappers over the base PortAudio C library.
 *)
module C_ffi = Portaudio_ffi

module SampleFormat : sig
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

    val is_interleaved : ('a, 'b, 'c) t -> bool
end

module PaError = C_ffi.PaError

exception PortAudio_exn of PaError.t * string

type device_index
type pa_time = float

module StreamParameters : sig
    type ('a, 'b, 'c) t = {
        device : device_index;
        channel_count : int;
        sample_format : ('a, 'b, 'c) SampleFormat.t;
        suggested_latency : pa_time;
    }
end

(**
 This module is a thin wrapper around Ctypes.CArray.t so you should be careful how you use values of this type. It's mostly to avoid copying during the PortAudio callbacks. If you need to keep data around after the callback, copy it into another array that you manage.
 *)
module View : sig
    type ('a, 'b) t
    val set : ('a, 'b) t -> int -> 'a -> unit
    val get : ('a, 'b) t -> int -> 'a
    val length : ('a, 'b) t -> int
    val buffer : ('a, 'b) t -> 'a Ctypes.CArray.t

    val create : 'a Ctypes.typ -> len:int -> ('a, SampleFormat.non_interleaved) t
    val slice : ('a, 'b) t -> start:int -> len:int -> ('a, 'b) t
    val of_views : ('a, SampleFormat.non_interleaved) t array -> ('a Ctypes.ptr, SampleFormat.non_interleaved) t
end

module Stream : sig
    type ('inp_fmt, 'inp_inter, 'out_fmt, 'out_inter) stream

    module Flags : sig
        type t = C_ffi.StreamFlags.t
    end
    
    module Callback : sig

        module StatusFlags = C_ffi.StreamCallbackFlags

        type time_info = {
            input_buffer_adc_time : pa_time;
            current_time : pa_time;
            output_buffer_dac_time : pa_time;
        }

        module Result = C_ffi.StreamCallbackResult

        type cb_result = Result.t

        type ('in_format, 'in_interleaved, 'out_format, 'out_interleaved) t = 
            ('in_format, 'in_interleaved) View.t array 
            -> ('out_format, 'out_interleaved) View.t array 
            -> time_info:time_info 
            -> status:StatusFlags.t 
            -> cb_result
    end
    
    (**
     @param callback An optional callback for the stream. Do not save the view values passed into the callback. Copy the data if it's needed outside the callback.
     *)
    val open_stream :  
        ?input_params:('a, 'b, 'c) StreamParameters.t
        -> ?output_params:('d, 'e, 'f) StreamParameters.t
        -> sample_rate:float
        -> frames_per_buffer:int
        -> stream_flags:Flags.t
        -> ?callback:(('a, 'c, 'd, 'f) Callback.t)
        -> unit
        -> ('a, 'c, 'd, 'f) stream

    (**
     @param callback An optional callback for the stream. Do not save the view values passed into the callback. Copy the data if it's needed outside the callback.
     *)
    val open_default_stream :
        num_input_channels:int
        -> num_output_channels:int
        -> format:('a, 'b, 'c) SampleFormat.t
        -> sample_rate:float
        -> frames_per_buffer:int
        -> ?callback:(('a, 'c, 'a, 'c) Callback.t)
        -> unit
        -> ('a, 'c, 'a, 'c) stream

    type stream_info = {
        version : int;
        input_latency : pa_time;
        output_latency : pa_time;
        sample_rate : float;
    }

    val start : ('a, 'b, 'c, 'd) stream -> unit
    val stop  : ('a, 'b, 'c, 'd) stream -> unit
    val close : ('a, 'b, 'c, 'd) stream -> unit
    val abort : ('a, 'b, 'c, 'd) stream -> unit
    val is_stopped : ('a, 'b, 'c, 'd) stream -> bool
    val is_active  : ('a, 'b, 'c, 'd) stream -> bool
    val get_info : ('a, 'b, 'c, 'd) stream -> stream_info
    val time : ('a, 'b, 'c, 'd) stream -> pa_time
    val cpu_load : ('a, 'b, 'c, 'd) stream -> float

    val read_interleaved : 
        ('a, SampleFormat.interleaved, 'c, 'd) stream 
        -> ('a, SampleFormat.interleaved) View.t
        -> unit

    val read_non_interleaved : 
        ('a, SampleFormat.non_interleaved, 'c, 'd) stream 
        -> ('a, SampleFormat.non_interleaved) View.t array
        -> unit

    val write_interleaved : 
        ('a, 'b, 'c, SampleFormat.interleaved) stream 
        -> ('c, SampleFormat.interleaved) View.t
        -> unit

    val write_non_interleaved : 
        ('a, 'b, 'c, SampleFormat.non_interleaved) stream 
        -> ('c, SampleFormat.non_interleaved) View.t array
        -> unit

    val read_available : ('a, 'b, 'c, 'd) stream -> int
    val write_available : ('a, 'b, 'c, 'd) stream -> int

    val set_finished_callback : ('a, 'b, 'c, 'd) stream -> (unit -> unit) option -> unit
end

(**
 @param terminate_on_exn if true will raise PortAudio_exn with information about what failed.
 *)
val initialize : ?terminate_on_exn:bool -> unit -> unit
val terminate : unit -> unit
val sleep : int -> unit
val sample_size : ('a, 'b, 'c) SampleFormat.t -> int

module VersionInfo : sig
    type t = {
        major : int;
        minor : int;
        sub_minor : int;
        version_control_revision : string;
        version_text : string;
    }
end

module HostApiTypeId = C_ffi.HostApiTypeId

module HostApiInfo : sig
    type t = {
        version : int;
        type_ : HostApiTypeId.t;
        name : string;
        device_count : int;
        default_input_device : device_index;
        default_output_device : device_index;
    }
end

module DeviceInfo : sig
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

val get_version : unit -> int
val get_version_text : unit -> string
val get_version_info : unit -> VersionInfo.t
val get_error_text : PaError.t -> string
val get_host_api_count : unit -> int
val get_default_host_api : unit -> device_index
val get_host_api_info : device_index -> HostApiInfo.t option
val host_api_type_id_to_host_api_index : HostApiTypeId.t -> device_index option
val get_device_count : unit -> int
val get_default_input_device : unit -> device_index
val get_default_output_device : unit -> device_index
val get_device_info : device_index -> DeviceInfo.t
val get_device_index : int -> device_index option
val is_format_supported : ('a, 'b, 'c) StreamParameters.t -> ('a, 'b, 'c) StreamParameters.t -> sample_rate:float -> (unit, PaError.t) Result.t 

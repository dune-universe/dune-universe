module Make(F : Cstubs.FOREIGN) = struct
  open Ctypes
  open F
  include Portaudio_types

  let get_version = foreign "Pa_GetVersion" (void @-> returning int)
  let get_version_text = foreign "Pa_GetVersionText" (void @-> returning string)
  let get_version_info = foreign "Pa_GetVersionInfo" (void @-> returning (ptr VersionInfo.t))

  let get_error_text = foreign "Pa_GetErrorText" (pa_error @-> returning string)

  let initialize = foreign "Pa_Initialize" (void @-> returning pa_error)
  let terminate = foreign "Pa_Terminate" (void @-> returning pa_error)

  let get_host_api_count = foreign "Pa_GetHostApiCount" (void @-> returning pa_host_api_index)
  let get_default_host_api = foreign "Pa_GetDefaultHostApi" (void @-> returning pa_host_api_index)

  let get_host_api_info = foreign "Pa_GetHostApiInfo" (pa_host_api_index @-> returning (ptr HostApiInfo.t))
  let host_api_type_id_to_host_api_index = foreign "Pa_HostApiTypeIdToHostApiIndex" (HostApiTypeId.t @-> returning pa_host_api_index)

  let get_last_host_error_info = foreign "Pa_GetLastHostErrorInfo" (void @-> returning (ptr HostErrorInfo.t)) 

  let get_device_count = foreign "Pa_GetDeviceCount" (void @-> returning pa_device_index)
  let get_default_input_device = foreign "Pa_GetDefaultInputDevice" (void @-> returning pa_device_index)
  let get_default_output_device = foreign "Pa_GetDefaultOutputDevice" (void @-> returning pa_device_index)

  let get_device_info = foreign "Pa_GetDeviceInfo" (pa_device_index @-> returning (ptr DeviceInfo.t))

  let is_format_supported = foreign "Pa_IsFormatSupported" (ptr StreamParameters.t @-> ptr StreamParameters.t @-> float @-> returning pa_error)

  let open_stream = foreign "Pa_OpenStream" (
      ptr (ptr stream) 
      @-> (ptr StreamParameters.t)
      @-> (ptr StreamParameters.t)
      @-> float
      @-> ulong
      @-> StreamFlags.t
      @-> Foreign.funptr_opt ~thread_registration:true ~runtime_lock:true pa_stream_callback
      @-> ptr void
      @-> returning pa_error
  )

  let open_default_stream = foreign "Pa_OpenDefaultStream" (
      ptr (ptr stream)
      @-> int
      @-> int
      @-> SampleFormat.t
      @-> float
      @-> ulong
      @-> Foreign.funptr_opt ~thread_registration:true ~runtime_lock:true pa_stream_callback
      @-> ptr void
      @-> returning pa_error
  )

  let close_stream = foreign "Pa_CloseStream" (ptr stream @-> returning pa_error)
  let start_stream = foreign "Pa_StartStream" (ptr stream @-> returning pa_error)
  let stop_stream = foreign "Pa_StopStream" (ptr stream @-> returning pa_error)
  let abort_stream = foreign "Pa_AbortStream" (ptr stream @-> returning pa_error)
  let is_stream_stopped = foreign "Pa_IsStreamStopped" (ptr stream @-> returning pa_error)
  let is_stream_active = foreign "Pa_IsStreamActive" (ptr stream @-> returning pa_error)

  let set_stream_finished_callback = foreign "Pa_SetStreamFinishedCallback" (
      ptr stream
      @-> Foreign.funptr_opt ~thread_registration:true ~runtime_lock:true pa_stream_finished_callback
      @-> returning pa_error
  )

  let get_stream_info = foreign "Pa_GetStreamInfo" (ptr stream @-> returning (ptr StreamInfo.t))
  let get_stream_time = foreign "Pa_GetStreamTime" (ptr stream @-> returning pa_time)
  let get_stream_cpu_load = foreign "Pa_GetStreamCpuLoad" (ptr stream @-> returning float)
  let read_stream = foreign "Pa_ReadStream" (ptr stream @-> ptr void (* buffer *) @-> ulong @-> returning pa_error)
  let write_stream = foreign "Pa_WriteStream" (ptr stream @-> ptr void (* buffer *) @-> ulong @-> returning pa_error)

  let get_stream_read_available = foreign "Pa_GetStreamReadAvailable" (ptr stream @-> returning long)
  let get_stream_write_available = foreign "Pa_GetStreamWriteAvailable" (ptr stream @-> returning long)

  let get_sample_size = foreign "Pa_GetSampleSize" (SampleFormat.t @-> returning pa_error)
  let sleep = foreign "Pa_Sleep" (long @-> returning void)
end

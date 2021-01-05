module Make(S : Cstubs_structs.TYPE) = struct
  open Ctypes
  open S

  let e_val a n = a, S.constant n S.int64_t

  module PaError = struct
      type t = NoError
             | Not_initialized
             | Unanticipated_host_error
             | Invalid_channel_count
             | Invalid_device
             | Invalid_flag
             | Sample_format_not_supported
             | Bad_io_device_combination
             | Insufficient_memory
             | Buffer_too_big
             | Buffer_too_small
             | Null_callback
             | Timed_out
             | Internal_error
             | Device_unavailable
             | Incompatible_host_api_specific_stream_info
             | Stream_is_stopped
             | Stream_is_not_stopped
             | Input_overflow
             | Host_api_not_found
             | Invalid_host_api
             | Can_not_read_from_a_callback_stream
             | Can_not_write_to_a_callback_stream
             | Can_not_read_from_output_only_stream
             | Can_not_write_to_input_only_stream
             | Incompatible_stream_host_api
             | Bad_buffer_ptr
             | Other of int

        let t = S.enum "PaError" ~typedef:true ~unexpected:(fun v -> Other (Int64.to_int v))
        [
            e_val NoError "paNoError";
            e_val Not_initialized "paNotInitialized";
            e_val Unanticipated_host_error "paUnanticipatedHostError";
            e_val Invalid_channel_count "paInvalidChannelCount";
            e_val Invalid_device "paInvalidDevice";
            e_val Invalid_flag "paInvalidFlag";
            e_val Sample_format_not_supported "paSampleFormatNotSupported";
            e_val Bad_io_device_combination "paBadIODeviceCombination";
            e_val Insufficient_memory "paInsufficientMemory";
            e_val Buffer_too_big "paBufferTooBig";
            e_val Buffer_too_small "paBufferTooSmall";
            e_val Null_callback "paNullCallback";
            e_val Timed_out "paTimedOut";
            e_val Internal_error "paInternalError";
            e_val Device_unavailable "paDeviceUnavailable";
            e_val Incompatible_host_api_specific_stream_info "paIncompatibleHostApiSpecificStreamInfo";
            e_val Stream_is_stopped "paStreamIsStopped";
            e_val Stream_is_not_stopped "paStreamIsNotStopped";
            e_val Input_overflow "paInputOverflow";
            e_val Host_api_not_found "paHostApiNotFound";
            e_val Invalid_host_api "paInvalidHostApi";
            e_val Can_not_read_from_a_callback_stream "paCanNotReadFromACallbackStream";
            e_val Can_not_write_to_a_callback_stream "paCanNotWriteToACallbackStream";
            e_val Can_not_read_from_output_only_stream "paCanNotReadFromAnOutputOnlyStream";
            e_val Can_not_write_to_input_only_stream "paCanNotWriteToAnInputOnlyStream";
            e_val Incompatible_stream_host_api "paIncompatibleStreamHostApi";
            e_val Bad_buffer_ptr "paBadBufferPtr";
        ]
  end

  let pa_error = PaError.t
  let pa_device_index = int
  let pa_host_api_index = int
  let pa_time = float

  module VersionInfo = struct
      type version_info
      type t = version_info structure
      let t : t typ = structure "PaVersionInfo"
      let version_major = field t "versionMajor" int
      let version_minor = field t "versionMinor" int
      let version_sub_minor = field t "versionSubMinor" int
      let version_control_revision = field t "versionControlRevision" string
      let version_text = field t "versionText" string
      let () = seal t
  end

  module SampleFormat = struct
      type t = Unsigned.ulong const
      let t = ulong
      let float32 : t = S.constant "paFloat32" ulong
      let int32 : t = S.constant "paInt32" ulong
      let int24 : t = S.constant "paInt24" ulong
      let int16 : t = S.constant "paInt16" ulong
      let int8 : t = S.constant "paInt8" ulong
      let uint8 : t = S.constant "paUInt8" ulong
      let custom_format : t = S.constant "paCustomFormat" ulong
      let non_interleaved : t = S.constant "paNonInterleaved" ulong
  end

  module StreamFlags = struct
      type t = Unsigned.ulong const
      let t = ulong
      let no_flag : t = S.constant "paNoFlag" ulong
      let clip_off : t = S.constant "paClipOff" ulong
      let dither_off : t = S.constant "paDitherOff" ulong
      let never_drop_input : t = S.constant "paNeverDropInput" ulong
      let prime_output_buffers_using_stream_callback : t = S.constant "paPrimeOutputBuffersUsingStreamCallback" ulong
      let platform_specific_flags = S.constant "paPlatformSpecificFlags" ulong
  end

  module DeviceInfo = struct
      type device_info
      type t = device_info structure
      let t : t typ = structure "PaDeviceInfo"
      let struct_version = field t "structVersion" int
      let name = field t "name" string
      let host_api_index = field t "hostApi" pa_host_api_index
      let max_input_channels = field t "maxInputChannels" int
      let max_output_channels = field t "maxOutputChannels" int
      let default_low_input_latency = field t "defaultLowInputLatency" pa_time
      let default_low_output_latency = field t "defaultLowOutputLatency" pa_time
      let default_high_input_latency = field t "defaultHighInputLatency" pa_time
      let default_high_output_latency = field t "defaultHighOutputLatency" pa_time
      let default_sample_rate = field t "defaultSampleRate" float
      let () = seal t
  end

  module StreamParameters = struct
      type stream_parameters
      type t = stream_parameters structure
      let t : t typ = structure "PaStreamParameters"
      let device = field t "device" pa_device_index
      let channel_count = field t "channelCount" int
      let sample_format = field t "sampleFormat" SampleFormat.t
      let suggested_latency = field t "suggestedLatency" pa_time
      let host_api_specific_stream_info = field t "hostApiSpecificStreamInfo" (ptr void)
      let () = seal t
  end

  module StreamCallbackTimeInfo = struct
      type stream_callback_time_info
      type t = stream_callback_time_info structure
      let t : t typ = structure "PaStreamCallbackTimeInfo"
      let input_buffer_adc_time = field t "inputBufferAdcTime" pa_time
      let current_time = field t "currentTime" pa_time
      let output_buffer_dac_time = field t "outputBufferDacTime" pa_time
      let () = seal t
  end

  module StreamCallbackFlags = struct
      type t = Unsigned.ulong const
      let t = ulong
      let input_underflow : t = S.constant "paInputUnderflow" t
      let input_overflow : t = S.constant "paInputOverflow" t
      let output_underflow : t = S.constant "paOutputUnderflow" t
      let output_overflow : t = S.constant "paOutputOverflow" t
      let priming_output : t = S.constant "paPrimingOutput" t
  end

  module StreamCallbackResult = struct
      type t = Continue
             | Complete
             | Abort

      let t = S.enum "PaStreamCallbackResult" ~typedef:true [
          e_val Continue "paContinue";
          e_val Complete "paComplete";
          e_val Abort "paAbort";
      ]
  end

  type stream = unit
  let stream = void

  type pa_stream_callback =
      unit ptr ->
      unit ptr ->
      Unsigned.ulong ->
      StreamCallbackTimeInfo.t ptr ->
      Unsigned.ulong -> unit ptr -> StreamCallbackResult.t

  let pa_stream_callback = 
      ptr void
      @-> ptr void
      @-> ulong
      @-> ptr StreamCallbackTimeInfo.t
      @-> StreamCallbackFlags.t
      @-> ptr void
      @-> (returning StreamCallbackResult.t)

  let pa_stream_finished_callback = ptr void @-> (returning void)

  module StreamInfo = struct
      type stream_info
      type t = stream_info structure
      let t : t typ = structure "PaStreamInfo"
      let struct_version = field t "structVersion" int
      let input_latency = field t "inputLatency" pa_time
      let output_latency = field t "outputLatency" pa_time
      let sample_rate = field t "sampleRate" float
      let () = seal t
  end

  module HostApiTypeId = struct
      type t = InDevelopment
             | DirectSound
             | MME
             | ASIO
             | SoundManager
             | CoreAudio
             | OSS
             | ALSA
             | AL
             | BeOS
             | WDMKS
             | JACK
             | WASAPI
             | AudioScienceHPI

        let t = S.enum "PaHostApiTypeId" ~typedef:true [
            e_val InDevelopment "paInDevelopment";
            e_val DirectSound "paDirectSound";
            e_val MME "paMME";
            e_val ASIO "paASIO";
            e_val SoundManager "paSoundManager";
            e_val CoreAudio "paCoreAudio";
            e_val OSS "paOSS";
            e_val ALSA "paALSA";
            e_val AL "paAL";
            e_val BeOS "paBeOS";
            e_val WDMKS "paWDMKS";
            e_val JACK "paJACK";
            e_val WASAPI "paWASAPI";
            e_val AudioScienceHPI "paAudioScienceHPI";
        ]
  end

  module HostApiInfo = struct
      type host_api_info
      type t = host_api_info structure
      let t : t typ = structure "PaHostApiInfo"
      let struct_version = field t "structVersion" int
      let type_ = field t "type" HostApiTypeId.t
      let name = field t "name" string
      let device_count = field t "deviceCount" int
      let default_input_device = field t "defaultInputDevice" pa_device_index
      let default_output_device = field t "defaultOutputDevice" pa_device_index
      let () = seal t
  end

  module HostErrorInfo = struct
      type host_error_info
      type t = host_error_info structure
      let t : t typ = structure "PaHostErrorInfo"
      let host_api_type = field t "hostApiType" HostApiTypeId.t
      let error_code = field t "errorCode" long
      let error_text = field t "errorText" string
      let () = seal t
  end

end

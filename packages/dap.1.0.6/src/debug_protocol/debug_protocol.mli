(** The Debug Adapter Protocol defines the protocol used between an editor or IDE and a debugger or runtime. *)
(* Auto-generated from json schema. Do not edit manually. *)

include module type of Debug_protocol_types

module Protocol_message : sig
  module Type : sig
    (** Message type. *)
    type t =
      | Request [@name "request"]
      | Response [@name "response"]
      | Event [@name "event"]
      | Custom of string

    include JSONABLE with type t := t
  end

  (** Base class of requests, responses, and events. *)
  type t = {
    seq : int; (** Sequence number (also known as message ID). For protocol messages of type 'request' this ID can be used to cancel the request. *)
    type_ : Type.t [@key "type"]; (** Message type. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Request : sig
  module Type : sig
    type t =
      | Request [@name "request"]

    include JSONABLE with type t := t
  end

  type t = {
    seq : int; (** Sequence number (also known as message ID). For protocol messages of type 'request' this ID can be used to cancel the request. *)
    type_ : Type.t [@key "type"];
    command : string; (** The command to execute. *)
    arguments : Yojson.Safe.t [@default `Assoc []]; (** Object containing arguments for the command. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Event : sig
  module Type : sig
    type t =
      | Event [@name "event"]

    include JSONABLE with type t := t
  end

  type t = {
    seq : int; (** Sequence number (also known as message ID). For protocol messages of type 'request' this ID can be used to cancel the request. *)
    type_ : Type.t [@key "type"];
    event : string; (** Type of event. *)
    body : Yojson.Safe.t [@default `Assoc []]; (** Event-specific information. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Response : sig
  module Type : sig
    type t =
      | Response [@name "response"]

    include JSONABLE with type t := t
  end

  module Message : sig
    (** Contains the raw error in short form if 'success' is false.
    This raw error might be interpreted by the frontend and is not shown in the UI.
    Some predefined values exist. *)
    type t =
      | Cancelled [@name "cancelled"]
      | Custom of string

    include JSONABLE with type t := t
  end

  type t = {
    seq : int; (** Sequence number (also known as message ID). For protocol messages of type 'request' this ID can be used to cancel the request. *)
    type_ : Type.t [@key "type"];
    request_seq : int; (** Sequence number of the corresponding request. *)
    success : bool; (** Outcome of the request.
    If true, the request was successful and the 'body' attribute may contain the result of the request.
    If the value is false, the attribute 'message' contains the error in short form and the 'body' may contain additional information (see 'ErrorResponse.body.error'). *)
    command : string; (** The command requested. *)
    message : Message.t option [@default None]; (** Contains the raw error in short form if 'success' is false.
    This raw error might be interpreted by the frontend and is not shown in the UI.
    Some predefined values exist. *)
    body : Yojson.Safe.t [@default `Assoc []]; (** Contains request result if success is true and optional error details if success is false. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Message : sig
  module Variables : sig
    (** An object used as a dictionary for looking up the variables in the format string. *)
    type t = String_dict.t
    [@@deriving yojson]
  end

  (** A structured message object. Used to return errors from requests. *)
  type t = {
    id : int; (** Unique identifier for the message. *)
    format : string; (** A format string for the message. Embedded variables have the form '\{name\}'.
    If variable name starts with an underscore character, the variable does not contain user data (PII) and can be safely used for telemetry purposes. *)
    variables : Variables.t option [@default None]; (** An object used as a dictionary for looking up the variables in the format string. *)
    send_telemetry : bool option [@key "sendTelemetry"] [@default None]; (** If true send to telemetry. *)
    show_user : bool option [@key "showUser"] [@default None]; (** If true show user. *)
    url : string option [@default None]; (** An optional url where additional information about this message can be found. *)
    url_label : string option [@key "urlLabel"] [@default None]; (** An optional label that is presented to the user as the UI for opening the url. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Error_response : sig
  module Body : sig
    type t = {
      error : Message.t option [@default None]; (** An optional, structured error message. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  type t = {
    body : Body.t;
  }
  [@@deriving make, yojson {strict = false}]
end

module Exception_breakpoints_filter : sig
  (** An ExceptionBreakpointsFilter is shown in the UI as an filter option for configuring how exceptions are dealt with. *)
  type t = {
    filter : string; (** The internal ID of the filter option. This value is passed to the 'setExceptionBreakpoints' request. *)
    label : string; (** The name of the filter option. This will be shown in the UI. *)
    default : bool option [@default None]; (** Initial value of the filter option. If not specified a value 'false' is assumed. *)
    supports_condition : bool option [@key "supportsCondition"] [@default None]; (** Controls whether a condition can be specified for this filter option. If false or missing, a condition can not be set. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Column_descriptor : sig
  module Type : sig
    (** Datatype of values in this column.  Defaults to 'string' if not specified. *)
    type t =
      | String [@name "string"]
      | Number [@name "number"]
      | Boolean [@name "boolean"]
      | Unix_timestamp_utc [@name "unixTimestampUTC"]

    include JSONABLE with type t := t
  end

  (** A ColumnDescriptor specifies what module attribute to show in a column of the ModulesView, how to format it,
  and what the column's label should be.
  It is only used if the underlying UI actually supports this level of customization. *)
  type t = {
    attribute_name : string [@key "attributeName"]; (** Name of the attribute rendered in this column. *)
    label : string; (** Header UI label of column. *)
    format : string option [@default None]; (** Format to use for the rendered values in this column. TBD how the format strings looks like. *)
    type_ : Type.t option [@key "type"] [@default None]; (** Datatype of values in this column.  Defaults to 'string' if not specified. *)
    width : int option [@default None]; (** Width of this column in characters (hint only). *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Checksum_algorithm : sig
  (** Names of checksum algorithms that may be supported by a debug adapter. *)
  type t =
    | MD5
    | SHA1
    | SHA256
    | Timestamp [@name "timestamp"]

  include JSONABLE with type t := t
end

module Capabilities : sig
  (** Information about the capabilities of a debug adapter. *)
  type t = {
    supports_configuration_done_request : bool option [@key "supportsConfigurationDoneRequest"] [@default None]; (** The debug adapter supports the 'configurationDone' request. *)
    supports_function_breakpoints : bool option [@key "supportsFunctionBreakpoints"] [@default None]; (** The debug adapter supports function breakpoints. *)
    supports_conditional_breakpoints : bool option [@key "supportsConditionalBreakpoints"] [@default None]; (** The debug adapter supports conditional breakpoints. *)
    supports_hit_conditional_breakpoints : bool option [@key "supportsHitConditionalBreakpoints"] [@default None]; (** The debug adapter supports breakpoints that break execution after a specified number of hits. *)
    supports_evaluate_for_hovers : bool option [@key "supportsEvaluateForHovers"] [@default None]; (** The debug adapter supports a (side effect free) evaluate request for data hovers. *)
    exception_breakpoint_filters : Exception_breakpoints_filter.t list option [@key "exceptionBreakpointFilters"] [@default None]; (** Available exception filter options for the 'setExceptionBreakpoints' request. *)
    supports_step_back : bool option [@key "supportsStepBack"] [@default None]; (** The debug adapter supports stepping back via the 'stepBack' and 'reverseContinue' requests. *)
    supports_set_variable : bool option [@key "supportsSetVariable"] [@default None]; (** The debug adapter supports setting a variable to a value. *)
    supports_restart_frame : bool option [@key "supportsRestartFrame"] [@default None]; (** The debug adapter supports restarting a frame. *)
    supports_goto_targets_request : bool option [@key "supportsGotoTargetsRequest"] [@default None]; (** The debug adapter supports the 'gotoTargets' request. *)
    supports_step_in_targets_request : bool option [@key "supportsStepInTargetsRequest"] [@default None]; (** The debug adapter supports the 'stepInTargets' request. *)
    supports_completions_request : bool option [@key "supportsCompletionsRequest"] [@default None]; (** The debug adapter supports the 'completions' request. *)
    completion_trigger_characters : string list option [@key "completionTriggerCharacters"] [@default None]; (** The set of characters that should trigger completion in a REPL. If not specified, the UI should assume the '.' character. *)
    supports_modules_request : bool option [@key "supportsModulesRequest"] [@default None]; (** The debug adapter supports the 'modules' request. *)
    additional_module_columns : Column_descriptor.t list option [@key "additionalModuleColumns"] [@default None]; (** The set of additional module information exposed by the debug adapter. *)
    supported_checksum_algorithms : Checksum_algorithm.t list option [@key "supportedChecksumAlgorithms"] [@default None]; (** Checksum algorithms supported by the debug adapter. *)
    supports_restart_request : bool option [@key "supportsRestartRequest"] [@default None]; (** The debug adapter supports the 'restart' request. In this case a client should not implement 'restart' by terminating and relaunching the adapter but by calling the RestartRequest. *)
    supports_exception_options : bool option [@key "supportsExceptionOptions"] [@default None]; (** The debug adapter supports 'exceptionOptions' on the setExceptionBreakpoints request. *)
    supports_value_formatting_options : bool option [@key "supportsValueFormattingOptions"] [@default None]; (** The debug adapter supports a 'format' attribute on the stackTraceRequest, variablesRequest, and evaluateRequest. *)
    supports_exception_info_request : bool option [@key "supportsExceptionInfoRequest"] [@default None]; (** The debug adapter supports the 'exceptionInfo' request. *)
    support_terminate_debuggee : bool option [@key "supportTerminateDebuggee"] [@default None]; (** The debug adapter supports the 'terminateDebuggee' attribute on the 'disconnect' request. *)
    supports_delayed_stack_trace_loading : bool option [@key "supportsDelayedStackTraceLoading"] [@default None]; (** The debug adapter supports the delayed loading of parts of the stack, which requires that both the 'startFrame' and 'levels' arguments and an optional 'totalFrames' result of the 'StackTrace' request are supported. *)
    supports_loaded_sources_request : bool option [@key "supportsLoadedSourcesRequest"] [@default None]; (** The debug adapter supports the 'loadedSources' request. *)
    supports_log_points : bool option [@key "supportsLogPoints"] [@default None]; (** The debug adapter supports logpoints by interpreting the 'logMessage' attribute of the SourceBreakpoint. *)
    supports_terminate_threads_request : bool option [@key "supportsTerminateThreadsRequest"] [@default None]; (** The debug adapter supports the 'terminateThreads' request. *)
    supports_set_expression : bool option [@key "supportsSetExpression"] [@default None]; (** The debug adapter supports the 'setExpression' request. *)
    supports_terminate_request : bool option [@key "supportsTerminateRequest"] [@default None]; (** The debug adapter supports the 'terminate' request. *)
    supports_data_breakpoints : bool option [@key "supportsDataBreakpoints"] [@default None]; (** The debug adapter supports data breakpoints. *)
    supports_read_memory_request : bool option [@key "supportsReadMemoryRequest"] [@default None]; (** The debug adapter supports the 'readMemory' request. *)
    supports_disassemble_request : bool option [@key "supportsDisassembleRequest"] [@default None]; (** The debug adapter supports the 'disassemble' request. *)
    supports_cancel_request : bool option [@key "supportsCancelRequest"] [@default None]; (** The debug adapter supports the 'cancel' request. *)
    supports_breakpoint_locations_request : bool option [@key "supportsBreakpointLocationsRequest"] [@default None]; (** The debug adapter supports the 'breakpointLocations' request. *)
    supports_clipboard_context : bool option [@key "supportsClipboardContext"] [@default None]; (** The debug adapter supports the 'clipboard' context value in the 'evaluate' request. *)
    supports_stepping_granularity : bool option [@key "supportsSteppingGranularity"] [@default None]; (** The debug adapter supports stepping granularities (argument 'granularity') for the stepping requests. *)
    supports_instruction_breakpoints : bool option [@key "supportsInstructionBreakpoints"] [@default None]; (** The debug adapter supports adding breakpoints based on instruction references. *)
    supports_exception_filter_options : bool option [@key "supportsExceptionFilterOptions"] [@default None]; (** The debug adapter supports 'filterOptions' as an argument on the 'setExceptionBreakpoints' request. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Module : sig
  (** A Module object represents a row in the modules view.
  Two attributes are mandatory: an id identifies a module in the modules view and is used in a ModuleEvent for identifying a module for adding, updating or deleting.
  The name is used to minimally render the module in the UI.

  Additional attributes can be added to the module. They will show up in the module View if they have a corresponding ColumnDescriptor.

  To avoid an unnecessary proliferation of additional attributes with similar semantics but different names
  we recommend to re-use attributes from the 'recommended' list below first, and only introduce new attributes if nothing appropriate could be found. *)
  type t = {
    id : Int_or_string.t; (** Unique identifier for the module. *)
    name : string; (** A name of the module. *)
    path : string option [@default None]; (** optional but recommended attributes.
    always try to use these first before introducing additional attributes.

    Logical full path to the module. The exact definition is implementation defined, but usually this would be a full path to the on-disk file for the module. *)
    is_optimized : bool option [@key "isOptimized"] [@default None]; (** True if the module is optimized. *)
    is_user_code : bool option [@key "isUserCode"] [@default None]; (** True if the module is considered 'user code' by a debugger that supports 'Just My Code'. *)
    version : string option [@default None]; (** Version of Module. *)
    symbol_status : string option [@key "symbolStatus"] [@default None]; (** User understandable description of if symbols were found for the module (ex: 'Symbols Loaded', 'Symbols not found', etc. *)
    symbol_file_path : string option [@key "symbolFilePath"] [@default None]; (** Logical full path to the symbol file. The exact definition is implementation defined. *)
    date_time_stamp : string option [@key "dateTimeStamp"] [@default None]; (** Module created or modified. *)
    address_range : string option [@key "addressRange"] [@default None]; (** Address range covered by this module. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Modules_view_descriptor : sig
  (** The ModulesViewDescriptor is the container for all declarative configuration options of a ModuleView.
  For now it only specifies the columns to be shown in the modules view. *)
  type t = {
    columns : Column_descriptor.t list;
  }
  [@@deriving make, yojson {strict = false}]
end

module Thread : sig
  (** A Thread *)
  type t = {
    id : int; (** Unique identifier for the thread. *)
    name : string; (** A name of the thread. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Checksum : sig
  (** The checksum of an item calculated by the specified algorithm. *)
  type t = {
    algorithm : Checksum_algorithm.t; (** The algorithm used to calculate this checksum. *)
    checksum : string; (** Value of the checksum. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Source : sig
  module Presentation_hint : sig
    (** An optional hint for how to present the source in the UI.
    A value of 'deemphasize' can be used to indicate that the source is not available or that it is skipped on stepping. *)
    type t =
      | Normal [@name "normal"]
      | Emphasize [@name "emphasize"]
      | Deemphasize [@name "deemphasize"]

    include JSONABLE with type t := t
  end

  (** A Source is a descriptor for source code.
  It is returned from the debug adapter as part of a StackFrame and it is used by clients when specifying breakpoints. *)
  type t = {
    name : string option [@default None]; (** The short name of the source. Every source returned from the debug adapter has a name.
    When sending a source to the debug adapter this name is optional. *)
    path : string option [@default None]; (** The path of the source to be shown in the UI.
    It is only used to locate and load the content of the source if no sourceReference is specified (or its value is 0). *)
    source_reference : int option [@key "sourceReference"] [@default None]; (** If sourceReference > 0 the contents of the source must be retrieved through the SourceRequest (even if a path is specified).
    A sourceReference is only valid for a session, so it must not be used to persist a source.
    The value should be less than or equal to 2147483647 (2^31-1). *)
    presentation_hint : Presentation_hint.t option [@key "presentationHint"] [@default None]; (** An optional hint for how to present the source in the UI.
    A value of 'deemphasize' can be used to indicate that the source is not available or that it is skipped on stepping. *)
    origin : string option [@default None]; (** The (optional) origin of this source: possible values 'internal module', 'inlined content from source map', etc. *)
    sources : t list option [@default None]; (** An optional list of sources that are related to this source. These may be the source that generated this source. *)
    adapter_data : Any.t option [@key "adapterData"] [@default None]; (** Optional data that a debug adapter might want to loop through the client.
    The client should leave the data intact and persist it across sessions. The client should not interpret the data. *)
    checksums : Checksum.t list option [@default None]; (** The checksums associated with this file. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Stack_frame : sig
  module Presentation_hint : sig
    (** An optional hint for how to present this frame in the UI.
    A value of 'label' can be used to indicate that the frame is an artificial frame that is used as a visual label or separator. A value of 'subtle' can be used to change the appearance of a frame in a 'subtle' way. *)
    type t =
      | Normal [@name "normal"]
      | Label [@name "label"]
      | Subtle [@name "subtle"]

    include JSONABLE with type t := t
  end

  (** A Stackframe contains the source location. *)
  type t = {
    id : int; (** An identifier for the stack frame. It must be unique across all threads.
    This id can be used to retrieve the scopes of the frame with the 'scopesRequest' or to restart the execution of a stackframe. *)
    name : string; (** The name of the stack frame, typically a method name. *)
    source : Source.t option [@default None]; (** The optional source of the frame. *)
    line : int; (** The line within the file of the frame. If source is null or doesn't exist, line is 0 and must be ignored. *)
    column : int; (** The column within the line. If source is null or doesn't exist, column is 0 and must be ignored. *)
    end_line : int option [@key "endLine"] [@default None]; (** An optional end line of the range covered by the stack frame. *)
    end_column : int option [@key "endColumn"] [@default None]; (** An optional end column of the range covered by the stack frame. *)
    instruction_pointer_reference : string option [@key "instructionPointerReference"] [@default None]; (** Optional memory reference for the current instruction pointer in this frame. *)
    module_id : Int_or_string.t option [@key "moduleId"] [@default None]; (** The module associated with this frame, if any. *)
    presentation_hint : Presentation_hint.t option [@key "presentationHint"] [@default None]; (** An optional hint for how to present this frame in the UI.
    A value of 'label' can be used to indicate that the frame is an artificial frame that is used as a visual label or separator. A value of 'subtle' can be used to change the appearance of a frame in a 'subtle' way. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Scope : sig
  module Presentation_hint : sig
    (** An optional hint for how to present this scope in the UI. If this attribute is missing, the scope is shown with a generic UI. *)
    type t =
      | Arguments [@name "arguments"]
      | Locals [@name "locals"]
      | Registers [@name "registers"]
      | Custom of string

    include JSONABLE with type t := t
  end

  (** A Scope is a named container for variables. Optionally a scope can map to a source or a range within a source. *)
  type t = {
    name : string; (** Name of the scope such as 'Arguments', 'Locals', or 'Registers'. This string is shown in the UI as is and can be translated. *)
    presentation_hint : Presentation_hint.t option [@key "presentationHint"] [@default None]; (** An optional hint for how to present this scope in the UI. If this attribute is missing, the scope is shown with a generic UI. *)
    variables_reference : int [@key "variablesReference"]; (** The variables of this scope can be retrieved by passing the value of variablesReference to the VariablesRequest. *)
    named_variables : int option [@key "namedVariables"] [@default None]; (** The number of named variables in this scope.
    The client can use this optional information to present the variables in a paged UI and fetch them in chunks. *)
    indexed_variables : int option [@key "indexedVariables"] [@default None]; (** The number of indexed variables in this scope.
    The client can use this optional information to present the variables in a paged UI and fetch them in chunks. *)
    expensive : bool; (** If true, the number of variables in this scope is large or expensive to retrieve. *)
    source : Source.t option [@default None]; (** Optional source for this scope. *)
    line : int option [@default None]; (** Optional start line of the range covered by this scope. *)
    column : int option [@default None]; (** Optional start column of the range covered by this scope. *)
    end_line : int option [@key "endLine"] [@default None]; (** Optional end line of the range covered by this scope. *)
    end_column : int option [@key "endColumn"] [@default None]; (** Optional end column of the range covered by this scope. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Variable_presentation_hint : sig
  module Kind : sig
    (** The kind of variable. Before introducing additional values, try to use the listed values. *)
    type t =
      | Property [@name "property"]
      | Method [@name "method"]
      | Class [@name "class"]
      | Data [@name "data"]
      | Event [@name "event"]
      | Base_class [@name "baseClass"]
      | Inner_class [@name "innerClass"]
      | Interface [@name "interface"]
      | Most_derived_class [@name "mostDerivedClass"]
      | Virtual [@name "virtual"]
      | Data_breakpoint [@name "dataBreakpoint"]
      | Custom of string

    include JSONABLE with type t := t
  end

  module Attributes : sig
    type t =
      | Static [@name "static"]
      | Constant [@name "constant"]
      | Read_only [@name "readOnly"]
      | Raw_string [@name "rawString"]
      | Has_object_id [@name "hasObjectId"]
      | Can_have_object_id [@name "canHaveObjectId"]
      | Has_side_effects [@name "hasSideEffects"]
      | Custom of string

    include JSONABLE with type t := t
  end

  module Visibility : sig
    (** Visibility of variable. Before introducing additional values, try to use the listed values. *)
    type t =
      | Public [@name "public"]
      | Private [@name "private"]
      | Protected [@name "protected"]
      | Internal [@name "internal"]
      | Final [@name "final"]
      | Custom of string

    include JSONABLE with type t := t
  end

  (** Optional properties of a variable that can be used to determine how to render the variable in the UI. *)
  type t = {
    kind : Kind.t option [@default None]; (** The kind of variable. Before introducing additional values, try to use the listed values. *)
    attributes : Attributes.t list option [@default None]; (** Set of attributes represented as an array of strings. Before introducing additional values, try to use the listed values. *)
    visibility : Visibility.t option [@default None]; (** Visibility of variable. Before introducing additional values, try to use the listed values. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Variable : sig
  (** A Variable is a name/value pair.
  Optionally a variable can have a 'type' that is shown if space permits or when hovering over the variable's name.
  An optional 'kind' is used to render additional properties of the variable, e.g. different icons can be used to indicate that a variable is public or private.
  If the value is structured (has children), a handle is provided to retrieve the children with the VariablesRequest.
  If the number of named or indexed children is large, the numbers should be returned via the optional 'namedVariables' and 'indexedVariables' attributes.
  The client can use this optional information to present the children in a paged UI and fetch them in chunks. *)
  type t = {
    name : string; (** The variable's name. *)
    value : string; (** The variable's value. This can be a multi-line text, e.g. for a function the body of a function. *)
    type_ : string option [@key "type"] [@default None]; (** The type of the variable's value. Typically shown in the UI when hovering over the value.
    This attribute should only be returned by a debug adapter if the client has passed the value true for the 'supportsVariableType' capability of the 'initialize' request. *)
    presentation_hint : Variable_presentation_hint.t option [@key "presentationHint"] [@default None]; (** Properties of a variable that can be used to determine how to render the variable in the UI. *)
    evaluate_name : string option [@key "evaluateName"] [@default None]; (** Optional evaluatable name of this variable which can be passed to the 'EvaluateRequest' to fetch the variable's value. *)
    variables_reference : int [@key "variablesReference"]; (** If variablesReference is > 0, the variable is structured and its children can be retrieved by passing variablesReference to the VariablesRequest. *)
    named_variables : int option [@key "namedVariables"] [@default None]; (** The number of named child variables.
    The client can use this optional information to present the children in a paged UI and fetch them in chunks. *)
    indexed_variables : int option [@key "indexedVariables"] [@default None]; (** The number of indexed child variables.
    The client can use this optional information to present the children in a paged UI and fetch them in chunks. *)
    memory_reference : string option [@key "memoryReference"] [@default None]; (** Optional memory reference for the variable if the variable represents executable code, such as a function pointer.
    This attribute is only required if the client has passed the value true for the 'supportsMemoryReferences' capability of the 'initialize' request. *)
    __vscode_variable_menu_context : string option [@key "__vscodeVariableMenuContext"] [@default None];
  }
  [@@deriving make, yojson {strict = false}]
end

module Breakpoint_location : sig
  (** Properties of a breakpoint location returned from the 'breakpointLocations' request. *)
  type t = {
    line : int; (** Start line of breakpoint location. *)
    column : int option [@default None]; (** Optional start column of breakpoint location. *)
    end_line : int option [@key "endLine"] [@default None]; (** Optional end line of breakpoint location if the location covers a range. *)
    end_column : int option [@key "endColumn"] [@default None]; (** Optional end column of breakpoint location if the location covers a range. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Source_breakpoint : sig
  (** Properties of a breakpoint or logpoint passed to the setBreakpoints request. *)
  type t = {
    line : int; (** The source line of the breakpoint or logpoint. *)
    column : int option [@default None]; (** An optional source column of the breakpoint. *)
    condition : string option [@default None]; (** An optional expression for conditional breakpoints.
    It is only honored by a debug adapter if the capability 'supportsConditionalBreakpoints' is true. *)
    hit_condition : string option [@key "hitCondition"] [@default None]; (** An optional expression that controls how many hits of the breakpoint are ignored.
    The backend is expected to interpret the expression as needed.
    The attribute is only honored by a debug adapter if the capability 'supportsHitConditionalBreakpoints' is true. *)
    log_message : string option [@key "logMessage"] [@default None]; (** If this attribute exists and is non-empty, the backend must not 'break' (stop)
    but log the message instead. Expressions within \{\} are interpolated.
    The attribute is only honored by a debug adapter if the capability 'supportsLogPoints' is true. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Function_breakpoint : sig
  (** Properties of a breakpoint passed to the setFunctionBreakpoints request. *)
  type t = {
    name : string; (** The name of the function. *)
    condition : string option [@default None]; (** An optional expression for conditional breakpoints.
    It is only honored by a debug adapter if the capability 'supportsConditionalBreakpoints' is true. *)
    hit_condition : string option [@key "hitCondition"] [@default None]; (** An optional expression that controls how many hits of the breakpoint are ignored.
    The backend is expected to interpret the expression as needed.
    The attribute is only honored by a debug adapter if the capability 'supportsHitConditionalBreakpoints' is true. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Data_breakpoint_access_type : sig
  (** This enumeration defines all possible access types for data breakpoints. *)
  type t =
    | Read [@name "read"]
    | Write [@name "write"]
    | Read_write [@name "readWrite"]

  include JSONABLE with type t := t
end

module Data_breakpoint : sig
  (** Properties of a data breakpoint passed to the setDataBreakpoints request. *)
  type t = {
    data_id : string [@key "dataId"]; (** An id representing the data. This id is returned from the dataBreakpointInfo request. *)
    access_type : Data_breakpoint_access_type.t option [@key "accessType"] [@default None]; (** The access type of the data. *)
    condition : string option [@default None]; (** An optional expression for conditional breakpoints. *)
    hit_condition : string option [@key "hitCondition"] [@default None]; (** An optional expression that controls how many hits of the breakpoint are ignored.
    The backend is expected to interpret the expression as needed. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Instruction_breakpoint : sig
  (** Properties of a breakpoint passed to the setInstructionBreakpoints request *)
  type t = {
    instruction_reference : string [@key "instructionReference"]; (** The instruction reference of the breakpoint.
    This should be a memory or instruction pointer reference from an EvaluateResponse, Variable, StackFrame, GotoTarget, or Breakpoint. *)
    offset : int option [@default None]; (** An optional offset from the instruction reference.
    This can be negative. *)
    condition : string option [@default None]; (** An optional expression for conditional breakpoints.
    It is only honored by a debug adapter if the capability 'supportsConditionalBreakpoints' is true. *)
    hit_condition : string option [@key "hitCondition"] [@default None]; (** An optional expression that controls how many hits of the breakpoint are ignored.
    The backend is expected to interpret the expression as needed.
    The attribute is only honored by a debug adapter if the capability 'supportsHitConditionalBreakpoints' is true. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Breakpoint : sig
  (** Information about a Breakpoint created in setBreakpoints, setFunctionBreakpoints, setInstructionBreakpoints, or setDataBreakpoints. *)
  type t = {
    id : int option [@default None]; (** An optional identifier for the breakpoint. It is needed if breakpoint events are used to update or remove breakpoints. *)
    verified : bool; (** If true breakpoint could be set (but not necessarily at the desired location). *)
    message : string option [@default None]; (** An optional message about the state of the breakpoint.
    This is shown to the user and can be used to explain why a breakpoint could not be verified. *)
    source : Source.t option [@default None]; (** The source where the breakpoint is located. *)
    line : int option [@default None]; (** The start line of the actual range covered by the breakpoint. *)
    column : int option [@default None]; (** An optional start column of the actual range covered by the breakpoint. *)
    end_line : int option [@key "endLine"] [@default None]; (** An optional end line of the actual range covered by the breakpoint. *)
    end_column : int option [@key "endColumn"] [@default None]; (** An optional end column of the actual range covered by the breakpoint.
    If no end line is given, then the end column is assumed to be in the start line. *)
    instruction_reference : string option [@key "instructionReference"] [@default None]; (** An optional memory reference to where the breakpoint is set. *)
    offset : int option [@default None]; (** An optional offset from the instruction reference.
    This can be negative. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Stepping_granularity : sig
  (** The granularity of one 'step' in the stepping requests 'next', 'stepIn', 'stepOut', and 'stepBack'. *)
  type t =
    | Statement [@name "statement"]
    | Line [@name "line"]
    | Instruction [@name "instruction"]

  include JSONABLE with type t := t
end

module Step_in_target : sig
  (** A StepInTarget can be used in the 'stepIn' request and determines into which single target the stepIn request should step. *)
  type t = {
    id : int; (** Unique identifier for a stepIn target. *)
    label : string; (** The name of the stepIn target (shown in the UI). *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Goto_target : sig
  (** A GotoTarget describes a code location that can be used as a target in the 'goto' request.
  The possible goto targets can be determined via the 'gotoTargets' request. *)
  type t = {
    id : int; (** Unique identifier for a goto target. This is used in the goto request. *)
    label : string; (** The name of the goto target (shown in the UI). *)
    line : int; (** The line of the goto target. *)
    column : int option [@default None]; (** An optional column of the goto target. *)
    end_line : int option [@key "endLine"] [@default None]; (** An optional end line of the range covered by the goto target. *)
    end_column : int option [@key "endColumn"] [@default None]; (** An optional end column of the range covered by the goto target. *)
    instruction_pointer_reference : string option [@key "instructionPointerReference"] [@default None]; (** Optional memory reference for the instruction pointer value represented by this target. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Completion_item_type : sig
  (** Some predefined types for the CompletionItem. Please note that not all clients have specific icons for all of them. *)
  type t =
    | Method [@name "method"]
    | Function [@name "function"]
    | Constructor [@name "constructor"]
    | Field [@name "field"]
    | Variable [@name "variable"]
    | Class [@name "class"]
    | Interface [@name "interface"]
    | Module [@name "module"]
    | Property [@name "property"]
    | Unit [@name "unit"]
    | Value [@name "value"]
    | Enum [@name "enum"]
    | Keyword [@name "keyword"]
    | Snippet [@name "snippet"]
    | Text [@name "text"]
    | Color [@name "color"]
    | File [@name "file"]
    | Reference [@name "reference"]
    | Customcolor [@name "customcolor"]

  include JSONABLE with type t := t
end

module Completion_item : sig
  (** CompletionItems are the suggestions returned from the CompletionsRequest. *)
  type t = {
    label : string; (** The label of this completion item. By default this is also the text that is inserted when selecting this completion. *)
    text : string option [@default None]; (** If text is not falsy then it is inserted instead of the label. *)
    sort_text : string option [@key "sortText"] [@default None]; (** A string that should be used when comparing this item with other items. When `falsy` the label is used. *)
    type_ : Completion_item_type.t option [@key "type"] [@default None]; (** The item's type. Typically the client uses this information to render the item in the UI with an icon. *)
    start : int option [@default None]; (** This value determines the location (in the CompletionsRequest's 'text' attribute) where the completion text is added.
    If missing the text is added at the location specified by the CompletionsRequest's 'column' attribute. *)
    length : int option [@default None]; (** This value determines how many characters are overwritten by the completion text.
    If missing the value 0 is assumed which results in the completion text being inserted. *)
    selection_start : int option [@key "selectionStart"] [@default None]; (** Determines the start of the new selection after the text has been inserted (or replaced).
    The start position must in the range 0 and length of the completion text.
    If omitted the selection starts at the end of the completion text. *)
    selection_length : int option [@key "selectionLength"] [@default None]; (** Determines the length of the new selection after the text has been inserted (or replaced).
    The selection can not extend beyond the bounds of the completion text.
    If omitted the length is assumed to be 0. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Value_format : sig
  (** Provides formatting information for a value. *)
  type t = {
    hex : bool option [@default None]; (** Display the value in hex. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Stack_frame_format : sig
  type t = {
    hex : bool option [@default None]; (** Display the value in hex. *)
    parameters : bool option [@default None]; (** Displays parameters for the stack frame. *)
    parameter_types : bool option [@key "parameterTypes"] [@default None]; (** Displays the types of parameters for the stack frame. *)
    parameter_names : bool option [@key "parameterNames"] [@default None]; (** Displays the names of parameters for the stack frame. *)
    parameter_values : bool option [@key "parameterValues"] [@default None]; (** Displays the values of parameters for the stack frame. *)
    line : bool option [@default None]; (** Displays the line number of the stack frame. *)
    module_ : bool option [@key "module"] [@default None]; (** Displays the module of the stack frame. *)
    include_all : bool option [@key "includeAll"] [@default None]; (** Includes all stack frames, including those the debug adapter might otherwise hide. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Exception_filter_options : sig
  (** An ExceptionFilterOptions is used to specify an exception filter together with a condition for the setExceptionsFilter request. *)
  type t = {
    filter_id : string [@key "filterId"]; (** ID of an exception filter returned by the 'exceptionBreakpointFilters' capability. *)
    condition : string option [@default None]; (** An optional expression for conditional exceptions.
    The exception will break into the debugger if the result of the condition is true. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Exception_path_segment : sig
  (** An ExceptionPathSegment represents a segment in a path that is used to match leafs or nodes in a tree of exceptions.
  If a segment consists of more than one name, it matches the names provided if 'negate' is false or missing or
  it matches anything except the names provided if 'negate' is true. *)
  type t = {
    negate : bool option [@default None]; (** If false or missing this segment matches the names provided, otherwise it matches anything except the names provided. *)
    names : string list; (** Depending on the value of 'negate' the names that should match or not match. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Exception_break_mode : sig
  (** This enumeration defines all possible conditions when a thrown exception should result in a break.
  never: never breaks,
  always: always breaks,
  unhandled: breaks when exception unhandled,
  userUnhandled: breaks if the exception is not handled by user code. *)
  type t =
    | Never [@name "never"]
    | Always [@name "always"]
    | Unhandled [@name "unhandled"]
    | User_unhandled [@name "userUnhandled"]

  include JSONABLE with type t := t
end

module Exception_options : sig
  (** An ExceptionOptions assigns configuration options to a set of exceptions. *)
  type t = {
    path : Exception_path_segment.t list option [@default None]; (** A path that selects a single or multiple exceptions in a tree. If 'path' is missing, the whole tree is selected.
    By convention the first segment of the path is a category that is used to group exceptions in the UI. *)
    break_mode : Exception_break_mode.t [@key "breakMode"]; (** Condition when a thrown exception should result in a break. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Exception_details : sig
  (** Detailed information about an exception that has occurred. *)
  type t = {
    message : string option [@default None]; (** Message contained in the exception. *)
    type_name : string option [@key "typeName"] [@default None]; (** Short type name of the exception object. *)
    full_type_name : string option [@key "fullTypeName"] [@default None]; (** Fully-qualified type name of the exception object. *)
    evaluate_name : string option [@key "evaluateName"] [@default None]; (** Optional expression that can be evaluated in the current scope to obtain the exception object. *)
    stack_trace : string option [@key "stackTrace"] [@default None]; (** Stack trace at the time the exception was thrown. *)
    inner_exception : t list option [@key "innerException"] [@default None]; (** Details of the exception contained by this exception, if any. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Disassembled_instruction : sig
  (** Represents a single disassembled instruction. *)
  type t = {
    address : string; (** The address of the instruction. Treated as a hex value if prefixed with '0x', or as a decimal value otherwise. *)
    instruction_bytes : string option [@key "instructionBytes"] [@default None]; (** Optional raw bytes representing the instruction and its operands, in an implementation-defined format. *)
    instruction : string; (** Text representing the instruction and its operands, in an implementation-defined format. *)
    symbol : string option [@default None]; (** Name of the symbol that corresponds with the location of this instruction, if any. *)
    location : Source.t option [@default None]; (** Source location that corresponds to this instruction, if any.
    Should always be set (if available) on the first instruction returned,
    but can be omitted afterwards if this instruction maps to the same source file as the previous instruction. *)
    line : int option [@default None]; (** The line within the source location that corresponds to this instruction, if any. *)
    column : int option [@default None]; (** The column within the line that corresponds to this instruction, if any. *)
    end_line : int option [@key "endLine"] [@default None]; (** The end line of the range that corresponds to this instruction, if any. *)
    end_column : int option [@key "endColumn"] [@default None]; (** The end column of the range that corresponds to this instruction, if any. *)
  }
  [@@deriving make, yojson {strict = false}]
end

module Invalidated_areas : sig
  (** Logical areas that can be invalidated by the 'invalidated' event. *)
  type t =
    | All [@name "all"]
    | Stacks [@name "stacks"]
    | Threads [@name "threads"]
    | Variables [@name "variables"]
    | Custom of string

  include JSONABLE with type t := t
end

module Initialized_event : sig
  val type_ : string

  module Payload : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

module Stopped_event : sig
  val type_ : string

  module Payload : sig
    module Reason : sig
      (** The reason for the event.
      For backward compatibility this string is shown in the UI if the 'description' attribute is missing (but it must not be translated). *)
      type t =
        | Step [@name "step"]
        | Breakpoint [@name "breakpoint"]
        | Exception [@name "exception"]
        | Pause [@name "pause"]
        | Entry [@name "entry"]
        | Goto [@name "goto"]
        | Function_breakpoint [@name "function breakpoint"]
        | Data_breakpoint [@name "data breakpoint"]
        | Instruction_breakpoint [@name "instruction breakpoint"]
        | Custom of string

      include JSONABLE with type t := t
    end

    type t = {
      reason : Reason.t; (** The reason for the event.
      For backward compatibility this string is shown in the UI if the 'description' attribute is missing (but it must not be translated). *)
      description : string option [@default None]; (** The full reason for the event, e.g. 'Paused on exception'. This string is shown in the UI as is and must be translated. *)
      thread_id : int option [@key "threadId"] [@default None]; (** The thread which was stopped. *)
      preserve_focus_hint : bool option [@key "preserveFocusHint"] [@default None]; (** A value of true hints to the frontend that this event should not change the focus. *)
      text : string option [@default None]; (** Additional information. E.g. if reason is 'exception', text contains the exception name. This string is shown in the UI. *)
      all_threads_stopped : bool option [@key "allThreadsStopped"] [@default None]; (** If 'allThreadsStopped' is true, a debug adapter can announce that all threads have stopped.
      - The client should use this information to enable that all threads can be expanded to access their stacktraces.
      - If the attribute is missing or false, only the thread with the given threadId can be expanded. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

module Continued_event : sig
  val type_ : string

  module Payload : sig
    type t = {
      thread_id : int [@key "threadId"]; (** The thread which was continued. *)
      all_threads_continued : bool option [@key "allThreadsContinued"] [@default None]; (** If 'allThreadsContinued' is true, a debug adapter can announce that all threads have continued. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

module Exited_event : sig
  val type_ : string

  module Payload : sig
    type t = {
      exit_code : int [@key "exitCode"]; (** The exit code returned from the debuggee. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

module Terminated_event : sig
  val type_ : string

  module Payload : sig
    type t = {
      restart : Any.t option [@default None]; (** A debug adapter may set 'restart' to true (or to an arbitrary object) to request that the front end restarts the session.
      The value is not interpreted by the client and passed unmodified as an attribute '__restart' to the 'launch' and 'attach' requests. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

module Thread_event : sig
  val type_ : string

  module Payload : sig
    module Reason : sig
      (** The reason for the event. *)
      type t =
        | Started [@name "started"]
        | Exited [@name "exited"]
        | Custom of string

      include JSONABLE with type t := t
    end

    type t = {
      reason : Reason.t; (** The reason for the event. *)
      thread_id : int [@key "threadId"]; (** The identifier of the thread. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

module Output_event : sig
  val type_ : string

  module Payload : sig
    module Category : sig
      (** The output category. If not specified, 'console' is assumed. *)
      type t =
        | Console [@name "console"]
        | Stdout [@name "stdout"]
        | Stderr [@name "stderr"]
        | Telemetry [@name "telemetry"]
        | Custom of string

      include JSONABLE with type t := t
    end

    module Group : sig
      (** Support for keeping an output log organized by grouping related messages. *)
      type t =
        | Start [@name "start"]
        | Start_collapsed [@name "startCollapsed"]
        | End [@name "end"]

      include JSONABLE with type t := t
    end

    type t = {
      category : Category.t option [@default None]; (** The output category. If not specified, 'console' is assumed. *)
      output : string; (** The output to report. *)
      group : Group.t option [@default None]; (** Support for keeping an output log organized by grouping related messages. *)
      variables_reference : int option [@key "variablesReference"] [@default None]; (** If an attribute 'variablesReference' exists and its value is > 0, the output contains objects which can be retrieved by passing 'variablesReference' to the 'variables' request. The value should be less than or equal to 2147483647 (2^31-1). *)
      source : Source.t option [@default None]; (** An optional source location where the output was produced. *)
      line : int option [@default None]; (** An optional source location line where the output was produced. *)
      column : int option [@default None]; (** An optional source location column where the output was produced. *)
      data : Any.t option [@default None]; (** Optional data to report. For the 'telemetry' category the data will be sent to telemetry, for the other categories the data is shown in JSON format. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

module Breakpoint_event : sig
  val type_ : string

  module Payload : sig
    module Reason : sig
      (** The reason for the event. *)
      type t =
        | Changed [@name "changed"]
        | New [@name "new"]
        | Removed [@name "removed"]
        | Custom of string

      include JSONABLE with type t := t
    end

    type t = {
      reason : Reason.t; (** The reason for the event. *)
      breakpoint : Breakpoint.t; (** The 'id' attribute is used to find the target breakpoint and the other attributes are used as the new values. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

module Module_event : sig
  val type_ : string

  module Payload : sig
    module Reason : sig
      (** The reason for the event. *)
      type t =
        | New [@name "new"]
        | Changed [@name "changed"]
        | Removed [@name "removed"]

      include JSONABLE with type t := t
    end

    type t = {
      reason : Reason.t; (** The reason for the event. *)
      module_ : Module.t [@key "module"]; (** The new, changed, or removed module. In case of 'removed' only the module id is used. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

module Loaded_source_event : sig
  val type_ : string

  module Payload : sig
    module Reason : sig
      (** The reason for the event. *)
      type t =
        | New [@name "new"]
        | Changed [@name "changed"]
        | Removed [@name "removed"]

      include JSONABLE with type t := t
    end

    type t = {
      reason : Reason.t; (** The reason for the event. *)
      source : Source.t; (** The new, changed, or removed source. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

module Process_event : sig
  val type_ : string

  module Payload : sig
    module Start_method : sig
      (** Describes how the debug engine started debugging this process. *)
      type t =
        | Launch [@name "launch"]
        | Attach [@name "attach"]
        | Attach_for_suspended_launch [@name "attachForSuspendedLaunch"]

      include JSONABLE with type t := t
    end

    type t = {
      name : string; (** The logical name of the process. This is usually the full path to process's executable file. Example: /home/example/myproj/program.js. *)
      system_process_id : int option [@key "systemProcessId"] [@default None]; (** The system process id of the debugged process. This property will be missing for non-system processes. *)
      is_local_process : bool option [@key "isLocalProcess"] [@default None]; (** If true, the process is running on the same computer as the debug adapter. *)
      start_method : Start_method.t option [@key "startMethod"] [@default None]; (** Describes how the debug engine started debugging this process. *)
      pointer_size : int option [@key "pointerSize"] [@default None]; (** The size of a pointer or address for this process, in bits. This value may be used by clients when formatting addresses for display. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

module Capabilities_event : sig
  val type_ : string

  module Payload : sig
    type t = {
      capabilities : Capabilities.t; (** The set of updated capabilities. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

module Progress_start_event : sig
  val type_ : string

  module Payload : sig
    type t = {
      progress_id : string [@key "progressId"]; (** An ID that must be used in subsequent 'progressUpdate' and 'progressEnd' events to make them refer to the same progress reporting.
      IDs must be unique within a debug session. *)
      title : string; (** Mandatory (short) title of the progress reporting. Shown in the UI to describe the long running operation. *)
      request_id : int option [@key "requestId"] [@default None]; (** The request ID that this progress report is related to. If specified a debug adapter is expected to emit
      progress events for the long running request until the request has been either completed or cancelled.
      If the request ID is omitted, the progress report is assumed to be related to some general activity of the debug adapter. *)
      cancellable : bool option [@default None]; (** If true, the request that reports progress may be canceled with a 'cancel' request.
      So this property basically controls whether the client should use UX that supports cancellation.
      Clients that don't support cancellation are allowed to ignore the setting. *)
      message : string option [@default None]; (** Optional, more detailed progress message. *)
      percentage : float option [@default None]; (** Optional progress percentage to display (value range: 0 to 100). If omitted no percentage will be shown. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

module Progress_update_event : sig
  val type_ : string

  module Payload : sig
    type t = {
      progress_id : string [@key "progressId"]; (** The ID that was introduced in the initial 'progressStart' event. *)
      message : string option [@default None]; (** Optional, more detailed progress message. If omitted, the previous message (if any) is used. *)
      percentage : float option [@default None]; (** Optional progress percentage to display (value range: 0 to 100). If omitted no percentage will be shown. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

module Progress_end_event : sig
  val type_ : string

  module Payload : sig
    type t = {
      progress_id : string [@key "progressId"]; (** The ID that was introduced in the initial 'ProgressStartEvent'. *)
      message : string option [@default None]; (** Optional, more detailed progress message. If omitted, the previous message (if any) is used. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

module Invalidated_event : sig
  val type_ : string

  module Payload : sig
    type t = {
      areas : Invalidated_areas.t list option [@default None]; (** Optional set of logical areas that got invalidated. This property has a hint characteristic: a client can only be expected to make a 'best effort' in honouring the areas but there are no guarantees. If this property is missing, empty, or if values are not understand the client should assume a single value 'all'. *)
      thread_id : int option [@key "threadId"] [@default None]; (** If specified, the client only needs to refetch data related to this thread. *)
      stack_frame_id : int option [@key "stackFrameId"] [@default None]; (** If specified, the client only needs to refetch data related to this stack frame (and the 'threadId' is ignored). *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** The 'cancel' request is used by the frontend in two situations:
- to indicate that it is no longer interested in the result produced by a specific request issued earlier
- to cancel a progress sequence. Clients should only call this request if the capability 'supportsCancelRequest' is true.
This request has a hint characteristic: a debug adapter can only be expected to make a 'best effort' in honouring this request but there are no guarantees.
The 'cancel' request may return an error if it could not cancel an operation but a frontend should refrain from presenting this error to end users.
A frontend client should only call this request if the capability 'supportsCancelRequest' is true.
The request that got canceled still needs to send a response back. This can either be a normal result ('success' attribute true)
or an error response ('success' attribute false and the 'message' set to 'cancelled').
Returning partial results from a cancelled request is possible but please note that a frontend client has no generic way for detecting that a response is partial or not.
 The progress that got cancelled still needs to send a 'progressEnd' event back.
 A client should not assume that progress just got cancelled after sending the 'cancel' request. *)
module Cancel_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'cancel' request. *)
    type t = {
      request_id : int option [@key "requestId"] [@default None]; (** The ID (attribute 'seq') of the request to cancel. If missing no request is cancelled.
      Both a 'requestId' and a 'progressId' can be specified in one request. *)
      progress_id : string option [@key "progressId"] [@default None]; (** The ID (attribute 'progressId') of the progress to cancel. If missing no progress is cancelled.
      Both a 'requestId' and a 'progressId' can be specified in one request. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** This optional request is sent from the debug adapter to the client to run a command in a terminal.
This is typically used to launch the debuggee in a terminal provided by the client.
This request should only be called if the client has passed the value true for the 'supportsRunInTerminalRequest' capability of the 'initialize' request. *)
module Run_in_terminal_command : sig
  val type_ : string

  module Arguments : sig
    module Kind : sig
      (** What kind of terminal to launch. *)
      type t =
        | Integrated [@name "integrated"]
        | External [@name "external"]

      include JSONABLE with type t := t
    end

    module Env : sig
      (** Environment key-value pairs that are added to or removed from the default environment. *)
      type t = String_opt_dict.t
      [@@deriving yojson]
    end

    (** Arguments for 'runInTerminal' request. *)
    type t = {
      kind : Kind.t option [@default None]; (** What kind of terminal to launch. *)
      title : string option [@default None]; (** Optional title of the terminal. *)
      cwd : string; (** Working directory for the command. For non-empty, valid paths this typically results in execution of a change directory command. *)
      args : string list; (** List of arguments. The first argument is the command to run. *)
      env : Env.t option [@default None]; (** Environment key-value pairs that are added to or removed from the default environment. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      process_id : int option [@key "processId"] [@default None]; (** The process ID. The value should be less than or equal to 2147483647 (2^31-1). *)
      shell_process_id : int option [@key "shellProcessId"] [@default None]; (** The process ID of the terminal shell. The value should be less than or equal to 2147483647 (2^31-1). *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** The 'initialize' request is sent as the first request from the client to the debug adapter
in order to configure it with client capabilities and to retrieve capabilities from the debug adapter.
Until the debug adapter has responded to with an 'initialize' response, the client must not send any additional requests or events to the debug adapter.
In addition the debug adapter is not allowed to send any requests or events to the client until it has responded with an 'initialize' response.
The 'initialize' request may only be sent once. *)
module Initialize_command : sig
  val type_ : string

  module Arguments : sig
    module Path_format : sig
      (** Determines in what format paths are specified. The default is 'path', which is the native format. *)
      type t =
        | Path [@name "path"]
        | Uri [@name "uri"]
        | Custom of string

      include JSONABLE with type t := t
    end

    (** Arguments for 'initialize' request. *)
    type t = {
      client_id : string option [@key "clientID"] [@default None]; (** The ID of the (frontend) client using this adapter. *)
      client_name : string option [@key "clientName"] [@default None]; (** The human readable name of the (frontend) client using this adapter. *)
      adapter_id : string [@key "adapterID"]; (** The ID of the debug adapter. *)
      locale : string option [@default None]; (** The ISO-639 locale of the (frontend) client using this adapter, e.g. en-US or de-CH. *)
      lines_start_at1 : bool option [@key "linesStartAt1"] [@default None]; (** If true all line numbers are 1-based (default). *)
      columns_start_at1 : bool option [@key "columnsStartAt1"] [@default None]; (** If true all column numbers are 1-based (default). *)
      path_format : Path_format.t option [@key "pathFormat"] [@default None]; (** Determines in what format paths are specified. The default is 'path', which is the native format. *)
      supports_variable_type : bool option [@key "supportsVariableType"] [@default None]; (** Client supports the optional type attribute for variables. *)
      supports_variable_paging : bool option [@key "supportsVariablePaging"] [@default None]; (** Client supports the paging of variables. *)
      supports_run_in_terminal_request : bool option [@key "supportsRunInTerminalRequest"] [@default None]; (** Client supports the runInTerminal request. *)
      supports_memory_references : bool option [@key "supportsMemoryReferences"] [@default None]; (** Client supports memory references. *)
      supports_progress_reporting : bool option [@key "supportsProgressReporting"] [@default None]; (** Client supports progress reporting. *)
      supports_invalidated_event : bool option [@key "supportsInvalidatedEvent"] [@default None]; (** Client supports the invalidated event. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    (** Information about the capabilities of a debug adapter. *)
    type t = Capabilities.t[@@deriving yojson]
  end
end

(** This optional request indicates that the client has finished initialization of the debug adapter.
So it is the last request in the sequence of configuration requests (which was started by the 'initialized' event).
Clients should only call this request if the capability 'supportsConfigurationDoneRequest' is true. *)
module Configuration_done_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'configurationDone' request. *)
    type t = Empty_dict.t
    [@@deriving yojson]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** This launch request is sent from the client to the debug adapter to start the debuggee with or without debugging (if 'noDebug' is true).
Since launching is debugger/runtime specific, the arguments for this request are not part of this specification. *)
module Launch_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'launch' request. Additional attributes are implementation specific. *)
    type t = {
      no_debug : bool option [@key "noDebug"] [@default None]; (** If noDebug is true the launch request should launch the program without enabling debugging. *)
      __restart : Any.t option [@default None]; (** Optional data from the previous, restarted session.
      The data is sent as the 'restart' attribute of the 'terminated' event.
      The client should leave the data intact. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** The attach request is sent from the client to the debug adapter to attach to a debuggee that is already running.
Since attaching is debugger/runtime specific, the arguments for this request are not part of this specification. *)
module Attach_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'attach' request. Additional attributes are implementation specific. *)
    type t = {
      __restart : Any.t option [@default None]; (** Optional data from the previous, restarted session.
      The data is sent as the 'restart' attribute of the 'terminated' event.
      The client should leave the data intact. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** Restarts a debug session. Clients should only call this request if the capability 'supportsRestartRequest' is true.
If the capability is missing or has the value false, a typical client will emulate 'restart' by terminating the debug adapter first and then launching it anew. *)
module Restart_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'restart' request. *)
    type t = Empty_dict.t
    [@@deriving yojson]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** The 'disconnect' request is sent from the client to the debug adapter in order to stop debugging.
It asks the debug adapter to disconnect from the debuggee and to terminate the debug adapter.
If the debuggee has been started with the 'launch' request, the 'disconnect' request terminates the debuggee.
If the 'attach' request was used to connect to the debuggee, 'disconnect' does not terminate the debuggee.
This behavior can be controlled with the 'terminateDebuggee' argument (if supported by the debug adapter). *)
module Disconnect_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'disconnect' request. *)
    type t = {
      restart : bool option [@default None]; (** A value of true indicates that this 'disconnect' request is part of a restart sequence. *)
      terminate_debuggee : bool option [@key "terminateDebuggee"] [@default None]; (** Indicates whether the debuggee should be terminated when the debugger is disconnected.
      If unspecified, the debug adapter is free to do whatever it thinks is best.
      The attribute is only honored by a debug adapter if the capability 'supportTerminateDebuggee' is true. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** The 'terminate' request is sent from the client to the debug adapter in order to give the debuggee a chance for terminating itself.
Clients should only call this request if the capability 'supportsTerminateRequest' is true. *)
module Terminate_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'terminate' request. *)
    type t = {
      restart : bool option [@default None]; (** A value of true indicates that this 'terminate' request is part of a restart sequence. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** The 'breakpointLocations' request returns all possible locations for source breakpoints in a given range.
Clients should only call this request if the capability 'supportsBreakpointLocationsRequest' is true. *)
module Breakpoint_locations_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'breakpointLocations' request. *)
    type t = {
      source : Source.t; (** The source location of the breakpoints; either 'source.path' or 'source.reference' must be specified. *)
      line : int; (** Start line of range to search possible breakpoint locations in. If only the line is specified, the request returns all possible locations in that line. *)
      column : int option [@default None]; (** Optional start column of range to search possible breakpoint locations in. If no start column is given, the first column in the start line is assumed. *)
      end_line : int option [@key "endLine"] [@default None]; (** Optional end line of range to search possible breakpoint locations in. If no end line is given, then the end line is assumed to be the start line. *)
      end_column : int option [@key "endColumn"] [@default None]; (** Optional end column of range to search possible breakpoint locations in. If no end column is given, then it is assumed to be in the last column of the end line. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      breakpoints : Breakpoint_location.t list; (** Sorted set of possible breakpoint locations. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** Sets multiple breakpoints for a single source and clears all previous breakpoints in that source.
To clear all breakpoint for a source, specify an empty array.
When a breakpoint is hit, a 'stopped' event (with reason 'breakpoint') is generated. *)
module Set_breakpoints_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'setBreakpoints' request. *)
    type t = {
      source : Source.t; (** The source location of the breakpoints; either 'source.path' or 'source.reference' must be specified. *)
      breakpoints : Source_breakpoint.t list option [@default None]; (** The code locations of the breakpoints. *)
      lines : int list option [@default None]; (** Deprecated: The code locations of the breakpoints. *)
      source_modified : bool option [@key "sourceModified"] [@default None]; (** A value of true indicates that the underlying source has been modified which results in new breakpoint locations. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      breakpoints : Breakpoint.t list; (** Information about the breakpoints.
      The array elements are in the same order as the elements of the 'breakpoints' (or the deprecated 'lines') array in the arguments. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** Replaces all existing function breakpoints with new function breakpoints.
To clear all function breakpoints, specify an empty array.
When a function breakpoint is hit, a 'stopped' event (with reason 'function breakpoint') is generated.
Clients should only call this request if the capability 'supportsFunctionBreakpoints' is true. *)
module Set_function_breakpoints_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'setFunctionBreakpoints' request. *)
    type t = {
      breakpoints : Function_breakpoint.t list; (** The function names of the breakpoints. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      breakpoints : Breakpoint.t list; (** Information about the breakpoints. The array elements correspond to the elements of the 'breakpoints' array. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** The request configures the debuggers response to thrown exceptions.
If an exception is configured to break, a 'stopped' event is fired (with reason 'exception').
Clients should only call this request if the capability 'exceptionBreakpointFilters' returns one or more filters. *)
module Set_exception_breakpoints_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'setExceptionBreakpoints' request. *)
    type t = {
      filters : string list; (** Set of exception filters specified by their ID. The set of all possible exception filters is defined by the 'exceptionBreakpointFilters' capability. The 'filter' and 'filterOptions' sets are additive. *)
      filter_options : Exception_filter_options.t list option [@key "filterOptions"] [@default None]; (** Set of exception filters and their options. The set of all possible exception filters is defined by the 'exceptionBreakpointFilters' capability. This attribute is only honored by a debug adapter if the capability 'supportsExceptionFilterOptions' is true. The 'filter' and 'filterOptions' sets are additive. *)
      exception_options : Exception_options.t list option [@key "exceptionOptions"] [@default None]; (** Configuration options for selected exceptions.
      The attribute is only honored by a debug adapter if the capability 'supportsExceptionOptions' is true. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** Obtains information on a possible data breakpoint that could be set on an expression or variable.
Clients should only call this request if the capability 'supportsDataBreakpoints' is true. *)
module Data_breakpoint_info_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'dataBreakpointInfo' request. *)
    type t = {
      variables_reference : int option [@key "variablesReference"] [@default None]; (** Reference to the Variable container if the data breakpoint is requested for a child of the container. *)
      name : string; (** The name of the Variable's child to obtain data breakpoint information for.
      If variablesReference isn’t provided, this can be an expression. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      data_id : string option [@key "dataId"]; (** An identifier for the data on which a data breakpoint can be registered with the setDataBreakpoints request or null if no data breakpoint is available. *)
      description : string; (** UI string that describes on what data the breakpoint is set on or why a data breakpoint is not available. *)
      access_types : Data_breakpoint_access_type.t list option [@key "accessTypes"] [@default None]; (** Optional attribute listing the available access types for a potential data breakpoint. A UI frontend could surface this information. *)
      can_persist : bool option [@key "canPersist"] [@default None]; (** Optional attribute indicating that a potential data breakpoint could be persisted across sessions. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** Replaces all existing data breakpoints with new data breakpoints.
To clear all data breakpoints, specify an empty array.
When a data breakpoint is hit, a 'stopped' event (with reason 'data breakpoint') is generated.
Clients should only call this request if the capability 'supportsDataBreakpoints' is true. *)
module Set_data_breakpoints_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'setDataBreakpoints' request. *)
    type t = {
      breakpoints : Data_breakpoint.t list; (** The contents of this array replaces all existing data breakpoints. An empty array clears all data breakpoints. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      breakpoints : Breakpoint.t list; (** Information about the data breakpoints. The array elements correspond to the elements of the input argument 'breakpoints' array. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** Replaces all existing instruction breakpoints. Typically, instruction breakpoints would be set from a diassembly window. 
To clear all instruction breakpoints, specify an empty array.
When an instruction breakpoint is hit, a 'stopped' event (with reason 'instruction breakpoint') is generated.
Clients should only call this request if the capability 'supportsInstructionBreakpoints' is true. *)
module Set_instruction_breakpoints_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'setInstructionBreakpoints' request *)
    type t = {
      breakpoints : Instruction_breakpoint.t list; (** The instruction references of the breakpoints *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      breakpoints : Breakpoint.t list; (** Information about the breakpoints. The array elements correspond to the elements of the 'breakpoints' array. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** The request starts the debuggee to run again. *)
module Continue_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'continue' request. *)
    type t = {
      thread_id : int [@key "threadId"]; (** Continue execution for the specified thread (if possible).
      If the backend cannot continue on a single thread but will continue on all threads, it should set the 'allThreadsContinued' attribute in the response to true. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      all_threads_continued : bool option [@key "allThreadsContinued"] [@default None]; (** If true, the 'continue' request has ignored the specified thread and continued all threads instead.
      If this attribute is missing a value of 'true' is assumed for backward compatibility. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** The request starts the debuggee to run again for one step.
The debug adapter first sends the response and then a 'stopped' event (with reason 'step') after the step has completed. *)
module Next_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'next' request. *)
    type t = {
      thread_id : int [@key "threadId"]; (** Execute 'next' for this thread. *)
      granularity : Stepping_granularity.t option [@default None]; (** Optional granularity to step. If no granularity is specified, a granularity of 'statement' is assumed. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** The request starts the debuggee to step into a function/method if possible.
If it cannot step into a target, 'stepIn' behaves like 'next'.
The debug adapter first sends the response and then a 'stopped' event (with reason 'step') after the step has completed.
If there are multiple function/method calls (or other targets) on the source line,
the optional argument 'targetId' can be used to control into which target the 'stepIn' should occur.
The list of possible targets for a given source line can be retrieved via the 'stepInTargets' request. *)
module Step_in_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'stepIn' request. *)
    type t = {
      thread_id : int [@key "threadId"]; (** Execute 'stepIn' for this thread. *)
      target_id : int option [@key "targetId"] [@default None]; (** Optional id of the target to step into. *)
      granularity : Stepping_granularity.t option [@default None]; (** Optional granularity to step. If no granularity is specified, a granularity of 'statement' is assumed. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** The request starts the debuggee to run again for one step.
The debug adapter first sends the response and then a 'stopped' event (with reason 'step') after the step has completed. *)
module Step_out_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'stepOut' request. *)
    type t = {
      thread_id : int [@key "threadId"]; (** Execute 'stepOut' for this thread. *)
      granularity : Stepping_granularity.t option [@default None]; (** Optional granularity to step. If no granularity is specified, a granularity of 'statement' is assumed. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** The request starts the debuggee to run one step backwards.
The debug adapter first sends the response and then a 'stopped' event (with reason 'step') after the step has completed.
Clients should only call this request if the capability 'supportsStepBack' is true. *)
module Step_back_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'stepBack' request. *)
    type t = {
      thread_id : int [@key "threadId"]; (** Execute 'stepBack' for this thread. *)
      granularity : Stepping_granularity.t option [@default None]; (** Optional granularity to step. If no granularity is specified, a granularity of 'statement' is assumed. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** The request starts the debuggee to run backward.
Clients should only call this request if the capability 'supportsStepBack' is true. *)
module Reverse_continue_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'reverseContinue' request. *)
    type t = {
      thread_id : int [@key "threadId"]; (** Execute 'reverseContinue' for this thread. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** The request restarts execution of the specified stackframe.
The debug adapter first sends the response and then a 'stopped' event (with reason 'restart') after the restart has completed.
Clients should only call this request if the capability 'supportsRestartFrame' is true. *)
module Restart_frame_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'restartFrame' request. *)
    type t = {
      frame_id : int [@key "frameId"]; (** Restart this stackframe. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** The request sets the location where the debuggee will continue to run.
This makes it possible to skip the execution of code or to executed code again.
The code between the current location and the goto target is not executed but skipped.
The debug adapter first sends the response and then a 'stopped' event with reason 'goto'.
Clients should only call this request if the capability 'supportsGotoTargetsRequest' is true (because only then goto targets exist that can be passed as arguments). *)
module Goto_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'goto' request. *)
    type t = {
      thread_id : int [@key "threadId"]; (** Set the goto target for this thread. *)
      target_id : int [@key "targetId"]; (** The location where the debuggee will continue to run. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** The request suspends the debuggee.
The debug adapter first sends the response and then a 'stopped' event (with reason 'pause') after the thread has been paused successfully. *)
module Pause_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'pause' request. *)
    type t = {
      thread_id : int [@key "threadId"]; (** Pause execution for this thread. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** The request returns a stacktrace from the current execution state of a given thread.
A client can request all stack frames by omitting the startFrame and levels arguments. For performance conscious clients and if the debug adapter's 'supportsDelayedStackTraceLoading' capability is true, stack frames can be retrieved in a piecemeal way with the startFrame and levels arguments. The response of the stackTrace request may contain a totalFrames property that hints at the total number of frames in the stack. If a client needs this total number upfront, it can issue a request for a single (first) frame and depending on the value of totalFrames decide how to proceed. In any case a client should be prepared to receive less frames than requested, which is an indication that the end of the stack has been reached. *)
module Stack_trace_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'stackTrace' request. *)
    type t = {
      thread_id : int [@key "threadId"]; (** Retrieve the stacktrace for this thread. *)
      start_frame : int option [@key "startFrame"] [@default None]; (** The index of the first frame to return; if omitted frames start at 0. *)
      levels : int option [@default None]; (** The maximum number of frames to return. If levels is not specified or 0, all frames are returned. *)
      format : Stack_frame_format.t option [@default None]; (** Specifies details on how to format the stack frames.
      The attribute is only honored by a debug adapter if the capability 'supportsValueFormattingOptions' is true. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      stack_frames : Stack_frame.t list [@key "stackFrames"]; (** The frames of the stackframe. If the array has length zero, there are no stackframes available.
      This means that there is no location information available. *)
      total_frames : int option [@key "totalFrames"] [@default None]; (** The total number of frames available in the stack. If omitted or if totalFrames is larger than the available frames, a client is expected to request frames until a request returns less frames than requested (which indicates the end of the stack). Returning monotonically increasing totalFrames values for subsequent requests can be used to enforce paging in the client. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** The request returns the variable scopes for a given stackframe ID. *)
module Scopes_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'scopes' request. *)
    type t = {
      frame_id : int [@key "frameId"]; (** Retrieve the scopes for this stackframe. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      scopes : Scope.t list; (** The scopes of the stackframe. If the array has length zero, there are no scopes available. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** Retrieves all child variables for the given variable reference.
An optional filter can be used to limit the fetched children to either named or indexed children. *)
module Variables_command : sig
  val type_ : string

  module Arguments : sig
    module Filter : sig
      (** Optional filter to limit the child variables to either named or indexed. If omitted, both types are fetched. *)
      type t =
        | Indexed [@name "indexed"]
        | Named [@name "named"]

      include JSONABLE with type t := t
    end

    (** Arguments for 'variables' request. *)
    type t = {
      variables_reference : int [@key "variablesReference"]; (** The Variable reference. *)
      filter : Filter.t option [@default None]; (** Optional filter to limit the child variables to either named or indexed. If omitted, both types are fetched. *)
      start : int option [@default None]; (** The index of the first variable to return; if omitted children start at 0. *)
      count : int option [@default None]; (** The number of variables to return. If count is missing or 0, all variables are returned. *)
      format : Value_format.t option [@default None]; (** Specifies details on how to format the Variable values.
      The attribute is only honored by a debug adapter if the capability 'supportsValueFormattingOptions' is true. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      variables : Variable.t list; (** All (or a range) of variables for the given variable reference. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** Set the variable with the given name in the variable container to a new value. Clients should only call this request if the capability 'supportsSetVariable' is true. *)
module Set_variable_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'setVariable' request. *)
    type t = {
      variables_reference : int [@key "variablesReference"]; (** The reference of the variable container. *)
      name : string; (** The name of the variable in the container. *)
      value : string; (** The value of the variable. *)
      format : Value_format.t option [@default None]; (** Specifies details on how to format the response value. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      value : string; (** The new value of the variable. *)
      type_ : string option [@key "type"] [@default None]; (** The type of the new value. Typically shown in the UI when hovering over the value. *)
      variables_reference : int option [@key "variablesReference"] [@default None]; (** If variablesReference is > 0, the new value is structured and its children can be retrieved by passing variablesReference to the VariablesRequest.
      The value should be less than or equal to 2147483647 (2^31-1). *)
      named_variables : int option [@key "namedVariables"] [@default None]; (** The number of named child variables.
      The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
      The value should be less than or equal to 2147483647 (2^31-1). *)
      indexed_variables : int option [@key "indexedVariables"] [@default None]; (** The number of indexed child variables.
      The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
      The value should be less than or equal to 2147483647 (2^31-1). *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** The request retrieves the source code for a given source reference. *)
module Source_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'source' request. *)
    type t = {
      source : Source.t option [@default None]; (** Specifies the source content to load. Either source.path or source.sourceReference must be specified. *)
      source_reference : int [@key "sourceReference"]; (** The reference to the source. This is the same as source.sourceReference.
      This is provided for backward compatibility since old backends do not understand the 'source' attribute. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      content : string; (** Content of the source reference. *)
      mime_type : string option [@key "mimeType"] [@default None]; (** Optional content type (mime type) of the source. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** The request retrieves a list of all threads. *)
module Threads_command : sig
  val type_ : string

  module Arguments : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end

  module Result : sig
    type t = {
      threads : Thread.t list; (** All threads. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** The request terminates the threads with the given ids.
Clients should only call this request if the capability 'supportsTerminateThreadsRequest' is true. *)
module Terminate_threads_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'terminateThreads' request. *)
    type t = {
      thread_ids : int list option [@key "threadIds"] [@default None]; (** Ids of threads to be terminated. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = Empty_dict.t
    [@@deriving yojson]
  end
end

(** Modules can be retrieved from the debug adapter with this request which can either return all modules or a range of modules to support paging.
Clients should only call this request if the capability 'supportsModulesRequest' is true. *)
module Modules_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'modules' request. *)
    type t = {
      start_module : int option [@key "startModule"] [@default None]; (** The index of the first module to return; if omitted modules start at 0. *)
      module_count : int option [@key "moduleCount"] [@default None]; (** The number of modules to return. If moduleCount is not specified or 0, all modules are returned. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      modules : Module.t list; (** All modules or range of modules. *)
      total_modules : int option [@key "totalModules"] [@default None]; (** The total number of modules available. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** Retrieves the set of all sources currently loaded by the debugged process.
Clients should only call this request if the capability 'supportsLoadedSourcesRequest' is true. *)
module Loaded_sources_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'loadedSources' request. *)
    type t = Empty_dict.t
    [@@deriving yojson]
  end

  module Result : sig
    type t = {
      sources : Source.t list; (** Set of loaded sources. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** Evaluates the given expression in the context of the top most stack frame.
The expression has access to any variables and arguments that are in scope. *)
module Evaluate_command : sig
  val type_ : string

  module Arguments : sig
    module Context : sig
      (** The context in which the evaluate request is run. *)
      type t =
        | Watch [@name "watch"]
        | Repl [@name "repl"]
        | Hover [@name "hover"]
        | Clipboard [@name "clipboard"]
        | Custom of string

      include JSONABLE with type t := t
    end

    (** Arguments for 'evaluate' request. *)
    type t = {
      expression : string; (** The expression to evaluate. *)
      frame_id : int option [@key "frameId"] [@default None]; (** Evaluate the expression in the scope of this stack frame. If not specified, the expression is evaluated in the global scope. *)
      context : Context.t option [@default None]; (** The context in which the evaluate request is run. *)
      format : Value_format.t option [@default None]; (** Specifies details on how to format the Evaluate result.
      The attribute is only honored by a debug adapter if the capability 'supportsValueFormattingOptions' is true. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      result : string; (** The result of the evaluate request. *)
      type_ : string option [@key "type"] [@default None]; (** The optional type of the evaluate result.
      This attribute should only be returned by a debug adapter if the client has passed the value true for the 'supportsVariableType' capability of the 'initialize' request. *)
      presentation_hint : Variable_presentation_hint.t option [@key "presentationHint"] [@default None]; (** Properties of a evaluate result that can be used to determine how to render the result in the UI. *)
      variables_reference : int [@key "variablesReference"]; (** If variablesReference is > 0, the evaluate result is structured and its children can be retrieved by passing variablesReference to the VariablesRequest.
      The value should be less than or equal to 2147483647 (2^31-1). *)
      named_variables : int option [@key "namedVariables"] [@default None]; (** The number of named child variables.
      The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
      The value should be less than or equal to 2147483647 (2^31-1). *)
      indexed_variables : int option [@key "indexedVariables"] [@default None]; (** The number of indexed child variables.
      The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
      The value should be less than or equal to 2147483647 (2^31-1). *)
      memory_reference : string option [@key "memoryReference"] [@default None]; (** Optional memory reference to a location appropriate for this result.
      For pointer type eval results, this is generally a reference to the memory address contained in the pointer.
      This attribute should be returned by a debug adapter if the client has passed the value true for the 'supportsMemoryReferences' capability of the 'initialize' request. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** Evaluates the given 'value' expression and assigns it to the 'expression' which must be a modifiable l-value.
The expressions have access to any variables and arguments that are in scope of the specified frame.
Clients should only call this request if the capability 'supportsSetExpression' is true. *)
module Set_expression_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'setExpression' request. *)
    type t = {
      expression : string; (** The l-value expression to assign to. *)
      value : string; (** The value expression to assign to the l-value expression. *)
      frame_id : int option [@key "frameId"] [@default None]; (** Evaluate the expressions in the scope of this stack frame. If not specified, the expressions are evaluated in the global scope. *)
      format : Value_format.t option [@default None]; (** Specifies how the resulting value should be formatted. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      value : string; (** The new value of the expression. *)
      type_ : string option [@key "type"] [@default None]; (** The optional type of the value.
      This attribute should only be returned by a debug adapter if the client has passed the value true for the 'supportsVariableType' capability of the 'initialize' request. *)
      presentation_hint : Variable_presentation_hint.t option [@key "presentationHint"] [@default None]; (** Properties of a value that can be used to determine how to render the result in the UI. *)
      variables_reference : int option [@key "variablesReference"] [@default None]; (** If variablesReference is > 0, the value is structured and its children can be retrieved by passing variablesReference to the VariablesRequest.
      The value should be less than or equal to 2147483647 (2^31-1). *)
      named_variables : int option [@key "namedVariables"] [@default None]; (** The number of named child variables.
      The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
      The value should be less than or equal to 2147483647 (2^31-1). *)
      indexed_variables : int option [@key "indexedVariables"] [@default None]; (** The number of indexed child variables.
      The client can use this optional information to present the variables in a paged UI and fetch them in chunks.
      The value should be less than or equal to 2147483647 (2^31-1). *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** This request retrieves the possible stepIn targets for the specified stack frame.
These targets can be used in the 'stepIn' request.
The StepInTargets may only be called if the 'supportsStepInTargetsRequest' capability exists and is true.
Clients should only call this request if the capability 'supportsStepInTargetsRequest' is true. *)
module Step_in_targets_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'stepInTargets' request. *)
    type t = {
      frame_id : int [@key "frameId"]; (** The stack frame for which to retrieve the possible stepIn targets. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      targets : Step_in_target.t list; (** The possible stepIn targets of the specified source location. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** This request retrieves the possible goto targets for the specified source location.
These targets can be used in the 'goto' request.
Clients should only call this request if the capability 'supportsGotoTargetsRequest' is true. *)
module Goto_targets_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'gotoTargets' request. *)
    type t = {
      source : Source.t; (** The source location for which the goto targets are determined. *)
      line : int; (** The line location for which the goto targets are determined. *)
      column : int option [@default None]; (** An optional column location for which the goto targets are determined. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      targets : Goto_target.t list; (** The possible goto targets of the specified location. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** Returns a list of possible completions for a given caret position and text.
Clients should only call this request if the capability 'supportsCompletionsRequest' is true. *)
module Completions_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'completions' request. *)
    type t = {
      frame_id : int option [@key "frameId"] [@default None]; (** Returns completions in the scope of this stack frame. If not specified, the completions are returned for the global scope. *)
      text : string; (** One or more source lines. Typically this is the text a user has typed into the debug console before he asked for completion. *)
      column : int; (** The character position for which to determine the completion proposals. *)
      line : int option [@default None]; (** An optional line for which to determine the completion proposals. If missing the first line of the text is assumed. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      targets : Completion_item.t list; (** The possible completions for . *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** Retrieves the details of the exception that caused this event to be raised.
Clients should only call this request if the capability 'supportsExceptionInfoRequest' is true. *)
module Exception_info_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'exceptionInfo' request. *)
    type t = {
      thread_id : int [@key "threadId"]; (** Thread for which exception information should be retrieved. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      exception_id : string [@key "exceptionId"]; (** ID of the exception that was thrown. *)
      description : string option [@default None]; (** Descriptive text for the exception provided by the debug adapter. *)
      break_mode : Exception_break_mode.t [@key "breakMode"]; (** Mode that caused the exception notification to be raised. *)
      details : Exception_details.t option [@default None]; (** Detailed information about the exception. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** Reads bytes from memory at the provided location.
Clients should only call this request if the capability 'supportsReadMemoryRequest' is true. *)
module Read_memory_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'readMemory' request. *)
    type t = {
      memory_reference : string [@key "memoryReference"]; (** Memory reference to the base location from which data should be read. *)
      offset : int option [@default None]; (** Optional offset (in bytes) to be applied to the reference location before reading data. Can be negative. *)
      count : int; (** Number of bytes to read at the specified location and offset. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      address : string; (** The address of the first byte of data returned.
      Treated as a hex value if prefixed with '0x', or as a decimal value otherwise. *)
      unreadable_bytes : int option [@key "unreadableBytes"] [@default None]; (** The number of unreadable bytes encountered after the last successfully read byte.
      This can be used to determine the number of bytes that must be skipped before a subsequent 'readMemory' request will succeed. *)
      data : string option [@default None]; (** The bytes read from memory, encoded using base64. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end

(** Disassembles code stored at the provided location.
Clients should only call this request if the capability 'supportsDisassembleRequest' is true. *)
module Disassemble_command : sig
  val type_ : string

  module Arguments : sig
    (** Arguments for 'disassemble' request. *)
    type t = {
      memory_reference : string [@key "memoryReference"]; (** Memory reference to the base location containing the instructions to disassemble. *)
      offset : int option [@default None]; (** Optional offset (in bytes) to be applied to the reference location before disassembling. Can be negative. *)
      instruction_offset : int option [@key "instructionOffset"] [@default None]; (** Optional offset (in instructions) to be applied after the byte offset (if any) before disassembling. Can be negative. *)
      instruction_count : int [@key "instructionCount"]; (** Number of instructions to disassemble starting at the specified location and offset.
      An adapter must return exactly this number of instructions - any unavailable instructions should be replaced with an implementation-defined 'invalid instruction' value. *)
      resolve_symbols : bool option [@key "resolveSymbols"] [@default None]; (** If true, the adapter should attempt to resolve memory addresses and other values to symbolic names. *)
    }
    [@@deriving make, yojson {strict = false}]
  end

  module Result : sig
    type t = {
      instructions : Disassembled_instruction.t list; (** The list of disassembled instructions. *)
    }
    [@@deriving make, yojson {strict = false}]
  end
end


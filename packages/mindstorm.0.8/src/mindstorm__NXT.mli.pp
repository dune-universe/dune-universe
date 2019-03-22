(* File: mindstorm__NXT.mli.pp 				-*-tuareg-*-

   Copyright (C) 2007-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/anum/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

type usb
(** USB connection type. *)

type bluetooth
(** Bluetooth connection type. *)

type 'a conn
  (** Abstract type representing a connection to a LEGO® mindstorm
      brick.  The type parameter indicates whether this connection
      is a USB or a Bluetooth one. *)

val connect_bluetooth : ?check_status:bool -> string -> bluetooth conn LWT_t
  (** [connect_bluetooth bdaddr] connects through bluetooth to the
      brick with bluetooth address [bdaddr].  See the section
      "{!section:connectBluetooth}" for more information.

      @param check_status set the default value for the [check_status]
      optional argument.  This global default allows to easily
      globally activate status checking for a given connection.
      Checking the status ensures the command was transmitted properly
      but incur a cost of 60ms between two transmissions.  Default:
      [false].

      @raise Unix.Unix_error in case of a connection problem.  In
      particular, [Unix.Unix_error(Unix.EHOSTDOWN, _,_)] is raised if
      the brick is not turned on.  *)

(** Functions to choose and connect to NXT bricks connected through
    USB.  So far, it works on Linux (users of other platforms, your
    help is welcome). *)
module USB :
sig
  type device
    (** Handle to a USB mindstorm device. *)

  val bricks : unit -> device list LWT_t
    (** [bricks()] returns the list of LEGO NXT bricks on the
        USB bus.

        @raise Failure in case of problems. *)

  val connect : ?check_status:bool -> device -> usb conn LWT_t
    (** [connect dev] connect through USB to the brick device [dev]
        (given by {!MINDSTORM.NXT.USB.bricks}).  See the section
        "{!section:connectUSB}" for more information.

        @param check_status set the default value for the [check_status]
        optional argument.  For more information, see
        {!MINDSTORM.NXT.connect_bluetooth}.

        @raise Failure in case of a connection problem.   *)
end

val close : 'a conn -> unit LWT_t
  (** [close conn] closes the connection [conn] to the brick. *)


(** {2 Exception for errors} *)

(** Error codes.  The codes starting with [Pending] are command error. *)
type error IF_LWT(= Mindstorm.NXT.error,) =
  | No_more_handles                   (** All 16 handles are in use. *)
  | No_space
  | No_more_files
  | EOF_expected
  | Not_a_linear_file
  | No_linear_space
  | Undefined_error
  | File_is_busy
  | No_write_buffers
  | Append_not_possible
  | File_is_full
  | File_exists
  | Module_not_found
  | Out_of_boundary
  | Illegal_file_name

  | Pending (** Pending communication transaction in progress *)
  | Empty_mailbox (** Specified mailbox queue is empty *)
  | Failed (** Request failed (i.e. specified file not found) *)
  | Unknown (** Unknown command opcode *)
  | Insane (** Insane packet *)
  | Out_of_range (** Data contains out-of-range values *)
  | Bus_error (** Communication bus error, can indicate a device failure. *)
  | Buffer_full (** No free memory in communication buffer *)
  | Invalid_conn (** Specified channel/connection is not valid *)
  | Busy_conn (** Specified channel/connection not configured or busy *)
  | No_program (** No active program *)
  | Bad_size (** Illegal size specified *)
  | Bad_mailbox (** Illegal mailbox queue ID specified *)
  | Bad_field (** Attempted to access invalid field of a structure *)
  | Bad_io (** Bad input or output specified *)
  | Out_of_memory (** Insufficient memory available *)
  | Bad_arg (** Bad arguments *)


exception Error of error
  (** This exception can be raised by any of the functions below
      except when the optional argument [~check_status] is set to
      false.  Note that checking for errors leads to up to
      approximately a 60ms latency between two commands.  *)

exception File_not_found
  (** Raised to indicate that a file is not present on the brick. *)


(* ---------------------------------------------------------------------- *)
(** {2 Direct commands} *)

(** Starting and stopping programs (.rxe files) on the brick. *)
module Program :
sig
  val start : ?check_status:bool -> 'a conn -> string -> unit LWT_t
    (** [start_program conn pgm] starts the program named [pgm].

        @param check_status whether to check the status returned by
        the brick.  Default: see {!MINDSTORM.NXT.connect_bluetooth}. *)

  val stop : ?check_status:bool -> 'a conn -> unit LWT_t
    (** [stop_program conn] stops the currently running program if any.
        If no program is running and [check_status=true], the exception
        {!MINDSTORM.NXT.Error}[(No_program)] is raised.

        @param check_status whether to check the status returned by
        the brick.  Default: see {!MINDSTORM.NXT.connect_bluetooth}. *)

  val name : 'a conn -> string LWT_t
    (** Return the name of the current program or raise
        {!MINDSTORM.NXT.Error}[(No_program)] if no program is running. *)
end


(** Output ports. *)
module Motor :
sig
  type port
      (** The three motor ports (immutable).  For more readability of
          your program, it is recommended you give appropriate aliases
          to the ports of interest at the beginning of your program,
          e.g. [let motor_right = MINDSTORM.NXT.Motor.a]. *)

  val a : port (** The motor port A. *)

  val b : port (** The motor port B. *)

  val c : port (** The motor port C. *)

  val all : port (** Special value representing all 3 ports. *)

  type regulation = [ `Idle | `Motor_speed | `Motor_sync ]
      (** Regulation mode.
          - [`Idle]: No regulation will be enabled.
          - [`Motor_speed]: enable power control: auto adjust PWM duty
          cycle if motor is affected by physical load.
          - [`Motor_sync]: enable synchronization: attempt to keep
          rotation in sync with another motor.  You typically use this
          mode is to maintain a straight path for a vehicle robot
          automatically.  You also can use this mode with the
          {!MINDSTORM.NXT.Motor.state} [turn_ratio] property to provide
          proportional turning.  You must set [`Motor_sync] on at
          least two motor ports to have the desired affect.  If it is
          set on all three motor ports, only the first two (A and B)
          are synchronized.  . *)

  type run_state = [ `Idle | `Ramp_up | `Running | `Ramp_down ]
      (** Specifies how to perform the transition to the new [speed]
          given in {!MINDSTORM.NXT.Motor.state}.

          - [`Idle]: The motor is not run.
          - [`Running] enables power to any output device connected to
          the specified port(s).
          - [`Ramp_up] enables automatic ramping to a new speed
          set-point that is greater than the current speed set-point.
          When you use [`Ramp_up] in conjunction with appropriate
          [tach_limit] and [speed] values, the NXT firmware attempts
          to increase the actual power smoothly to the [speed]
          set-point over the number of degrees specified by
          [tach_limit].
          - [`Ramp_down] enables automatic ramping to a new speed
          set-point that is less than the current speed set-point.
          When you use [`Ramp_down] in conjunction with appropriate
          [tach_limit] and [speed] values, the NXT firmware attempts
          to smoothly decrease the actual power to the [speed]
          set-point over the number of degrees specified by
          [tach_limit].  *)

  type state IF_LWT(= Mindstorm.NXT.Motor.state,) = {
    speed : int; (** Power set-point.  Range: -100 .. 100.  Values
                     larger than 100 will be taken as 100 and values
                     less than -100 will be takes as -100.  *)
    motor_on : bool; (** if [true], turns the motor on: enables
                         pulse-width modulation (PWM) power according
                         to speed. *)
    brake : bool; (** Use run/brake instead of run/float.  "Braking"
                      in this sense means that the output voltage is not
                      allowed to float between active PWM pulses.
                      Electronic braking improves the accuracy (and is
                      necessary to see a rotation at small speeds) of
                      motor output, but uses slightly more battery
                      power.  *)
    regulation : regulation; (** Turns on the chosen regulation. *)
    turn_ratio : int; (** Range: -100 .. 100.
                          See {!MINDSTORM.NXT.Motor.regulation}. *)
    run_state : run_state; (** See {!MINDSTORM.NXT.Motor.run_state}. *)
    tach_limit : int; (** Number of degrees to rotate; [0]: run forever.
                          Range: 0 .. 4294967295 (unsigned 32 bits).
                          See also {!MINDSTORM.NXT.Motor.run_state}. *)
  }
      (** The absolute value of [speed] is used as a percentage of the
          full power capabilities of the motor.  The sign of [speed]
          specifies rotation direction.  You must set some other
          properties appropriately for the [speed] set-point to have the
          desired effect:
          - [motor_on] must be [true].
          - [run_state] must be set to a non-[`Idle] value.

          If [not motor_on && not brake], motors are in COAST mode:
          motors connected to the specified port(s) will rotate
          freely.  *)

  val speed : ?tach_limit:int -> ?brake:bool -> ?sync:bool
    -> ?turn_ratio:int -> int -> state
    (** [speed s] returns a state where [speed] is [s], [motor_on] is
        [true] if and only if [s <> 0], and [run_state = `Running].

        @param tach_limit set the number of degrees to rotate.
           The movement is unfortunately not fully accurate.
           Default: [0] which means "no limit".
        @param brake turns on brake.  Default: [true].
        @param sync  turn on [`Motor_sync].  Default: [false].
        @param turn_ratio set a turn-ratio.  Default: [0]. *)

  val set : ?check_status:bool -> 'a conn -> port -> state -> unit LWT_t
    (** [set conn p st] sets the state of the motor connected to the
        port [p] to [st].

        @param check_status whether to check the status returned by
        the brick.  Default: see {!MINDSTORM.NXT.connect_bluetooth}. *)

  val get : 'a conn -> port -> (state * int * int * int) LWT_t
    (** [get conn p] returns [(state, tach_count, block_tach_count,
        rotation_count)] where
        - [state] is the current state of the motors;
        - [tach_count] is the number of counts since the last reset of the
        motor counter (the reset occurs when {!MINDSTORM.NXT.Motor.set} is
        issued);
        - [block_tach_count] is the current position relative to the
        last programmed movement.
        - [rotation_count] is the program-relative position counter
        relative to the last reset of the rotation sensor for motor [p].  *)

  val reset_pos : ?check_status:bool -> 'a conn -> ?relative:bool ->
                  port -> unit LWT_t
    (** [reset_pos conn p] resets the rotation count (given by the
        [rotation_count] field of {!MINDSTORM.NXT.Motor.get}) of the motor
        connected to port [p].

        @param relative if [true], relative to the last movement,
        otherwise absolute position.  Default: [false].

        @param check_status whether to check the status returned by
        the brick.  Default: see {!MINDSTORM.NXT.connect_bluetooth}.  *)
end


(** Input ports.

    The NXT brick also accepts the sensors for the previous version of
    the mindstorm brick, called RCX, so several options refer to RCX.
 *)
module Sensor :
sig
  type t
  type port = [ `S1 | `S2 | `S3 | `S4 ]
      (** The four sensor ports, labeled 1 to 4 on the brick.  It is
          recommended you give meaningful names to values of type
          [port] through let bindings.  *)

  type sensor_type =
      [ `No_sensor
      | `Switch
      | `Temperature
      | `Reflection
      | `Angle
      | `Light_active
      | `Light_inactive
      | `Sound_db
      | `Sound_dba
      | `Custom
      | `Lowspeed
      | `Lowspeed_9v
      | `Highspeed
      | `Color_full
      | `Color_red
      | `Color_green
      | `Color_blue
      | `Color_none
      ]
  (** Sensor type for a port.  The sensor type primarily affects
      scaling factors used to calculate the normalized sensor value
      [`Raw], but some values have other side effects.

      - [`No_sensor]:      No sensor configured
      - [`Switch]:	   NXT or RCX touch sensor
      - [`Temperature]:    RCX temperature sensor
      - [`Reflection]:     RCX light sensor
      - [`Angle]:          RCX rotation sensor
      - [`Light_active]:   NXT light sensor with floodlight enabled
      - [`Light_inactive]: NXT light sensor with floodlight disabled
      - [`Sound_db]:       NXT sound sensor; includes sounds too high
                           or too low for our ears.
      - [`Sound_dba]:      NXT sound sensor; focuses on sounds within
                           human hearing.
      - [`Custom]
      - [`Lowspeed]:      I2C digital sensor
      - [`Lowspeed_9v]:   I2C digital sensor, 9V power (e.g. ultrasonic).
      - [`Highspeed]:     Set [`S4] to highspeed mode.  This is currently
      unused by LEGO® sensors.  This targets the P-Net communication
      protocol (www.P-net.org).

      The LEGO® NXT 2.0 (8547) includes a color sensor with a
      tri-color led.  The following values allow to configure it.  If
      you have an older brick, this may require that you update its
      firmware.  To do it under Linux, you can issue the command
      [./fwflash path_to_firmware.rfw] (as root) where [fwflash] comes
      from {{:http://code.google.com/p/libnxt/}libnxt}.  The firmware
      1.28 can be dowloaded from
      http://legoengineering.com/library/doc_details/250-nxt-firmware-v128.html

      - [Color_full] white floodlight (all 3 leds on).
      - [Color_red] red floodlight.
      - [Color_green] green floodlight.
      - [Color_blue] blue  floodlight.
      - [Color_none] no floodlight (passive mode).  *)

  type mode =
      [ `Raw
      | `Bool
      | `Transition_cnt
      | `Period_counter
      | `Pct_full_scale
      | `Celsius
      | `Fahrenheit
      | `Angle_steps
      | `Slope_mask ]
        (** Sensor mode.
            {ul
            {- [`Raw]: Report scaled value equal to raw value.
            }
            {- [`Bool]: Report scaled value as 1 (TRUE) or 0 (FALSE).
            Note that for the switch sensor, the value is 1 of the
            button is pressed at the moment the data is requested.
            Use [`Transition_cnt] if you want not to miss button
            presses between two requests.
            The firmware uses inverse Boolean logic to match the
            physical characteristics of NXT sensors.  Readings
            are FALSE if raw value exceeds 55% of total range;
            readings are TRUE if raw value is less than 45% of
            total range.
            }
            {- [`Transition_cnt]: Report scaled value as number of
            transitions between TRUE and FALSE.  May not be fully exact
            if transitions are fast.
            }
            {- [`Period_counter]: Report scaled value as number of
            transitions from FALSE to TRUE, then back to FALSE.
            }
            {- [`Pct_full_scale]: Report scaled value as percentage of full
            scale reading for configured sensor type.}
            {- [`Celsius]: Scale temperature reading to degrees Celsius.}
            {- [`Fahrenheit]: Scale temperature reading to degrees Fahrenheit.}
            {- [`Angle_steps]: Report scaled value as count of ticks on
            RCX-style rotation sensor.}
            }
        *)

  val set : ?check_status:bool -> 'a conn ->
            port -> sensor_type -> mode -> unit LWT_t
    (** [set conn p ty m] set the sensor connected to port [p] to type
        [ty] and mode [m].

        @param check_status whether to check the status returned by
        the brick.  Default: see {!MINDSTORM.NXT.connect_bluetooth}.  *)


  (** Data read from sensors. *)
  type data IF_LWT(= Mindstorm.NXT.Sensor.data,) = {
    sensor_type : sensor_type;
    mode : mode;
    valid : bool; (** [true] if new data value should be seen as valid *)
    raw : int; (** Raw A/D value.  Device dependent.  Range: 0 .. 1023 *)
    normalized : int; (** Normalized A/D value.  Range: 0 .. 1023 *)
    scaled : int;
    (** Scaled value.  Its range depend on the {!MINDSTORM.NXT.Sensor.mode}
        chosen:
        - [`Raw]: 0 .. 1023
        - [`Boolean]: 0 or 1
        - [`Transition_cnt]: 0 .. 65535
        - [`Period_counter]: 0 .. 65535
        - [`Pct_full_scale]: 0 .. 100
        - [`Celsius]: -200 .. 700 (10th of degree Celsius)
        - [`Fahrenheit]: -400 .. 1580 (10th of degree Fahrenheit)
        - [`Angle_steps]: 0 .. 65535
    *)
  }

  val get : 'a conn -> port -> data LWT_t
    (** [get conn p] returns the data read on port [p].  Before using
        this function, you must set the sensor type with
        {!MINDSTORM.NXT.Sensor.set}. *)

  val color_of_data : data ->
    [`Black | `Blue | `Green | `Yellow | `Red | `White]
      (** Returns the color seen by the color sensor from its reading.
          This is usually only accurate at a distance of about 1cm.
          @raise Invalid_argument if not applied to a [`Color_full] sensor. *)

  val reset_scaled : ?check_status:bool -> 'a conn -> port -> unit LWT_t
    (** [reset_scaled conn port]

        @param check_status whether to check the status returned by
        the brick.  Default: see {!MINDSTORM.NXT.connect_bluetooth}. *)


  (** {4 Low speed} *)
  (** Commands dealing with the I2C bus available on every digital
      sensor.  (The port number 4 may also be high speed.) *)

  val get_status : 'a conn -> port -> int LWT_t
    (** [get_status conn port] returns the number of bytes ready to be
        read. *)

  val write : ?check_status:bool ->
              'a conn -> port -> ?rx_length:int -> string -> unit LWT_t
    (** [write conn port yx_data] writes [tx_data] to lowspeed I2C sensor
        connected to the [port].  This is the protocol (e.g. for
        talking to the ultrasonic sensor).  Communication errors will
        be reported by raising [Error Bus_error]; your application
        should be ready to handle such exceptions.

        @param rx_length gives the number of bytes to receive.
        Default: [0] i.e. no answer expected.

        @param check_status whether to check the status returned by
        the brick.  Default: see {!MINDSTORM.NXT.connect_bluetooth}.  *)

  val read : 'a conn -> port -> string LWT_t
    (** Read data from from lowspeed I2C port (e.g. for receiving data
        from the ultrasonic sensor).  Communication errors will be
        reported by raising [Error Bus_error]; your application should
        be ready to handle such exceptions.  *)


  (** {4 Convenience} *)

  (** Ultrasonic sensor.  Convenience functions to interact with the
      ultrasonic sensor through the I2C protocol. *)
  module Ultrasonic :
  sig
    type 'a t
      (** Represent an initialized ultrasonic sensor connected to a
          given port. *)

    val make : 'a conn -> port -> 'a t LWT_t
      (** [make conn port] initialize the sensor on port [port] as
          being an ultrasonic one. *)

    val set : ?check_status:bool -> 'a t ->
      [ `Off | `Meas | `Meas_cont | `Event | `Reset
      | `Meas_interval of int | `Zero of int
      | `Scale_mul of int | `Scale_div of int ] -> unit LWT_t
      (** [Ultrasonic.set us cmd] set the state or parameters for the
          ultrasonic sensor [us].  [cmd] may be:

          - [`Off]: turns the ultrasonic sensor off.
          - [`Meas]: single shot command.  In this mode, the
          ultrasonic sensor will only make a new measurement every
          time the command byte is send to the sensor.  The sensor
          will measure distances for up to 8 objects and save the
          distances within the [`Byte0] .. [`Byte7] variables.
          - [`Meas_cont] Continuous measurement command.  This is the
          default mode, where the sensor continuously makes new
          measurement with the specified interval (see
          [`Meas_interval] below).
          - [`Event]: Event capture command.  Within this mode the
          sensor will measure whether any other ultrasonic sensors are
          within the vicinity.  With this information a program can
          evaluate when it is best to make a new measurement which
          will not conflict with other ultrasonic sensors.

          - [`Meas_interval t]: set continuous measurment interval.
          - [`Zero z]: set the actual zero.
          - [`Scale_mul m]: set the actual scale factor.
          - [`Scale_div d]: set the actual scale divisor.

          - [`Reset]: resets the ultrasonic sensor.

          @param check_status check the return status.  Exceptionally,
          the default is [true] because the sensor needs to time to
          set itself up anyway and this avoids [MINDSTORM.NXT.Buffer_full]
          errors if we try to get values from the sensor. *)

    val get : 'a t ->
      [ `Byte0 | `Byte1 | `Byte2 | `Byte3 | `Byte4 | `Byte5 | `Byte6 | `Byte7
      | `Meas_interval | `Zero | `Scale_mul | `Scale_div
      ] -> int LWT_t
      (** [Ultrasonic.get us var] returns the content of the variable
          [var] on the sensor (detailed underneath).  All values are
          between 0 and 255.  Communication errors will be reported by
          raising [Error Bus_error]; your application should be ready
          to handle such exceptions.

          - [`Byte0] .. [`Byte7] are the 8 variables containing the
          distances measured with the [`Meas] or [`Meas_cont] command
          (in centimeters).

          - [`Meas_interval] returns the interval (in 0.01 sec)
          between two consecutive measurments when the sensor is in
          continuous measurment (command [`Meas_cont]).

          - [`Zero] returns the value of the actual zero (settable
          with the [`Zero] command).

          - [`Scale_mul] (resp. [`Scale_div]) return the current
          scaling factor (resp. divisor).  It is settable with the
          [`Scale_mul] (resp. [`Scale_div]) command.  *)

    val get_state : 'a t -> [`Off | `Meas | `Meas_cont | `Event | `Reset] LWT_t
      (** [get_state us] get the current state of the ultrasonic
          sensor [us]. *)
  end
end


(** Play sounds (.rso files) and tones. *)
module Sound :
sig
  val play : ?check_status:bool -> 'a conn -> ?loop:bool -> string -> unit LWT_t
    (** [play_soundfile conn file] plays the sound file named [file].
        The sound files extension, namely ".rso", must be part of [file].

        @param loop if [true] repeat the play indefinitely.
        Default: [false].

        @param check_status whether to check the status returned by the brick.
        Default: see {!MINDSTORM.NXT.connect_bluetooth}.  *)

  val stop : ?check_status:bool -> 'a conn -> unit LWT_t
    (** Stop the current playback.  Does nothing if no sound file is
        playing.

        @param check_status whether to check the status returned by the brick.
        Default: see {!MINDSTORM.NXT.connect_bluetooth}. *)

  val play_tone : ?check_status:bool -> 'a conn -> int -> int -> unit LWT_t
    (** [play_tone conn freq duration] play a tone with [freq] Hz
        lasting [duration] miliseconds.

        @param check_status whether to check the status returned by the brick.
        Default: see {!MINDSTORM.NXT.connect_bluetooth}. *)
end


(** Read and write messages from the 10 message queues.  This can be
    thought as advanced direct commands.

    Interesting information concerning this can be found in Sivan
    Toledo's paper:
    {{:http://www.tau.ac.il/~stoledo/lego/btperformance.html}Analysis
    of the NXT Bluetooth-Communication Protocol}.  *)
module Message :
sig
  type mailbox = [`B0 | `B1 | `B2 | `B3 | `B4 | `B5 | `B6 | `B7 | `B8 | `B9]
      (** The 10 available mailboxes on the NXT brick. *)

  type remote = [`R0 | `R1 | `R2 | `R3 | `R4 | `R5 | `R6 | `R7 | `R8 | `R9]
      (** Due to the master-slave relationship, slave devices may not
          initiate communication with their master, so they store
          outgoing messages in these mailboxes. *)

  val write : ?check_status:bool -> 'a conn -> mailbox -> string -> unit LWT_t
    (** [write conn box msg] writes the message [msg] to the inbox
        [box] on the NXT.  This is used to send messages to a
        currently running program.

        @param check_status whether to request a status code from the
        brick.  If [true] (the default for this fonction), the NXT
        only send a reply when it is able to queue the message without
        overflowing the queue (this prevent message loss).  If
        [false], the message may delete the oldest message in the NXT
        queue if it is full (the NXT queues are 5 messages long).  *)

  val read : 'a conn -> ?remove:bool -> [mailbox | remote] -> string LWT_t
    (** [read conn box] returns the message from the inbox [box] on
        the NXT.
        @param remove if true, clears the message from the remote inbox.
        Default: [false]. *)
end


(* ---------------------------------------------------------------------- *)
(** {2 System commands} *)

(** {3 Files} *)

type 'a in_channel
    (** Handle for reading from the brick. *)

val open_in : 'a conn -> string -> 'a in_channel LWT_t
  (** [open_in conn fname] opens the file named [fname] on the brick
      for reading.  The channel must be closed with
      {!MINDSTORM.NXT.close_in}.  Close it as soon as possible as channels
      are a scarce resource.

      @raise Invalid_argument if [fname] is not a ASCIIZ string with
      maximum 15.3 characters.  *)

val in_channel_length : 'a in_channel -> int LWT_t
  (** [in_channel_length ch] returns the length of the channel [ch]. *)

val close_in : 'a in_channel -> unit LWT_t
  (** [close_in ch] closes the channel [ch].  Closing an already
      closed channel does nothing.  *)

val input : 'a in_channel -> Bytes.t -> int -> int -> int LWT_t
  (** [input ch buf ofs len] reads a block of data of length [len]
      from the channel [ch] and write it to [buf] starting at position
      [ofs].

      @raise End_of_file if there is no more data to read. *)

type 'a out_channel
  (** Handle for writing data to the brick. *)

(** The standard NXT firmware requires that executable files and icons
    are linear but all other types of files (including sound files)
    can be non-contiguous (i.e., fragmented).

    - [`File length]: Default file, the parameter is its [length].
    - [`Linear length]: Write a linear file, the parameter is its [length].
*)
type out_flag =
    [ `File of int
    | `Linear of int
    | `Data of int
    | `Append ]

val open_out : 'a conn -> out_flag -> string -> 'a out_channel LWT_t
  (** [open_out conn flag fname] opens the file [fname] for writing.
      The channel must be closed with {!MINDSTORM.NXT.close_in}.  Close it
      as soon as possible as channels are a scarce resource.

      If the the file exists, [Error File_exists] is raised.  If the
      brick does not like the extension you use, [Error File_is_full]
      may be raised. *)

val close_out : 'a out_channel -> unit LWT_t
  (** [close_out ch] closes the channel [ch].  Closing an already
      closed channel does nothing. *)

val output : 'a out_channel -> string -> int -> int -> int LWT_t
  (** [output ch buf ofs len] ouputs the substring [buf.[ofs
      .. ofs+len-1]] to the channel [fd].  Returns the number of bytes
      actually written.  If you try to write more bytes than declared
      when opening the file, [Error File_is_full] is raised.  *)

val remove : 'a conn -> string -> unit LWT_t
  (** [remove conn fname] remove the file [fname] from the brick. *)

(** List files on the brick matching a given pattern. *)
module Find :
sig
  type 'a iterator
      (** An iterator to allow to enumerate files on the brick. *)

  val patt : 'a conn -> string -> 'a iterator LWT_t
    (** [Find.patt conn fpatt] returns an iterator listing the filenames
        mathing the pattern [fpatt].  The following types of wildcards
        are accepted:
        - filename.extension
        - *.\[file type name\]
        - filename.*
        - *.*

        @raise File_not_found if no file was found *)

  val current : 'a iterator -> string LWT_t
    (** [Find.current i] returns the current filename. *)

  val current_size : 'a iterator -> int LWT_t
    (** [Find.current_size i] returns the current filename size
        (number of bytes). *)

  val next : 'a iterator -> unit LWT_t
    (** Execute a new request to the brick to retrieve the next
        filename matching the pattern.

        @raise File_not_found if no more file was found.  When this
        exception is raised, the iterator is closed. *)

  val close : 'a iterator -> unit LWT_t
    (** [close_iterator i] closes the iterator [i].  Closing an
        already closed iterator does nothing. *)

  val iter : 'a conn -> f:(string -> int -> unit LWT_t) -> string -> unit LWT_t
    (** [iter f fpatt] iterates [f name size] on all the filenames
        matching the pattern [fpatt] (see {!MINDSTORM.NXT.Find.patt} for
        the accepted patterns).  *)

  val map : 'a conn -> f:(string -> int -> 'b LWT_t) -> string -> 'b list LWT_t
    (** [map f fpatt] maps [f name size] on all the filenames matching
        the pattern [fpatt] and return the list formed of those.  (See
        {!MINDSTORM.NXT.Find.patt} for the accepted patterns.)  *)

  val fold : 'a conn -> f:(string -> int -> 'b -> 'b LWT_t) ->
             string -> 'b -> 'b LWT_t
    (** [fold f fpatt a0] folds [f] on all the filenames matching the
        pattern [fpatt] (see {!MINDSTORM.NXT.Find.patt} for the accepted
        patterns).  *)
end


(** {3 Brick information} *)

val firmware_version : 'a conn -> (int * int * int * int) LWT_t
  (** [firmware_version conn] returns a tuple [(p1, p0, f1, f0)] where
      [p1] is the major version of the protocol, [p0] is the minor
      version of the protocol, [f1] is the major version of the
      firmware, [f0] is the minor version of the firmware, *)

val set_brick_name : ?check_status:bool -> 'a conn -> string -> unit LWT_t
  (** [set_brick_name conn name] change the name to which one is
      connected through [conn] to [name].

      @param check_status whether to check the status returned by the
      brick (and raise [Error] accordingly).  Default: see
      {!MINDSTORM.NXT.connect_bluetooth}.  *)

type brick_info IF_LWT(= Mindstorm.NXT.brick_info,) = {
  brick_name : string;
    (** NXT name (set with {!MINDSTORM.NXT.set_brick_name}) *)
  bluetooth_addr : string; (** Bluetooth address *)
  signal_strength : int; (** Bluetooth signal strength (for some reason
                             is always 0) *)
  free_user_flash : int; (** Free user FLASH *)
}

val get_device_info : 'a conn -> brick_info LWT_t
  (** [get_device_info conn] returns some informations about the brick
      connected through [conn]. *)

val keep_alive : 'a conn -> int LWT_t
  (** [keep_alive conn] returns the current sleep time limit in
      milliseconds. *)

val battery_level : 'a conn -> int LWT_t
  (** [battery_level conn] return the voltages in millivolts of the
      battery on the brick. *)

val delete_user_flash : 'a conn -> unit LWT_t

val bluetooth_reset : usb conn -> unit LWT_t
  (** [bluetooth_reset conn] performs a factory reset of the brick.
      (The type system does will a allow this command to be
      transmitted via bluetooth because all bluetooth functionality is
      reset by this command.) *)

val boot : usb conn -> unit LWT_t



(** {3 Polling} *)

val poll_length : 'a conn -> [`Poll_buffer | `High_speed_buffer] -> int LWT_t
  (** Returns the number of bytes for a command in the low-speed
      buffer or the high-speed buffer (0 = no command is ready).  *)

val poll_command : 'a conn -> [`Poll_buffer | `High_speed_buffer] -> int
  -> (int * string) LWT_t
  (** Reads bytes from the low-speed or high-speed buffer. *)

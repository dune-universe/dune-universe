(* File: mindstorm.mli

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


(** Drive Lego Minsdstorm bricks with OCaml. *)

(** Mindstorm is a library that enables you to drive Lego
    mindstorm NXT or EV3 bricks from OCaml (the computer is the master
    and the brick is the slave). Communication with the brick is done
    through Bluetooth and USB.

    @author Christophe Troestler <Christophe.Troestler@umons.ac.be>
*)

(** {2 NXT and EV3 bricks} *)

(** [Mindstorm.NXT] — Interface to NXT bricks. *)
#if OCAML_MAJOR >= 4 && OCAML_MINOR >= 2
module NXT = Mindstorm__NXT
#else
module NXT : module type of Mindstorm__NXT
#endif

(** (ALPHA VERSION)
    [Mindstorm.EV3] — Interface to EV3 bricks. *)
#if OCAML_MAJOR >= 4 && OCAML_MINOR >= 2
module EV3 = Mindstorm__EV3
#else
module EV3 : module type of Mindstorm__EV3
#endif

(** {2:connectBluetooth How to connect the brick through bluetooth}

You need to create a serial port connection using the instructions
below for your platform.  Then use {!NXT.connect_bluetooth} to
create a handle for the brick.

{3 Linux}

First make sure your kernel has bluetooth support (this is likely) and
that the bluez and gnome-bluetooth (or kdebluetooth) pakages are
installed.  You should see a bluetooth applet icon.  Then do (the
text after the $ sign is what you type, underneath is the answer):

{v
        $ hcitool scan
        Scanning ...
                00:16:53:03:A5:32     NXT
v}

to discover the address of your brick.  Then use
{!NXT.connect_bluetooth}[ "00:16:53:03:A5:32"]
or {!EV3.connect_bluetooth}[ "00:16:53:03:A5:32"] to establish the
connection (of course, replace ["00:16:53:03:A5:32"] by your actual
bluetooth address) — the first time, the brick will ask you to enter
a code and the bluetooth applet will pop up a box in which you need to
copy the very same code (this is to forbid unwanted connections).

If test programs fail with [Unix.Unix_error(Unix.EUNKNOWNERR ...)]
and your computer does not ask you the passkey (which may indicate
that you should check that the blueman applet is not running multiple
times), pair the brick with your computer first.  One way to do it is
to run [bluetoothctl] and type at its prompt (output only partly
shown):

{v
        [bluetooth]# scan on
        Discovery started
        [CHG] Controller 87:EE:A8:C3:A5:83 Discovering: yes
        [NEW] Device 01:15:34:56:31:11 EV3
        [bluetooth]# agent on
        Agent registered
        [bluetooth]# default-agent
        Default agent request successful
        [bluetooth]# pair 01:15:34:56:31:11
        Attempting to pair with 01:15:34:56:31:11
        Request PIN code
        [agent] Enter PIN code: 1234
        ...
v}

You should then be able to connect to the brick without a confirmation
being requested.


{3 MacOS X}

+ Turn on the NXT brick and make sure bluetooth is on and visible (you
  should see a bluetooth icon at the top left corner).
+ Turn on the Bluetooth on your Mac. Then click the bluetooth icon in
  the menubar, select "Open Bluetooth Preferences..."
+ You should see the brick (it's just called [NXT] by default) listed
  under 'Devices', with an option called 'Pair' on the right. If you
  don't see the device or don't see 'Pair', consider restarting the
  bluetooth on your computer and your brick and double-checking
  visibility.
+ Select the [Pair] option beside [NXT] from the list.;
+ The NXT will beep and ask for a passkey. Choose 1234 (this is the
  default but you can choose anything you like) and press the orange
  button.
+ The Mac will immediately complain, saying "Pairing failed". Ignore
  this. You will see that a new button, called "Options", has become
  available in the bluetooth menu. Click it. You'll now see a prompt
  for a code. Enter the same passkey as you did on the brick itself.
+ The NXT will beep again. You must enter your code yet again. This
  three-step process should get you your connection. On the Bluetooth
  menu, you will find that NXT is 'Connected'.
+ Note: It is pointless to proceed unless the steps above have been
  completed. Even if you run into trouble, please work on getting
  through the above before proceeding.
+ Now the brick and the Mac are talking, but we need to know which
  egress port is being used by the Mac, so we can send out our
  instructions to the brick via that port. Fortunately, this is easy
  enough to do. Run [ls -t /dev] in Terminal. This lists the files in
  the directory [/dev] arranged by the last time they were modified,
  with the most recent on top.
+ Somewhere near the top, you will see an entry alike
  [tty.NXT-DevB].  Figure out exactly what it is for your
  computer. This name is very important.

You're done! You should now have a [/dev/tty.NXT-DevB]. This means you
can connect to the brick using something to the tone of
{!NXT.connect_bluetooth}[ "/dev/tty.NXT-DevB"].  Substitute
with the name you found above, of course. Beware that if you rename
the brick with {!NXT.set_brick_name}, you will have to change
the name accordingly. Note also that this name may change when you
disconnect and reconnect.  If you can't establish a connection some
time in the future, consider going back and checking the [/dev]
directory to see if things have changed.



{3 Windows}

{4 Without the fantom drivers installed}

From windows, open the bluetooth control panel, create a new
connection to the NXT brick, right click on your connection and select
"details" to see which serial port is used, for example COM40.  Then
use {!NXT.connect_bluetooth}[ "COM40"] to connect to the brick from
your programs.  ATM, you have to always start by establishing the
connection by hand before you can use the brick.  Patches are welcome
so that is is enough to pass the bluetooth address to
{!NXT.connect_bluetooth} and the library performs the
connection.

Windows Vista uses different ports for outgoing and incoming
connections (e.g. COM4 for outgoing connections and COM5 for incoming
ones).  With this library, you must use the outgoing port.


{4 With the fantom drivers installed}

Once the fantom drivers are on your machine (which is the case if you
installed the LEGO® NXTG software), the above method does not work
anymore.  It is then probably necessary to use these drivers through
the {{:http://mindstorms.lego.com/Overview/NXTreme.aspx}Driver SDK}.
This will be investigated in a subsequent revision of this library
but you are encouraged to contribute.



{2:connectUSB How to connect the brick through USB}

{3 Linux}

For easy access, create a [lego] group, add your user to it, and create
a file [/etc/udev/rules.d/70-lego.rules] containing:
{v
        # Lego NXT                                               -*-conf-*-
        BUS=="usb", SYSFS{idVendor}=="0694", GROUP="lego", MODE="0660"
v}

To list the NXT bricks connected through USB to your computer, use
{!NXT.USB.bricks}. To connect to one of these bricks, say [b],
use {!NXT.USB.connect}[ b] (you can the query the brick, say for
its name, to decide whether it is the device you want to talk to).

{3 MacOS X}

TBD.

{3 Windows}

Install {{:http://libusb-win32.sourceforge.net/}libusb-win32}.

You need the LEGO® Mindstorms NXT software installed, as its USB
drivers are used. (Please contribute.)
 *)

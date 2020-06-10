# Examples
**To run:**
`dune exec examples/$EXAMPLE_NAME.exe`
Be sure to **update the port** in the examples according to you environment.

**No serial port?**
If you don't have a serial device to play with, you may try:
`socat -d -d pty,raw,echo=0 pty,raw,echo=0`.

Open the first device using the examples, and the second one using:
`socat - $DEVICE_LOCATION,raw,echo=0`

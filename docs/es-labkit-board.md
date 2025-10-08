# Using the Embedded Systems Lab-Kit

The [ES-LabKit](https://gits-15.sys.kth.se/mabecker/ES-Lab-Kit.git)
is supposed to both be able to be installed through the `install_student.sh` and manually.
However, it seems that even so some hardcoded paths have slunk in to some files,
e.g. the `newembproj` wrapper which assumes it's installed into `~/Documents`.
If you don't want to resolve these compatibility problems yourself it is easiest
to just clone it into `~/Documents` even when not using the `install_student.sh` script.

To install, please follow the instructions in the link above.

## Interacting with the board's UART

On linux, the serial device should show up as `/dev/ttyACM<N>`, e.g. `/dev/ttyACM0`.
The number might change if you have other devices using the same driver, such as a USB LTE modem.
Note that the USB cable needs to be plugged into the debug portion of the board and the
switch set to DBG PWR for the UART to appear.

Use your favorite terminal software to interact with it. The baud rate seems to be 115200.
With picocom you can connect with `picocom -b115200 /dev/ttyACM0`.

## OpenOCD

To communicate with the board, you need the downstream OpenOCD version
by the Raspberry Pi foundation which has support for the protocol the Pico2
uses. The version which is used by the install script is built using older libraries
to work on the school computers, and needs to be changed into the other version
if your software is more recent.
This can be done by uncommenting the lines which begin with the wget from the
Raspberry Pi Pico SDK github and commenting out the lines starting
with the wget from KTH OneDrive.

It can also be built from the [sources](https://github.com/raspberrypi/openocd) if preferred.

## The toolchain

### GCC and Binutils

The version of GCC from the install script should be fine. However, if you want to build
your own cross-compiler, the triplet is `arm-none-eabi`.

### GDB

With default build options GDB only understands the host ISA,
but it can be built to be multitarget.
This might be a separate package depending on your distribution, in which case it
will have to be installed separately.

## Bypassing VSCode

If you prefer to not use VSCode and the extension, that is possible.
This is after all based on the PicoSDK which has no such limitation.

If you have already created a project with the `newembproj` script and built
it once with VSCode (this records the paths of e.g. the toolchain),
you can build with the following commands:

```
$ cmake -GNinja -S . -B ./build
$ ninja -C ./build
```

OpenOCD can be connected with:
```
OCD_DIR="$HOME/.pico-sdk/openocd/0.12.0+dev"
$OCD_DIR/openocd --search $OCD_DIR/scripts \
        --file interface/cmsis-dap.cfg --file target/rp2350.cfg \
        -c 'adapter speed 5000'
```

which allows you to connect with gdb:
```
$ gdb build/<project-name>.elf
(gdb) target remote :3333
```

You can load the executable onto the board with `load`
and reset with `monitor reset init`, after which the pico2 will break
at the main function. This allows you to set up any breakpoints you want
before issuing `continue`.

OpenOCD can also be used to flash the program onto the board without VSCode
instead of using GDB:
```
OCD_DIR="$HOME/.pico-sdk/openocd/0.12.0+dev"
$OCD_DIR/openocd --search $OCD_DIR/scripts \
        --file interface/cmsis-dap.cfg --file target/rp2350.cfg \
        -c 'adapter speed 5000' -c "program build/$PROGRAM_NAME.elf"
```

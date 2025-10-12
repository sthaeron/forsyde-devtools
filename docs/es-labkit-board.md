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

## Runtime environment

The ES-LabKit BSP already manages much of the board initialization
(the UART, gpio pin directions, etc).

There are also support functions for setting the different leds, reading
the accelerometer, setting the seven segment display, etc.

### Functions from the BSP

From `bsp.h`.

| Function | Return | Arguments | Description |
| -------- | ------ | --------- | ----------- |
| `BSP_Init` | `void` | `void` | Initializes the ES-LabKit board |
| `BSP_ShiftRegisterWriteAll` | `void` | `uint8_t *data` | Write contents of `data` to the shift register (controls the circular leds, D1-D24) |
| `BSP_ShiftRegisterSetLed` | `void` | `uint8_t nr`, `bool state` | Set LED `nr` to `state` |
| `BSP_ShiftRegisterSetBrightness` | `void` | `uint8_t value` | Set brightness of circular leds to `value` |
| `BSP_SetLed` | `void` | `uint32_t gpio`, `bool value` | Wrapper around `gpio_put` (for leds D25-D29) |
| `BSP_ToggleLed` | `void` | `uint32_t gpio` | Inverts the value of the GPIO `gpio` (for leds D25-D29) |
| `BSP_GetInput` | `bool` | `uint32_t gpio` | Reads value of GPIO `gpio` (for buttons SW5-SW8 and switches SW10-SW17) |
| `BSP_GetAxisAcceleration` | `float` | `axis_t axis` | Get the acceleration along axis `axis` |
| `BSP_GetTapCount` | `int8_t` | `void` | Get "tap" along the Z-axis |
| `BSP_GetAcceleration` | `bool` | `float *x`, `float *y`, `float *z` | Get the acceleration along all axes |
| `BSP_7SegBrightness` | `bool` | `uint8_t level` | Set brightness of seven segment display to `level` |
| `BSP_7SegClear` | `void` | `void` | Clear the seven segment display |
| `BSP_7SegDispString` | `void` | `char *string` | Display `string` on seven segment display |
| `BSP_7SegDispInt` | `void` | `int32_t value` | Display integer `value` on the seven segment display |
| `BSP_7SegDispFloat` | `void` | `float value` | Display floating `value` on the seven segment display |
| `BSP_HasPSRam` | `size_t` | `void` | Get the size of the PSRAM (if any) |
| `BSP_WaitClkCycles` | `void` | `uint32_t n` | Spin for `n` clock cycles (busy loop 3 cycles long) |

### PicoSDK

The information in this section is mostly just a summary of the [PicoSDK](https://www.raspberrypi.com/documentation/pico-sdk/index_doxygen.html#raspberry-pi-pico-sdk)
documentation which is likely to be useful for this project.

#### StdIO
[`pico/stdio.h`](https://www.raspberrypi.com/documentation/pico-sdk/runtime.html#group_pico_stdio)

Functions for more control over the IO on the pico2.
Other than the standard input/output functions, it's also possible e.g. to limit IO
to a specific driver.

#### Time
[`pico/time.h`](https://www.raspberrypi.com/documentation/pico-sdk/high_level.html#group_pico_time)

Higher level abstractions around `hardware/timer.h`.

This is divided into the modules:
- [timestamp](https://www.raspberrypi.com/documentation/pico-sdk/high_level.html#group_timestamp):
    Timestamp functions relating to points in time (including the current time).
- [sleep](https://www.raspberrypi.com/documentation/pico-sdk/high_level.html#group_sleep):
    Sleep functions for delaying execution in a lower power state.
- [alarm](https://www.raspberrypi.com/documentation/pico-sdk/high_level.html#group_alarm):
    Alarm functions for scheduling future execution.
- [repeating\_timer](https://www.raspberrypi.com/documentation/pico-sdk/high_level.html#group_repeating_timer):
    Repeating Timer functions for simple scheduling of repeated execution.

#### StdLib

[`pico/stdlib.h`](https://www.raspberrypi.com/documentation/pico-sdk/high_level.html#group_pico_stdlib)

This header includes a few other commonly used headers, such as stdio, timers, gpio, and uart.

#### Queues
[`pico/util/queue.h`](https://www.raspberrypi.com/documentation/pico-sdk/high_level.html#group_queue)

The queue library provided by picosdk. This is used for the buffers between the SDF actors.
These are in the official documentation noted to be multi-core and IRQ safe.
This means that we can use them directly in the multi-core implementation if we get there.

Both blocking and non-blocking accessors are provided.
The non-blocking variants could add some sanity checking for core-internal buffers,
i.e. we get an error on a bad schedule instead of just hanging.
For cross-core communication, it makes more sense to use the blocking variants.

#### Multi-core control
[`pico/multicore.h`](https://www.raspberrypi.com/documentation/pico-sdk/high_level.html#group_pico_multicore)

Functions to control execution on the second core.

#### Status-led

[`pico/status_led.h`](https://www.raspberrypi.com/documentation/pico-sdk/high_level.html#group_pico_status_led)

If for some reason, the LEDs accessible through the BSP are not enough,
there is also control of the status led connected directly to the RP2350.

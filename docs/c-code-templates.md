# C Code generation using actor templates

This file will provide an overview on how to generate C code using
actor templates and the surrounding libraries. It is loosely based
on the file `/examples/implementation/platform_independent/main_platform_independent.c`


## Layout

The general file layout will consist of the top of the file, containing
some imports and definitions. Afterwards, the functions will be placed,
followed by the main function.

## Top of file

The top of the file should contain the following imports and declarations:

```C
typedef xxxx token;
#define PLATFORM {platform}
#include <stdio.h>
#include "include/common.h"
```

where `xxxx` should be the numerical type used for `Signal` in the SDF model,
typically `int`, `float` or `double` . Note that this will limit the whole SDF model to
have a singular data type that the whole graph needs to use, this is because
`actor_templates.h` relies on the `token` definition. Afterwards, the platform is
defined via the `PLATFORM` pre-processor variable. This helps wrap platform
abstractions later for the `include/common.h` file, which in turns also pulls
in `include/actor_templates.h`.

If the `--input-type` flag is used for the compiler, and this variable is set to `predefined`,
instead of relying on `scanf` to feed input tokens, the code should use a file called
`input.h` defining the following variables:

```C
int iteration_max = x;
```

where `x` is how many hyperperiods the provided input signal contains. Furthermore, the inputs need to be defined following the naming conventions of the `input_` variables, example file

```C
int iteration_max = 4;

token input_s_in_x[4] = {1,2,3,4};
token input_s_in_y[4] = {1,2,3,4};
```

This file needs to be imported in this mode in the C code with
```c
#include "input.h"
```

## Functions

After the top of the file, the functions should be placed. When used with the
actor templates, all functions should have the signature `static void`, and as
such should perform computation strictly on input/output `token` pointers.

An example actor function implementation is shown below:

```C
// D = actor22SDF (2,1) (1,2) f_d where
//  f_d [x,y] [z] = ([x + y + z], [x + y, x + y + z])
static void f_d(token *in1, token *in2, token *out1, token *out2) {
    out1[0] = in1[0] + in1[1] + in2[0];
    out2[0] = in1[0] + in1[1];
    out2[1] = in1[0] + in1[1] + in2[0];
}
```

The argument order for the function should follow the port order of the actor,
and the tokens with lowest index number should be produced/consumed first.


## Main


### Start

Start the `main` block with:

```C
int main() {
```

### BSP

Afterwards, include a line that initializes the environment, typically a board support package:

```C
init()
```

This line is wrapped in `common.h` and contains whatever the platform needs to initialize, so that
the compiler implementation does not have to worry about it.

### Status variables

After `init()`, declare a return variable that can handle returns for `scanf`
calls:

```C
int status;
```

in the case when input mode for `stdin` is used. If the `predefined` mode is used,
then declare and assign an iteration variable in the following manner:

```C
int iteration_current = 0;
```

if the `--runs` flag is set to `inf`. If the `runs` flag is set to `--runs={x}` then additionally declare

```C
int run_current = 0;
int run_max = {x};
```

### Input/Output tokens

This section should be followed by a declaration of arrays for input/output tokens:

- The output tokens should be scalar, of type `token`, since the for-loop will iterate
and read a token into this scalar each iteration of the loop. This means that one
`output` variable can be created regardless of how many outputs the graph features.

- The input tokens should be of type `token *`, and a unique array needs to be created
for every input in the graph. The inputs will need to be processed for the whole period
at once, which means that the (static) length of each array will need to be equal to
tokens consumed per firing, multiplied by that actors value in the repetition vector.
The input arrays should **only** be declared in `stdin` IO mode, and should be ommitted
in the `predefined` mode since they are already defined in the `input.h` file.

Example:

- `input_a` input for actor A. A consumes 2 tokens from input each time, and is ran 2
times per schedule: `2*2 -> 4`
- `input_b` input for actor B. B consumes 1 token from input each time, and is ran 1
time per schedule.

```C
// Temporary tokens for print/scan functions for input and output
token input_a[4];
token input_b[1];
token output;
```

### Buffers

Declare all of the local non-blocking buffers, example:

```C
buffer_nonblocking *s_1 = buffer_nonblocking_new(x);
```

### Initial tokens

Before the main while-loop starts for the schedules, place any initial tokens
in this portion of the code. Place initial tokens using `write_token()` function
for non-blocking buffers. Example:

```
write_token(s_4, 0);
```

to put initial token `0` on buffer `s_4`.


### While loop


#### Start

The while loop should have the form:

```C
while(1) {
```

#### Input

The start of the while loop should be followed by input processing. In order
to process input tokens in the `stdin` input mode, the `scanf` function should be
used to read all tokens.

Each input in the SDF model should have a translation block consisting of the
following 3 steps:

1. Read the input tokens with `scanf`, and store the return value in `ret`
2. Check if the return value `ret` is less than 1
3. Write the tokens into the corresponding buffer

Example:

```C
for (i = 0; i < 4; i++) {
    ret = scanf("%d", &input_a[i]);
}
if (ret < 1) {
    break;
}
for(i = 0; i < 4; i++) {
    write_token(s_in_a, input_a[i]);
}
```

where `%d` needs to be replaced in case another data type than decimal is read.
Similarly as [Input/Output tokens](#inputoutput-tokens), the upper bound on the
for-loop for (1.) needs to consider how many times this input needs every firing,
and how many times this buffer will be read from per period. In this example,
2 tokens are consumed from `input_a` each time the actor fires, and the actor
fires 2 times per schedule meaning 4 tokens need to be read per period.

For (2.), only the final `scanf` return value is checked for that particular
input, which will break out of the main while-loop. Afterwards for (3.), a
for-loop is used to run `write_token()` to write the input tokens to their
corresponding buffers. All `for` and `if` scopes shall be enclosed with curly
braces.

In the case when `predetermined` input is used, a modified version of step (3.) is
used. The signal input will contain strided data for each full period of the
schedule, so the `write_token` needs to subindex within a period. This is done with
for example:

```C
for(i = 0; i < 4; i++) {
    write_token(s_in_a, input_a[iteration_current * 4 + i]);
}
```

since each period contains 4 tokens of data.

#### Schedule

After all of the input tokens have been handled, the schedule should be
executed, using the `actorSDF[1-4][1-4]()` function calls. Example:

```C
actor21SDF(2, 1, 1, a_1, a_4, a_3, f_c);
```

Since the actor is of type `actor21SDF`, the first 2 arguments are consumption
rates for the inputs, the 3rd argument is the production rate on the output.
Arguments 5-6 are the input buffer pointers, 7 is the output buffer pointer.
The last argument is a function pointer to the function that the actor shall
execute. For `actorXYSDF`
- Argument 1 -> X - Token consumption rates
- Argument X + 1 -> X + Y + 1 - Token production rates
- Argument X + Y + 2 -> 2X + Y + 2 - Input buffers
- Argument 2X + Y + 3 -> 2X + 2Y + 3 - Output buffers
- Last argument - Function pointer

#### Output

For the output, all of the output buffers' tokens should be read into the
`output` variable through the `read_token()` function, and printed directly
after the token is read. Example:

```C
for(i = 0; i < 2; i++) {
    read_token(s_out, &output);
    printf("%d ", output);
}
```

or another format than `%d` if something else than integer is used. After
all of the tokens have been printed, print a new line to end the schedule:

```C
printf("\n");
```

#### Predefined input

If in `predefined` input mode with runs set to `inf`, it needs to be checked whether all the data has
been consumed. Start by incrementing the iteration counter, and then wrap
the value back to 0 if all inputs have been consumed:

```C
 iteration_current = iteration_current + 1;
 if (iteration_current == iteration_max) {
     iteration_current = 0;
 }
```

In the case where runs is set to `runs={x}`, then use the following code block

```C
if (iteration_current == iteration_max) {
    iteration_current = 0;
    run_current = run_current + 1;
}
if (run_current == run_max) {
    break;
}
```

#### End

After the output, end the while loop with a closing curl:

```C
}
```

### End of main

The end of the main should call `buffer_nonblocking_free()` for every
buffer that was created before the while-loop. Example:

```C
buffer_nonblocking_free(s_in_a);
```

After free, call `return 0;` and terminate the file with a curl followed by a
newline:

```C
}

```

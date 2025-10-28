typedef int token;

#include <stdio.h>
#include "common.h"

/* Netlist */

/*
system :: Signal Int -> Signal Int -> Signal Int
system s_ina s_inb = s_out where
         s_1 = actor_a s_ina
         s_2 = actor_b s_inb
         s_3 = actor_c s_1 s_4_delayed
(s_4, s_out) = actor_d s_2 s_3
 s_4_delayed = delaySDF [0] s_4
*/


/* Process specification: */

/*
actor_a :: Signal Int -> Signal Int
actor_a = actor11SDF 2 1 f_1 where
    f_1 [x, y] = [x + y]

actor_b :: Signal Int -> Signal Int
actor_b = actor11SDF 1 2 f_2 where
    f_2 [x] = [x, x+1]

actor_c :: Signal Int -> Signal Int -> Signal Int
actor_c = actor21SDF (2,1) 1 f_3 where
    f_3 [x, y] [z] = [x + y + z]

actor_d :: Signal Int -> Signal Int -> (Signal Int, Signal Int)
actor_d = actor22SDF (2,1) (1,2) f_4 where
    f_4 [x, y] [z] = ([x + y + z], [x + y, x + y + z])
*/


// A = actor11SDF 2 1 f_a where
//  f_a [x, y] = [x + y]
static void f_a(token *in, token *out) {
    out[0] = in[0] + in[1];
}

// B = actor11SDF 1 2 f_a where
//  f_b [x] = [x, x+1]
static void f_b(token *in, token *out) {
    out[0] = in[0];
    out[1] = in[0] + 1;
}

// C = actor21SDF (2,1) 1 f_c where
//  f_c [x,y] [z] = [x + y + z]
static void f_c(token *in1, token *in2, token *out) {
    out[0] = in1[0] + in1[1] + in2[0];
}


// D = actor22SDF (2,1) (1,2) f_c where
//  f_d [x,y] [z] = ([x + y + z], [x + y, x + y + z])
static void f_d(token *in1, token *in2, token *out1, token *out2) {
    out1[0] = in1[0] + in1[1] + in2[0];
    out2[0] = in1[0] + in1[1];
    out2[1] = in1[0] + in1[1] + in2[0];
}


/* Main Program */

int main() {

    #if PLATFORM == PICO2
        // Initialize all components on the lab-kit.
        BSP_Init();
    #endif

    // Temporary tokens for print/scan functions for input and output
    token input_a[4];
    token input_b[1];
    token output;

    // C99 array iteration variable
    int i;

    //Create FIFO-Buffers for signals

    // Buffer s_in_a: Size: 4
    buffer_nonblocking *s_in_a = buffer_nonblocking_new(4);
    // Buffer s_in_b: Size: 1
    buffer_nonblocking *s_in_b = buffer_nonblocking_new(1);
    // Buffer s_out: Size: 2

    buffer_nonblocking *s_out = buffer_nonblocking_new(2);
    // Buffer a_1: Size 2
    buffer_nonblocking *a_1 = buffer_nonblocking_new(2);
    // Buffer a_2: Size 2
    buffer_nonblocking *a_2 = buffer_nonblocking_new(2);
    // Buffer a_3: Size 1
    buffer_nonblocking *a_3 = buffer_nonblocking_new(1);
    // Buffer a_4: Size 1
    buffer_nonblocking *a_4 = buffer_nonblocking_new(1);

    // Put initial token in channel a_4
    write_token(a_4, 0);

    // Repeating Schedule: A A B C D
    while(1) {
        int ret;
        // Read input tokens
        printf("Read 5 input tokens: ");
        for(i = 0; i < 4; i++)
            ret = scanf("%d", &input_a[i]);
        for(i = 0; i < 1; i++)
            ret = scanf("%d", &input_b[i]);

        if (ret < 1)
            break;

        // Write inputs to buffer(s)
        for(i = 0; i < 4; i++)
            write_token(s_in_a, input_a[i]);
        for(i = 0; i < 1; i++)
            write_token(s_in_b, input_b[i]);

        // A
        actor11SDF(2, 1, s_in_a, a_1, f_a);

        // A
        actor11SDF(2, 1, s_in_a, a_1, f_a);

        // B
        actor11SDF(1, 2, s_in_b, a_2, f_b);

        // C
        actor21SDF(2, 1, 1, a_1, a_4, a_3, f_c);

        // D
        actor22SDF(2, 1, 1, 2, a_2, a_3, a_4, s_out, f_d);

        // Write output tokens
        printf("Output: ");
        for(i = 0; i < 2; i++) {
            read_token(s_out, &output);
            printf("%d ", output);
        }
        printf("\n");
    }

    // Free all buffers
    buffer_nonblocking_free(s_in_a);
    buffer_nonblocking_free(s_in_b);
    buffer_nonblocking_free(s_out);
    buffer_nonblocking_free(a_1);
    buffer_nonblocking_free(a_2);
    buffer_nonblocking_free(a_3);
    buffer_nonblocking_free(a_4);
    return 0;
}

typedef int token;

#include <stdio.h>

#include "common.h"
/* Netlist */

/*
system :: Signal Int -> Signal Int -> Signal Int
system s_in_x s_in_y = s_out where
        s_1 = a_a s_in_x s_in_y
        (s_out, s_2)  = a_b s_1  s_2_delayed
        s_2_delayed = d_1 s_2
*/

/* Process specifications: */

/*
a_a :: Signal Int -> Signal Int -> Signal Int
a_a s_1 s_2 = actor21SDF (1, 1) 1 add s_1 s_2

a_b :: Signal Int -> Signal Int -> (Signal Int, Signal Int)
a_b s_1 s_2 = actor22SDF (1, 1) (1, 1) accumulate s_1 s_2

d_1 :: Signal Int -> Signal Int
d_1 s = delaySDF [0] s
*/

/* Function definitions: */
/*
add :: [Int] -> [Int] -> [Int]
add [x] [y] = [x + y]

accumulate :: [Int] -> [Int]-> ([Int], [Int])
accumulate [x] [y] = ([x + y], [x + y])
*/

// accumulate :: [Int] -> [Int]-> ([Int], [Int])
// accumulate [x] [y] = ([x + y], [x + y])
static void accumulate(token *input_1, token *input_2, token *output_1,
                       token *output_2);
static void accumulate(token *input_1, token *input_2, token *output_1,
                       token *output_2) {
    output_1[0] = input_1[0] + input_2[0];
    output_2[0] = input_1[0] + input_2[0];
}

// add :: [Int] -> [Int] -> [Int]
// add [x] [y] = [x + y]
static void add(token *input_1, token *input_2, token *output);
static void add(token *input_1, token *input_2, token *output) {
    output[0] = input_1[0] + input_2[0];
}

/* Main Program */
int main() {
    init();

    // Temporary tokens for print/scan functions for input and output
    int input_a[1];
    int input_b[1];
    int output;

    // Create FIFO-Buffers for signals

    // Buffer s_in_x: Size: 1
    buffer_nonblocking *s_in_x = buffer_nonblocking_new(1);

    // Buffer s_in_y: Size: 1
    buffer_nonblocking *s_in_y = buffer_nonblocking_new(1);

    // Buffer s_out: Size: 1
    buffer_nonblocking *s_out = buffer_nonblocking_new(1);

    // Buffer s_1: Size 1
    buffer_nonblocking *s_1 = buffer_nonblocking_new(1);

    // Buffer d : Size 1
    buffer_nonblocking *s_2 = buffer_nonblocking_new(1);

    // Put initial token in channel s_2
    write_token(s_2, 0);

    // Repeating Schedule: a_a a_b
    while (1) {
        int ret;
        // Read input tokens
        for (int i = 0; i < 1; i++) {
            ret = scanf("%d", &input_a[i]);
        }
        if (ret < 1) {
            break;
        }
        for (int i = 0; i < 1; i++) {
            write_token(s_in_x, input_a[i]);
        }

        for (int i = 0; i < 1; i++) {
            scanf("%d", &input_b[i]);
        }
        if (ret < 1) {
            break;
        }
        for (int i = 0; i < 1; i++) {
            write_token(s_in_y, input_b[i]);
        }
        actor21SDF(1, 1, 1, s_in_x, s_in_y, s_1, add);

        // a_b
        actor22SDF(1, 1, 1, 1, s_1, s_2, s_out, s_2, accumulate);

        // Write output tokens
        for (int i = 0; i < 1; i++) {
            read_token(s_out, &output);
            printf("%d", output);
        }
        printf("\n");
    }

    // Free all buffers
    buffer_nonblocking_free(s_in_x);
    buffer_nonblocking_free(s_in_y);
    buffer_nonblocking_free(s_out);
    buffer_nonblocking_free(s_1);
    buffer_nonblocking_free(s_2);
    return 0;
}

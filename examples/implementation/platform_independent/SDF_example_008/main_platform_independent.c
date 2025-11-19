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
static void accumulate(token* input_1, token* input_2, token* output_1, token* output_2);
static void accumulate(token* input_1, token* input_2, token* output_1, token* output_2) {
    output_1[0] = input_1[0] + input_2[0];
    output_2[0] = input_1[0] + input_2[0];
}

// add :: [Int] -> [Int] -> [Int]
// add [x] [y] = [x + y]
static void add(token* input_1, token* input_2, token* output);
static void add(token* input_1, token* input_2, token* output) {
    output[0] = input_1[0] + input_2[0];
}

/* Main Program */
int main() {

    init();

    // Temporary tokens for print/scan functions for input and output
    token input_a;
    token input_b;
    token output;

    // C99 array iteration variable
    int i;

    //Create FIFO-Buffers for signals

    // Buffer s_in_x: Size: 1
    buffer_nonblocking *s_in_x = buffer_nonblocking_new(1);
    // Buffer s_in_y: Size: 1
    buffer_nonblocking *s_in_y = buffer_nonblocking_new(1);

    // Buffer s_out: Size: 1
    buffer_nonblocking *s_out = buffer_nonblocking_new(1);

    // Buffer a_a: Size 1
    buffer_nonblocking *a_a = buffer_nonblocking_new(1);

    // Buffer a_b: Size 1
    buffer_nonblocking *a_b = buffer_nonblocking_new(1);

    // Buffer d : Size 1
    buffer_nonblocking *d_1 = buffer_nonblocking_new(1);

    // Put initial token in channel d_1
    write_token(d_1, 0);

    // Repeating Schedule: a_a a_b
    while(1) {
        int ret;
        // Read input tokens
        printf("Read 1 input tokens: ");
        for(i = 0; i < 1; i++)
            ret = scanf("%d", &input_a);
        for(i = 0; i < 1; i++)
            ret = scanf("%d", &input_b);

        if (ret < 1)
            break;

        // Write inputs to buffer(s)
        for(i = 0; i < 1; i++)
            write_token(s_in_x, input_a);
        for(i = 0; i < 1; i++)
            write_token(s_in_y, input_b);

        // a_a
        actor21SDF(1, 1, 1, s_in_x, s_in_y, a_a, add);
        
        // a_b
        actor22SDF(1, 1, 1, 1, a_a, d_1, d_1, s_out, accumulate);

        // Write output tokens
        printf("Output: ");
        for(i = 0; i < 1; i++) {
            read_token(s_out, &output);
            printf("%d ", output);
        }
        printf("\n");
    }

    // Free all buffers
    buffer_nonblocking_free(s_in_x);
    buffer_nonblocking_free(s_in_y);
    buffer_nonblocking_free(s_out);
    buffer_nonblocking_free(a_a);
    buffer_nonblocking_free(a_b);
    buffer_nonblocking_free(d_1);
    return 0;
}

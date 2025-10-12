#include <stdio.h>
#include <pico/util/queue.h>
#include "bsp.h"

/*************************************************************/

/*
 * Buffers
 */

queue_t s_ina;
queue_t s_inb;
queue_t s_1;
queue_t s_2;
queue_t s_3;
queue_t s_4;
queue_t s_out;

/*
 * Actors
 */

static void actor_a(queue_t *s_in, queue_t *s_out)
{
    int input[2];
    int output;
    queue_remove_blocking(s_in, &input[0]);
    queue_remove_blocking(s_in, &input[1]);
    output = input[0] + input[1];
    queue_add_blocking(s_out, &output);
}

static void actor_b(queue_t *s_in, queue_t *s_out)
{
    int input;
    int output[2];

    queue_remove_blocking(s_in, &input);
    output[0] = input;
    output[1] = input + 1;
    queue_add_blocking(s_out, &output[0]);
    queue_add_blocking(s_out, &output[1]);
}

static void actor_c(queue_t *s_in1, queue_t *s_in2, queue_t *s_out)
{
    int input1[2];
    int input2;
    int output;

    queue_remove_blocking(s_in1, &input1[0]);
    queue_remove_blocking(s_in1, &input1[1]);
    queue_remove_blocking(s_in2, &input2);
    output = input1[0] + input1[1] + input2;
    queue_add_blocking(s_out, &output);
}

static void actor_d(queue_t *s_in1, queue_t *s_in2,
                    queue_t *s_out1, queue_t *s_out2)
{
    int input1[2];
    int input2;
    int output1;
    int output2[2];

    queue_remove_blocking(s_in1, &input1[0]);
    queue_remove_blocking(s_in1, &input1[1]);
    queue_remove_blocking(s_in2, &input2);
    output1 = input1[0] + input1[1] + input2;
    output2[0] = input1[0] + input1[1];
    output2[1] = input1[0] + input1[1] + input2;
    queue_add_blocking(s_out1, &output1);
    queue_add_blocking(s_out2, &output2[0]);
    queue_add_blocking(s_out2, &output2[1]);
}

/**
 * @brief Main function.
 *
 * @return int
 */
int main()
{
    int delay = 0;

    /* Initialize all components on the lab-kit. */
    BSP_Init();

    queue_init(&s_ina, sizeof(int), 2);
    queue_init(&s_inb, sizeof(int), 1);
    queue_init(&s_1, sizeof(int), 2);
    queue_init(&s_2, sizeof(int), 2);
    queue_init(&s_3, sizeof(int), 1);
    queue_init(&s_4, sizeof(int), 1);
    queue_init(&s_out, sizeof(int), 2);

    /* Put the delay token on s_4 */
    queue_add_blocking(&s_4, &delay);

    while (true) {
        int input;
        int output[2];
        for (size_t i = 0; i < 2; ++i) {
            for (size_t j = 0; j < 2; ++j) {
                printf("s_ina[%zu]: ", j);
                input = getchar();
                printf("%d\n", input);
                queue_add_blocking(&s_ina, &input);
            }

            actor_a(&s_ina, &s_1);
        }

        printf("s_inb: ");
        input = getchar();
        printf("%d\n", input);
        queue_add_blocking(&s_inb, &input);

        actor_b(&s_inb, &s_2);

        actor_c(&s_1, &s_4, &s_3);
        actor_d(&s_2, &s_3, &s_4, &s_out);

        queue_remove_blocking(&s_out, &output[0]);
        queue_remove_blocking(&s_out, &output[1]);
        printf("s_out: %d %d\n", output[0], output[1]);
    }
}
/*-----------------------------------------------------------*/

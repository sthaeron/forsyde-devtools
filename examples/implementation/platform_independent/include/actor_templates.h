
#ifndef ACTOR_TEMPLATES_H
#define ACTOR_TEMPLATES_H

static void actor11SDF(int consum, int prod,
                buffer_nonblocking *ch_in, buffer_nonblocking *ch_out,
                void (*f) (token *, token *))
{
    token input[consum], output[prod];
    int i;

    for(i = 0; i < consum; i++) {
        read_token(ch_in, &input[i]);
    }

    f(input, output);

    for(i = 0; i < prod; i++) {
        write_token(ch_out, output[i]);
    }
}

static void actor12SDF(int consum, int prod1, int prod2,
                buffer_nonblocking *ch_in, buffer_nonblocking *ch_out1,
                buffer_nonblocking *ch_out2, void (*f) (token *, token *,
                token *))
{

    token input[consum], output1[prod1], output2[prod2];
    int i;

    for(i = 0; i < consum; i++) {
        read_token(ch_in, &input[i]);
    }

    f(input, output1, output2);

    for(i = 0; i < prod1; i++) {
        write_token(ch_out1, output1[i]);
    }

    for(i = 0; i < prod2; i++) {
        write_token(ch_out2, output2[i]);
    }
}

static void actor13SDF(int consum, int prod1, int prod2, int prod3,
                buffer_nonblocking *ch_in, buffer_nonblocking *ch_out1,
                buffer_nonblocking *ch_out2, buffer_nonblocking *ch_out3,
                void (*f) (token *, token *, token *, token *))
{

    token input[consum], output1[prod1], output2[prod2], output3[prod3];
    int i;

    for(i = 0; i < consum; i++) {
        read_token(ch_in, &input[i]);
    }

    f(input, output1, output2, output3);

    for(i = 0; i < prod1; i++) {
        write_token(ch_out1, output1[i]);
    }

    for(i = 0; i < prod2; i++) {
        write_token(ch_out2, output2[i]);
    }

    for(i = 0; i < prod3; i++) {
        write_token(ch_out3, output3[i]);
    }
}


static void actor14SDF(int consum, int prod1, int prod2, int prod3, int prod4,
                buffer_nonblocking *ch_in, buffer_nonblocking *ch_out1,
                buffer_nonblocking *ch_out2, buffer_nonblocking *ch_out3,
                buffer_nonblocking *ch_out4, void (*f) (token *, token *,
                token *, token *, token *))
{

    token input[consum], output1[prod1], output2[prod2], output3[prod3],
        output4[prod4];

    int i;

    for(i = 0; i < consum; i++) {
        read_token(ch_in, &input[i]);
    }

    f(input, output1, output2, output3, output4);

    for(i = 0; i < prod1; i++) {
        write_token(ch_out1, output1[i]);
    }

    for(i = 0; i < prod2; i++) {
        write_token(ch_out2, output2[i]);
    }

    for(i = 0; i < prod3; i++) {
        write_token(ch_out3, output3[i]);
    }

    for(i = 0; i < prod4; i++) {
        write_token(ch_out4, output4[i]);
    }
}


static void actor21SDF(int consum1, int consum2, int prod,
                buffer_nonblocking *ch_in1, buffer_nonblocking *ch_in2,
                buffer_nonblocking *ch_out, void (*f) (token *, token *,
                token *))
{
    token input1[consum1], input2[consum2], output[prod];
    int i;

    for(i = 0; i < consum1; i++) {
        read_token(ch_in1, &input1[i]);
    }

    for(i = 0; i < consum2; i++) {
        read_token(ch_in2, &input2[i]);
    }

    f(input1, input2, output);

    for(i = 0; i < prod; i++) {
        write_token(ch_out, output[i]);
    }
}

static void actor22SDF(int consum1, int consum2, int prod1, int prod2,
                buffer_nonblocking *ch_in1, buffer_nonblocking *ch_in2,
                buffer_nonblocking *ch_out1, buffer_nonblocking *ch_out2,
                void (*f) (token *, token *, token *, token *))
{
    token input1[consum1], input2[consum2], output1[prod1], output2[prod2];
    int i;

    for(i = 0; i < consum1; i++) {
        read_token(ch_in1, &input1[i]);
    }

    for(i = 0; i < consum2; i++) {
        read_token(ch_in2, &input2[i]);
    }

    f(input1, input2, output1, output2);

    for(i = 0; i < prod1; i++) {
        write_token(ch_out1, output1[i]);
    }

    for(i = 0; i < prod2; i++) {
        write_token(ch_out2, output2[i]);
    }
}

static void actor23SDF(int consum1, int consum2, int prod1, int prod2,
                int prod3, buffer_nonblocking *ch_in1,
                buffer_nonblocking *ch_in2, buffer_nonblocking *ch_out1, 
                buffer_nonblocking *ch_out2, buffer_nonblocking *ch_out3,
                void (*f) (token *, token *, token *, token *, token *))
{
    token input1[consum1], input2[consum2], output1[prod1], output2[prod2],
        output3[prod3];
    int i;

    for(i = 0; i < consum1; i++) {
        read_token(ch_in1, &input1[i]);
    }

    for(i = 0; i < consum2; i++) {
        read_token(ch_in2, &input2[i]);
    }

    f(input1, input2, output1, output2, output3);

    for(i = 0; i < prod1; i++) {
        write_token(ch_out1, output1[i]);
    }

    for(i = 0; i < prod2; i++) {
        write_token(ch_out2, output2[i]);
    }

    for(i = 0; i < prod3; i++) {
        write_token(ch_out3, output3[i]);
    }
}

static void actor24SDF(int consum1, int consum2, int prod1, int prod2,
                int prod3, int prod4, buffer_nonblocking *ch_in1,
                buffer_nonblocking *ch_in2, buffer_nonblocking *ch_out1,
                buffer_nonblocking *ch_out2, buffer_nonblocking *ch_out3,
                buffer_nonblocking *ch_out4, void (*f) (token *, token *,
                token *, token *, token *, token *))
{
    token input1[consum1], input2[consum2], output1[prod1], output2[prod2],
        output3[prod3], output4[prod4];
    int i;

    for(i = 0; i < consum1; i++) {
        read_token(ch_in1, &input1[i]);
    }

    for(i = 0; i < consum2; i++) {
        read_token(ch_in2, &input2[i]);
    }

    f(input1, input2, output1, output2, output3, output4);

    for(i = 0; i < prod1; i++) {
        write_token(ch_out1, output1[i]);
    }

    for(i = 0; i < prod2; i++) {
        write_token(ch_out2, output2[i]);
    }

    for(i = 0; i < prod3; i++) {
        write_token(ch_out3, output3[i]);
    }

    for(i = 0; i < prod4; i++) {
        write_token(ch_out4, output4[i]);
    }
}

static void actor31SDF(int consum1, int consum2, int consum3, int prod1,
                buffer_nonblocking *ch_in1, buffer_nonblocking *ch_in2,
                buffer_nonblocking *ch_in3, buffer_nonblocking *ch_out1,
                void (*f) (token *, token *, token *, token *))
{
    token input1[consum1], input2[consum2], input3[consum1], output1[prod1];
    int i;

    for(i = 0; i < consum1; i++) {
        read_token(ch_in1, &input1[i]);
    }

    for(i = 0; i < consum2; i++) {
        read_token(ch_in2, &input2[i]);
    }

    for(i = 0; i < consum3; i++) {
        read_token(ch_in3, &input3[i]);
    }

    f(input1, input2, input3, output1);

    for(i = 0; i < prod1; i++) {
        write_token(ch_out1, output1[i]);
    }
}

static void actor32SDF(int consum1, int consum2, int consum3, int prod1,
                int prod2, buffer_nonblocking *ch_in1,
                buffer_nonblocking *ch_in2, buffer_nonblocking *ch_in3,
                buffer_nonblocking *ch_out1, buffer_nonblocking *ch_out2,
                void (*f) (token *, token *, token *, token *, token *))
{
    token input1[consum1], input2[consum2], input3[consum1], output1[prod1],
        output2[prod2];
    int i;

    for(i = 0; i < consum1; i++) {
        read_token(ch_in1, &input1[i]);
    }

    for(i = 0; i < consum2; i++) {
        read_token(ch_in2, &input2[i]);
    }

    for(i = 0; i < consum3; i++) {
        read_token(ch_in3, &input3[i]);
    }

    f(input1, input2, input3, output1, output2);

    for(i = 0; i < prod1; i++) {
        write_token(ch_out1, output1[i]);
    }

    for(i = 0; i < prod2; i++) {
        write_token(ch_out2, output2[i]);
    }
}

static void actor33SDF(int consum1, int consum2, int consum3, int prod1,
                int prod2, int prod3, buffer_nonblocking *ch_in1,
                buffer_nonblocking *ch_in2, buffer_nonblocking *ch_in3,
                buffer_nonblocking *ch_out1, buffer_nonblocking *ch_out2,
                buffer_nonblocking *ch_out3, void (*f) (token *, token *,
                token *, token *, token *, token *))
{
    token input1[consum1], input2[consum2], input3[consum1], output1[prod1],
        output2[prod2], output3[prod3];
    int i;

    for(i = 0; i < consum1; i++) {
        read_token(ch_in1, &input1[i]);
    }

    for(i = 0; i < consum2; i++) {
        read_token(ch_in2, &input2[i]);
    }

    for(i = 0; i < consum3; i++) {
        read_token(ch_in3, &input3[i]);
    }

    f(input1, input2, input3, output1, output2, output3);

    for(i = 0; i < prod1; i++) {
        write_token(ch_out1, output1[i]);
    }

    for(i = 0; i < prod2; i++) {
        write_token(ch_out2, output2[i]);
    }

    for(i = 0; i < prod3; i++) {
        write_token(ch_out3, output3[i]);
    }
}

static void actor34SDF(int consum1, int consum2, int consum3, int prod1,
                int prod2, int prod3, int prod4, buffer_nonblocking *ch_in1,
                buffer_nonblocking *ch_in2, buffer_nonblocking *ch_in3,
                buffer_nonblocking *ch_out1, buffer_nonblocking *ch_out2,
                buffer_nonblocking *ch_out3, buffer_nonblocking *ch_out4,
                void (*f) (token *, token *, token *, token *, token *,
                token *, token *))
{
    token input1[consum1], input2[consum2], input3[consum1], output1[prod1],
        output2[prod2], output3[prod3], output4[prod4];
    int i;

    for(i = 0; i < consum1; i++) {
        read_token(ch_in1, &input1[i]);
    }

    for(i = 0; i < consum2; i++) {
        read_token(ch_in2, &input2[i]);
    }

    for(i = 0; i < consum3; i++) {
        read_token(ch_in3, &input3[i]);
    }

    f(input1, input2, input3, output1, output2, output3, output4);

    for(i = 0; i < prod1; i++) {
        write_token(ch_out1, output1[i]);
    }

    for(i = 0; i < prod2; i++) {
        write_token(ch_out2, output2[i]);
    }

    for(i = 0; i < prod3; i++) {
        write_token(ch_out3, output3[i]);
    }

    for(i = 0; i < prod4; i++) {
        write_token(ch_out4, output4[i]);
    }
}

static void actor41SDF(int consum1, int consum2, int consum3, int consum4,
                int prod1, buffer_nonblocking *ch_in1,
                buffer_nonblocking *ch_in2, buffer_nonblocking *ch_in3,
                buffer_nonblocking *ch_in4, buffer_nonblocking *ch_out1,
                void (*f) (token *, token *, token *, token *, token *))
{
    token input1[consum1], input2[consum2], input3[consum1], input4[consum4],
        output1[prod1];
    int i;

    for(i = 0; i < consum1; i++) {
        read_token(ch_in1, &input1[i]);
    }

    for(i = 0; i < consum2; i++) {
        read_token(ch_in2, &input2[i]);
    }

    for(i = 0; i < consum3; i++) {
        read_token(ch_in3, &input3[i]);
    }

    for(i = 0; i < consum4; i++) {
        read_token(ch_in4, &input4[i]);
    }

    f(input1, input2, input3, input4, output1);

    for(i = 0; i < prod1; i++) {
        write_token(ch_out1, output1[i]);
    }
}

static void actor42SDF(int consum1, int consum2, int consum3, int consum4,
                int prod1, int prod2, buffer_nonblocking *ch_in1,
                buffer_nonblocking *ch_in2, buffer_nonblocking *ch_in3,
                buffer_nonblocking *ch_in4, buffer_nonblocking *ch_out1,
                buffer_nonblocking *ch_out2, void (*f) (token *, token *,
                token *, token *, token *, token *))
{
    token input1[consum1], input2[consum2], input3[consum1], input4[consum4],
        output1[prod1], output2[prod2];
    int i;

    for(i = 0; i < consum1; i++) {
        read_token(ch_in1, &input1[i]);
    }

    for(i = 0; i < consum2; i++) {
        read_token(ch_in2, &input2[i]);
    }

    for(i = 0; i < consum3; i++) {
        read_token(ch_in3, &input3[i]);
    }

    for(i = 0; i < consum4; i++) {
        read_token(ch_in4, &input4[i]);
    }

    f(input1, input2, input3, input4, output1, output2);

    for(i = 0; i < prod1; i++) {
        write_token(ch_out1, output1[i]);
    }

    for(i = 0; i < prod2; i++) {
        write_token(ch_out2, output2[i]);
    }
}

static void actor43SDF(int consum1, int consum2, int consum3, int consum4,
                int prod1, int prod2, int prod3, buffer_nonblocking *ch_in1,
                buffer_nonblocking *ch_in2, buffer_nonblocking *ch_in3,
                buffer_nonblocking *ch_in4, buffer_nonblocking *ch_out1,
                buffer_nonblocking *ch_out2, buffer_nonblocking *ch_out3,
                void (*f) (token *, token *, token *, token *, token *,
                token *, token *))
{
    token input1[consum1], input2[consum2], input3[consum1], input4[consum4],
        output1[prod1], output2[prod2], output3[prod3];
    int i;

    for(i = 0; i < consum1; i++) {
        read_token(ch_in1, &input1[i]);
    }

    for(i = 0; i < consum2; i++) {
        read_token(ch_in2, &input2[i]);
    }

    for(i = 0; i < consum3; i++) {
        read_token(ch_in3, &input3[i]);
    }

    for(i = 0; i < consum4; i++) {
        read_token(ch_in4, &input4[i]);
    }

    f(input1, input2, input3, input4, output1, output2, output3);

    for(i = 0; i < prod1; i++) {
        write_token(ch_out1, output1[i]);
    }

    for(i = 0; i < prod2; i++) {
        write_token(ch_out2, output2[i]);
    }

    for(i = 0; i < prod3; i++) {
        write_token(ch_out3, output3[i]);
    }
}

static void actor44SDF(int consum1, int consum2, int consum3, int consum4,
                int prod1, int prod2, int prod3, int prod4,
                buffer_nonblocking *ch_in1, buffer_nonblocking *ch_in2,
                buffer_nonblocking *ch_in3, buffer_nonblocking *ch_in4,
                buffer_nonblocking *ch_out1, buffer_nonblocking *ch_out2,
                buffer_nonblocking *ch_out3, buffer_nonblocking *ch_out4,
                void (*f) (token *, token *, token *, token *, token *,
                token *, token *, token *))
{
    token input1[consum1], input2[consum2], input3[consum1], input4[consum4],
        output1[prod1], output2[prod2], output3[prod3], output4[prod4];
    int i;

    for(i = 0; i < consum1; i++) {
        read_token(ch_in1, &input1[i]);
    }

    for(i = 0; i < consum2; i++) {
        read_token(ch_in2, &input2[i]);
    }

    for(i = 0; i < consum3; i++) {
        read_token(ch_in3, &input3[i]);
    }

    for(i = 0; i < consum4; i++) {
        read_token(ch_in4, &input4[i]);
    }

    f(input1, input2, input3, input4, output1, output2, output3, output4);

    for(i = 0; i < prod1; i++) {
        write_token(ch_out1, output1[i]);
    }

    for(i = 0; i < prod2; i++) {
        write_token(ch_out2, output2[i]);
    }

    for(i = 0; i < prod3; i++) {
        write_token(ch_out3, output3[i]);
    }

    for(i = 0; i < prod4; i++) {
        write_token(ch_out4, output4[i]);
    }
}

#endif

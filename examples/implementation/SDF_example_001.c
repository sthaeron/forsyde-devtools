#define _REENTRANT

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

/*
 * in1 -> A -> B -> out1
 *        \    \
 *         v    v
 *          C -> D -> out2
 */

#define E_F "%d"
typedef int element;

struct fifo {
	pthread_mutex_t lock;
	pthread_cond_t notempty;
	pthread_cond_t notfull;
	size_t size;
	size_t used;
	size_t head;
	size_t tail;
	element elements[];
};

static void fifo_init(struct fifo *fifo, size_t size)
{
	pthread_mutex_init(&fifo->lock, NULL);
	pthread_cond_init(&fifo->notempty, NULL);
	pthread_cond_init(&fifo->notfull, NULL);

	fifo->size = size;
	fifo->used = 0;
	fifo->head = 0;
	fifo->tail = 0;
}

static struct fifo *fifo_new(size_t size)
{
	struct fifo *fifo = malloc(sizeof(*fifo) + size * sizeof(element));

	fifo_init(fifo, size);

	return fifo;
}

static void fifo_free(struct fifo *fifo)
{
	free(fifo);
}

static void fifo_put_element(struct fifo *fifo, element element)
{
	pthread_mutex_lock(&fifo->lock);

	while (fifo->used >= fifo->size) {
		// fprintf(stderr, "Waiting for space...\n");
		pthread_cond_wait(&fifo->notfull, &fifo->lock);
	}

	fifo->elements[fifo->head] = element;
	fifo->head = (fifo->head + 1) % fifo->size;
	++fifo->used;

	pthread_mutex_unlock(&fifo->lock);
	pthread_cond_broadcast(&fifo->notempty);
}

static void fifo_put_multiple(struct fifo *fifo, size_t count, element elements[count])
{
	size_t count_a, count_b;

	pthread_mutex_lock(&fifo->lock);

	while (fifo->used + count > fifo->size) {
		// fprintf(stderr, "Waiting for space...\n");
		pthread_cond_wait(&fifo->notfull, &fifo->lock);
	}

	count_a = fifo->head + count <= fifo->size ? count : fifo->size - fifo->head;
	memcpy(&fifo->elements[fifo->head], &elements[0], sizeof(element)*count);

	if (count_a < count) {
		count_b = count - count_a;
		memcpy(&fifo->elements[0], &elements[count_a], sizeof(element)*count_b);
	}

	fifo->head = (fifo->head + count) % fifo->size;
	fifo->used += count;

	pthread_mutex_unlock(&fifo->lock);
	pthread_cond_broadcast(&fifo->notempty);
}

static void fifo_get_element(struct fifo *fifo, element *element)
{
	pthread_mutex_lock(&fifo->lock);

	while (fifo->used == 0) {
		// fprintf(stderr, "Waiting for element...\n");
		pthread_cond_wait(&fifo->notempty, &fifo->lock);
	}

	*element = fifo->elements[fifo->tail];
	fifo->tail = (fifo->tail + 1) % fifo->size;
	--fifo->used;

	pthread_mutex_unlock(&fifo->lock);
	pthread_cond_broadcast(&fifo->notfull);
}

static void fifo_get_multiple(struct fifo *fifo, size_t count, element elements[count])
{
	size_t count_a, count_b;

	pthread_mutex_lock(&fifo->lock);

	while (fifo->used < count) {
		// fprintf(stderr, "Waiting for space...\n");
		pthread_cond_wait(&fifo->notempty, &fifo->lock);
	}

	count_a = fifo->tail + count <= fifo->size ? count : fifo->size - fifo->tail;
	memcpy(&elements[0], &fifo->elements[fifo->tail], sizeof(element)*count);

	if (count_a < count) {
		count_b = count - count_a;
		memcpy(&fifo->elements[0], &elements[count_a], sizeof(element)*count_b);
	}

	fifo->tail = (fifo->tail + count) % fifo->size;
	fifo->used -= count;

	pthread_mutex_unlock(&fifo->lock);
	pthread_cond_broadcast(&fifo->notempty);
}

enum process {
	Input = 0,
	ProcessA = 1,
	ProcessB,
	ProcessC,
	ProcessD,
	Output,
	End
};

struct fifo *fifo_in_a;
struct fifo *fifo_a_b;
struct fifo *fifo_a_c;
struct fifo *fifo_b_d;
struct fifo *fifo_c_d;
struct fifo *fifo_b_out1;
struct fifo *fifo_d_out2;

static void *process_input(void *arg)
{
	element input[2];
	int ret;

	fprintf(stderr, "Process Input starting\n");
	while (!feof(stdin)) {
		ret = scanf(E_F " " E_F, &input[0], &input[1]);
		if (ret < 2)
			break;
		fifo_put_multiple(fifo_in_a, 2, input);
	}
	fprintf(stderr, "Input exiting\n");
	pthread_exit(NULL);
}

static void *process_a(void *arg)
{
	element input[2];

	fprintf(stderr, "Process A starting\n");
	while (1) {
		fifo_get_multiple(fifo_in_a, 2, input);
		fifo_put_element(fifo_a_b, 2*input[0]);
		fifo_put_element(fifo_a_c, 3*input[1]);
	}
	pthread_exit(NULL);
}

static void *process_b(void *arg)
{
	element input;

	fprintf(stderr, "Process B starting\n");
	while (1) {
		fifo_get_element(fifo_a_b, &input);
		fifo_put_element(fifo_b_out1, input-2);
		fifo_put_element(fifo_b_d, input-1);
	}
	pthread_exit(NULL);
}

static void *process_c(void *arg)
{
	element input;

	fprintf(stderr, "Process C starting\n");
	while (1) {
		fifo_get_element(fifo_a_c, &input);
		fifo_put_element(fifo_c_d, input+1);
	}
	pthread_exit(NULL);
}

static void *process_d(void *arg)
{
	element input_b;
	element input_c;

	fprintf(stderr, "Process D starting\n");
	while (1) {
		fifo_get_element(fifo_b_d, &input_b);
		fifo_get_element(fifo_c_d, &input_c);
		fifo_put_element(fifo_d_out2, input_b*input_c);
	}
	pthread_exit(NULL);
}

static void *process_output(void *arg)
{
	element out1;
	element out2;

	fprintf(stderr, "Process Output starting\n");
	while (1) {
		fifo_get_element(fifo_b_out1, &out1);
		fifo_get_element(fifo_d_out2, &out2);
		printf("out1: " E_F ", out2: " E_F "\n", out1, out2);
	}
	pthread_exit(NULL);
}

struct {
	pthread_t thread;
	void *(*func)(void *);
} processes[] = {
	[Input] = {
		.func = &process_input,
	},
	[ProcessA] = {
		.func = &process_a,
	},
	[ProcessB] = {
		.func = &process_b,
	},
	[ProcessC] = {
		.func = &process_c,
	},
	[ProcessD] = {
		.func = &process_d,
	},
	[Output] = {
		.func = &process_output,
	},
	[End] = {},
};

int main(int argc, char **argv)
{
	int ret;

	fifo_in_a = fifo_new(2);
	fifo_a_b = fifo_new(1);
	fifo_a_c = fifo_new(1);
	fifo_b_d = fifo_new(1);
	fifo_c_d = fifo_new(1);
	fifo_b_out1 = fifo_new(1);
	fifo_d_out2 = fifo_new(1);

	for (size_t i = Input; i < End; ++i) {
		ret = pthread_create(&processes[i].thread, NULL, processes[i].func, NULL);
	}

	ret = pthread_join(processes[Input].thread, NULL);

	for (size_t i = ProcessA; i < End; ++i) {
		ret = pthread_cancel(processes[i].thread);
	}

	return 0;
}

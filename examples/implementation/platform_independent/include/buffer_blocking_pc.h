#ifndef BUFFER_BLOCKING_PC_H
#define BUFFER_BLOCKING_PC_H

#define _REENTRANT

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

typedef struct {
	pthread_mutex_t lock;
	pthread_cond_t notempty;
	pthread_cond_t notfull;
	size_t size;
	size_t used;
	size_t head;
	size_t tail;
	token tokens[];
} buffer_blocking;

static void buffer_blocking_init(buffer_blocking *buf, size_t size)
{
	pthread_mutex_init(&buf->lock, NULL);
	pthread_cond_init(&buf->notempty, NULL);
	pthread_cond_init(&buf->notfull, NULL);

	buf->size = size;
	buf->used = 0;
	buf->head = 0;
	buf->tail = 0;
}

static buffer_blocking *buffer_blocking_new(size_t size)
{
	buffer_blocking *buf = malloc(sizeof(buffer_blocking*) + size * sizeof(token));

	buffer_blocking_init(buf, size);

	return buf;
}

static void buffer_blocking_free(buffer_blocking *buf)
{
	free(buf);
}

static void write_token_blocking(buffer_blocking *buf, token data)
{
	pthread_mutex_lock(&buf->lock);

	while (buf->used >= buf->size) {
		// fprintf(stderr, "Waiting for space...\n");
		pthread_cond_wait(&buf->notfull, &buf->lock);
	}

	buf->tokens[buf->head] = data;
	buf->head = (buf->head + 1) % buf->size;
	++buf->used;

	pthread_mutex_unlock(&buf->lock);
	pthread_cond_broadcast(&buf->notempty);
}

static void read_token_blocking(buffer_blocking *buf, token *data)
{
	pthread_mutex_lock(&buf->lock);

	while (buf->used == 0) {
		pthread_cond_wait(&buf->notempty, &buf->lock);
	}

	*data = buf->tokens[buf->tail];
	buf->tail = (buf->tail + 1) % buf->size;
	--buf->used;

	pthread_mutex_unlock(&buf->lock);
	pthread_cond_broadcast(&buf->notfull);
}

#endif

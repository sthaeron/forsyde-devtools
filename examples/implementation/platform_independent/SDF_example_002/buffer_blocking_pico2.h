#ifndef BUFFER_BLOCKING_PICO2_H
#define BUFFER_BLOCKING_PICO2_H

#include <pico/util/queue.h>
#include "bsp.h"

// Wrap the pico2 implementation
typedef queue_t buffer_blocking;

static buffer_blocking *buffer_blocking_new(size_t size)
{

	buffer_blocking *buf = malloc(sizeof(buffer_blocking*));

    queue_init(buf, sizeof(token), size);

    return buf;
}

static void buffer_blocking_free(buffer_blocking *buf)
{
	free(buf);
}

static void write_token_blocking(buffer_blocking *buf, token data)
{
    queue_add_blocking(buf, &data);
}

static void read_token_blocking(buffer_blocking *buf, token *data)
{
    queue_remove_blocking(buf, data);
}

#endif

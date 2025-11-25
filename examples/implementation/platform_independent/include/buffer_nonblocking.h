#ifndef BUFFER_NONBLOCKING_H
#define BUFFER_NONBLOCKING_H

#include <stdlib.h>
#include <stdint.h>


/** @brief Minimal non-blocking buffer, does not check for empty/full.
 *  @param size - How many elements the buffer fits
 *  @param used - How namy elements are in use
 *  @param head - Index for current position to write to (write before
 *                increment)
 *  @param tail - Index for current position to read from (read before
 *                increment)
 *  @param tokens - Array of elements
 */
typedef struct {
	size_t size;
	size_t used;
	size_t head;
	size_t tail;
	token *tokens;
} buffer_nonblocking;

static void buffer_nonblocking_init(buffer_nonblocking *buffer,
	size_t size)
{
	buffer->size = size;
	buffer->used = 0;
	buffer->head = 0;
	buffer->tail = 0;
}

static buffer_nonblocking *buffer_nonblocking_new(size_t size)
{
	// Malloc for the struct
	buffer_nonblocking * buf =
		malloc(sizeof(buffer_nonblocking));

	// Separate malloc for the array
	buf->tokens = (token *)
		malloc(size*sizeof(token));

	buffer_nonblocking_init(buf, size);

	return buf;
}

static void buffer_nonblocking_free(buffer_nonblocking *buf)
{
	free(buf->tokens);
	free(buf);
}

static void write_token(buffer_nonblocking *buf, token data)
{
	buf->tokens[buf->head] = data;
	buf->head = (buf->head + 1) %
		buf->size;
	++buf->used;
}

static void read_token(buffer_nonblocking *buf, token *data)
{
	*data = buf->tokens[buf->tail];
	buf->tail = (buf->tail + 1) %
		buf->size;
	--buf->used;
}

#endif

#ifndef COMMON_H
#define COMMON_H

#include "buffer_nonblocking.h"
#include "actor_templates.h"

/** @brief Specification for functionality that each platform needs supported:
 * @param token - This is the data type that is passed around in the signal
 * @param read_token_blocking - Function for reading a token with a blocking
 *                              fifo
 * @param write_token_blocking - Function for writing a token to a blocking fifo
*/

/** @brief Selected target platform is a Linux PC */
#define PC 1

/** @brief Selected target platform is a Pico 2 board */
#define PICO2 2

/**  @brief Platform modes, use this parameter to select platform.
 *   Available platforms so far:
 *  @param PC - Linux PC (pthreads)
 *  @param PICO2 - PICO 2 board
 */
#ifndef PLATFORM
#define PLATFORM PC
#endif

#if PLATFORM == PC
    #include "buffer_blocking_pc.h"
    static void init(void) {}
#endif

#if PLATFORM == PICO2
    #include "buffer_blocking_pico2.h"
    #include <bsp.h>
    static void init(void) {
        BSP_Init();
    }
#endif

#endif

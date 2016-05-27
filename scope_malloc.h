/******************************************************************************
Copyright (c) 2014, Ilja Hoffmann
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
******************************************************************************/

#ifndef __SCOPED_MALLOC_H__
#define __SCOPED_MALLOC_H__

// 'Scoped Malloc - stack based': associate memory allocations by a key ('scope') to be
// collectively purged after use. Use a stack of scopes to represent hierarchical
// dependencies
//
// => 'scope' is any non-zero value
// => all alloctions are raising assertations on 'out of memory'

// pushes new scope for use with tos_malloc
void new_scope(void);
// pops (except for GROUNDFLOOR) and frees current top-of-stack scope
void end_scope(void);

// Detach the topmost scope created by new_scope from the global stack.
// While detached, no further allocations are added to the scope; only the TOS-scope will receive
// new requests.
// Store and keep as you wish - later call free_detached_scope with the detached scope for cleanup.
// btw: dont mess with the returned data or risk to be doomed
// GROUNDFLOOR can not be detached; trying to do so yields an assertation
void *detach_scope(void);
void push_detached_scope(void *scope);	// make previously detached scope TOS again
void free_detached_scope(void *scope);	// 'scope' has to be a result of a previous detach_scope

// these functions use the current top-of-stack as scope
// => both may be called without a prior 'new_scope', in which case their scope is the 'GROUNDFLOOR'
void *tos_malloc(size_t size);
void *tos_calloc(size_t nmemb, size_t size);
void *tos_strndup(const char *src, size_t max);
void *tos_memdup(void *src, size_t len);
void *tos_ftdup(char *from, char *to);

// This function takes a normally allocated memory-pointer and adds it to current tos for cleanup.
// Warning: using this function violents the incapsulation of scope_malloc system, so be warned:
// *never add a pointer twice*!
void tos_owned_mem(void *ptr);

// ----------------------------------------------------------------
// Pool-based scoping
// ----------------------------------------------------------------

void *new_mem_pool(void);
void free_mem_pool(void *mem_pool);
void *pool_malloc(size_t size, void *mem_pool);
void *pool_calloc(size_t nmemb, size_t size, void *mem_pool);
void *pool_strndup(const char *src, size_t max, void *mem_pool);
void *pool_memdup(void *src, size_t len, void *mem_pool);
void *pool_ftdup(char *from, char *to, void *mem_pool);
void pool_owned_mem(void *ptr, void *mem_pool);


#endif // __SCOPED_MALLOC_H__

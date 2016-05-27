// vim: ts=4 sw=4 ai sm
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

#include <malloc.h>
#include <string.h>
#include <assert.h>

#include "cparsing_list.h"
#include "scope_malloc.h"

#define STRNLEN_MISSING

static const char *OOM = "out of memory";

// ==================================================================================================================
// global scope stack
// ==================================================================================================================

struct _mem_entry
{
	LIST_OF(struct _mem_entry);
	void *address;
	// no bookkeeping for now
};
typedef struct _mem_entry _mem_entry;
typedef _mem_entry _mem_pool;

struct ScopeStack
{
	LIST_OF(struct ScopeStack);
	_mem_entry allocations;
};
typedef struct ScopeStack ScopeStack;

static ScopeStack s_global_scope_stack = {
	&s_global_scope_stack, &s_global_scope_stack,
	{ &s_global_scope_stack.allocations, &s_global_scope_stack.allocations, 0 }
	// "GROUNDFLOOR"
};

void push_detached_scope(void *scope)
{
	assert("trying to push a NULL scope" && scope);
	list_append_end((list *) &s_global_scope_stack, (list *) scope);
}

void new_scope(void)
{
ScopeStack *new_scope = (ScopeStack *) malloc(sizeof(ScopeStack));

	assert(OOM && new_scope);

	list_init((list *) &new_scope->allocations);
	list_append_end((list *) &s_global_scope_stack, (list *) new_scope);
}

static void _mem_queue_dtor(list *alloc)
{
_mem_entry *deletee = (_mem_entry *) alloc;

	assert("list element is NULL in dtor" && deletee);
	assert("stack scope structure corrupt" && deletee->address);

	free(deletee->address);
	free(deletee);
}

void end_scope(void)
{
ScopeStack *tos = s_global_scope_stack.prev;
int on_groundfloor;

	on_groundfloor = list_empty((list *) &s_global_scope_stack);

	if(!on_groundfloor)
		list_remove((list *) tos);

	list_clear((list *) &tos->allocations, _mem_queue_dtor);

	if(!on_groundfloor)
		free(tos);
}

void *detach_scope(void)
{
ScopeStack *tos = s_global_scope_stack.prev;

	assert("trying to detach a scope from empty stack" && list_not_empty((list *) &s_global_scope_stack));
	return list_remove((list *) tos);
}

void free_detached_scope(void *scope)
{
ScopeStack *s = (ScopeStack *) scope;

	assert("free_detached_scope called with NULL" && s);

	list_clear((list *) &s->allocations, _mem_queue_dtor);
	free(s);
}

static void _init__mem_entry(void *address)
{
_mem_entry *entry = (_mem_entry *) malloc(sizeof(_mem_entry));
ScopeStack *tos = s_global_scope_stack.prev;

	assert(OOM && entry);

	entry->address = address;
	list_append_end((list *) &tos->allocations, (list *) entry);
}

void *tos_malloc(size_t size)
{
void *result = malloc(size);

	assert(OOM && result);
	_init__mem_entry(result);
	return result;
}

void *tos_calloc(size_t nmemb, size_t size)
{
void *result = calloc(nmemb, size);

	assert(OOM && result);
	_init__mem_entry(result);
	return result;
}

void *tos_strndup(const char *src, size_t max)
{
char *result;
size_t len;

	assert(src && max);

#ifdef STRNLEN_MISSING
    len = strlen(src);
    assert(len <= max);
#else
	len = strnlen(src, max);
#endif // STRNLEN_MISSING

	result = (char *) tos_malloc(len+2);
	strncpy(result, src, len);
	result[len] = 0;
	return result;
}

void *tos_memdup(void *src, size_t len)
{
void *result = tos_malloc(len);

	memcpy(result, src, len);
	return result;
}

void *tos_ftdup(char *from, char *to)
{
int len;
void *result;

	assert(from && to && from <= to);
	
	len = to - from + 1;
	result = tos_malloc(len + 1);	// +1, because we'll add a terminating '0'

	if(len > 1)
		memcpy(result, from, len);

	((char *) result)[len] = 0;

	return result;
}

// add some otherwise allocated block to tos scope
void tos_owned_mem(void *ptr)
{
	assert(ptr);
	_init__mem_entry(ptr);
}

// ------------------------------------------------------------------

void *new_mem_pool(void)
{
_mem_pool *newp = (_mem_pool *) malloc(sizeof(_mem_pool));

	assert(OOM && newp);

	list_init((list *) newp);
	return newp;
}

void free_mem_pool(void *mem_pool)
{
_mem_pool *pool = (_mem_pool *) mem_pool;

	assert(pool);

	list_clear((list *) pool, _mem_queue_dtor);
	free(pool);
}

static void _init_poolmem_entry(void *address, _mem_pool *mem_pool)
{
_mem_entry *entry = (_mem_entry *) malloc(sizeof(_mem_entry));

	assert(mem_pool);
	assert(OOM && entry);

	entry->address = address;
	list_append_end((list *) mem_pool, (list *) entry);
}

void *pool_malloc(size_t size, void *mem_pool)
{
void *result = malloc(size);

	assert(OOM && result);
	_init_poolmem_entry(result, mem_pool);
	return result;
}

void *pool_calloc(size_t nmemb, size_t size, void *mem_pool)
{
void *result = calloc(nmemb, size);

	assert(OOM && result);
	_init_poolmem_entry(result, mem_pool);
	return result;
}

void *pool_strndup(const char *src, size_t max, void *mem_pool)
{
char *result;
size_t len;

	assert(src && max);

#ifdef STRNLEN_MISSING
    len = strlen(src);
    assert(len <= max);
#else
	len = strnlen(src, max);
#endif // STRNLEN_MISSING

	result = (char *) pool_malloc(len+2, mem_pool);
	strncpy(result, src, len);
	result[len] = 0;
	return result;
}

void *pool_memdup(void *src, size_t len, void *mem_pool)
{
void *result = pool_malloc(len, mem_pool);

	memcpy(result, src, len);
	return result;
}

void *pool_ftdup(char *from, char *to, void *mem_pool)
{
int len;
void *result;

	assert(from && to && from <= to);
	assert(mem_pool);
	
	len = to - from + 1;
	result = pool_malloc(len + 1, mem_pool);	// +1, because we'll add a terminating '0'

	if(len > 0)
		memcpy(result, from, len);

	((char *) result)[len] = 0;

	return result;
}

// add some otherwise allocated block to tos scope
void pool_owned_mem(void *ptr, void *mem_pool)
{
	assert(ptr);
	_init_poolmem_entry(ptr, mem_pool);
}

#if 0
// ==================================================================================================================
// Heritage Scopes
// ==================================================================================================================

struct NNode
{
	NAMED_LIST_OF(struct NNode, siblings);
	NAMED_LIST_OF(struct NNode, children);
};
typedef struct NNode NNode;

NNode *init_NNode(NNode *parent, void *storage)
{
NNode *self = (NNode *) storage;	// malloced size >= sizeof(NNode)

	assert(self);

	self->siblings.next = self->siblings.prev = self;	// ring of siblings
	self->children.next = self->children.prev = self;	// ring of children

	if(parent)	// empty(siblings) => root node
	{
		// attach to parent's children-list, but using our sibling-pointers

		if(parent->children.prev != parent)	// there already are children
		{
			parent->children.prev->siblings.next = self;
		} else {	// first child
			parent->children.next = self;
		}

		self->siblings.prev = parent->children.prev;	// take old prev

		parent->children.prev = self;					// we're new last entry
		self->siblings.next = parent;					// -"-
	}

	return self;
}

void destroy_NNode(NNode *deletee)
{

}




void MemScope_dtor(list *deletee)
{
MemScope *self = (MemScope *) deletee;
MemScope *child;

	assert(self);

	for(child = self->children.next; child != self; child = child->siblings.next)

	list_clear((list *) &self->children, MemScope_dtor);


	list_clear((list *) &self->allocations, _mem_queue_dtor);
	
}

struct MemScope
{
	NAMED_LIST_OF(struct MemScope, siblings);
	NAMED_LIST_OF(struct MemScope, children);
	_mem_pool	allocations;
};
typedef struct MemScope MemScope;


void free_scope(MemScope *scope)
{


}

static void _init_scope_mem_entry(MemScope *scope, void *address)
{
_mem_entry *entry = (_mem_entry *) malloc(sizeof(_mem_entry));

	assert(scope);
	assert(OOM && entry);

	entry->address = address;
	list_append_end((list *) &scope->allocations, (list *) entry);
}

void *scope_malloc(MemScope *scope, size_t size)
{
void *result = malloc(size);

	assert(scope);
	assert(OOM && result);
	_init_scope_mem_entry(scope, result);
	return result;
}

#endif

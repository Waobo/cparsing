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

#include <assert.h>
#include <malloc.h>
#include "cparsing_list.h"

void print( const char* format, ... );

int list_empty(list *l)
{
    assert(l);
    assert(l->prev);        // fail on invalidated elements

    return l == l->prev;    // "im seeing the back of my head" .. 'l->next == l' is equivalent
}

int list_not_empty(list *l)
{
    assert(l);
    assert(l->prev);        // fail on invalidated elements

    return l != l->prev;    // negated list_empty()
}

list *list_insert(list *elem, list *prev, list *next)
{
    assert(elem);
    assert(prev);
    assert(next);
    assert(prev->next);
    assert(next->prev);

    // link next
    next->prev = elem;
    elem->next = next;

    // link prev
    prev->next = elem;
    elem->prev = prev;

    return elem;
}

list *list_remove(list *elem)
{
    assert(elem);
    assert(elem->next);
    assert(elem->prev);

    // remove from old ring - does nothing when list is empty ..
    elem->prev->next = elem->next;
    elem->next->prev = elem->prev;

    // .. so we explicity invalidate the removed element
    elem->next = elem->prev = 0L;

    return elem;
}

list *list_insert_begin(list *l, list *elem)
{
    assert(l);
    return list_insert(elem, l, l->next);
}

list *list_append_end(list *l, list *elem)
{
    assert(l);
    return list_insert(elem, l->prev, l);
}

int list_is_first(list *l, list *elem)
{
    assert(elem);
    return list_not_empty(l) && (elem->prev == l);
}

int list_is_last(list *l, list *elem)
{
    assert(elem);
    return list_not_empty(l) && (elem->next == l);
}

int list_len_is_one(list *l)
{
    assert(l);
    assert(l->prev);

    return (l != l->prev) && (l->next == l->prev);
}

void list_clear(list *l, list_dtor dtor)
{
list *deletee;

    while(list_not_empty(l))
    {
        deletee = l->next;
        assert(deletee);

		// clang SCA reports 'use after free' here, but in fact fails to recognize l->next as changing
        list_remove(deletee);

        if(dtor)
            dtor(deletee);
        else
            free(deletee);
    }
}

#if 0
list *list_new(void)
{
list *result = (list *) malloc(sizeof(list));

    assert(result);
    list_init(result);
    return result;
}
#endif

void list_free(list *deletee, list_dtor dtor)
{
    list_clear(deletee, dtor);

    if(dtor)
        dtor(deletee);
    else
        free(deletee);
}

// ===============================================


stack *make_stack_elem(void *data)
{
stack *result = (stack *) malloc(sizeof(stack));

    assert(result);
    list_init(result);
    result->data = data;
    return result;
}

void *stack_push(stack *s, void *elem)
{
    list_append_end((list *) s, (list *) make_stack_elem(elem));
    // print("push\n");
    return elem;
}

void *stack_pop(stack *s)
{
stack *elem;
void *result;

    assert(list_not_empty((list *) s));
    elem = s->prev;
    result = elem->data;
    list_remove((list *) elem);
    free(elem);
    // print("pop\n");
    return result;
}

void *stack_tos(stack *s)
{
    assert(list_not_empty((list *) s));
    return s->prev->data;
}

list *list_insert_sorted(list *l, list *elem, element_comparator cmp)
{
list *current;

	assert(l);
	assert(elem);
	assert(cmp);
	assert(l->next);
	assert(l->prev);

	if(list_empty(l))
		return list_insert_begin(l, elem);

	for(current = l->next; current != l; current = current->next)
	{
	int diff = cmp(elem, current);

		// keep entries unique
		if(diff == 0)
			return elem;

		// insert before current entry if elem is smaller
		if(diff < 0)
			return list_insert(elem, current->prev, current);
	}

	// elem is bigger than any other entry
	return list_append_end(l, elem);
}

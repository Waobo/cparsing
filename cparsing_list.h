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

#ifndef __CPARSING_LIST_H__
#define __CPARSING_LIST_H__

// A list is a list of a list of lists ...
// This one rings next and prev of an empty list header onto itself (curtsey to linux kernel lists).
// good: super simple - not so good: len(list) is O(n)

// For inclusion in type-definition -- assumes all pointers are of same size, thus:
// address_of(list) == address_of(list.next)

// default names
#define LIST_OF(type) type *next, *prev
#define NAMED_LIST_OF(name,type) struct { type *next, *prev; } name

//// when using multiple lists in one scope the links must be  uniquely named

struct list
{
    LIST_OF(struct list);
};
typedef struct list list;

typedef void (*list_dtor)(list *deletee);

struct stack
{
    LIST_OF(struct stack);
    void *data;
};
typedef struct stack stack;

#define list_init(LIST) {assert(LIST); ((list *) LIST)->next = ((list *) LIST)->prev = (list *) (LIST);}

// a few operations on lists
int     list_empty(list *l);
int     list_not_empty(list *l);
int     list_len_is_one(list *l);
list *  list_insert(list *elem, list *prev, list *next);
list *  list_remove(list *elem);
list *  list_insert_begin(list *l, list *elem);
list *  list_append_end(list *l, list *elem);
int     list_is_first(list *l, list *elem);
int     list_is_last(list *l, list *elem);
void    list_clear(list *l, list_dtor dtor);
void    list_free(list *deletee, list_dtor dtor);

// sorted and unique
typedef int (*element_comparator)(list *a, list *b);
list *	list_insert_sorted(list *l, list *elem, element_comparator cmp);

// while on it...
void *  stack_push(stack *l, void *elem);
void *  stack_pop(stack *l);
void *  stack_tos(stack *l);

#define list_for_each(TYPE, ELEM, LIST) for(ELEM = (TYPE *) (LIST)->next; ((void *) ELEM) != ((void *) LIST); ELEM = (TYPE *) ELEM->next)
#define list_for_each_by(TYPE, ELEM, LIST, NAME) for(ELEM = (TYPE *) (LIST)->next; ((void *) ELEM) != ((void *) LIST); ELEM = (TYPE *) ELEM->NAME.next)


#endif // __CPARSING_LIST_H__

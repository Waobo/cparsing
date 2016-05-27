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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <malloc.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>

#include "cparsing_dict.h"

/*
Description ListDict's algorithm:

(not the most efficient one -it's O(N = #entries)-, but quickly implemented
and allows for easy adding of values - which is why i used it..)

All 'added' regions are kept in a lexicalical sorted list.

On 'init_lookup', two pointers are initialized to begin ('upper')
and end ('lower') of this list. The current lookup-index into
the stored regions is set to zero.

Invariant:
     order(*upper) <= order(*lower)

After each loop:
	 all regions inbetween the upper and lower mark confirm
     to the pattern presented so far - or the lookup is a fail

The pattern which is to be tested for its existence in the dict
is presented one char at a time (by calling 'extend_pattern'), starting
with the leftmost char.

Each time the next char of the tested pattern is presented, the
range given by upper and lower is ajusted to confirm to the promise
by moving upper downwards and lower upwards, until their region
holds the current char at the position 'lookup-index' (thus shrinking
the intervall).

If the invariant is broken (because upper crossed lower or vice versa)
the lookup is a fail, else a success. Loop reinit is to increment
lookup-index by one to the next char.

To verify that a confirmed match is an exact match, we test
wether upper[lookup-index] is the end of the region in
'exact_match' (exact match is shortest possible match, aka the
one with the lowest order).

USAGE:
-   create new
    Dict *d = new_list_dict();

-   (optional) init some keys with optional values
    char *keys[] = {"A", "B", "C", 0L);     // don't forget to terminate the key-array with 0L
    void *values[] = {"a", "b", c");        // no termination needed, but you may
    dict_add_array(d, keys, values);        // if it's keys-only: use dict_add_array(d, keys, 0L);

-   lookup
    d->init_lookup(d);

    now repeatedly
    int still_matched = d->extend_pattern(d, <next single char>);

    until still_matched goes false or
    d->exact_match(d) == 1

-   get a found value -most likely after an exact match- by calling
    void *user_data = d->get_value(d);

-   when not in lookup, you may add new key/values
    d->add(d, <key region>, <optional value or 0L>);

*/

struct region_element
{
	LIST_OF(struct region_element);
	Region region;
	void *value;
};
typedef struct region_element region_element;

typedef struct
{
	Dict op;

	list head;
	region_element *upper, *lower;			// upper and lower bound for match; if upper == lower its the only possible match
	int current_offset;				// offset in pattern: region->from + current_offset < region->to <=> current_offset < region->to - region->from
} ListDict;

typedef struct
{
	int (*get_next)(DictIterator *di, Region *key, void **value);
	ListDict *dict;
	region_element *current;
} ListDictIterator;

void dump_ListDict(Dict *d)
{
region_element *curr;

	list_for_each(region_element, curr, &((ListDict *) d)->head)
	{
		print_ft(curr->region.begin, curr->region.end); printf(": %s\n", (char *) curr->value);
	}
}

static region_element *new_region_element(Region *data, void *value)
{
region_element *result = (region_element *) malloc(sizeof(region_element));

	assert(result);
	copy_region(&result->region, data);
	result->value = value;
	return result;
}

static int compare_region_element(list *a, list *b)
{
region_element *ae = (region_element *) a, *be = (region_element *) b;

	assert(ae);
	assert(be);

	return compare_region(&ae->region, &be->region);
}

static void list_dict_dtor(Dict *dict)
{
ListDict *d = (ListDict *) dict;

	assert(d);
	list_clear(&d->head, 0L);
	free(dict);
}

static void list_dict_add(Dict *dict, Region *r, void *value)
{
ListDict *d = (ListDict *) dict;
region_element *elem = new_region_element(r, value);

	assert(d);
	list_insert_sorted(&d->head, (list *) elem, compare_region_element);
}

static void list_dict_init_lookup(Dict *dict)
{
ListDict *d = (ListDict *) dict;

	assert(d);
	d->upper	= (region_element *) d->head.next;
	d->lower	= (region_element *) d->head.prev;
	d->current_offset = 0;
}

static int list_dict_extend_pattern(Dict *dict, char c)
{
int result = 1;
ListDict *d = (ListDict *) dict;

    assert(d);
	if(list_empty(&d->head)) return 0;
	assert(d->upper);
	assert(d->lower);

	// adjust upper bound
	while(d->upper != d->lower)
	{
	region_element *u = (region_element *) d->upper;
	int len;
	int len_ok;
	int current_match = 0;
	char current_u = 0;


		assert(u);

		len = (u->region.end - u->region.begin + 1);
		len_ok = (d->current_offset < len);

		if(len_ok)
		{
			current_u = u->region.begin[d->current_offset];
			current_match = (current_u == c);
		}

		if( len_ok && current_match )
			break;
		else
			d->upper = d->upper->next;
	}

	if(d->upper == d->lower)
	{
	region_element *u = (region_element *) d->upper;

		assert(u);
		result = ((u->region.end - u->region.begin) >= d->current_offset) && (u->region.begin[d->current_offset] == c);
		goto list_dict_extend_pattern_out;
	}

	// adjust lower bound
	while(d->lower != d->upper)
	{
	region_element *l = (region_element *) d->lower;

		assert(l);

		if( ((l->region.end - l->region.begin) >= d->current_offset) && (l->region.begin[d->current_offset] == c) )
			break;
		else
			d->lower = d->lower->prev;
	}

	if(d->lower == d->upper)
	{
	region_element *l = (region_element *) d->lower;

		assert(l);
		result = ((l->region.end - l->region.begin) >= d->current_offset) && (l->region.begin[d->current_offset] == c);
	}

list_dict_extend_pattern_out:
	// even multiple matches so far, prepare for next char
	++d->current_offset;
	return result;
}

static int list_dict_exact_match(Dict *dict)
{
ListDict *d = (ListDict *) dict;
    assert(d);
	assert(d->upper);
	return (d->current_offset > 0) && ((d->upper->region.end - d->upper->region.begin) == d->current_offset-1);
}


static void *list_dict_get_value(Dict *dict)
{
ListDict *d = (ListDict *) dict;

    assert(d);
	assert(d->upper);
    return d->upper->value;
}

int list_dict_get_next(DictIterator *di, Region *key, void **value)
{
ListDictIterator *i = (ListDictIterator *) di;

	assert(i);

	if(i->current  && ((list *) i->current->next ==  &i->dict->head))
		return 0;

	if(i->current)
	{
		i->current = i->current->next;
	} else {
		i->current = (region_element *) i->dict->head.next;
	}

	if(key)
		*key = i->current->region;

	if(value)
		*value = i->current->value;

	return 1;
}


DictIterator list_dict_get_iterator(Dict *d)
{
ListDictIterator di = { list_dict_get_next, (ListDict *) d, 0L };
DictIterator result;

	assert(d);
	assert(sizeof(DictIterator) >= sizeof(ListDictIterator));
	memcpy(&result, &di, sizeof(ListDictIterator));
	return result;
}

void dict_add_array(Dict *d, char *input[], void *values[])
{
int i = 0;

    assert(d);
    assert(input);

	while(input[i])
	{
	Region r;

		init_region(&r, input[i], 0);
		d->add(d, &r, values ? values[i] : 0L);
		++i;
	}
}

int dict_lookup(Dict *d, char *key, void **value)
{
char *act = key;
	
	assert(d);
	assert(act);

	d->init_lookup(d);

	while(*act && d->extend_pattern(d, *act))
	{
		if(d->exact_match(d))
		{
			if(value) *value = d->get_value(d);
			return 1;
		}
		act++;
	}
	return 0;
}

Dict *new_list_dict(void)
{
ListDict *result = (ListDict *) malloc(sizeof(ListDict));

	assert(result);

	result->op.destroy			= list_dict_dtor;	// default dtor
	result->op.add				= list_dict_add;
	result->op.init_lookup		= list_dict_init_lookup;
	result->op.extend_pattern	= list_dict_extend_pattern;
	result->op.exact_match		= list_dict_exact_match;
	result->op.get_value		= list_dict_get_value;
	result->op.iterator			= list_dict_get_iterator;

	list_init((&result->head));
	result->upper			    = 0L;
	result->lower		    	= 0L;
	result->current_offset	    = 0;

	return (Dict *) result;
}



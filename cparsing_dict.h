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

#ifndef __CPARSING_DICT_H__
#define __CPARSING_DICT_H__

#include "cparsing_list.h"
#include "cparsing_region.h"

/*
A Dict allows to test, wether a given text is an element
of itself.

It is intended to be use within a Parser component and
thus works by char-wise extending the presented text, asking
if the new pattern is still part of the Dict.

Finally and in case the match succeed, we can ask wether this
was a exact match (aka the given text is not just a substring of
the Dict's matching member(s)).
*/

struct Dict;
typedef struct Dict Dict;

#define MAX_DICT_ITER_PAYLOAD void *dummy[2]

struct DictIterator
{
	int (*get_next)(struct DictIterator *di, Region *key, void **value);		// 1st call will fill in 1st entry's key and value, if present - returns 1 while entries available, 0 else
	MAX_DICT_ITER_PAYLOAD;
};
typedef struct DictIterator DictIterator;

typedef void (*dict_dtor)(Dict *deletee);
typedef void (*dict_adder)(Dict *dict, Region *r, void *value);
typedef void (*dict_lookup_init)(Dict *dict);
typedef int  (*dict_pattern_extender)(Dict *dict, char c);
typedef int  (*dict_exact_match_checker)(Dict *dict);
typedef void *(*dict_value_getter)(Dict *dict);
typedef DictIterator (*dict_iterator_getter)(Dict *dict);

struct Dict
{
	dict_dtor						destroy;			// how to delete this Dict
	dict_adder						add;				// add a member region to the dict
	dict_lookup_init				init_lookup;		// begin/reset a new lookup
	dict_pattern_extender			extend_pattern;		// extend the text by a char, asking if is still a member of Dict
														// WARNING: to not call extend_pattern without resetting the dict once it had failed
	dict_exact_match_checker		exact_match;		// marks the test complete, asking if the matched pattern is completed too
														// The result is meaningless after extend_pattern failed
    dict_value_getter               get_value;          // get associated value for current (upper) key
	dict_iterator_getter			iterator;
};

// A Dict using lists
Dict *new_list_dict(void);

// init Dict from array of strings
void dict_add_array(Dict *d, char *input[], void *values[]);    // if 'values' is given, it has to be of the same size as 'input'

// dict-lookup - return 1 on match and sets *value to associated data if(value != 0)
// returns 0 on failure
int dict_lookup(Dict *d, char *key, void **value);

#endif // __CPARSING_DICT_H__

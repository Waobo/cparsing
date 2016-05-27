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
#include <stdio.h>
#include <stdarg.h>
#include <malloc.h>
#include <ctype.h>
#include <string.h>

#include "cparsing.h"
#define FROM_LOL_BODY
#include "lol.h"
#undef FROM_LOL_BODY

void print( const char* format, ... );

ListOList *LOL_init(ListOList *result, int type, void *data)
{
    assert(result);
    assert(type == DATA_ELEMENT || type == CONTAINER_ELEMENT);

    list_init(((list *) &(result->chain)));

    if( (result->type = type) == CONTAINER_ELEMENT )
    {
        list_init(((list *) &(result->inv.children)));
    } else {
        result->inv.data = data;
    }

    return result;
}

ListOList *list_of_list(int type, void *data)
{
ListOList *result;

    result = (ListOList *) calloc(1, sizeof(ListOList));
    return LOL_init(result, type, data);
}

ListOList *LOL_container(void)
{
    return list_of_list(CONTAINER_ELEMENT, 0L);
}

ListOList *LOL_data(void *data)
{
    return list_of_list(DATA_ELEMENT, data);
}

void list_of_list_dtor(list *deletee)
{
ListOList *l = (ListOList *) deletee;
    assert(l);
    assert(l->type == CONTAINER_ELEMENT || l->type == DATA_ELEMENT);
    if(l->type == CONTAINER_ELEMENT)
        list_clear((list *) &l->inv.children, list_of_list_dtor);
    free(l);
}

static void _print_LOL(ListOList *l, int is_last)
{
    assert(l);
    assert(l->type == DATA_ELEMENT || l->type == CONTAINER_ELEMENT);

    if(l->type == DATA_ELEMENT)
    {
        print("'%s'%s", (char *) l->inv.data, is_last ? "" : " ");
    } else {
    ListOList *child;

        print("[");
        list_for_each_by(ListOList, child, &(l->inv.children), chain)
        {
            _print_LOL(child, list_is_last((list *) &l->inv.children, (list *) child));
        }
        print("]");
    }
}

void print_LOL(ListOList *l)
{
    _print_LOL(l, 1);
}

ListOList *LOL_append(ListOList *l, ListOList *d)
{
    return (ListOList *) list_append_end((list *) &l->inv.children, (list *) d);
}

ListOList *LOL_append_container(ListOList *l)
{
    return (ListOList *) list_append_end((list *) &l->inv.children, (list *) list_of_list(CONTAINER_ELEMENT, 0L));
}

ListOList *LOL_append_string(ListOList *l, char *data)
{
    return (ListOList *) list_append_end((list *) &l->inv.children, (list *) list_of_list(DATA_ELEMENT, data));
}

void LOL_clear(ListOList *l)
{
    assert(l);
    assert(l->type == DATA_ELEMENT || l->type == CONTAINER_ELEMENT);

    if(l->type == CONTAINER_ELEMENT)
        list_clear((list *) &l->inv.children, list_of_list_dtor);
}

void LOL_free(ListOList *l)
{
    LOL_clear(l);
//    list_remove((list *) l); not needed
    free(l);
}

void LOL_free_data(ListOList *l)
{
    assert(l);
    assert(l->type == DATA_ELEMENT || l->type == CONTAINER_ELEMENT);

    if(l->type == DATA_ELEMENT)
    {
        assert(l->inv.data);
        free(l->inv.data);
    } else {
    ListOList *child;

        list_for_each_by(ListOList, child, &(l->inv.children), chain)
        {
            LOL_free_data(child);
        }
    }
}

int LOL_number_of_elements(ListOList *l, int recursive)
{
int result = 0;

    assert(l);
    assert(l->type == DATA_ELEMENT || l->type == CONTAINER_ELEMENT);
    assert(recursive <= MAX_RECURSION_DEPTH);


    if(l->type == DATA_ELEMENT)
    {
        result = 1;
    }
    else if(recursive)
    {
    ListOList *child;
        list_for_each_by(ListOList, child, &(l->inv.children), chain)
        {
            result += LOL_number_of_elements(child, recursive+1);
        }
    }

    return result;
}

int LOL_of_string_number_of_chars(ListOList *l, int max_word_len, int recursive)
{
ListOList *child;
int result = 0;

    assert(l);
    assert(recursive <= MAX_RECURSION_DEPTH);
    assert(l->type == DATA_ELEMENT || l->type == CONTAINER_ELEMENT);

    if(l->type == DATA_ELEMENT)
    {
        // single value
#if defined(HAVE_STRNLEN)
        result = strnlen((char *) l->inv.data, max_word_len);
#else
        result = strlen((char *) l->inv.data);
#endif
    }
    else if(recursive)
    {
        // a list
        list_for_each_by(ListOList, child, &(l->inv.children), chain)
        {
            result += LOL_of_string_number_of_chars(child, max_word_len, recursive+1);
        }
    }

    return result;
}

static char *_LOL_of_string_join_to_buffer(char *buffer, char *separator, ListOList *l, int max_word_len, int recursive, int is_last)
{
ListOList *child;

    assert(l);
    assert(buffer);
    assert(separator);
    assert(recursive <= MAX_RECURSION_DEPTH);

    if(l->type == DATA_ELEMENT)
    {
        buffer += snprintf(buffer, max_word_len, "%s%s", (char *) (l->inv.data), (is_last ? "" : separator));
    }
    else if(recursive)
    {
        list_for_each_by(ListOList, child, &(l->inv.children), chain)
        {
            buffer = _LOL_of_string_join_to_buffer(buffer, separator, child, max_word_len, recursive+1, list_is_last((list *) &(l->inv.children), (list *) child));
        }
        if(!is_last)
            buffer += snprintf(buffer, max_word_len, "%s", separator);
    }

    return buffer;
}

char *LOL_of_string_join_to_buffer(char *buffer, char *separator, ListOList *l, int max_word_len, int recursive)
{
    return _LOL_of_string_join_to_buffer(buffer, separator, l, max_word_len, recursive, 1);
}

char *LOL_of_string_join(char *separator, ListOList *l, int max_word_len, int recursive)
{
char *result, *current;
ListOList *child;
int separator_len;
int token_count = 0;
int content_len = 0;
int buffer_len = 0;

    assert(l);
    assert(l->type == CONTAINER_ELEMENT);
    assert(separator);

#if defined(HAVE_STRNLEN)
    separator_len = strnlen(separator, max_word_len);
#else
    separator_len = strlen(separator);
#endif

    // we always want at least the first depth to be joined
    list_for_each_by(ListOList, child, &(l->inv.children), chain)
    {
        token_count += LOL_number_of_elements(child, recursive);
        content_len += LOL_of_string_number_of_chars(child, max_word_len, recursive);
    }

    if(!token_count)
        return 0;

    buffer_len = (token_count-1) * separator_len + content_len + 1;

   //  print("\ntokens: %d, content: %d, buffer: %d\n", token_count, content_len, buffer_len);

    result = current = (char *) malloc(buffer_len);
    assert(result);

    list_for_each_by(ListOList, child, &(l->inv.children), chain)
    {
        current = _LOL_of_string_join_to_buffer(current, separator, child, max_word_len, recursive, list_is_last((list *) &(l->inv.children), (list *) child));
    }
    return result;
}

void test_LOL(void)
{
ListOList root, *sub1, *sub2;
char *joined;

    LOL_init(&root, CONTAINER_ELEMENT, 0L);

    LOL_append_string(&root, "Hallo");

    sub1 = LOL_append_container(&root);
    LOL_append_string(sub1, "geehrte");
    LOL_append_string(sub1, "Du");

    sub2 = LOL_append_container(&root);
    LOL_append_string(sub2, "liebe");
    LOL_append_string(sub2, "weite");

    LOL_append_string(&root, "Wält");

    print_LOL(&root);
    print_LOL(&root);
    print_LOL(&root);

    joined = LOL_of_string_join(" ", (ListOList *) &root, 1024, 1);
    assert(joined);

    print("\njoined: '%s'\n", joined);

    LOL_clear(&root);
    free(joined);
}


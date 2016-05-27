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

#ifndef __LOL_H__
#define __LOL_H__

#define MAX_RECURSION_DEPTH 100

#ifdef FROM_LOL_BODY
const int DATA_ELEMENT      = 0;
const int CONTAINER_ELEMENT = 1;
#else
extern const int DATA_ELEMENT;
extern const int CONTAINER_ELEMENT;
#endif

struct ListOList
{
    struct
    {
        LIST_OF(struct ListOList);
    } chain;

    union
    {
        struct
        {
            LIST_OF(struct ListOList);
        } children;
        void *data;
    } inv;
    int type;
};
typedef struct ListOList ListOList;

ListOList *LOL_init(ListOList *result, int type, void *data);
ListOList *list_of_list(int type, void *data);
ListOList *LOL_container(void);
ListOList *LOL_data(void *data);
void list_of_list_dtor(list *deletee);
void print_LOL(ListOList *l);
ListOList *LOL_append(ListOList *l, ListOList *d);
ListOList *LOL_append_container(ListOList *l);
ListOList *LOL_append_string(ListOList *l, char *data);
void LOL_clear(ListOList *l);
void LOL_free(ListOList *l);
void LOL_free_data(ListOList *l);
int LOL_number_of_elements(ListOList *l, int recursive);
int LOL_of_string_number_of_chars(ListOList *l, int max_word_len, int recursive);
char *LOL_of_string_join_to_buffer(char *buffer, char *separator, ListOList *l, int max_word_len, int recursive);
char *LOL_of_string_join(char *separator, ListOList *l, int max_word_len, int recursive);
void test_LOL(void);

#endif // __LOL_H__

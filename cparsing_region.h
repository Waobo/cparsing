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

#ifndef __CPARSING_REGION_H__
#define __CPARSING_REGION_H__


#define POOL_MEM_SUPPORT


#ifdef POOL_MEM_SUPPORT
#include "scope_malloc.h"
#endif

// #define MAX_REGION_SIZE ... but how?

struct Region
{
    char        *begin, *end;           // begin == end => empty, end = begin+1 => 1 char; end points to 1st char _after_ region
    char        *current;               // begin and end are not modified .. keep current position separate (init current = begin)
};
typedef struct Region Region;

size_t			get_max_region_len(void);
void			set_max_region_len(size_t maxlen);

Region *        init_region(Region *r, char *from, char *to);
Region *        copy_region(Region *target, Region *source);
Region *        region(char *from, char *to);
#ifdef POOL_MEM_SUPPORT
Region *        pool_region(char *from, char *to, void *mem_pool);
#endif
Region *		region_from_file(char *filename);		// free_region needs dispose_contents set on this
Region *        clone_region(Region *src);
Region *		join_regions(Region *src1, Region *src2, Region *result);	// concatinates src1 and src2 into new buffer; result gets updated to new buffer; returns result
Region *		skip_spaces(Region *r);
void            free_region(Region *r, int dispose_contents);
void            print_region(Region *r);
int             consumed_some(Region *r);
int             consumed_all(Region *r);
int				compare_region(Region *a, Region *b);	// lexical order of 2 regions
void			print_ft(char *from, char *to);

void print( const char* format, ... );			// this is how we print - must be implemented by application

#define INIT_STR_REGION(X) {(X), (X)+strlen(X)-1, (X)}
#define INIT_REGION(F,T) {(F), (T), (F)}

#define ASSERT_VALID_REGION(R) assert((R) && ((R)->begin) && ((R)->end) && (((char *)((R)->begin)) <= ((char *)((R)->end)) + 1))

#define REGION_EMPTY(R) ((!(R)->begin) || (((char *)((R)->begin)) == ((char *)((R)->end)) + 1))

// get the length of a region given From and To ptrs
#define REGION_LEN(F,T) (((char *) T) - ((char *) F) + 1)

// get the To ptr given From and Length
#define REGION_END(F,L)	(((char *) F) + L - 1)

typedef struct
{
	int         row;	// both indices are 0-based
	int         col;
} TextLocation;

TextLocation get_text_location(Region *r, char *p);
char *line_start(Region *r, int row);
char *line_end(char *p, char *end);			// does not belong here, but brother line_start() does..

char *strcpy_ft(char *target, char *from, char *to);
char *strdup_ft(char *from, char *to);
int scanf_ft(char *from, char *to, char *fmt, ...);

#define region_as(T,R) (*((T *) (R).begin))
#define region_as_ptr(T,R) ((T *) (R).begin)

#endif // __CPARSING_REGION_H__

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

#define POOL_MEM_SUPPORT

#ifdef POOL_MEM_SUPPORT
#include "scope_malloc.h"
#endif

#include "cparsing_region.h"

#define STRNLEN_MISSING

static size_t s_max_region_len = 65535;

size_t get_max_region_len(void)
{
	return s_max_region_len;
}

void set_max_region_len(size_t maxlen)
{
	assert("zero is invalid maximum size for regions " && maxlen);
	s_max_region_len = maxlen;
}

Region *init_region(Region *r, char *from, char *to)
{
    assert(r);
    assert(from);
	assert(!to || from <= to+1);

    r->begin = r->current = from;

    if(to)
        r->end = to;
    else
    {
#ifdef STRNLEN_MISSING
		size_t len = strlen(from);
		assert(len <= s_max_region_len);
        r->end	= from + len - 1;
#else
        r->end	= from + strnlen(from, s_max_region_len) - 1;
#endif
    }

	assert(from == to+1 || ("region is larger than max_region_len - see set_max_region_len()" && (r->end - r->begin <= s_max_region_len)));

    return r;
}

Region *copy_region(Region *target, Region *source)
{
	assert(target);
	assert(source);
	assert(source->begin && source->end);
	assert(!source->current || (source->begin <= source->current && source->current <= source->end));
	assert("region is larger than max_region_len - see set_max_region_len()" && (source->end - source->begin <= s_max_region_len));

	target->begin = source->begin;
	target->current = source->current;
	target->end = source->end;

	return target;
}

Region *region(char *from, char *to)
{
Region *result = (Region *) malloc(sizeof(Region));

    assert(result);

    init_region(result, from, to);
    return result;
}

#ifdef POOL_MEM_SUPPORT
Region *pool_region(char *from, char *to, void *mem_pool)
{
Region *result = (Region *) pool_malloc(sizeof(Region), mem_pool);

    init_region(result, from, to);
    return result;
}
#endif

Region *clone_region(Region *src)
{
Region *result;

    assert(src);

    result = region(src->begin, src->end);
    result->current = src->current;

    return result;
}

Region *join_regions(Region *src1, Region *src2, Region *result)
{
int len1, len2;
char *joined;

	assert(src1);
	assert(src2);
	assert(result);

	len1 = REGION_LEN(src1->begin, src1->end);
	len2 = REGION_LEN(src2->begin, src2->end);

	assert("joined region is larger than max_region_len - see set_max_region_len()" && (len1+len2 <= s_max_region_len));

	joined = (char *) malloc(len1+len2);
	assert(joined);

	memcpy(joined + 0, src1->begin, len1);
	memcpy(joined + len1, src2->begin, len2);

	result->begin	= result->current = joined;
	result->end		= REGION_END(joined, len1+len2);

	return result;
}

static long _filesize(FILE *f)
{
long size = 0;

	if (fseek(f,0,SEEK_END)==0)
		size = ftell(f);

	fseek(f, 0, SEEK_SET);

	return size;
}

Region *region_from_file(char *filename)
{
FILE *f = fopen(filename,"rb");
char *buffer;
Region *result;
long size, rsize;

	if(f)
	{
		if( (size = _filesize(f)) > 0 )
		{
			assert("file is larger than max_region_len - see set_max_region_len()" && (size <= s_max_region_len));

			buffer = malloc(size);
			assert("out of memory" && buffer);

			rsize = fread(buffer, 1, size, f);
			assert("file read incomplete" && (rsize == size));

			result = (Region *) malloc(sizeof(Region));
			assert("out of memory" && result);

			result->begin	= result->current = buffer;
			result->end		= buffer + size - 1;

			fclose(f);
			return result;
		}

		fclose(f);
	} else printf("Could not open file '%s'.\n", filename);

	return 0;
}

void free_region(Region *r, int dispose_contents)
{
    assert(r);
	if(dispose_contents && r->begin) free(r->begin);
    free(r);
}

void print_ft(char *from, char *to)
{
char *c = from;

	if(!from || !to)
		return;

	assert(from <= to + 1);

    for(; c <= to && *c; ++c)
		/*
		if(*c == '\n')
			print("<CR>");
		else
		*/
			print("%c", *c);
}

void print_region(Region *r)
{
    assert(r);
    assert(r->begin);
    assert(r->current);
    assert(r->end);
	assert(r->begin <= r->current && r->current <= r->end);

    print("{");
    print_ft(r->current, r->end);
    print(" [");
    print_ft(r->begin, r->end);
    print("]}");

    fflush(stdout);
}

// ===========================================================================

int consumed_some(Region *r)
{
    if(r) return (r->current != r->begin);
    else return 0;
}

int consumed_all(Region *r)
{
    if(r) return (r->current == r->end+1);
    else return 0;
}

// ===========================================================================

int	compare_region(Region *a, Region *b)
{
char *pa, *pb;

	assert(a && a->begin && a->end && a->begin <= a->end);
	assert(b && b->begin && b->end && b->begin <= b->end);

	pa = a->begin;
	pb = b->begin;

	// skip to first difference or end
	while( ((*pa) == (*pb)) && (pa <= a->end) && (pb <= b->end) )
	{
		++pa;
		++pb;
	}

	// equal?
	if( (pa > a->end) && (pb > b->end) )
		return 0;

	if( pa > a->end )
	{
		// A at end ?
		return -1;
	} else
	if( pb > b->end )
	{
		// B at end ?
		return 1;
	} else
	{
		// we found a different char
		return (*pa) > (*pb) ? 1 : -1;
	}
}

// ===========================================================================

TextLocation get_text_location(Region *r, char *p)
{
TextLocation result = {-1, -1};

	assert(r);
	assert(p);

	// inside reference region ?
	if((r->begin <= p) && (p <= r->end))
	{
		int col = 0, row = 0;
		char *l = r->begin;

		while(l <= p)
		{
			if(*l == '\n')
			{
				++row;
				col = 0;
			} else ++col;

			++l;
		}
		result.row	= row;
		result.col	= col;
	}

	return result;
}

char *line_start(Region *r, int row)
{
char *result;

	assert(r);
	result = r->begin;

	while(row--)
	{
		while(*result++ != '\n')
			if(result > r->end)
				return 0L;

		if(result > r->end)
			return 0L;
	}

	return result;
}

char *line_end(char *p, char *end)
{
	while(*p != '\n' && *p != '\0' && p != end)
		++p;

	return p;
}


// --- Helpers ---------------------------------------------------------------------------------------------------------------
char *strcpy_ft(char *target, char *from, char *to)
{
int i;

    assert(target && from && to && (from <= to));

    for(i = 0; from+i <= to; ++i)
        target[i] = from[i];

    target[i] = 0;
	return target;
}


char *strdup_ft(char *from, char *to)
{
char *result;
int i;

    assert(from && to && (from <= to));		// what about empty (from == to+1) ? .. don't care atm
    result = (char *) malloc(to - from + 2);

    for(i = 0; from+i <= to; ++i)
        result[i] = from[i];

    result[i] = 0;
    return result;
}

int scanf_ft(char *from, char *to, char *fmt, ...)
{
va_list ap;
char *source;
int result;

	assert(from && to && fmt);
	va_start(ap, fmt);

	source = strdup_ft(from, to);
	result = vsscanf(source, fmt, ap);

	free(source);
	va_end(ap);

	return result;
}



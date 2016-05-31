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

#define MSWIN
// #define POSIX_LINUX

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>
#include <malloc.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include <string.h>
#if defined(POSIX_LINUX)
#include <arpa/inet.h>
#endif
#include "scope_malloc.h"
#include "cparsing.h"

/*	5 lines
int EDITOR(void *param, char **from, char **to)
{

	return EDIT_OK;
}
*/

#if defined(MSWIN)
#if BYTE_ORDER == BIG_ENDIAN
short htons(n) { return n; }
short ntohs(n) { return n; }
unsigned long htonl(n) { return n; }
unsigned long ntohl(n) { return n; }
#else
short htons(n) { return ((((unsigned short)(n) & 0xFF)) << 8) | (((unsigned short)(n) & 0xFF00) >> 8); }
short ntohs(n) { return ((((unsigned short)(n) & 0xFF)) << 8) | (((unsigned short)(n) & 0xFF00) >> 8); }

unsigned long htonl(n) 
{ 
	return ((((unsigned long)(n) & 0xFF)) << 24) | 
			((((unsigned long)(n) & 0xFF00)) << 8) | 
			((((unsigned long)(n) & 0xFF0000)) >> 8) | 
			((((unsigned long)(n) & 0xFF000000)) >> 24);
}

unsigned long ntohl(n) 
{ 
	return ((((unsigned long)(n) & 0xFF)) << 24) | 
			((((unsigned long)(n) & 0xFF00)) << 8) | 
			((((unsigned long)(n) & 0xFF0000)) >> 8) | 
			((((unsigned long)(n) & 0xFF000000)) >> 24);
}
#endif
#endif

static int _vasprintf(char **strp, const char *fmt, va_list ap)
{
va_list ap1;
size_t size;

    va_copy(ap1, ap);
    size = vsnprintf(NULL, 0, fmt, ap1) + 1;
    va_end(ap1);
    *strp = parse_calloc(1, size);

    return vsnprintf(*strp, size, fmt, ap);
}

int psprintf(char **strp, const char *fmt, ...)
{
int error;
va_list ap;

	assert(strp && fmt);
    va_start(ap, fmt);
    error = _vasprintf(strp, fmt, ap);
    va_end(ap);

    return error;
}

int _equal_ranges(char *f1, char *t1, char *f2, char *t2)
{
	if( (t1-f1) != (t2-f2) )
		return 0;

	for(; f1 <= t1 ; ++f1, ++f2)
		if( *f1 != *f2 )
			return 0;

	return 1;
}

int region2string(void *unused, char **from, char **to)
{
char **result;
unsigned len;

	assert(from && to && *from && *to);
	assert(*from <= *to);
	
	len = *to - *from + 1;
	
	result = (char **) parse_malloc(sizeof(char *));
	*result = parse_malloc(len+1);

	memcpy(*result, *from, len);

	// in case this is a string-region we add a '\0';
	// since this is outside of the 'true' region, it
	// should not interfere with anything else.
	(*result)[len] = 0;				
	
	*from	= (char *) result;
	*to		= ((char *) result) + sizeof(result) - 1;
	
	return EDIT_OK;
}

int region2pointer(void *unused, char **from, char **to)
{
char **result;

	assert(from && to && *from && *to);
	assert(*from <= *to);
	
	result = (char **) parse_malloc(sizeof(char *));
	
	*from	= (char *) result;
	*to		= ((char *) result) + sizeof(result) - 1;
	
	return EDIT_OK;
}

int region2region(void *unused, char **from, char **to)
{
Region *result;

	assert(from && to && *from && *to);
	assert(*from <= *to);
	
	result = parse_malloc(sizeof(Region));

	result->begin	= *from;
	result->current	= *from;
	result->end		= *to;
	
	*from	= (char *) result;
	*to		= ((char *) result) + sizeof(Region) - 1;
	
	return EDIT_OK;
}

int	add2dict(void *dict, char **from, char **to)
{
Region r = {*from, *to, *from};
	assert(dict);
	((Dict *) dict)->add(dict, &r, 0);

	return EDIT_OK;
}

typedef char (*c2c_fcn)(char);

int map_chars(void *function, char **from, char **to)
{
c2c_fcn f = (c2c_fcn) function;
char *result;
unsigned len, i;

	assert(from && to && *from && *to);
	assert(*from <= *to);
	
	len = *to - *from + 1;
	result = parse_malloc(len+1);

	for(i=0; i<len; ++i)
		result[i] = f((*from)[i]);

	result[len] = 0;
	
	*from	= result;
	*to		= result + len - 1;
	
	return EDIT_OK;
}

#define CARDINAL_CONVERTER_FROM_STRING(NAME,TYPE,OP) int NAME(void *base_of_number, char **from, char **to)\
{\
long bt;\
int base;\
TYPE value, *result;\
\
	assert(from && to && *from && *to);\
\
	bt = base_of_number ? (long) base_of_number : 10;\
	base = (int) bt;\
\
	value = OP(*from, 0, base);\
\
	if(errno == ERANGE)\
	{\
		errno = 0;\
		return EDIT_FAILED;\
	}\
\
	result	= parse_malloc(sizeof(TYPE));\
	*result	= value;\
	*from	= (char *) result;\
	*to		= ((char *) result) + sizeof(TYPE) - 1;\
\
	return EDIT_OK;\
}

#define CARDINAL_CONVERTER_TO_STRING(NAME,TYPE,FORMAT) int NAME(void *fmt, char **from, char **to)\
{\
char	*_fmt, *_str;\
size_t	len;\
\
	assert(from && *from && to && *to);\
	assert((*to - *from) == sizeof(TYPE)-1);\
\
	_fmt = fmt ? fmt : FORMAT;\
	len	 = psprintf(&_str, _fmt, *((TYPE *) *from));\
\
	if(len < 0) return EDIT_FAILED;\
	*from	= _str;\
	*to		= _str + len - 1;\
\
	return EDIT_OK;\
}

#define CARDINAL_CONVERTER_TO_CARDINAL(SRC_TYPE,TRG_TYPE) int SRC_TYPE##2##TRG_TYPE(void *unused, char **from, char **to)\
{\
TRG_TYPE *result;\
	assert(from && *from && to && *to);\
	assert(sizeof(SRC_TYPE) <= sizeof(TRG_TYPE));\
\
	result = parse_calloc(1, sizeof(TRG_TYPE));\
	*result = (TRG_TYPE) *((SRC_TYPE *) *from);\
\
	*from	= ((char *) result);\
	*to		= ((char *) result) + sizeof(TRG_TYPE) - 1;\
\
	return EDIT_OK;\
}

typedef char byte;

CARDINAL_CONVERTER_TO_CARDINAL(byte,int)
CARDINAL_CONVERTER_TO_CARDINAL(byte,short)
CARDINAL_CONVERTER_TO_CARDINAL(byte,long)

CARDINAL_CONVERTER_TO_CARDINAL(short,int)
CARDINAL_CONVERTER_TO_CARDINAL(short,long)

CARDINAL_CONVERTER_TO_CARDINAL(int,long)

// Coverity reports CWE-570 for the following macro-expansions:
// "Macro compares unsigned to 0 (NO_EFFECT) 
// unsigned_compare: This less-than-zero comparison of an unsigned value is never true. len < 0UL.",
// for which I fail to detect the cause. The code in comment below shows an exemplary expansion
// (the preprocessor's output is similiar, just assert and errno being replaced). 

// CARDINAL_CONVERTER_FROM_STRING(str2byte,char,strtol)

int str2byte(void *base_of_number, char **from, char **to)
{
long bt;
int base;
char value, *result;

	assert(from && to && *from && *to);

	bt = base_of_number ? (long) base_of_number : 10;
	base = (int) bt;

	value = strtol(*from, 0, base);

	if(errno == ERANGE)
	{
		errno = 0;
		return EDIT_FAILED;
	}

	result	= parse_malloc(sizeof(char));
	*result	= value;
	*from	= (char *) result;
	*to		= ((char *) result) + sizeof(char) - 1;

	return EDIT_OK;
}


CARDINAL_CONVERTER_TO_STRING(char2str,char,"%c")

CARDINAL_CONVERTER_FROM_STRING(str2int,int,strtol)
CARDINAL_CONVERTER_TO_STRING(int2str,int,"%d")

CARDINAL_CONVERTER_FROM_STRING(str2long,long,strtol)
CARDINAL_CONVERTER_TO_STRING(long2str,long,"%ld")

CARDINAL_CONVERTER_FROM_STRING(str2short,short,strtol)
CARDINAL_CONVERTER_TO_STRING(short2str,short,"%hd")

CARDINAL_CONVERTER_FROM_STRING(str2llong,long long,strtoll)
CARDINAL_CONVERTER_TO_STRING(llong2str,long long,"%lld")

CARDINAL_CONVERTER_TO_STRING(double2str,double,"%lf")
int str2double(void *unused, char **from, char **to)
{
double value, *result;

	assert(from && to && *from && *to);

	value = strtod(*from, 0);

	if(errno == ERANGE)
	{
		errno = 0;
		return EDIT_FAILED;
	}

	result	= parse_malloc(sizeof(double));
	*result	= value;
	*from	= (char *) result;
	*to		= ((char *) result) + sizeof(double) - 1;

	return EDIT_OK;
}


int str2str(void *fmt, char **from, char **to)
{
char *value, *tmp;
size_t len;

	assert(fmt && from && to && *from && *to);

	tmp		= strdup_ft(*from, *to);
	len		= psprintf(&value, fmt, tmp);
	*from	= value;
	*to		= value + len - 1;
	free(tmp);

	return EDIT_OK;
}


int bytes2bits(void *optional_01_symbols, char **from, char **to)
{
char *sym = optional_01_symbols ? (char *) optional_01_symbols : "01";
char *buffer;
char *src, *trg;
int len, i;

	assert(sym && sym[0] && sym[1]);
	assert(from && to && *from && *to);

	len = (*to - *from + 1) << 3;
	assert(get_max_region_len() >= len);

	buffer = parse_malloc(len+1);

	for(trg = buffer, src = *from; src <= *to; ++src)
		for(i=7; i>=0; --i)
			*trg++ = ((*src) & (1<<i)) ? sym[1] : sym[0];

	*trg	= 0;

	*from	= buffer;
	*to		= buffer + len - 1;

	return EDIT_OK;
}

#define BYTEORDER(type,postfix,function) int type##_##postfix(void *unused, char **from, char **to)\
{\
type *buffer;\
\
	assert(from && to && *from && *to);\
	assert((*to - *from + 1) == sizeof(type));\
\
	buffer = (type *) parse_malloc(sizeof(type));\
	\
	*buffer = function(*((type *) *from));\
\
	*from	= ((char *) buffer);\
	*to		= ((char *) buffer) + sizeof(type) - 1;\
\
	return EDIT_OK;\
}

BYTEORDER(short,h2n,htons)

BYTEORDER(long,h2n,htonl)

BYTEORDER(short,n2h,ntohs)

BYTEORDER(long,n2h,ntohl)

int dict_replace_editor(void *param, char **from, char **to)
{
Dict *d = (Dict *) param;
Region r;

	assert(d);
	init_region(&r, (char *) d->get_value(d), 0L);
	*from = r.begin;
	*to = r.end;

	return EDIT_OK;
}

Parser *DictSubst(Dict *substitutions)
{
	return Edit(dict_replace_editor, substitutions, ElementOf(substitutions));
}

// emits regions with len >= target_size; the function assumes big endian layout,
// copying smaller tokens into the least significant bytes.
int expand_to_size(void *param, char **from, char **to)
{
int target_size = (int) param;
int region_len;

	assert(target_size);
	assert(from && to && *from && *to);

	region_len = REGION_LEN(*from, *to);

	if(region_len < target_size)
	{
		char *buffer = (char *) parse_calloc(1, target_size);
		int offset = target_size - region_len;
		memcpy(buffer+offset, *from, region_len);
		*from = buffer;
		*to = REGION_END(buffer, target_size);
	}

	return EDIT_OK;
}

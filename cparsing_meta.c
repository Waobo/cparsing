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
#include <string.h>
#include <math.h>

#include "scope_malloc.h"
#include "cparsing_list.h"
#include "cparsing.h"
#include "cparsing_meta.h"
#include "cparsing_impl.h"

/* ----------------------------------------------

X Word(const char *character_pool, int min, int max);
X CharNotIn(void *character_pool, int min, int max);	
X Literal(char *string_to_match);
0 StringSpec(char quote, char escape);			TODO: reduce quoting-level (\\ => \ ...)
X String(void);
X Base2(int min_value, int max_value);
X Base10(int min_value, int max_value, int is_signed);
X Base16(int min_value, int max_value);
X Eol(void);

X Bytes(int size, int min, int max);
Int8(int min, int max);									
Int16(int min, int max);
Int24(int min, int max);
Int32(int min, int max);
Int40(int min, int max);
Int48(int min, int max);
Int56(int min, int max);
Int64(int min, int max);
Int128(int min, int max);
Int256(int min, int max);
 
X Sequence(Parser *p1, ...);
X MatchAll(Parser *p1, ...);								
X Or(Parser *p1, ...);									
X Not(Parser *p);

X Chain(Parser *producer, Parser *consumer);

X Repeat(Parser *p, int min, int max);					
X Optional(Parser *p);									
X OneOrMore(Parser *p);									
X ZeroOrMore(Parser *p);									

X Any ZeroOrMore				
X Some OneOrMore

X ElementOf(Dict *dict);
X ForwardDeclaration(void);
X ForwardAssign(Parser *forward_declaration, Parser *assignment);
X CaptureUntil(int include_marker, Parser *marker);
X Record(Parser *p);				
X Nth(unsigned index, Parser *p);

X Pack(Parser *p);	

X ToIntB(BASE,PPP) 		Edit(str2int, (BASE), (PPP))
X ToShortB(BASE,PPP) 	Edit(str2short, (BASE), (PPP))
X ToLongB(BASE,PPP) 	Edit(str2long, (BASE), (PPP))

X AddToDict(DICT,PPP)	Edit(add2dict, (DICT), (PPP))

X Subst(FMT,PPP) 		Edit(str2str, (FMT), (PPP))

X ToLower(PPP) 			Edit(map_chars, tolower, (PPP))
X ToUpper(PPP) 			Edit(map_chars, toupper, (PPP))

X ToByte(PPP) 			Edit(str2byte, 0, (PPP))
X ToInt(PPP) 			Edit(str2int, 0, (PPP))
X ToShort(PPP) 			Edit(str2short, 0, (PPP))
X ToLong(PPP) 			Edit(str2long, 0, (PPP))
// #define ToFloat(PPP) Edit(str2float, 0, (PPP))	TODO
X ToDouble(PPP) 		Edit(str2double, 0, (PPP))
X ToStr(PPP) 			Edit(region2string, 0, (PPP))
X ToPtr(PPP) 			Edit(region2pointer, 0, (PPP))
X ToBits(PPP) 			Edit(bytes2bits, 0, (PPP))
X Htons(PPP) 			Edit(short_h2n, 0, (PPP))
X Ntohs(PPP) 			Edit(short_n2h, 0, (PPP))
X Htonl(PPP) 			Edit(long_h2n, 0, (PPP))
X Ntohl(PPP) 			Edit(long_n2h, 0, (PPP))
 
X Emit(char *from, size_t len);					len?
X Print(char *delimiter, Parser *source);
// 0 Printf(char *format, ...);

X Parser *Ref(Parser *p);
X Parser *Foreign(Parser *src);
  
--------
 
0 Flagged(short flags, short mask, Parser *p);
X Named(char *name, Parser *p);			* add to default dict * ?

X Caseless(Parser *p);							
X Skipped(Parser *p);								
X Weak(Parser *p);							
X Strong(Parser *p);							
X Binary(Parser *p);						
X Traced(Parser *p);						
X Restart(Parser *p);
 
--------------------------------------------------
validation mode (C) only

0 Put(T,P)
0 Edit(Editor editor, void *editor_param, Parser *source);
0 Call(ctor_function ctor, void *user_data, Parser *p);  
0 OnFail(void *user, error_function ctor, const char *error_message, Parser *p);
0 Fail(void *user, error_function error_handler, const char *error_message); 

------------------------------------------------ */

#define MAXLEN 8

typedef Parser *(*ParserListCtor)(Parser *parser_list);
typedef Parser *(*ParameterlessParser)(void);
typedef Parser *(*ParsersParser)(Parser *);
typedef Parser *(*TwoIntParser)(int, int);
typedef Parser *(*ThreeIntParser)(int, int, int);
typedef Parser *(*TwoParsersParser)(Parser *, Parser *);
typedef Parser *(*IntAndParserParser)(int, Parser *);
typedef Parser *(*StringAndParserParser)(char *, Parser *);
typedef Parser *(*DictAndParserParser)(char *, Parser *);
typedef Parser *(*StringAndIntParser)(char *, int);

void report_error(void *user_data, const char *msg, Parser *src, Region *r)
{
ErrorContext *ec = (ErrorContext *) user_data;

	assert(ec);
	assert(msg);
	assert(r);

	// we'll take the first error only
	if(ec->error) return;

	ec->error = r->current;
	ec->where = get_text_location(ec->all_input, r->current);

	if(ec->where.row >= 0)
	{
	int i;
		char *_start, *_end;

		_start	= line_start(ec->all_input, ec->where.row);
		_end	= line_end(_start, ec->all_input->end);

#ifdef HAVE_ANSI_COLOR
		printf(YELLOW "ERROR from %s: %s at (%d, %d):" NORMAL "\n", src->name, msg, ec->where.row, ec->where.col);
		printf(BOLD WHITE); print_ft(_start, r->current); printf(RED); print_ft(r->current+1, _end); printf(NORMAL "\n");
#else
		printf("ERROR from %s: %s at (%d, %d):" "\n", src->name, msg, ec->where.row, ec->where.col);
		print_ft(_start, r->current); print_ft(r->current+1, _end); printf("\n");
#endif

		for(i=0; i<ec->where.col-1; ++i)
			printf("-");

		printf("^\n");
	} else 
	{
#ifdef HAVE_ANSI_COLOR
		printf(YELLOW "ERROR from %s: %s" NORMAL "\n", src->name, msg);
		printf(BOLD WHITE); print_ft(r->begin, r->current); printf(RED); print_ft(r->current+1, r->end); printf(NORMAL "\n");
#else
		printf("ERROR from %s: %s" "\n", src->name, msg);
		print_ft(r->begin, r->current); print_ft(r->current+1, r->end); printf("\n");
#endif
	}
}


void dump_data_source(Parser *data_source)
{
RecordIterator ri;
Region tmp;

	record_init_interator(&ri, data_source);

	while(record_get_next(&ri, &tmp))
	{
		printf("->>"); print_ft(tmp.begin, tmp.end); printf("<<-\n");

	}
	printf("\n\n");
}

// ---------------------------------------------------------------------------------------------

static int word_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Region r = INIT_STR_REGION("");
Parser *result, *builder;
char *cmd;
char *pattern;
int min = 0, max = 0;
int ok;

	assert(data_source);

	builder = Sequence(
				Put(&cmd,		ToStr(Nth(0, Foreign(data_source)))), 
				Put(&pattern,	ToStr(Nth(1, Foreign(data_source)))), 
				Put(&min,		ToInt(Nth(2, Foreign(data_source)))), 
				Put(&max,		ToInt(Nth(3, Foreign(data_source)))), 
			0L);

	ok = builder->parse(builder, I0, &r);
	builder->destroy(builder);

	if(!ok) return PARSER_FAILED;

	if(strcmp("Word", cmd))
		result = CharNotIn(pattern, min, max);
	else
		result = Word(pattern, min, max);

	if(I.interpreter)
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);

	return PARSER_MATCH;
}

static int literal_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Region r;
Parser *result;
char *pattern;

	assert(data_source);

	if(record_get_nth(data_source, 0, &r))
	{
		if(I.interpreter)
		{
			pattern	= parse_ftdup(r.begin, r.end);

			result	= Literal(pattern);

			I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);
		}

		return PARSER_MATCH;
	}	
	return PARSER_FAILED;
}

static int parameterless_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
ParameterlessParser pg;
Parser *result;

	assert(vdata);
	pg = (ParameterlessParser) vdata;

	if(I.interpreter)
	{
		result = pg();
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);
	}

	return PARSER_MATCH;
}

static int parsers_parser_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Parser *result;
RecordIterator ri;
ParsersParser pg = (ParsersParser) vdata;
Region tmp;
Parser *arg;
int ok = 0;

	assert(pg);
	assert(data_source);

	record_init_interator(&ri, data_source);

	ok = record_get_next(&ri, &tmp);

	if(!ok)
	{
		assert("grammar failure" && 1==0);
		return PARSER_FAILED;
	}

	assert("invalid size of token" && (REGION_LEN(tmp.begin,tmp.end) == sizeof(Parser)));
	arg = region_as_ptr(Parser,tmp);

	assert(record_iterator_at_end(&ri));

	if(I.interpreter)
	{
		result = pg(arg);
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);
	}

	return PARSER_MATCH;
}

#if 0
static int parsers_editor_noparam_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Parser *result;
RecordIterator ri;
Editor ed = (Editor) vdata;
Region tmp;
Parser *arg;
int ok = 0;

	assert(ed);
	assert(data_source);

	record_init_interator(&ri, data_source);

	ok = record_get_next(&ri, &tmp);

	if(!ok)
	{
		assert("grammar failure" && 1==0);
		return PARSER_FAILED;
	}

	arg = region_as_ptr(Parser,tmp);

	assert(record_iterator_at_end(&ri));

	if(I.interpreter)
	{
		result = Edit(ed, 0L, arg);
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);
	}

	return PARSER_MATCH;
}
#endif

static int two_parsers_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Parser *result;
RecordIterator ri;
TwoParsersParser pg = (TwoParsersParser) vdata;
Region tmp;
Parser *arg[2] = {0, 0};	// prevent SCAM
int cnt = 0;

	assert(pg);
	assert(data_source);

	record_init_interator(&ri, data_source);

	while(cnt < 2 && record_get_next(&ri, &tmp))
	{
		assert("invalid size of token" && (REGION_LEN(tmp.begin,tmp.end) == sizeof(Parser)));
		arg[cnt] = region_as_ptr(Parser,tmp);
		++cnt;
	}

	assert(record_iterator_at_end(&ri));

	if(I.interpreter)
	{
		result = pg(arg[0], arg[1]);
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);
	}

	return PARSER_MATCH;
}

static int forward_assign_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
RecordIterator ri;
Region tmp;
Parser *fwd;
int arg1present;

	assert(data_source);
	record_init_interator(&ri, data_source);

	arg1present = record_get_next(&ri, &tmp);

	if(!arg1present)
	{
		assert("grammar failed" && 1==0);
		return PARSER_FAILED;
	}

	assert("invalid size of token" && (REGION_LEN(tmp.begin,tmp.end) == sizeof(Parser)));
	fwd = (Parser *) tmp.begin;

	if(fwd->parse != unimplemented_forward_declaration)
	{
		assert("provided parser is not a forward declaration" && 1==0);
		return PARSER_FAILED;
	}

	return two_parsers_ctor(vdata, I, data_source);
}

static int three_int_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Parser *result;
RecordIterator ri;
ThreeIntParser pg = (ThreeIntParser) vdata;
Region tmp;
int arg[3] = {0, 0, 0}, cnt = 0;

	assert(pg);
	assert(data_source);

	record_init_interator(&ri, data_source);

	while(cnt < 3 && record_get_next(&ri, &tmp))
	{
		assert(REGION_LEN(tmp.begin, tmp.end) == sizeof(int));
		arg[cnt] = * ((int *) tmp.begin);
		++cnt;
	}

	assert(record_iterator_at_end(&ri));

	if(I.interpreter)
	{
		result = pg(arg[0], arg[1], arg[2]);
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);
	}

	return PARSER_MATCH;
}

static int intX_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Parser *result;
RecordIterator ri;
TwoIntParser pg;
Region tmp;
int arg[2] = {0, 0}, cnt = 0, size;

	assert(data_source);

	record_init_interator(&ri, data_source);

	if(!record_get_next(&ri, &tmp))
	{
		assert(1==0);
		return PARSER_FAILED;
	}

	size = *((int *) tmp.begin);

	switch(size)
	{
		case 8:		pg = Int8;		break;
		case 16:	pg = Int16;		break;
		case 24:	pg = Int24;		break;
		case 32:	pg = Int32;		break;
		case 40:	pg = Int40;		break;
		case 48:	pg = Int48;		break;
		case 56:	pg = Int56;		break;
		case 64:	pg = Int64;		break;
		case 128:	pg = Int128;	break;
		case 256:	pg = Int256;	break;
	
		default:
			return PARSER_FAILED;
	 }

	while(cnt < 2 && record_get_next(&ri, &tmp))
	{
		assert("invalid size of token" && (REGION_LEN(tmp.begin, tmp.end) == sizeof(int)));
		arg[cnt] = * ((int *) tmp.begin);
		++cnt;
	}

	assert(record_iterator_at_end(&ri));

	if(I.interpreter)
	{
		result = pg(arg[0], arg[1]);
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);
	}

	return PARSER_MATCH;
}

static int baseX_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Parser *result;
RecordIterator ri;
TwoIntParser pg;
Region tmp;
int arg[2] = {0, 0}, cnt = 0;

	assert(data_source);

	record_init_interator(&ri, data_source);

	if(!record_get_next(&ri, &tmp))
	{
		assert("grammar failure" && 1==0);
		return PARSER_FAILED;
	}

	switch(*tmp.begin)
	{
		case '2':	pg = Base2;		break;
		case '1':	pg = Base16;	break;
	
		default:
			assert("grammar failure" && 1==0);
			return PARSER_FAILED;
	 }

	while(cnt < 2 && record_get_next(&ri, &tmp))
	{
		assert("invalid size of token" && (REGION_LEN(tmp.begin, tmp.end) == sizeof(int)));
		arg[cnt] = * ((int *) tmp.begin);
		++cnt;
	}

	assert(record_iterator_at_end(&ri));

	if(I.interpreter)
	{
		result = pg(arg[0], arg[1]);
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);
	}

	return PARSER_MATCH;
}

static int int_and_parser_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Parser *result;
RecordIterator ri;
IntAndParserParser pg = (IntAndParserParser) vdata;
Region tmp;
Parser *arg2;
int arg1;
int ok;

	assert(pg);
	assert(data_source);

	record_init_interator(&ri, data_source);

	ok = record_get_next(&ri, &tmp);
	if(!ok)
	{
		assert("grammar failure" && 1==0);
		return PARSER_FAILED;
	}

	assert("invalid size of token" && (REGION_LEN(tmp.begin, tmp.end) == sizeof(int)));
	arg1 = region_as(int, tmp);

	ok = record_get_next(&ri, &tmp);
	if(!ok)
	{
		assert("grammar failure" && 1==0);
		return PARSER_FAILED;
	}

	assert("invalid size of token" && (REGION_LEN(tmp.begin, tmp.end) == sizeof(Parser)));
	arg2 = region_as_ptr(Parser,tmp);

	assert(record_iterator_at_end(&ri));

	if(I.interpreter)
	{
		result = pg(arg1, arg2);
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);
	}

	return PARSER_MATCH;
}

static int string_and_parser_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Parser *result;
RecordIterator ri;
StringAndParserParser pg = (StringAndParserParser) vdata;
Region tmp;
Parser *arg2;
char *arg1;
int ok;

	assert(pg);
	assert(data_source);

	record_init_interator(&ri, data_source);

	ok = record_get_next(&ri, &tmp);
	if(!ok)
	{
		assert("grammar failure" && 1==0);
		return PARSER_FAILED;
	}

	arg1 = parse_ftdup(tmp.begin, tmp.end);

	ok = record_get_next(&ri, &tmp);
	if(!ok)
	{
		assert("grammar failure" && 1==0);
		return PARSER_FAILED;
	}

	assert("invalid size of token" && (REGION_LEN(tmp.begin, tmp.end) == sizeof(Parser)));
	arg2 = region_as_ptr(Parser,tmp);

	assert(record_iterator_at_end(&ri));

	if(I.interpreter)
	{
		result = pg(arg1, arg2);
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);
	}

	return PARSER_MATCH;
}

static int addtodict_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Parser *result;
RecordIterator ri;
Dict *all_dict = (Dict *) vdata;
Dict *the_dict;
Region tmp;
Parser *the_parser;
char *dict_name;
void *value;
int ok;

	assert(all_dict);
	assert(data_source);

	record_init_interator(&ri, data_source);

	ok = record_get_next(&ri, &tmp);
	if(!ok)
	{
		assert("grammar failure" && 1==0);
		return PARSER_FAILED;
	}

	dict_name = parse_ftdup(tmp.begin, tmp.end);

	if(!dict_lookup(all_dict, dict_name, &value))
	{
		assert("dict has not been declared" && 1==0);
		return PARSER_FAILED;
	}
	the_dict = (Dict *) value;

	ok = record_get_next(&ri, &tmp);
	if(!ok)
	{
		assert("grammar failure" && 1==0);
		return PARSER_FAILED;
	}

	assert("invalid size of token" && (REGION_LEN(tmp.begin, tmp.end) == sizeof(Parser)));
	the_parser = region_as_ptr(Parser,tmp);

	assert(record_iterator_at_end(&ri));

	if(I.interpreter)
	{
		result = AddToDict(the_dict, the_parser);
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);
	}

	return PARSER_MATCH;
}

static int elementof_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Parser *result;
RecordIterator ri;
Dict *all_dict = (Dict *) vdata;
Dict *the_dict;
Region tmp;
char *dict_name;
void *value;
int ok;

	assert(all_dict);
	assert(data_source);

	record_init_interator(&ri, data_source);

	ok = record_get_next(&ri, &tmp);
	if(!ok)
	{
		assert("grammar failure" && 1==0);
		return PARSER_FAILED;
	}

	dict_name = parse_ftdup(tmp.begin, tmp.end);

	if(!dict_lookup(all_dict, dict_name, &value))
	{
		assert("dict has not been declared" && 1==0);
		return PARSER_FAILED;
	}
	the_dict = (Dict *) value;

	assert(record_iterator_at_end(&ri));

	if(I.interpreter)
	{
		result = ElementOf(the_dict);
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);
	}

	return PARSER_MATCH;
}

static int string_and_int_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Parser *result;
RecordIterator ri;
StringAndIntParser pg = (StringAndIntParser) vdata;
Region tmp;
char *arg1;
int arg2;
int ok;

	assert(pg);
	assert(data_source);

	record_init_interator(&ri, data_source);

	ok = record_get_next(&ri, &tmp);
	if(!ok)
	{
		assert("grammar failure" && 1==0);
		return PARSER_FAILED;
	}

	arg1 = region_as_ptr(char, tmp);

	ok = record_get_next(&ri, &tmp);
	if(!ok)
	{
		assert("grammar failure" && 1==0);
		return PARSER_FAILED;
	}

	assert("invalid size of token" && (REGION_LEN(tmp.begin, tmp.end) == sizeof(int)));
	arg2 = region_as(int,tmp);

	assert(record_iterator_at_end(&ri));

	if(I.interpreter)
	{
		result = pg(arg1, arg2);
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);
	}

	return PARSER_MATCH;
}


static int variadic_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Parser *plist;
Parser *result;
ParserListCtor ctor_function = (ParserListCtor) vdata;

	assert(ctor_function);
	assert(data_source);

	plist = record_to_parser_list(data_source);

	if(plist)
	{
		if(I.interpreter)
		{
			result = ctor_function(plist);
			I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);
		}

		return PARSER_MATCH;
	}

	return PARSER_FAILED;
}

static int repeat_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Region r = INIT_STR_REGION("");
Parser *result, *builder;
Parser *embedded;
int min = 0, max = 0, ok;

	assert(data_source);

	builder = Sequence(
				Put(&embedded,		ToPtr(Nth(0, Foreign(data_source)))), 
				Put(&min,			ToInt(Nth(1, Foreign(data_source)))), 
				Put(&max,			ToInt(Nth(2, Foreign(data_source)))), 
			0L);

	ok = builder->parse(builder, I0, &r);
	builder->destroy(builder);

	assert("Grammar does not match ctor" && ok);

	result = Repeat(embedded, min, max);

	if(I.interpreter)
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);

	return PARSER_MATCH;
}

static int len_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Region r = INIT_STR_REGION("");
Parser *result, *builder;
Parser *embedded;
int min = 0, max = 0, ok;

	assert(data_source);

	builder = Sequence(
				Put(&min,			ToInt(Nth(0, Foreign(data_source)))), 
				Put(&max,			ToInt(Nth(1, Foreign(data_source)))), 
				Put(&embedded,		ToPtr(Nth(2, Foreign(data_source)))), 
			0L);

	ok = builder->parse(builder, I0, &r);
	builder->destroy(builder);

	assert("Grammar does not match ctor" && ok);

	result = Len(min, max, embedded);

	if(I.interpreter)
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);

	return PARSER_MATCH;
}

static int repeat_short_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Region r;
Parser *result;
char key;

	assert(data_source);

	record_get_nth(data_source, 0, &r);
	key = r.begin[1];	// keywords differ in 2nd character

	record_get_nth(data_source, 1, &r);

	// zEroormore, oNeormore, oPtional
	switch(key)
	{
		case 'e':
			result	= ZeroOrMore((Parser *) r.begin);
			break;
			
		case 'n':
			result	= OneOrMore((Parser *) r.begin);
			break;
			
		case 'p':
			result	= Optional((Parser *) r.begin);
			break;
			
		default:
			assert("Grammar does not match ctor" && 0);
	}

	if(I.interpreter)
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);

	return PARSER_MATCH;
}

static int declared_parser_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Region r;
Dict *known_parser = (Dict *) vdata;
Parser *result;
char *parser_name;
int lookup_ok;

	assert(known_parser);
	assert(data_source);

	record_get_nth(data_source, 0, &r);
	
	parser_name = parse_ftdup(r.begin, r.end);

	lookup_ok = dict_lookup(known_parser, parser_name, (void **) &result);

	assert("grammar failure" && lookup_ok);
	
	if(I.interpreter)
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(Parser)-1);

	return PARSER_MATCH;
}

// ---------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------

Parser *cparsing_basic_parser_grammar(ErrorContext *ec, error_function reporter, Dict *parser_by_name_storage, Dict *all_dicts)
{
Parser *parserG = ForwardDeclaration();

#define OPEN		Skipped(Literal("("))
#define CLOSE 		Skipped(Literal(")"))
#define REQ_OPEN	OnFail(ec, report_error, "expecting open bracket", Skipped(Literal("(")))
#define REQ_CLOSE 	OnFail(ec, report_error, "expecting closing bracket", Skipped(Literal(")")))
#define COMMA 		Skipped(Literal(","))
#define COLON 		Skipped(Literal(";"))
#define REQ_COMMA 	OnFail(ec, report_error, "comma or argument missing", Skipped(Literal(",")))
#define MMVAL 		Base10(-1, MAXLEN, 1)
#define TERMINATOR	OnFail(ec, report_error, "Terminator ', 0L' required", Skipped(Sequence(COMMA, Literal("0"), Literal("L"), 0L)))

Parser *LiteralG	= Call(literal_ctor, 0, Sequence(
						Skipped(Literal("Literal")), 
						REQ_OPEN, 
							OnFail(ec, reporter, "expected string constant", String()), 
						REQ_CLOSE, 
					0L));

Parser *WordG		= Call(word_ctor, 0, Sequence(
						Or(Literal("Word"), Literal("CharNotIn"), 0L), 
						REQ_OPEN, 
							OnFail(ec, reporter, "expected string constant", String()), 
							REQ_COMMA, 
							OnFail(ec, reporter, "'minimum' must be valid number", MMVAL), 
							REQ_COMMA, 
							OnFail(ec, reporter, "'maximum' must be valid number", MMVAL), 
						REQ_CLOSE, 
					0L));

Parser *SequenceG	= Call(variadic_ctor, SequenceL, Sequence(
						Skipped(Literal("Sequence")),
						REQ_OPEN, 
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							ZeroOrMore(
								Sequence(
									COMMA, 
									Ref(parserG),
								0L)
							), 
							TERMINATOR, 
						REQ_CLOSE, 
					0L));

Parser *OrG			= Call(variadic_ctor, OrL, Sequence(
						Skipped(Literal("Or")), 
						REQ_OPEN, 
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							ZeroOrMore(Sequence(COMMA, Ref(parserG), 0L)), 
							TERMINATOR, 
						REQ_CLOSE, 
					0L));

Parser *MatchAllG	= Call(variadic_ctor, MatchAll, Sequence(
						Skipped(Literal("MatchAll")), 
						REQ_OPEN, 
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							ZeroOrMore(Sequence(COMMA, Ref(parserG), 0L)), 
							TERMINATOR, 
						REQ_CLOSE, 
					0L));

Parser *ChainG		= Call(two_parsers_ctor, Chain, Sequence(
						Skipped(Literal("Chain")), 
						REQ_OPEN, 
							OnFail(ec, reporter, "invalid producer parser definition", Ref(parserG)), 
							REQ_COMMA,
							OnFail(ec, reporter, "invalid consumer parser definition", Ref(parserG)), 
						REQ_CLOSE, 
					0L));

Parser *ForwardAssignG	= Call(forward_assign_ctor, ForwardAssign, Sequence(
						Skipped(Literal("ForwardAssign")), 
						REQ_OPEN, 
							OnFail(ec, reporter, "valid forward-declaration required", Ref(parserG)), 
							REQ_COMMA,
							OnFail(ec, reporter, "invalid parser-implementation definition", Ref(parserG)), 
						REQ_CLOSE, 
					0L));

Parser *RepeatG		= Call(repeat_ctor, 0, Sequence(
						Skipped(Literal("Repeat")), 
						REQ_OPEN, 
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_COMMA,
							OnFail(ec, reporter, "'minimum' must be valid number", MMVAL), 
							REQ_COMMA, 
							OnFail(ec, reporter, "'maximum' must be valid number", MMVAL), 
						REQ_CLOSE, 
					0L));

Parser *LenG		= Call(len_ctor, 0, Sequence(
						Skipped(Literal("Len")), 
						REQ_OPEN, 
							OnFail(ec, reporter, "'minimum length' must be valid number", MMVAL), 
							REQ_COMMA, 
							OnFail(ec, reporter, "'maximum length' must be valid number", MMVAL), 
							REQ_COMMA,
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
						REQ_CLOSE, 
					0L));

Parser *RepeatShortG	= Call(repeat_short_ctor, 0, Sequence(
						Or(
							Literal("OneOrMore"),
							Subst("OneOrMore", Literal("Some")),
							Literal("ZeroOrMore"),
							Subst("ZeroOrMore", Literal("Any")),
							Literal("Optional")
						, 0L), 
						REQ_OPEN, 
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
						REQ_CLOSE, 
					0L));

Parser *StringG			= Call(parameterless_ctor, String, Sequence(
							Skipped(Literal("String")),
							REQ_OPEN,
							REQ_CLOSE,
						0L));

Parser *ForwardDeclarationG	= Call(parameterless_ctor, ForwardDeclaration, Sequence(
							Skipped(Literal("ForwardDeclaration")),
							REQ_OPEN,
							REQ_CLOSE,
						0L));

Parser *EolG			= Call(parameterless_ctor, Eol, Sequence(
							Skipped(Literal("Eol")),
							REQ_OPEN,
							REQ_CLOSE,
						0L));

Parser *BytesG			= Call(three_int_ctor, Bytes, Sequence(
							Skipped(Literal("Base")),
							REQ_OPEN,
							OnFail(ec, reporter, "'size' must be a valid number", MMVAL), 
							REQ_COMMA, 
							OnFail(ec, reporter, "'minimum amount' must be valid number", MMVAL), 
							REQ_COMMA, 
							OnFail(ec, reporter, "'maximum amount' must be valid number", MMVAL), 
							REQ_CLOSE,
						0L));

Parser *Base10G			= Call(three_int_ctor, Base10, Sequence(
							Skipped(Literal("Base10")),
							REQ_OPEN,
							OnFail(ec, reporter, "'minimum' must be valid number", MMVAL), 
							REQ_COMMA, 
							OnFail(ec, reporter, "'maximum' must be valid number", MMVAL), 
							REQ_COMMA, 
							OnFail(ec, reporter, "valid 'signed' values are [1,0,SIGNED,UNSIGNED]",
								Or(Literal("SIGNED"), Literal("UNSIGNED"), Word("01", 1, 1), 0L)
							), 
							REQ_CLOSE,
						0L));

Parser *IntXG			= Call(intX_ctor, 0L, Sequence(
							Binary(Sequence(Skipped(Literal("Int")), ToInt(Base10(8, 256, UNSIGNED)), 0L)),
							REQ_OPEN,
							OnFail(ec, reporter, "'minimum amount' must be valid number", ToInt(MMVAL)), 
							REQ_COMMA, 
							OnFail(ec, reporter, "'maximum amount' must be valid number", ToInt(MMVAL)), 
							REQ_CLOSE,
						0L));

Parser *BaseXG			= Call(baseX_ctor, 0L, Sequence(
							Binary(Sequence(Skipped(Literal("Base")), Or(Literal("2"), Literal("16"),  0L), 0L)),
							REQ_OPEN,
							OnFail(ec, reporter, "'minimum' must be valid number", ToInt(MMVAL)), 
							REQ_COMMA, 
							OnFail(ec, reporter, "'maximum' must be valid number", ToInt(MMVAL)), 
							REQ_CLOSE,
						0L));

Parser *CaptureUntilG	= Call(int_and_parser_ctor, CaptureUntil, Sequence(
							Skipped(Literal("CaptureUntil")),
							REQ_OPEN,
							OnFail(ec, reporter, "valid 'include_marker' values are [1,0,INCLUDE_MATCH,EXCLUDE_MATCH]",
								ToInt(Or(MMVAL, Subst("1", Literal("INCLUDE_MATCH")), Subst("0", Literal("EXCLUDE_MATCH")), 0L))
							),
							REQ_COMMA, 
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_CLOSE,
						0L));

Parser *NthG			= Call(int_and_parser_ctor, Nth, Sequence(
							Skipped(Literal("Nth")),
							REQ_OPEN,
							OnFail(ec, reporter, "valid 'index' values limited to [0..100]",
								ToInt(Base10(0, 100, SIGNED))
							),
							REQ_COMMA, 
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_CLOSE,
						0L));

Parser *RecordG			= Call(parsers_parser_ctor, Record, Sequence(
							Skipped(Literal("Record")),
							REQ_OPEN,
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_CLOSE,
						0L));

Parser *PackG			= Call(parsers_parser_ctor, Pack, Sequence(
							Skipped(Literal("Pack")),
							REQ_OPEN,
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_CLOSE,
						0L));

Parser *RefG			= Call(parsers_parser_ctor, Ref, Sequence(
							Skipped(Literal("Ref")),
							REQ_OPEN,
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_CLOSE,
						0L));

Parser *ForeignG		= Call(parsers_parser_ctor, Foreign, Sequence(
							Skipped(Literal("Foreign")),
							REQ_OPEN,
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_CLOSE,
						0L));

Parser *ToIntBG			= Call(int_and_parser_ctor, ToIntB, Sequence(
							Skipped(Literal("ToIntB")),
							REQ_OPEN,
							OnFail(ec, reporter, "valid 'base' values limited to [2..100]",
								ToInt(Base10(2, 100, SIGNED))
							),
							REQ_COMMA, 
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_CLOSE,
						0L));

Parser *ToShortBG			= Call(int_and_parser_ctor, ToShortB, Sequence(
							Skipped(Literal("ToShortB")),
							REQ_OPEN,
							OnFail(ec, reporter, "valid 'base' values limited to [2..100]",
								ToInt(Base10(2, 100, SIGNED))
							),
							REQ_COMMA, 
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_CLOSE,
						0L));

Parser *ToLongBG			= Call(int_and_parser_ctor, ToLongB, Sequence(
							Skipped(Literal("ToLongB")),
							REQ_OPEN,
							OnFail(ec, reporter, "valid 'base' values limited to [2..100]",
								ToInt(Base10(2, 100, SIGNED))
							),
							REQ_COMMA, 
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_CLOSE,
						0L));


Parser *SubstG			= Call(string_and_parser_ctor, Subst, Sequence(
							Skipped(Literal("Subst")),
							REQ_OPEN,
							OnFail(ec, reporter, "expected format string constant", String()), 	// TODO: accept identifiers too
							REQ_COMMA, 
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_CLOSE,
						0L));

Parser *PrintG			= Call(string_and_parser_ctor, Print, Sequence(
							Skipped(Literal("Print")),
							REQ_OPEN,
							OnFail(ec, reporter, "expected delimiter string constant", String()), 	// TODO: accept identifiers too
							REQ_COMMA, 
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_CLOSE,
						0L));

Parser *EmitG			= Call(string_and_int_ctor, Emit, Sequence(
							Skipped(Literal("Emit")),
							REQ_OPEN,
							OnFail(ec, reporter, "expected format string constant", String()), 	// TODO: accept identifiers too
							REQ_COMMA, 
							OnFail(ec, reporter, "valid 'len' values limited to [0..1024]",
								ToInt(Base10(0, 1024, SIGNED))
							),
							REQ_CLOSE,
						0L));

Parser *ToByteG			= Call(parsers_parser_ctor, ToByte, Sequence(
							Skipped(Literal("ToByte")),
							REQ_OPEN,
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_CLOSE,
						0L));

Parser *NotG			= Call(parsers_parser_ctor, Not, Sequence(
							Skipped(Literal("Not")),
							REQ_OPEN,
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_CLOSE,
						0L));

#define SIMPLE_CONVERTER_GRAMMER(CONV) Parser *CONV##G	= Call(parsers_parser_ctor, CONV, Sequence(\
							Skipped(Literal(#CONV)),\
							REQ_OPEN,\
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), \
							REQ_CLOSE,\
						0L));

SIMPLE_CONVERTER_GRAMMER(ToInt)

SIMPLE_CONVERTER_GRAMMER(ToShort)

SIMPLE_CONVERTER_GRAMMER(ToLong)

// SIMPLE_CONVERTER_GRAMMER(ToFloat)

SIMPLE_CONVERTER_GRAMMER(ToDouble)

SIMPLE_CONVERTER_GRAMMER(ToStr)

SIMPLE_CONVERTER_GRAMMER(ToPtr)

SIMPLE_CONVERTER_GRAMMER(ToBits)

SIMPLE_CONVERTER_GRAMMER(Htons)

SIMPLE_CONVERTER_GRAMMER(Ntohs)

SIMPLE_CONVERTER_GRAMMER(Htonl)

SIMPLE_CONVERTER_GRAMMER(Ntohl)

SIMPLE_CONVERTER_GRAMMER(ToLower)

SIMPLE_CONVERTER_GRAMMER(ToUpper)

SIMPLE_CONVERTER_GRAMMER(Caseless)

SIMPLE_CONVERTER_GRAMMER(Skipped)

SIMPLE_CONVERTER_GRAMMER(Weak)

SIMPLE_CONVERTER_GRAMMER(Strong)

SIMPLE_CONVERTER_GRAMMER(Binary)

SIMPLE_CONVERTER_GRAMMER(Traced)

SIMPLE_CONVERTER_GRAMMER(Restart)

Parser *NamedG			= Call(string_and_parser_ctor, Named, Sequence(
							Skipped(Literal("Named")),
							REQ_OPEN,
							OnFail(ec, reporter, "expected format string constant", String()), 	// TODO: accept identifiers too
							REQ_COMMA, 
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_CLOSE,
						0L));

Parser *ElementOfG		= Call(elementof_ctor, all_dicts, Sequence(
							Skipped(Literal("ElementOf")),
							REQ_OPEN,
							OnFail(ec, reporter, "dictionary required", 
								ElementOf(all_dicts)
							),
							REQ_CLOSE,
						0L));

Parser *AddToDictG		= Call(addtodict_ctor, all_dicts, Sequence(
							Skipped(Literal("AddToDict")),
							REQ_OPEN,
							OnFail(ec, reporter, "dictionary required", 
								ElementOf(all_dicts)
							),
							REQ_COMMA, 
							OnFail(ec, reporter, "invalid parser definition", Ref(parserG)), 
							REQ_CLOSE,
						0L));

Parser *DeclaredP		= Call(declared_parser_ctor, parser_by_name_storage, ElementOf(parser_by_name_storage));

Parser *flaggers			= Or(
								CaselessG,
								SkippedG,
								WeakG,
								StrongG,
								BinaryG,
								TracedG,
								NamedG,
								RestartG,
							0L);

Parser *basic_parsers		= Or(
								LiteralG, 
								WordG, 
								StringG, 
								EolG, 
								BytesG, 
								IntXG, 
								Base10G,
								BaseXG,
								CaptureUntilG,
								ElementOfG,
							0L);

Parser *converters			= Or(
								ToByteG,
								ToIntG,
								ToShortG,
								ToLongG,
								ToDoubleG,
								ToStrG,
								ToPtrG,
								ToBitsG,
								HtonsG,
								NtohsG,
								HtonlG,
								NtohlG,
								ToLowerG,
								ToUpperG,
								SubstG,
								ToIntBG,
								ToShortBG,
								ToLongBG,
							0L);

Parser *composite_parsers	= Or(
								ForwardDeclarationG,
								ForwardAssignG,
								SequenceG, 
								OrG, 
								MatchAllG, 
								ChainG, 
								RepeatG, 
								RepeatShortG, 
								RefG,
								ForeignG,
								NotG,
								LenG,
							0L);

Parser *processing_parsers	= Or(
								NthG,
								RecordG,
								PackG,
								EmitG,
								PrintG,
								AddToDictG,
							0L);

Parser *all_parser			= Or(
								composite_parsers,
								basic_parsers,
								processing_parsers,
								converters,
								flaggers,
								DeclaredP,
							0L);

	ForwardAssign(parserG, all_parser);

	return parserG;
}

// ---------------------------------------------------------------------------------------------

static int create_dict(void *dict, char **from, char **to)
{
Region r = INIT_REGION(*from, *to);
Dict *new_dict = new_list_dict();

	assert(dict);
	((Dict *) dict)->add(dict, &r, new_dict);

	return EDIT_OK;
}

Parser *cparsing_dict_declaration(ErrorContext *ec, error_function reporter, Dict *all_dict)
{
Parser *IdentifierG	=	Pack(Binary(Sequence(Weak(Word(ALPHA, 1, 1)), Optional(Word(ALPHANUM, 1, -1)), 0L)));
Parser *id_rec		=	Record(IdentifierG);
Parser *dict_decl	=	Sequence(
							Chain(
								Sequence(
									Skipped(Literal("Dict")),
									Skipped(Literal("*")),
									Restart(Ref(id_rec)),
									Skipped(Literal("=")),
									Skipped(CaptureUntil(INCLUDE_MATCH, Literal(";"))), // too weak, but sufficient for now
								0L),
								Not(ElementOf(all_dict))
							),
							Edit(create_dict, all_dict, id_rec),
						0L);
	return dict_decl;
}

static int perform_parser_assignment(void *vdata, InterpreterInfo I, Parser *data_source)
{
Region ident_region, parser_region;
char *parser_ident = 0;
Dict *d = vdata;

	assert(d);
	assert(data_source);

	if(!record_get_nth(data_source, 0, &ident_region))
		return PARSER_FAILED;		// should not happen - guaranteed by grammar

	parser_ident = parse_ftdup(ident_region.begin, ident_region.end);

	// is it a redefinition ?
	if(dict_lookup(d, parser_ident, 0))
		return PARSER_FAILED;		// New Fail - TODO: report this error

	if(!record_get_nth(data_source, 1, &parser_region))
		return PARSER_FAILED;		// should not happen - guaranteed by grammar

	{
	Region r = INIT_STR_REGION(parser_ident);
		d->add(d, &r, parser_region.begin);
		return PARSER_MATCH;
	}

	// dump_data_source(data_source);	
	return PARSER_MATCH;
}

Parser *cparsing_basic_parser_definition(ErrorContext *ec, error_function reporter, Dict *parser_by_name_storage, Dict *all_dict)
{
Parser *result;
Parser *parser_grammer = cparsing_basic_parser_grammar(ec, reporter, parser_by_name_storage, all_dict);
Parser *dict_decl_grammer = cparsing_dict_declaration(ec, reporter, all_dict);

	//TODO:	   
	result =	OnFail(ec, reporter, "parser redefinition", Call(perform_parser_assignment, parser_by_name_storage, Sequence(
					Optional(Skipped(Sequence(Literal("Parser"), Literal("*"), 0L))),
					OnFail(ec, reporter, "invalid identifier name", 
						ToLower(Caseless(Word(ALPHANUM, 1, -1)))
					), 
					OnFail(ec, reporter, "Expected assignment",
						Skipped(Literal("="))
					), 
					parser_grammer, 
					OnFail(ec, reporter, "colon missing", 
						COLON
					),
				0L))); 

	return OneOrMore(Or(dict_decl_grammer, result, 0L));
}


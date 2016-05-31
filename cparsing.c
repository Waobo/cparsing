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
// #include <sys/queue.h>

// --- Config ----------------------------------------------------------------
#define SET_PARSER_NAME
#define SHOW_TRACE

// ---------------------------------------------------------------------------

#include "scope_malloc.h"
#define FROM_CPARSING_BODY
#include "cparsing.h"
#undef FROM_CPARSING_BODY
#include "cparsing_impl.h"

#include "cparsing_dict.h"

// Trace parsers processing:
// absolutely not threadsafe - undef SHOW_TRACE to get rid of it
#ifdef SHOW_TRACE
int trace_indent = 0;
#endif

// --- Allocation ------------------------------------------------------------

// Having dynamically created deep structures with parse-results,
// we ease memory handling using allocation pools.
typedef struct
{
	void *runtime_pool;			// (thread specific) runtime -aka while performing parse() operation- memory alloction pool
	Parser *spaces;				// std whitespace parser
	Parser *custom_whitespace;	// current user-defined whitespace
} CParsingEnvironment;

typedef struct
{
	Parser 			*first_parser;
	Parser 			*second_parser;
} ParserPairParam;

// Multithreading is supported for (POSIX) pthreads only
#ifdef CPARSING_MULTI_THREADED
static pthread_key_t thread_local_storage_key;
static pthread_mutex_t local_storage_setup_mutex = PTHREAD_MUTEX_INITIALIZER;
#else
static CParsingEnvironment *single_thread_environment = NULL;
#endif

static int pool_init_complete = 0;	// since easy-mode is wanted, we'll do auto-init

static CParsingEnvironment *new_environment(void)
{
CParsingEnvironment *env = (CParsingEnvironment *) calloc(1, sizeof(CParsingEnvironment));

	assert("out of memory" && env);
	if(env) env->runtime_pool = new_mem_pool();

	return env;
}

static void free_thread_environment(void *deletee)
{
CParsingEnvironment *env = (CParsingEnvironment *) deletee;

	if(env)
	{
		if(env->runtime_pool) free_mem_pool(env->runtime_pool);
		if(env->spaces) env->spaces->destroy(env->spaces);
		if(env->custom_whitespace) env->custom_whitespace->destroy(env->custom_whitespace);
		free(env);
	}
}

// ifdef _POSIX_THREADS ?
#ifdef CPARSING_MULTI_THREADED

// setup thread-local storage-key
static int _init_thread_storage(void)
{
int rc;

	pthread_mutex_lock(&local_storage_setup_mutex);

	if(!pool_init_complete)
	{
		rc = pthread_key_create(&thread_local_storage_key, free_thread_environment);
		
		if(rc != 0)
		{
			perror("While trying to create thread specific storage:");
			pthread_mutex_unlock(&local_storage_setup_mutex);
			return 0;
		}
		pool_init_complete = 1;
	}

	pthread_mutex_unlock(&local_storage_setup_mutex);
	return 1;
}

// setup thread-local environment
static int _init_thread_environment(void)
{
CParsingEnvironment *env = new_environment();
int rc;

	if(!env) return 0;

	rc = pthread_setspecific(thread_local_storage_key, env);
	
	if(rc != 0)
	{
		perror("While trying to create thread environment:");
		return 0;
	}

	return 1;
}

// Accessor to environment allows for thread-safe access if needed
// returns NULL on error
CParsingEnvironment *get_env(void)
{
CParsingEnvironment *result;

	if(!pool_init_complete) 
	{
		if(!_init_thread_storage())	// first call - setup local storage key
			return NULL;
	}

	result = (CParsingEnvironment *) pthread_getspecific(thread_local_storage_key);

	if(result == NULL)	// first call in this thread
	{
		if(!_init_thread_environment())
			return NULL;

		result = (CParsingEnvironment *) pthread_getspecific(thread_local_storage_key);
		assert("thread local storage failed" && result);
	}
	return result;
}
#else // CPARSING_MULTI_THREADED
static void _init_simple_environment(void)
{
	assert(single_thread_environment == NULL);
	single_thread_environment = new_environment();
	pool_init_complete = 1;
}

CParsingEnvironment *get_env(void)
{
	if(!pool_init_complete) _init_simple_environment();
	return single_thread_environment;
}
#endif // CPARSING_MULTI_THREADED

void *runtime_pool(void)
{
CParsingEnvironment *env = get_env();

	return env ? env->runtime_pool : NULL;
}

void *detach_runtime_pool(void)
{
CParsingEnvironment *env = get_env();
void *pool = NULL;

	assert(env);

	if(env)
	{
		pool = env->runtime_pool;
		env->runtime_pool = new_mem_pool();
	}

	return pool;
}

void restore_runtime_pool(void *pool)
{
CParsingEnvironment *env = get_env();

	assert(pool && env && env->runtime_pool);

	// we need to free this pool before it is forgotten
	if(env && pool)
	{
		if(env->runtime_pool) free_mem_pool(env->runtime_pool);
		env->runtime_pool = pool;
	}
}

void clear_runtime_pool(void)
{
CParsingEnvironment *env = get_env();

	assert(env && env->runtime_pool);

	if(env && env->runtime_pool)
		free_mem_pool(env->runtime_pool);
}

void cparsing_cleanup(void)
{
CParsingEnvironment *env = get_env();

	if(env)
	{
		free_thread_environment(env);

#ifdef CPARSING_MULTI_THREADED
		// This is linux specific - if gettid() is not available on your system,
		// you need to save the result from pthread_self() when called by the main-thread and
		// compare that value to a call of pthread_self() here: if they are equal, the pthread_key
		// has to be deleted. I.e change the if-statement to:
		// 
		// if(main_pthread_self == pthread_self())
		// ...

		if(getpid() == syscall(SYS_gettid))
		{
		int rc;

			rc = pthread_key_delete(thread_local_storage_key);
			if(rc != 0)
				perror("thread-local storage-key could not be released:");
		}
#else
		single_thread_environment = 0;	
#endif
	}
}

// ---------------------------------------------------------------------------
// This is C, so there probably are no inline functions - and even those can not declare local vars.
// Thats why we're gonna use macros for a few times instead.

// Normal notification routine: invoked when finishing a parser to signal end-of-token to our interpreter
#define PARSER_STD_NOTIFY_END	if(I.interpreter)\
    {\
        if(result)\
        {\
            result = I.interpreter(self, I.param, TOKEN_COMPLETED, input->current, input->end);\
			if((self->flags & TRACED) != 0) print("%*s%s OK\n", trace_indent, " ", self->name);\
        } else {\
            I.interpreter(self, I.param, TOKEN_FAILED, input->current, input->end);\
			if((self->flags & TRACED) != 0) print("%*s%s FAILED\n", trace_indent, " ", self->name);\
        }\
    }

// the effective parsing-flags -while associated with the parser- are propagated using the 
// (call-by-value) interpreter; this allows for local overwrites while preserving those 
// previously set in other scopes
// => clear not-propagated, internal and masked flags, then add all local flags and enlarge mask to keep newly propagated flags
#define ADJUST_FLAGS { I.flags &= I.flags_mask; I.flags &= ~(self->flags_mask | 0xff00); I.flags |= self->flags; I.flags_mask |= (self->flags_mask | 0xff00); }

#define TRACE_REMAINDER_LEN 20

#define DEPTH_CHECK { if(++I.call_depth > CPARSING_MAX_CALL_DEPTH) { printf("ERROR: MAXIMUM CALLDEPTH REACHED.\n"); return PARSER_FAILED; } }

#ifdef SHOW_TRACE
// a tree-like dump of calls and matches; check call depth
#define TRACE_CORE	if((self->flags & TRACED) != 0) {\
		print("%*sEnter %s |", trace_indent, " ", self->name);\
		if(input->end > input->current + TRACE_REMAINDER_LEN) { print_ft(input->current, input->current + TRACE_REMAINDER_LEN); }\
		else { print_ft(input->current, input->end); }\
		printf("|\n");\
	}

#ifdef CPARSING_MAX_CALL_DEPTH 
#define TRACE_ENTER {\
		DEPTH_CHECK\
		/* { printf("[%03d] ", I.call_depth); }*/ \
		TRACE_CORE\
}
#else // CPARSING_MAX_CALL_DEPTH
#define TRACE_ENTER { TRACE_CORE }
#endif // CPARSING_MAX_CALL_DEPTH

#define TRACE_INC	{ ++trace_indent; }
#define TRACE_DEC	{ --trace_indent; }

#else // SHOW_TRACE

#ifdef CPARSING_MAX_CALL_DEPTH 
#define TRACE_ENTER DEPTH_CHECK
#else // CPARSING_MAX_CALL_DEPTH
#define TRACE_ENTER ;
#endif // CPARSING_MAX_CALL_DEPTH

#define TRACE_INC ;
#define TRACE_DEC ;

#endif // SHOW_TRACE

// ---------------------------------------------------------------------------

const InterpreterInfo I0 = {
	0, 0, 0, 0
#ifdef CPARSING_MAX_CALL_DEPTH
	,0
#endif	
};

// --- Helper ----------------------------------------------------------------

static Parser *parser(void)
{
Parser *result	= (Parser *) calloc(1, sizeof(Parser));

    assert("Out of memory" && result);

    if(!result)
	{
		perror(NULL);
		return NULL;
	}

    list_init(result);
    result->refcount = 1;
    return result;
}

void free_parser(Parser *p)
{
	assert(p);
	if(p) p->destroy(p);
}

static int invalid_parse_function(struct Parser *self, InterpreterInfo I, Region *input)
{
    assert(self);
    assert(input);
    assert("ERROR invalid parse function." && 1 == 0);
	return 0;
}

int unimplemented_forward_declaration(struct Parser *self, InterpreterInfo I, Region *input)
{
    assert(self);
    assert(input);
    assert("ERROR unimplemented forward declaration." && 1 == 0);
	return 0;
}

static void invalid_destroy_function(struct Parser *self)
{
    assert(self);
    assert("ERROR invalid dtor." && 1 == 0);
}

// ---------------------------------------------------------------------------

long ft_to_long(char *f, char *t, int base)
{
    return strtol(f, 0L, base);
}

typedef struct
{
	Parser *parser;
} parser_reference;

int _get_nth_token(void *target, size_t target_size, Editor e, void *eparam, int n, Parser *source)
{
Parser *holder;
Parser *getter;
Region dummy_region = INIT_STR_REGION("");
int ok = 0;

	if(e)
		getter = _Put(target, target_size, Edit(e, eparam, holder = Nth(0, source)));
	else
		getter = _Put(target, target_size, holder = Nth(0, source));

	if(getter)
		ok = getter->parse(getter, I0, &dummy_region);

	if(holder && holder->param) ((parser_reference *) holder->param)->parser = 0;
	if(getter) getter->destroy(getter);

	return ok;
}


// ---------------------------------------------------------------------------
// --- Generic dtors ---------------------------------------------------------

// ---------------------------------------------------------------------------

static void destroy_simple_parser(Parser *deletee)
{
    assert(deletee);
    if(deletee && (--deletee->refcount == 0))
        free(deletee);
}

static void destroy_parser_and_param(Parser *deletee)
{
    assert(deletee);
	if(!deletee) return;

	if(deletee->param)
	{
		free(deletee->param);
		deletee->param = 0L;
	}

    if(--deletee->refcount == 0)
        free(deletee);
}

// A parser using one other parser can put that reference at the first position
// of its member and thus reuse a common destructor
typedef struct
{
	Parser *refered_parser;
} ParserParamWithParserReference;

static void destroy_param_and_referred_parser(Parser *deletee)
{
ParserParamWithParserReference *param;
Parser *proxied_parser;

    assert(deletee);
	if(!deletee) return;

    param = (ParserParamWithParserReference *) deletee->param;

	if(param)
	{
		proxied_parser = param->refered_parser;

		if(proxied_parser)
		{
			proxied_parser->destroy(proxied_parser);
			param->refered_parser = 0L;
		}

		free(param);
		deletee->param = 0L;
	}

    if(--deletee->refcount == 0)
		free(deletee);
}


static void call_parser_dtor(list *deletee)
{
Parser *p = (Parser *) deletee;

    if(p) free_parser(p);
}

static void destroy_parser_and_children(Parser *deletee)
{
list *children;

    assert(deletee);
	if(!deletee) return;

	if(--deletee->refcount == 0)
	{
		children = (list *) deletee->param;

		if(children)
		{
			deletee->param = 0;
			list_clear(children, call_parser_dtor);
			free(children);
		}

        free(deletee);
	}
}

static void destroy_with_param_as_parser(Parser *deletee)
{
parser_reference *ref;

    assert(deletee);
	if(!deletee) return;

	ref = (parser_reference *) deletee->param;

	if(ref) 
	{
		if(ref->parser)
			ref->parser->destroy(ref->parser);

		deletee->param = 0;
	}

    if(--deletee->refcount == 0)
        free(deletee);
}

static void destroy_ParserPair(Parser *deletee)
{
ParserPairParam *param;

    assert(deletee);
	if(!deletee) return;

    param = (ParserPairParam *) deletee->param;

    if(param)
	{
		if(param->first_parser)
		{
			param->first_parser->destroy(param->first_parser);	
			param->first_parser = 0;
		}

		if(param->second_parser)
		{
			param->second_parser->destroy(param->second_parser);	
			param->second_parser = 0;
		}
		
		free(param);
		deletee->param = 0;
	}

    if(--deletee->refcount == 0)
		free(deletee);
}


// ---------------------------------------------------------------------------
// --- Set parser flags ------------------------------------------------------

Parser *Flagged(short flags, short mask, Parser *p)
{
    assert(p);
	if(!p) return NULL;

    p->flags |= (flags & 0xff);		// lower byte is user-flags
    p->flags_mask |= (mask & 0xff);
    return p;
}

Parser *Caseless(Parser *p)
{
    assert(p);
	if(!p) return NULL;

    p->flags |= IGNORE_CASE;
    p->flags_mask |= IGNORE_CASE;
    return p;
}

Parser *Skipped(Parser *p)
{
    assert(p);
	if(!p) return NULL;

    p->flags |= SKIP_TOKEN;
    p->flags_mask |= SKIP_TOKEN;
    // *NO* p->flags_mask |= SKIP_TOKEN; -> not propagated, because otherwise
	// embedded parsers would drop their potentially wanted results
    return p;
}

Parser *Weak(Parser *p)
{
    assert(p);
	if(!p) return NULL;

    p->flags |= WEAK_MAX;
    // *NO* p->flags_mask |= WEAK_MAX; -> not propagated; applies to current 
	// parser only
    return p;
}

Parser *Strong(Parser *p)
{
    assert(p);
	if(!p) return NULL;

    p->flags &= ~WEAK_MAX;
    // *NO* p->flags_mask |= WEAK_MAX; -> not propagated; applies to current 
	// parser only
    return p;
}

Parser *Binary(Parser *p)
{
	assert(p);
	if(!p) return NULL;

    p->flags |= DONT_SKIP_SPACES;
    p->flags_mask |= DONT_SKIP_SPACES;
	return p;
}

Parser *Named(char *name, Parser *p)
{
	assert(p);
	if(!p) return NULL;

    p->name = name;
	return p;
}

Parser *Traced(Parser *p)
{
	assert(p);
	if(!p) return NULL;

    p->flags |= TRACED;
    p->flags_mask |= TRACED;
	return p;
}

Parser *Restart(Parser *p)
{
    assert(p);
	if(!p) return NULL;

    p->flags |= RESTART;
    // *NO* p->flags_mask |= RESTART; -> not propagated; applies to current 
	// parser only
    return p;
}


// ---------------------------------------------------------------------------
// --- Primitive Parser ------------------------------------------------------

// --- Word and spacing  -----------------------------------------------------
// This impl is 8-bit specific - a future WWord may not ...

#define CHAR_SHIFT	8
#define CHAR_MASK	0xFF
#define LOOKUP_SIZE	256

// ---------------------------------------------------------------------------

typedef struct
{
    int min, max;
    char mapping[LOOKUP_SIZE];
} WordParam;

static int parse_Word(Parser *self, InterpreterInfo I, Region *input)
{
WordParam *p;
char *real_begin;
int result = PARSER_FAILED;
int cnt = 0;
char c;

    assert(self);                                                                                       // Needs self-reference
	// input must be valid so that we gain a valid start (aka we dont parse empty input)
    assert(input && input->begin && input->end && input->current && (input->begin <= input->current) && (input->current <= (input->end+1)) );

	if(!(self && input))
		return 0;

    p = (WordParam *) self->param;					// get our data

    assert(p);
	if(!p)
		return 0;

	ADJUST_FLAGS

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

    real_begin = input->current;

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, input->begin, input->end);

    while(1)
    {
        c = *input->current;

        if(I.flags & IGNORE_CASE)
            c = tolower(c);

        // if( (input->current > input->end) || (*input->current == 0) || (p->mapping[(int) c] == 0) )
        if( (input->current > input->end) || (p->mapping[(int) c] == 0) )
        {
            // at end of input and all match until now
            // was that enough?
            if(p->min == -1 || cnt >= p->min)
            {
                result = PARSER_MATCH;
                if((input->current != real_begin) && ((I.flags & SKIP_TOKEN) == 0))
                {
                    if(I.interpreter) I.interpreter(self, I.param, TOKEN_VALUE, real_begin, input->current-1);
                }
            }

            break;
        }

        if(p->max >= 0 && cnt >= p->max)
        {
			if(I.flags & WEAK_MAX)
			{
                result = PARSER_MATCH;
                if((input->current != real_begin) && ((I.flags & SKIP_TOKEN) == 0))
                {
                    if(I.interpreter) I.interpreter(self, I.param, TOKEN_VALUE, real_begin, input->current-1);
                }
			}
			
            // else: eek, too many
            break;
        }

        // well then all ok until now -> next step
        ++cnt;
        ++input->current;
    }

    PARSER_STD_NOTIFY_END   // notify interpreter of end-of-token

    return result;
}

Parser *Word(const char *character_pool, int min, int max)
{
Parser *result	= parser();
WordParam *p	= (WordParam *) calloc(1, sizeof(WordParam));
unsigned char *cp = (unsigned char *) character_pool;

    assert(character_pool);
    assert(p);
    assert("Word: minimum len must be > 0" && (min > 0));

	if(!(result && character_pool && p))
		return NULL;

    p->min = min;
    p->max = max;

    while(*cp)
    {
        p->mapping[*cp] = 1;
        ++cp;
    }

	PNAME("Word")

    result->parse	= parse_Word;
    result->destroy	= destroy_parser_and_param;
    result->param	= p;
	if(min < 0)
		result->flags |= OPTIONAL;
    return result;
}

Parser *CharNotIn(void *character_pool, int min, int max)
{
Parser *result = Word(character_pool, min, max);
WordParam *param;
int i;

	if(!result) return NULL;

	PNAME("CharNotIn")

	param	= (WordParam *) result->param;

	assert(param);
	if(!param) return NULL;

    for(i=0; i<LOOKUP_SIZE; ++i)
        param->mapping[i] = !param->mapping[i];

    return result;
}

// ---------------------------------------------------------------------------

Parser *spaces(void)
{
CParsingEnvironment *env = get_env();

    if(env->spaces == 0)
		env->spaces = Word(DEFAULT_WHITE_CHARS, 1, -1);

    return env->spaces;
}

#if 0
void free_spaces(void)
{
CParsingEnvironment *env = get_env();

	if(env->spaces) env->spaces->destroy(spaces);
	spaces = 0;
}
#endif

// ---------------------------------------------------------------------------

void set_whitespace_pattern(Parser *w)
{
CParsingEnvironment *env = get_env();

	assert(!w || ("whitespace pattern must be a Word()-parser" && w->parse == parse_Word));

	if(!w || w->parse != parse_Word)
	{
		printf("Error: set_whitespace_pattern(): whitespace pattern must be a Word()-parser\n");
		return;
	}

	if(env->custom_whitespace)
		env->custom_whitespace->destroy(env->custom_whitespace);

    env->custom_whitespace = w;
}

Region *skip_spaces(Region *r)
{
CParsingEnvironment *env = get_env();
WordParam *p;
int cnt = 0;

    p = (WordParam *) env->custom_whitespace ? env->custom_whitespace->param : spaces()->param;

    assert(r && r->begin && r->current && r->end);
    assert(p);

	if(!( r && r->begin && r->current && r->end && p))
		return NULL;

	if(r)
	{
		for(; (r->current <= r->end) && (*r->current != 0); ++r->current)
		{
			if((p->mapping[(int) *r->current] == 0) || (p->max > 0 && (cnt >= p->max)))
				break;

			++cnt;
		}
	}

    return r;
}

// ---------------------------------------------------------------------------

static int parse_Literal(Parser *self, InterpreterInfo I, Region *input)
{
int result = PARSER_FAILED;
const char *string_to_match;
char *real_begin;
char c;

    assert(self);                                                                                       // Needs self-reference
    assert(input && input->begin && input->end && input->current && (input->begin <= input->current) && (input->current <= (input->end+1)) );   // needs valid start (aka we dont parse empty input)

	if(!(input && input->begin && input->end && input->current && (input->begin <= input->current) && (input->current <= (input->end+1)) ))
		return 0;

    string_to_match = (const char *) self->param;	// get our template-string

    assert(string_to_match && *string_to_match);                       // we dont accept empty bottles

	if(!(string_to_match && *string_to_match))		// and thus empty pattern will fail
		return 0;

	ADJUST_FLAGS

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

    real_begin = input->current;

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, input->begin, input->end);

    while(1)
    {
        if(*string_to_match == 0)
        {
            // at the end, we were victorious
            result = PARSER_MATCH;

            if( (input->current != input->begin) && ( (I.flags & SKIP_TOKEN) == 0 ) )
            {
                if(I.interpreter) I.interpreter(self, I.param, TOKEN_VALUE, real_begin, input->current-1);
            }

            break;
        }

        // if((*input->current == 0) || (input->current > input->end))
        if(input->current > input->end)
        {
            // at end of the input, but chars left in pattern ... thats a fail
            break;
        }

        c = *input->current;

        if(I.flags & IGNORE_CASE)
            c = tolower(c);

        if(*string_to_match != c)
        {
            // chars left in both, but they dont match ... fail
            break;
        }

        // well then, all ok until now -> next step
        ++input->current;
        ++string_to_match;
    }

    PARSER_STD_NOTIFY_END   // notify interpreter of end-of-token

    return result;
}

Parser *Literal(char *string_to_match)
{
Parser *result;

	assert(string_to_match && *string_to_match);

	if(!(string_to_match && *string_to_match))
	{
		printf("Literal needs a valid pattern.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	PNAME(string_to_match)
    result->parse	= parse_Literal;
    result->destroy	= destroy_simple_parser;
    result->param	= string_to_match;
    return result;
}

// ---------------------------------------------------------------------------

static int parse_LiteralRegion(Parser *self, InterpreterInfo I, Region *input)
{
int result = PARSER_FAILED;
Region *region_to_match;
char *real_begin;
char *current_in_region;
char c;

    assert(self);                                                                                       // Needs self-reference
    assert(input && input->begin && input->end && input->current && (input->begin <= input->current) && (input->current <= (input->end+1)) );   // need valid start (aka we dont parse empty input)

	if(!(input && input->begin && input->end && input->current && (input->begin <= input->current) && (input->current <= (input->end+1)) ))
		return 0;

    region_to_match = (Region *) self->param;	// get our template-string

    assert(region_to_match);

	if(!region_to_match)		// and thus empty pattern will fail
		return 0;

	ADJUST_FLAGS

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

    real_begin = input->current;

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, input->begin, input->end);

	current_in_region = region_to_match->begin;
    while(1)
    {
        if(current_in_region > region_to_match->end)
        {
            // at the end, we were victorious
            result = PARSER_MATCH;

            if( (input->current != input->begin) && ( (I.flags & SKIP_TOKEN) == 0 ) )
            {
                if(I.interpreter) I.interpreter(self, I.param, TOKEN_VALUE, real_begin, input->current-1);
            }

            break;
        }

        // if((*input->current == 0) || (input->current > input->end))
        if(input->current > input->end)
        {
            // at end of the input, but chars left in pattern ... thats a fail
            break;
        }

        c = *input->current;

        if(I.flags & IGNORE_CASE)
            c = tolower(c);

        if(*current_in_region != c)
        {
            // chars left in both, but they dont match ... fail
            break;
        }

        // well then, all ok until now -> next step
        ++input->current;
        ++current_in_region;
    }

    PARSER_STD_NOTIFY_END   // notify interpreter of end-of-token

    return result;
}

Parser *LiteralRegion(Region *region_to_match)
{
Parser *result;

	assert(region_to_match);

	if(!region_to_match)
	{
		printf("LiteralRegion needs a valid region to compare against.\n");
		return NULL;
	}

	result = parser();

	PNAME("LiteralRegion")
    result->parse	= parse_LiteralRegion;
    result->destroy	= destroy_simple_parser;
    result->param	= region_to_match;
    return result;
}

// ---------------------------------------------------------------------------

#if 0
static void parse_String_unquote(char *f, char *t, char escape, Region *r)
{
char *act;
int len, escaped = 0;

	assert(f && t && (f <= t+1));	// accept empty input, but ..
	assert(r);

	r->begin = f;
	r->end = t;
	
	len = REGION_LEN(f,t);

	if(len)
	{
		// result can not be larger than the original
		act = r->begin = parse_malloc(len);

		for( ;f <= t; ++f)
		{
			if(*f == escape && !escaped)
			{
				escaped = 1; continue;	// just skip every new escape
			}
			escaped = 0;
			*act++ = *f;
		}

		r->end = act-1;	// because regions end 'on' the last char, not after it
	}
}
#endif

static int parse_String(Parser *self, InterpreterInfo I, Region *input)
{
char *real_begin;
int result = PARSER_FAILED;
int escaped = 0;
char quote, escape;

    assert(self);
    assert(input && input->begin && input->end && input->current && (input->begin <= input->current) && (input->current <= (input->end+1)) );

	if(!(self && input))
		return 0;

	quote	= (char) (((long) self->param) & CHAR_MASK);
	escape	= (char) ((((long) self->param) >> CHAR_SHIFT) & CHAR_MASK);
	assert(quote && escape);

	ADJUST_FLAGS

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, input->begin, input->end);

	// must start with a quote
	if(*input->current != quote)
		goto parse_String_out;

	++input->current;

    real_begin = input->current;

    for(; (*input->current != 0) || (input->current <= input->end); ++input->current)
    {
        if( *input->current == escape )
		{
			escaped = !escaped;
			continue;
		}

		if( !escaped && (*input->current == quote) )
		{
			result = PARSER_MATCH;
			++input->current;

			if((I.flags & SKIP_TOKEN) == 0)
			{
			/*
			Region unquoted;

				parse_String_unquote(real_begin, input->current-2, escape, &unquoted);	// -1, because current advanced; -1 because drop quote-char
			*/
				if(I.interpreter)
				{
					// printf("[["); print_ft(real_begin, input->current-2); printf("]]\n");
					I.interpreter(self, I.param, TOKEN_VALUE, real_begin, input->current-2);
					// I.interpreter(self, I.param, TOKEN_VALUE, unquoted.begin, unquoted.end);
				}

				break;
			}
		}

		escaped = 0;
    }

parse_String_out:
    PARSER_STD_NOTIFY_END   // notify interpreter of end-of-token

    return result;
}

Parser *StringSpec(char quote, char escape)
{
Parser *result		= parser();
long param;

	PNAME("String")

    result->parse	= parse_String;
    result->destroy	= destroy_simple_parser;
	param			= ((escape << CHAR_SHIFT) | quote);
    result->param	= (void *) param;

    return result;
}

Parser *String(void)
{
	return StringSpec('"', '\\');
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

int is_base2_symbol(char c, void *dummy_data)
{
    return ('0' == c) || ('1' == c);
}

int is_base10_symbol(char c, void *dummy_data)
{
    return ('0' <= c) && (c <= '9');
}

int is_base16_symbol(char c, void *dummy_data)
{
    return (('0' <= c) && (c <= '9')) || (('a' <= c) && (c <= 'f'));
}

// ---------------------------------------------------------------------------

typedef struct
{
    Parser                  *parser;
	InterpreterInfo			I;
    Region                  value;
    long                    min, max;
	int						flags, number_base;
} BaseParam;

static const int BaseParamFlag_is_signed        = 1;
static const int BaseParamFlag_first_skipped    = 2;

static int interpreter_BaseX(Parser *self, void *vparam, int mode, char *from, char *to)
{
BaseParam *param = (BaseParam *) vparam;
char *start;
int result = 1;

    assert(self);
    assert(param);

    switch(mode)
    {
    case TOKEN_ENTER:
        if(param->I.interpreter)
            param->I.interpreter(self, param->I.param, TOKEN_ENTER, from, to);

        break;

    case TOKEN_VALUE:
        param->value.begin    = from;
        param->value.current  = from;
        param->value.end      = to;

        start = param->value.begin;
        if(param->flags & BaseParamFlag_first_skipped)
            --start;

        if(param->I.interpreter)
            param->I.interpreter(self, param->I.param, TOKEN_VALUE, start, to);

        break;

    case TOKEN_COMPLETED:
        start = param->value.begin;
        if(param->flags & BaseParamFlag_first_skipped)
            --start;

        long number = ft_to_long(start, param->value.end, param->number_base);

        if( !((param->min <= number) && (number <= param->max)) )
		{
            result = 0;
		}

        // unusual end-of-token notification, so not the usual macro invocation
        if(param->I.interpreter)
        {
            if(result)
            {
                result = param->I.interpreter(self, param->I.param, TOKEN_COMPLETED, start, param->value.end);
            } else {
                param->I.interpreter(self, param->I.param, TOKEN_FAILED, start, param->value.end);
            }
        }
        break;

    case TOKEN_FAILED:
        start = param->value.begin;
        if(param->flags & BaseParamFlag_first_skipped)
            --start;

        if(param->I.interpreter)
            param->I.interpreter(self, param->I.param, TOKEN_FAILED, start, param->value.end);

        break;

    default:
        assert("Illegal interpreter mode." && 0);
    }

    return result;
}

static int parse_BaseX(Parser *self, InterpreterInfo I, Region *input)
{
InterpreterInfo II;
BaseParam *param;
int result = PARSER_FAILED;

    assert(self);
    assert(input && input->current);

	if(!(self && input))
		return result;

    param = (BaseParam *) self->param;
    assert(param && param->parser);

	if(!(param && param->parser))
		return result;

	ADJUST_FLAGS
	param->I = I;	// save interpreter info in param to sub-interpreter

    // scan for sign symbol on first char
	TRACE_ENTER

    if(input->current == input->begin)
    {
    int skip = 0;

        if((I.flags & DONT_SKIP_SPACES) == 0)
            skip_spaces(input);

        char symbol = *input->current;
        if( (param->flags & BaseParamFlag_is_signed) && (symbol == '-') )
            skip = 1;

        if(symbol == '+')
            skip = 1;

        if(skip)
        {
            ++input->current;
            param->flags |= BaseParamFlag_first_skipped;
        } else {
            param->flags &= ~BaseParamFlag_first_skipped;   // must be reset in case on further uses
        }
    }

    if(I.flags & IGNORE_CASE)
        param->parser->flags |= IGNORE_CASE;

	II = I;
	II.interpreter = interpreter_BaseX;
	II.param = param;

    result = param->parser->parse(param->parser, II, input);

	if( (I.flags & TRACED) != 0 )
		printf("%*s%s %s\n", trace_indent, " ", self->name, result ? "OK" : "Failed");

	return result;
}

// was: BaseParam *param    = (BaseParam *) calloc(1, sizeof(BaseParam));
#define PARSER_BASE_INTRO Parser *result      = parser();\
	BaseParam *param    = (BaseParam *) calloc(1, sizeof(BaseParam));\
    assert(param);\
	if(!param) return NULL;

#define PARSER_BASE_EGRESS     param->min              = min_value;\
    param->max              = max_value;\
    param->flags            = (is_signed ? BaseParamFlag_is_signed : 0);\
    result->param           = param;\
    result->parse           = parse_BaseX;\
    result->destroy         = destroy_baseX;\
    return result;

static void destroy_baseX(Parser *deletee)
{
BaseParam *param;

    assert(deletee);
	if(!deletee) return;

	param = (BaseParam *) deletee->param;

	if(param)
	{
		if(param->parser) 
		{
			param->parser->destroy(param->parser);
			param->parser = 0;
		}

        free(param);
		deletee->param = 0;
	}

    if(--deletee->refcount == 0)
        free(deletee);
}

Parser *Base2(int min_value, int max_value)
{
    PARSER_BASE_INTRO
    const int is_signed           = 0;

    // we could calculate min and max len using logX(), but that would require libm
    // without adding real benefits, so we just read greedy and discard in interpreter
    param->parser           = Word("01", 1, -1);
    param->number_base      = 2;

	PNAME("Base2")

    PARSER_BASE_EGRESS
}

Parser *Base10(int min_value, int max_value, int is_signed)
{
    PARSER_BASE_INTRO

    param->parser           = Word(DIGIT, 1, -1);
    param->number_base      = 10;

	PNAME("Base10")

    PARSER_BASE_EGRESS
}

Parser *Base16(int min_value, int max_value)
{
    PARSER_BASE_INTRO
    const int is_signed			= 0;

    param->parser			= Word(HEX, 1, -1);
    param->number_base		= 16;
    result->flags			|= IGNORE_CASE;
    result->flags_mask		|= IGNORE_CASE;

	PNAME("Base16")

    PARSER_BASE_EGRESS
}

// ---------------------------------------------------------------------------

static int parse_Eol(Parser *self, InterpreterInfo I, Region *input)
{
int result = PARSER_FAILED;

    assert(self);

	if(!(self && input))
		return 0;

	ADJUST_FLAGS

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, input->current, input->end);

    if( (input->current > input->end) || (*input->current == 0) || (*input->current == '\n') )
    {
        result = 1;
        if((I.flags & SKIP_TOKEN) == 0)
        {
            if(I.interpreter) I.interpreter(self, I.param, TOKEN_VALUE, input->current, input->end);
        }
		if((input->current < input->end) && (*input->current == '\n')) ++input->current;
    }

    PARSER_STD_NOTIFY_END   // notify interpreter of end-of-token

    return result;
}

Parser *Eol(void)
{
Parser *result = parser();

	PNAME("Eol")

    result->destroy		= destroy_simple_parser;
    result->parse		= parse_Eol;
    result->flags		|= (SKIP_TOKEN | ACCEPT_EMPTY_INPUT);

    return result;
}

// ---------------------------------------------------------------------------

typedef struct
{
	int			size;			// how long is one 'object'
	int			min;			// min and max in units of 'object'
	int			max;
} BytesParam;

static int parse_Bytes(Parser *self, InterpreterInfo I, Region *input)
{
BytesParam *param;
int cnt = 0;
int result = PARSER_FAILED;

    assert(self);
	assert(input && (input->current <= input->end));

	if(!(self && input))
		return 0;

    param = (BytesParam *) self->param;
    assert(param);

    ADJUST_FLAGS

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, input->current, input->end);

	while(1)
	{
		// using WEAK_MAX, there may be further matched occurances, but those are being ignored
		if((I.flags & WEAK_MAX) && (param->max > 0) && (cnt >= param->max))
			break;

        // to many occurances ? (in NON-WEAK_MAX mode)
        if( (param->max >= 0) && (cnt > param->max) )
            goto parse_Bytes_error_out;

        // at the end ?
        // if((*input->current == 0) || (input->current > input->end) )
        if(input->current > input->end) 
            break;

		// is an 'object' available?
		if( (input->end - input->current + 1) < param->size )
            break;

        if((I.flags & SKIP_TOKEN) == 0)
            if(I.interpreter) I.interpreter(self, I.param, TOKEN_VALUE, input->current, input->current+param->size-1);

		++cnt;
		input->current += param->size;
	}

    // at the end here ... was it enough?
    if(param->min < 0 || (cnt >= param->min))
        result = 1;

parse_Bytes_error_out:
	PARSER_STD_NOTIFY_END

	return result;
}


Parser *Bytes(int size, int min, int max)
{
Parser *result;
BytesParam *param;

	assert(size);
	assert(min || max);

	if(!(size && (min || max)))
	{
		printf("Bytes parser needs valid size and min/max definition.\n");
		return NULL;	
	}

	result = parser();
	param = (BytesParam *) malloc(sizeof(BytesParam));

	if(!(result && param)) return NULL;

	PNAME("Bytes")

	param->size			= size;
	param->min			= min;
	param->max			= max;

	result->parse		= parse_Bytes;
	result->destroy		= destroy_parser_and_param;
	result->param		= param;
	result->flags		|= (DONT_SKIP_SPACES + WEAK_MAX);	// we're looking at bytes, not chars (DONT_SKIP_SPACES) and there are usually more bytes comming (WEAK_MAX)
	result->flags_mask	|= (DONT_SKIP_SPACES);			// 'WEAK_MAX' is local-only property

	return result;
}

Parser *Int8(int min, int max)   { return Bytes(1, min, max); }
Parser *Int16(int min, int max)  { return Bytes(2, min, max); }
Parser *Int24(int min, int max)  { return Bytes(3, min, max); }
Parser *Int32(int min, int max)  { return Bytes(4, min, max); }
Parser *Int40(int min, int max)  { return Bytes(5, min, max); }
Parser *Int48(int min, int max)  { return Bytes(6, min, max); }
Parser *Int56(int min, int max)  { return Bytes(7, min, max); }
Parser *Int64(int min, int max)  { return Bytes(8, min, max); }
Parser *Int128(int min, int max) { return Bytes(16, min, max); }
Parser *Int256(int min, int max) { return Bytes(32, min, max); }

// ---------------------------------------------------------------------------

typedef struct
{
	min_max_size_getter getter;
	void *getter_param;
} DynBytesParam;

static int parse_DynBytes(Parser *self, InterpreterInfo I, Region *input)
{
Parser *bytes;
DynBytesParam *param;
MinMaxSize mms;
int result = PARSER_FAILED;

    assert(self);

	if(!(self && input))
		return result;

	param = self->param;
    assert(param && param->getter);
	if(!(param && param->getter))
		return result;
	
	mms = param->getter(param->getter_param);
	bytes = Bytes(mms.size, mms.min, mms.max);

	ADJUST_FLAGS

    if(mms.min <= 0)
        bytes->flags |= OPTIONAL;

	TRACE_ENTER

    result = bytes->parse(bytes, I, input);

	bytes->destroy(bytes);

    return result;
}

Parser *DynBytes(min_max_size_getter f, void *vparam)
{
Parser *result;
DynBytesParam *param;

	assert(f);
	if(!f)
	{
		printf("DynBytes parser needs valid min_max_size_getter.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	param = (DynBytesParam *) malloc(sizeof(DynBytesParam));
	assert(param);
	if(!param) return NULL;

	PNAME("DynBytes")

	param->getter		= f;
	param->getter_param	= vparam;

    result->param		= param;
    result->destroy		= destroy_parser_and_param;
    result->parse		= parse_DynBytes;

    return result;
}


// ---------------------------------------------------------------------------

#define MAX_BITFIELD_NUMBER	32

typedef struct	 // : ParserParamWithParserReference
{
    Parser			*parser;		// the given source
    InterpreterInfo	I;				// the original interpreter

	unsigned char	*current_field_data;			// pointer to the currently processed field's resulting data
	unsigned		current_field_byte_len;			// the len of the resulting data in bytes
	unsigned		current_field_bits_copied;		// the number of bits already copied

	unsigned		fault;
	unsigned		no_of_fields;					// the number of fields in the Bitfield-definition
	unsigned		field_index;					// the index of the field currently in process
	unsigned		fields[MAX_BITFIELD_NUMBER];	// array of field-lengths from call-parameters	// need more? use nested BFs...
} BitfieldParam;


BitfieldParam *make_BitfieldParam(Parser *source, unsigned no_of_fields, unsigned fields[])
{
BitfieldParam *result = (BitfieldParam *) malloc(sizeof(BitfieldParam));

	assert(result);
	if(!result)
	{
		printf("Out of memory while trying to allocate BitfieldParam.\n");
		return 0L;
	}

	result->parser = source;
	result->no_of_fields = no_of_fields;
	result->field_index = 0;
	result->fault = 0;
	result->current_field_data = 0L;
	result->current_field_byte_len = 0;
	result->current_field_bits_copied = 0;
	memcpy(result->fields, fields, sizeof(unsigned)*no_of_fields);

	return result;
}

static void add_Bitfield_bit(BitfieldParam *p, char bit_value)
{
unsigned i;

	for(i = 0; i < p->current_field_byte_len; ++i)
	{
		p->current_field_data[i] <<= 1;

		if(i < p->current_field_byte_len-1)
			p->current_field_data[i] |= ((p->current_field_data[i+1] & 0x80) ? 1 : 0);
		else
			p->current_field_data[i] |= (bit_value ? 1 : 0);

	}
	++p->current_field_bits_copied;
}

static int interpreter_Bitfield(Parser *self, void *vparam, int mode, char *from, char *to)
{
BitfieldParam *param = (BitfieldParam *) vparam;

    assert(self);
    assert(param && param->parser);

	if(!(self && param && param->parser && from && to))
		return PARSER_FAILED;

    switch(mode)
    {
    case TOKEN_ENTER:
        if(param->I.interpreter)
            param->I.interpreter(self, param->I.param, TOKEN_ENTER, from, to);

        return 1;

    case TOKEN_VALUE:
		{
		unsigned source_bit_index = 0;
		unsigned bits_offered = 0;
			do 
			{
			unsigned rlen = REGION_LEN(from,to);
			unsigned bits_requested, bits_left;

				bits_offered = (rlen << 3) - source_bit_index;

				if(param->field_index >= param->no_of_fields)
				{
					param->fault = 1;
					return 0;	// Quietly ignore bogus data ??
				}

				bits_requested = param->fields[param->field_index];

				if(param->current_field_data == 0L)
				{
					unsigned bytes_required = 1 + ((bits_requested - 1) >> 3);

						param->current_field_data = (unsigned char *) parse_calloc(1, bytes_required);
						// TODO OOM check
						param->current_field_byte_len = bytes_required;	
						param->current_field_bits_copied = 0;
				}

				bits_left = bits_requested - param->current_field_bits_copied;

				// copy bits loop
				while(from <= to)
				{
					for( ; source_bit_index<8; ++source_bit_index)
					{
						if(!(bits_left && bits_offered))
							break;

						// shift the top-most bit into the current result
						add_Bitfield_bit(param, *from & (1 << (7-source_bit_index)));
						--bits_left;
						--bits_offered;
					}

					if( source_bit_index >= 8 )
					{
						source_bit_index = 0;
						++from;
					}

					if(!(bits_left && bits_offered))
						break;
				}

				// new field ready?
				if(!bits_left)
				{
					if(param->I.interpreter)
					{
						if((param->I.flags & SKIP_TOKEN) == 0)
							param->I.interpreter(self, param->I.param, TOKEN_VALUE, (char *) param->current_field_data, (char *) param->current_field_data+param->current_field_byte_len-1);
					}

					param->current_field_data = 0L;
					++param->field_index;
				}

				if(param->field_index >= param->no_of_fields)
				{
					if(!bits_offered)
						return 1;
					
					param->fault = 1;
					return 0;
				}

			} while(bits_offered);

			return 1;
		}

    case TOKEN_COMPLETED:
		if(param->I.interpreter)
		{
			if(param->fault || (param->field_index < param->no_of_fields))
			{
				param->fault = 1;
				param->I.interpreter(self, param->I.param, TOKEN_FAILED, from, to);
			} else {
				param->I.interpreter(self, param->I.param, TOKEN_COMPLETED, from, to);
			}
		}

        return (param->fault == 0);

    case TOKEN_FAILED:
        if(param->I.interpreter)
            param->I.interpreter(self, param->I.param, TOKEN_FAILED, from, to);

		return 0;

    default:
        assert("Illegal interpreter mode." && 0);
    }

    return 0;
}



static int parse_Bitfield(Parser *self, InterpreterInfo I, Region *input)
{
InterpreterInfo info;
BitfieldParam *param;
Parser *p;
char *real_begin;
int result = PARSER_FAILED;

    assert(self);
    assert(input);

	if(!(self && input))
		return 0;

    param = (BitfieldParam *) self->param;
    assert(param);

	if(!param)
		return 0;

	p = param->parser;
	assert(p);
	if(!p)
		return 0;

	ADJUST_FLAGS

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

    real_begin = input->current;

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, real_begin, input->end);

	TRACE_INC

	info					= I;
	param->I				= I;
	info.interpreter		= interpreter_Bitfield;
	info.param				= param;
	param->field_index	= 0;		// reset bitfield-param
	param->fault = 0;
	param->current_field_data = 0;
	param->current_field_byte_len = 0;
	param->current_field_bits_copied = 0;

    result = param->parser->parse(param->parser, info, input);

	TRACE_DEC

    PARSER_STD_NOTIFY_END   // notify interpreter of end-of-token

    return result;
}



Parser *Bitfield(Parser *source, unsigned bit_count, ...)
{
va_list argp;
Parser *result = 0L;

unsigned count;
unsigned sum = 0;
unsigned idx = 0;
unsigned tmp[MAX_BITFIELD_NUMBER];

	assert(source);
	assert(bit_count);

	va_start(argp, bit_count);

	count = bit_count;

	if(count)
	{
		do
		{
			if(idx >= MAX_BITFIELD_NUMBER)
			{
				printf("Maximum number of fields in one Bitfield is %d -- use nested Bitfields if you need more.\n", MAX_BITFIELD_NUMBER);
				goto Bitfield_out;
			}

			sum += count;			// vanity: sum % 8 == 0
			tmp[idx++] = count;
		} while( (count = va_arg(argp, unsigned)) != 0 );

		if( (sum & 0x07) != 0 )
		{
			printf("Bit-count of a Bitfield must add up to a multiple of 8.\n");
			goto Bitfield_out;
		}

		result = parser();
		if(!result)
			goto Bitfield_out;

		result->param = make_BitfieldParam(source, idx, tmp);
		if(!result->param)
		{
			free_parser(result);
			result = 0L;
			free_parser(source);
			goto Bitfield_out;
		}

		result->parse = parse_Bitfield;
		result->destroy = destroy_param_and_referred_parser;

		PNAME("Bitfield")

	} else printf("Bitfield requires at least one, non-zero length.\n");

Bitfield_out:
	va_end(argp);
	return result;
}



// ---------------------------------------------------------------------------

static int parse_ElementOf(Parser *self, InterpreterInfo I, Region *input)
{
Dict *dict;
int result = 0;
int current_match = 0, exact_match = 0;
char *real_begin;
char c;

    assert(self && self->param);
    assert(input && input->begin && input->end && input->current && (input->begin <= input->current) && (input->current <= (input->end+1)) );   // so that we gain a valid start (aka we dont parse empty input)

	if(!(input && input->begin && input->end && input->current && (input->begin <= input->current) && (input->current <= (input->end+1)) ))
		return 0;

    ADJUST_FLAGS

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

	TRACE_ENTER

	// remember start in input
    real_begin = input->current;

	// the dict holds the set of allowed Literals
	dict = (Dict *) self->param;

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, input->begin, input->end);

	dict->init_lookup(dict);
    while(1)
    {
		// and the end of input?
        // if((*input->current == 0) || (input->current > input->end))
        if(input->current > input->end)
		{
			// loop exit 1 of 2
			break;
		}

		// get the current char from input
        c = *input->current;

        if(I.flags & IGNORE_CASE)
            c = tolower(c);

		// still a match?
		current_match = dict->extend_pattern(dict, c);

		if(!current_match)
		{
			// loop exit 2 of 2
			break;	// nope, but here exact_match may still be set from the previous iteration
		}

		exact_match = dict->exact_match(dict);	// if this yields true, we've got a match
												// .. but maybe theres still a longer match
		// reinit
        ++input->current;
    }

	if(exact_match)
	{
		if(I.interpreter) I.interpreter(self, I.param, TOKEN_VALUE, real_begin, current_match ? input->current : input->current-1);
		result = 1;

		if(current_match && (input->current < input->end))
			++input->current;
	}

    PARSER_STD_NOTIFY_END   // notify interpreter of end-of-token

    return result;
}

Parser *ElementOf(Dict *dict)
{
Parser *result;

	assert(dict);

	if(!dict)
	{
		printf("ElementOf parser needs a valid dictionary.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	PNAME("ElementOf")

	result->parse	= parse_ElementOf;
	result->destroy	= destroy_simple_parser;
	result->param	= dict;

	return result;
}

// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// --- Combinatoric Parser ---------------------------------------------------

static void destroy_parser_list_header(Parser *deletee)
{
    assert(deletee);

	if(!deletee) return;

    if(--deletee->refcount == 0)
    {
        list_clear((list *) deletee, call_parser_dtor);
        free(deletee);
    }
}

// waste of a few bytes here by using a full parser-struct as a list-header
Parser *parser_list_header(void)
{
    Parser *result = parser();

	if(result)
	{
		result->parse = invalid_parse_function;
		result->destroy = destroy_parser_list_header;	// safety only: the param gets cleared in the container's dtor
	}

    return result;
}

// Common init sequence for container/combinator:
#define PARSER_CONTAINER_PARSE_INTRO int result = PARSER_FAILED, current_result = PARSER_FAILED;\
    Parser *p;\
    char *real_begin;\
    assert(self);\
    assert(self->param);\
    assert(input && input->begin && input->end && (input->begin <= input->end+1));\
    assert(input->current && (input->begin <= input->current) && (input->current <= (input->end+1)) );\
	if(!(self && input))\
		return PARSER_FAILED;\
	ADJUST_FLAGS

static int parse_Sequence(Parser *self, InterpreterInfo I, Region *input)
{
    PARSER_CONTAINER_PARSE_INTRO

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

    real_begin = input->current;

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, real_begin, input->end);

	TRACE_INC

	// sequence is match if each parser matches in order, but we have to take care
	// of optional entries
    list_for_each(Parser, p, ((list *) self->param))
    {
    Region view;

		// prepare current region
        init_region(&view, input->current, input->end);

        // if((input->current > input->end) || (*input->current == 0))
        if(input->current > input->end)
        {
			// at the end - but too early, if there are further non-optional parsers
            while(p != (Parser *) self->param)
            {
                if((p->flags & OPTIONAL) == 0)
				{
					if(p->flags & ACCEPT_EMPTY_INPUT)
					{
						if(0 == p->parse(p, I, &view))
							goto sequence_failed_out;
					} else
						goto sequence_failed_out;   // this one still needed, so we have a miss
				}

                p = p->next;
            }

            break;  // no one gets noisy - we have a match
        }

        current_result = p->parse(p, I, &view);

        if(!current_result)
            goto sequence_failed_out;

        input->current = view.current;

        // skip to next token
        if((I.flags & DONT_SKIP_SPACES) == 0)
            skip_spaces(input);
    }

	// all went well, so
    result = PARSER_MATCH;

sequence_failed_out:

	TRACE_DEC
    PARSER_STD_NOTIFY_END   // notify interpreter of end-of-token

    return result;
}

#if 0
#define PARSER_CONTAINER_INTRO va_list argp;\
Parser *p = 0;\
Parser *parser_list;\
    assert(p1);\
	if(!p1) return NULL;\
	parser_list = parser_list_header();\
	if(!parser_list) return NULL;\
    va_start(argp, p1);\
    p = p1;\
    do\
    {\
        list_append_end((list *) parser_list, (list *) p);\
    } while( (p = va_arg(argp, Parser *)) != 0L );\
	va_end(argp);

#define PARSER_CONTAINER_EGRESS result->flags		|= COMBINATOR;\
	result->destroy		= destroy_parser_and_children;\
    return result;
#endif

Parser *SequenceL(Parser *parser_list)
{
Parser *result;

	assert(parser_list && list_not_empty((list *) parser_list));
	if(!(parser_list && list_not_empty((list *) parser_list)))
	{
		printf("Sequence parser needs a valid parser list.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	PNAME("Sequence")

    result->parse	= parse_Sequence;
	result->param 	= parser_list;

    PARSER_CONTAINER_EGRESS
}

Parser *Sequence(Parser *p1, ...)
{
	PARSER_CONTAINER_INTRO
	return SequenceL(parser_list);
}


// ---------------------------------------------------------------------------

static int parse_Or(Parser *self, InterpreterInfo I, Region *input)
{
char *max_fail = 0;

    PARSER_CONTAINER_PARSE_INTRO

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

    real_begin = input->current;

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, real_begin, input->end);

	TRACE_INC

	max_fail = input->current;
    list_for_each(Parser, p, (list *) self->param)
    {
    Region view;

        init_region(&view, input->current, input->end);

        current_result = p->parse(p, I, &view);

        // report first match
        if(current_result)
        {
            input->current = view.current;
            result = 1;
            break;
        }

		if(input->current > max_fail)
			max_fail = input->current;
    }

	// report furthest parse-position on fail
	if(!result)
		input->current = max_fail;

	TRACE_DEC
    PARSER_STD_NOTIFY_END

    return result;
}

Parser *OrL(Parser *parser_list)
{
Parser *result;

	assert(parser_list && list_not_empty((list *) parser_list));
	if(!(parser_list && list_not_empty((list *) parser_list)))
	{
		printf("Or parser needs a valid parser list.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	PNAME("Or")

    result->parse	= parse_Or;
    result->param	= parser_list;

    PARSER_CONTAINER_EGRESS
}

Parser *Or(Parser *p1, ...)
{
    PARSER_CONTAINER_INTRO
	return OrL(parser_list);
}

// ---------------------------------------------------------------------------

static int parse_MatchAll(Parser *self, InterpreterInfo I, Region *input)
{
Region view = {0, 0, 0};	// prevent SCAM (statical code analyzer mutterings)

    PARSER_CONTAINER_PARSE_INTRO

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

    real_begin = input->current;

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, real_begin, input->end);

	TRACE_INC

    list_for_each(Parser, p, (list *) self->param)
    {
		// start each parser with the same input
        init_region(&view, real_begin, input->end);

        current_result = p->parse(p, I, &view);

        // if any parser fails, MatchAll fails
        if(current_result == 0)
			goto parse_MatchAll_fail;
    }
	
	// all matched, so MatchAll matched too..
	result = PARSER_MATCH;

	// .. and we update input's position to that of the last parser
	input->current = view.current;

parse_MatchAll_fail:

	TRACE_DEC
    PARSER_STD_NOTIFY_END

    return result;
}

Parser *MatchAllL(Parser *parser_list)
{
Parser *result;

	assert(parser_list && list_not_empty((list *) parser_list));
	if(!(parser_list && list_not_empty((list *) parser_list)))
	{
		printf("MatchAll parser needs a valid parser list.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	PNAME("MatchAll")

    result->parse	= parse_MatchAll;
    result->param	= parser_list;

    PARSER_CONTAINER_EGRESS
}

Parser *MatchAll(Parser *p1, ...)
{
    PARSER_CONTAINER_INTRO
	return MatchAllL(parser_list);
}

// ---------------------------------------------------------------------------


typedef struct // : ParserParamWithParserReference
{
    Parser			*source;	// the given consumer
	Parser			*receiver;
	Parser			*current_receiver;
	InterpreterInfo	I;			// the original interpreter
	int				fault;
} DistributeParam;

static DistributeParam *make_Distribute_param(Parser *source, Parser *receiver)
{
DistributeParam *result = (DistributeParam *) calloc(1, sizeof(DistributeParam));

	assert(result);
	if(result)
	{
		result->source = source;
		result->receiver = receiver;
	}

	return result;
}

static void destroy_Distribute(Parser *deletee)
{
DistributeParam *param;
list *children;

    assert(deletee);
	if(!deletee) return;

	if(--deletee->refcount == 0)
	{
		param = (DistributeParam *) deletee->param;

		if(param)
		{
			if(param->source)
			{
				free_parser(param->source);
				param->source = 0L;
			}

			children = (list *) param->receiver;

			if(children)
			{
				param->receiver = 0;
				list_clear(children, call_parser_dtor);
				free(children);
			}

			free(param);
			deletee->param = 0L;
		}
		free(deletee);
	}
}

static int interpreter_Distribute(Parser *self, void *vparam, int mode, char *from, char *to)
{
DistributeParam *param = (DistributeParam *) vparam;

    assert(self);
    assert(param);
    assert(from && to);

	if(!(self && param && from && to))
		return PARSER_FAILED;

    switch(mode)
    {
    case TOKEN_ENTER:
        if(param->I.interpreter)
            param->I.interpreter(self, param->I.param, TOKEN_ENTER, from, to);

        return 1;

    case TOKEN_VALUE:
		{
			if(param->fault)
				return 0;

			if(param->current_receiver)
			{
			Region r;
			int result;

				init_region(&r, from, to);

				// call receiver, providing the token as input region and our parental interpreter as context
				result = param->current_receiver->parse(param->current_receiver, param->I, &r);

				// reinit for next TOKEN_VALUE
				param->current_receiver = ((param->current_receiver->next == param->receiver) ? 0 : param->current_receiver->next);

				if(!result)
					param->fault = 1;

				return result;
			} else {
				if(param->I.interpreter)
				{
					if((param->I.flags & SKIP_TOKEN) == 0)
						param->I.interpreter(self, param->I.param, TOKEN_VALUE, from, to);
				}

				return 1;
			}
		}

    case TOKEN_COMPLETED:
		if(param->I.interpreter)
		{
			if(param->fault || param->current_receiver)	// is there still a receiver waiting for input?
			{
				param->I.interpreter(self, param->I.param, TOKEN_FAILED, from, to);
				param->fault = 1;
			} else {
				param->I.interpreter(self, param->I.param, TOKEN_COMPLETED, from, to);
			}
		}

        return (param->fault == 0);

    case TOKEN_FAILED:
        if(param->I.interpreter)
            param->I.interpreter(self, param->I.param, TOKEN_FAILED, from, to);

		return 0;

    default:
        assert("Illegal interpreter mode." && 0);
    }

    return 0;
}


static int parse_Distribute(Parser *self, InterpreterInfo I, Region *input)
{
InterpreterInfo info;
DistributeParam *param;
Parser *p;
char *real_begin;
int result = PARSER_FAILED;

    assert(self);
    assert(input);

	if(!(self && input))
		return 0;

    param = (DistributeParam *) self->param;
    assert(param && param->source && param->receiver);

	if(!(param && param->source && param->receiver))
		return 0;

	p = param->source;
	assert(p);
	if(!p) return 0;

	ADJUST_FLAGS

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

    real_begin = input->current;

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, real_begin, input->end);

	TRACE_INC

	info				= I;
	param->I			= I;
	info.interpreter	= interpreter_Distribute;
	info.param			= param;
	param->current_receiver	= param->receiver->next;
	param->fault		= 0;

    result = param->source->parse(param->source, info, input);

	TRACE_DEC

    PARSER_STD_NOTIFY_END   // notify interpreter of end-of-token

    return result;
}

Parser *DistributeL(Parser *source, Parser *parser_list)
{
Parser *result;
DistributeParam *param;

	assert(source);
	if(!source)
	{
		printf("Distribute parser needs a valid source.\n");
		return NULL;
	}

	assert(parser_list && list_not_empty((list *) parser_list));
	if(!(parser_list && list_not_empty((list *) parser_list)))
	{
		printf("Distribute parser needs a valid parser list.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	PNAME("Distribute")

	param = make_Distribute_param(source, parser_list);
	if(!param) return NULL;

    result->parse	= parse_Distribute;
    result->param	= param;

	result->flags		|= COMBINATOR;
	result->destroy		= destroy_Distribute;
    return result;
}

Parser *Distribute(Parser *source, Parser *p1, ...)
{
    PARSER_CONTAINER_INTRO
	return DistributeL(source, parser_list);
}

// ---------------------------------------------------------------------------

static int parse_Not(Parser *self, InterpreterInfo I, Region *input)
{
Parser *p;
Region view;

	assert(self);
	assert(input);

	p = (Parser *) self->param;
	assert(p);

	init_region(&view, input->current, input->end);

	return !p->parse(p, I0, &view);
}

Parser *Not(Parser *p)
{
Parser *result;

    assert(p);
	if(!p)
	{
		printf("Not parser needs a valid parser as its parameter.\n");
		return NULL;
	}

	result			= parser();
	if(!result) return NULL;

	PNAME("Not")

    result->parse	= parse_Not;
	result->param	= p;
    result->destroy = destroy_with_param_as_parser;
	result->flags	|= ACCEPT_EMPTY_INPUT;

    return result;
}

// ---------------------------------------------------------------------------

typedef struct // : ParserParamWithParserReference
{
    Parser *parser;
    int		min;
    int		max;
} ParserAndRangeParam;

static ParserAndRangeParam *parser_and_range_param(Parser *p, int min, int max)
{
ParserAndRangeParam *result = (ParserAndRangeParam *) malloc(sizeof(ParserAndRangeParam));

    assert(result);
	if(!result) return NULL;

    result->parser	= p;

    result->min		= min;
    result->max		= max;

    return result;
}

static int parse_Repeat(Parser *self, InterpreterInfo I, Region *input)
{
ParserAndRangeParam *param;
Parser *p;
char *real_begin;
int result = 0;
int current_result;
int cnt = 0;

    assert(self);
	if(!self) return PARSER_FAILED;

    param = (ParserAndRangeParam *) self->param;
    assert(param);
	if(!param) return PARSER_FAILED;

	p = param->parser;
	assert(p);
	if(!p) return PARSER_FAILED;

	ADJUST_FLAGS

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

    real_begin = input->current;

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, real_begin, input->end);

	TRACE_INC

    while(1)
    {
    Region view;

		// using WEAK_MAX, there may be further matched occurances, but those are being ignored
		if((I.flags & WEAK_MAX) && cnt >= param->max)
			break;

        init_region(&view, input->current, input->end);

        // to many occurances ? (in NON-WEAK_MAX mode)
        if( (param->max >= 0) && (cnt > param->max) )
        {
            goto parse_Repeat_error_out;
        }

        // at the end ?
        // if((input->current > input->end) || (*input->current == 0))
        if(input->current > input->end)
            break;

        current_result = p->parse(param->parser, I, &view);

        if(current_result)
        {
            // count that match:
            ++cnt;

            // skip to next token
            if((I.flags & DONT_SKIP_SPACES) == 0)
                skip_spaces(&view);

            input->current = view.current;
        } else {
            break;
        }
    }

    // at the end here ... was it enough?
    if(cnt >= param->min)
        result = 1;

parse_Repeat_error_out:

	TRACE_DEC

    PARSER_STD_NOTIFY_END   // notify interpreter of end-of-token

    return result;
}

Parser *Repeat(Parser *p, int min, int max)
{
Parser *result;
ParserAndRangeParam *param;

    assert(p);
	if(!p)
	{
		printf("Repeat parser needs a valid parser as its parameter.\n");
		return NULL;
	}

	result		= parser();
	if(!result) return NULL;

	param		= parser_and_range_param(p, min, max);
	if(!param)
	{
		free(result);
		return NULL;
	}

	PNAME("Repeat")

    result->parse	= parse_Repeat;
    // result->destroy = destroy_Repeat;
    result->destroy = destroy_param_and_referred_parser;

    result->param	= param;

    if(min <= 0)
        result->flags |= OPTIONAL;

    return result;
}

Parser *Optional(Parser *p)
{
Parser *result = Repeat(p, 0, 1);

	if(!result) return NULL;

	PNAME("Optional")

	result->flags |= WEAK_MAX;
	// result->flags_mask |= WEAK_MAX;
    return result;
}

Parser *OneOrMore(Parser *p)
{
Parser *result = Repeat(p, 1, -1);

	if(!result) return NULL;

	PNAME("OneOrMore")

    return result;
}

Parser *ZeroOrMore(Parser *p)
{
Parser *result = Repeat(p, 0, -1);

	if(!result) return NULL;

	PNAME("ZeroOrMore")

    return result;
}

// -----------------------------------------------------------------

static int parse_Until(Parser *self, InterpreterInfo I, Region *input)
{
ParserPairParam *param;
Parser *mark, *consumer;
char *real_begin;
int result = PARSER_FAILED;
int current_result;

    assert(self);
	if(!self) return PARSER_FAILED;

    param = (ParserPairParam *) self->param;
    assert(param);
	if(!param) return PARSER_FAILED;

	mark = param->first_parser;
	assert(mark);
	if(!mark) return PARSER_FAILED;

	consumer = param->second_parser;
	assert(consumer);
	if(!consumer) return PARSER_FAILED;

	ADJUST_FLAGS

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

    real_begin = input->current;

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, real_begin, input->end);

	TRACE_INC

    while(1)
    {
    Region view;
	int mark_found;

		// skip to next token
		if((I.flags & DONT_SKIP_SPACES) == 0)
			skip_spaces(input);
		
        // at the end without hitting 'mark'
        // if((input->current > input->end) || (*input->current == 0))
        if(input->current > input->end)
            break;

        init_region(&view, input->current, input->end);
		mark_found = mark->parse(mark, I0, &view);

		if(mark_found)
		{
			result = PARSER_MATCH;
			break;
		}

        current_result = consumer->parse(consumer, I, &view);

        if(current_result)
        {
            input->current = view.current;
        } else {
            break;
        }
    }

	TRACE_DEC

    PARSER_STD_NOTIFY_END   // notify interpreter of end-of-token

    return result;
}

Parser *Until(Parser *mark, Parser *consumer)
{
Parser *result;
ParserPairParam *param;

    assert(mark);
	if(!mark)
	{
		printf("Repeat parser needs a valid parser as its mark.\n");
		return NULL;
	}

    assert(consumer);
	if(!consumer)
	{
		printf("Repeat parser needs a valid parser as its consumer.\n");
		return NULL;
	}

	result		= parser();
	if(!result) return NULL;

	param		= (ParserPairParam *) malloc(sizeof(ParserPairParam));
	if(!param)
	{
		free(result);
		return NULL;
	}

	PNAME("Until")

	param->first_parser		= Lookahead(mark);
	param->second_parser	= consumer;

    result->parse			= parse_Until;
    result->destroy 		= destroy_ParserPair;
    result->param			= param;

    return result;
}

// ---------------------------------------------------------------------------

typedef struct // : ParserParamWithParserReference
{
    Parser			*parser;	// the given consumer
    InterpreterInfo	I;			// the original interpreter
	int				min;
    int				max;
	int				fault;
} LenParam;


static int interpreter_Len(Parser *self, void *vparam, int mode, char *from, char *to)
{
LenParam *param = (LenParam *) vparam;

    assert(self);
    assert(param && param->parser);

	if(!(self && param && param->parser && from && to))
		return PARSER_FAILED;

    switch(mode)
    {
    case TOKEN_ENTER:
        if(param->I.interpreter)
            param->I.interpreter(self, param->I.param, TOKEN_ENTER, from, to);

        return 1;

    case TOKEN_VALUE:
		if(param->fault) 
			return 0;
		else
		{
		int rlen	= REGION_LEN(from,to);
		int min_ok	= ((param->min <= 0) || (param->min <= rlen));
		int max_ok	= ((param->max <= 0) || (rlen <= param->max));

			if(min_ok && max_ok)
			{
				if(param->I.interpreter)
				{
					if((param->I.flags & SKIP_TOKEN) == 0)
						param->I.interpreter(self, param->I.param, TOKEN_VALUE, from, to);
				}

				return 1;
			} else {
				param->fault = 1;
				return 0;
			}
		}

    case TOKEN_COMPLETED:
		if(param->I.interpreter)
		{
			if(param->fault)
			{
				param->I.interpreter(self, param->I.param, TOKEN_FAILED, from, to);
			} else {
				param->I.interpreter(self, param->I.param, TOKEN_COMPLETED, from, to);
			}
		}

        return (param->fault == 0);

    case TOKEN_FAILED:
        if(param->I.interpreter)
            param->I.interpreter(self, param->I.param, TOKEN_FAILED, from, to);

		return 0;

    default:
        assert("Illegal interpreter mode." && 0);
    }

    return 0;
}


static int parse_Len(Parser *self, InterpreterInfo I, Region *input)
{
InterpreterInfo info;
LenParam *param;
Parser *p;
char *real_begin;
int result = PARSER_FAILED;

    assert(self);
    assert(input);

	if(!(self && input))
		return 0;

    param = (LenParam *) self->param;
    assert(param);

	if(!param)
		return 0;

	p = param->parser;
	assert(p);
	if(!p)
		return 0;

	ADJUST_FLAGS

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

    real_begin = input->current;

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, real_begin, input->end);

	TRACE_INC

	info				= I;
	param->I			= I;
	info.interpreter	= interpreter_Len;
	info.param			= param;
	param->fault		= 0;

    result = param->parser->parse(param->parser, info, input);

	TRACE_DEC

    PARSER_STD_NOTIFY_END   // notify interpreter of end-of-token

    return result;
}

static LenParam *len_param(Parser *p, int min, int max)
{
LenParam *result = calloc(1, sizeof(LenParam));

    assert(result);
	if(!result)
	{
		perror(NULL);
		return NULL;
	}

    result->parser	= p;

    result->min		= min;
    result->max		= max;

    return result;
}

Parser *Len(int min_len, int max_len, Parser *producer)
{
Parser *result;
LenParam *param;

    assert(producer);
	if(!producer)
	{
		printf("Len parser needs a valid parser as producer.\n");
		return NULL;
	}

	result	= parser();
	if(!result)
		return NULL;

	param	= len_param(producer, min_len, max_len);
	if(!param)
	{
		free(result);	
		return NULL;
	}

	PNAME("Len")

    result->parse	= parse_Len;
    result->destroy = destroy_param_and_referred_parser;

    result->param	= param;

    if(min_len <= 0)
        result->flags |= OPTIONAL;

    return result;
}

// ---------------------------------------------------------------------------

typedef struct
{
	Parser *parser;
	min_max_getter getter;
	void *getter_param;
} DynRepeatParam;

static int parse_DynRepeat(Parser *self, InterpreterInfo I, Region *input)
{
Parser *repeat;
DynRepeatParam *param;
MinMax mm;
int result = PARSER_FAILED;

    assert(self);
	if(!self) return result;

	param = self->param;
    assert(param && param->parser && param->getter);

	if(!(param && param->parser && param->getter))
		return result;
	
	mm = param->getter(param->getter_param);
	repeat = Repeat(param->parser, mm.min, mm.max);

	ADJUST_FLAGS

    if(mm.min <= 0)
        repeat->flags |= OPTIONAL;

	TRACE_ENTER

    result = repeat->parse(repeat, I, input);

	((parser_reference *) repeat->param)->parser = 0;
	repeat->destroy(repeat);

    return result;
}

Parser *DynRepeat(Parser *p, min_max_getter f, void *vparam)
{
Parser *result;
DynRepeatParam *param;

    assert(p);
	assert(f);

	if(!(p && f))
	{
		printf("DynRepeat parser needs valid parser and min_max_getter definition.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	param = (DynRepeatParam *) malloc(sizeof(DynRepeatParam));
	assert(param);
	if(!param)
	{
		free(result);
		return NULL;
	}

	PNAME("DynRepeat")

	param->parser		= p;
	param->getter		= f;
	param->getter_param	= vparam;

    result->param		= param;
    // result->destroy		= destroy_Repeat;
    result->destroy		= destroy_param_and_referred_parser;
    result->parse		= parse_DynRepeat;

    return result;
}

// ---------------------------------------------------------------------------

static int parse_Ref(Parser *self, InterpreterInfo I, Region *input)
{
Parser *proxied_parser;
int result = PARSER_FAILED;

    assert(self);
	if(!self) return result;

    proxied_parser = (Parser *) self->param;
    assert(proxied_parser);
	if(!proxied_parser) return result;

	ADJUST_FLAGS

	if(++I.call_depth > CPARSING_MAX_CALL_DEPTH)
	{
		printf("ERROR: MAXIMUM CALLDEPTH REACHED.\n");
		return PARSER_FAILED;
	}

    result = proxied_parser->parse(proxied_parser, I, input);

    return result;
}

static void destroy_parser_proxy(Parser *deletee)
{
Parser *proxied_parser;
int refcount;

    assert(deletee);
	if(!deletee) return;

	refcount = --deletee->refcount;

	proxied_parser = (Parser *) deletee->param;

	if(proxied_parser) 
	{
		deletee->param = 0L;
		proxied_parser->destroy(proxied_parser);
	}

    if(refcount == 0)
	{
        free(deletee);
	}
}

Parser *Ref(Parser *src)
{
Parser *result;

    assert(src);
	if(!src)
	{
		printf("Ref parser needs a valid parser definition.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	PNAME("Ref")

    result->param = src;
	++src->refcount;

    result->destroy		= destroy_parser_proxy;
    result->parse			= parse_Ref;
    result->flags			|= (PROXY | src->flags);
    result->flags_mask	|= (PROXY | src->flags_mask);

    return result;
}

Parser *Foreign(Parser *src)
{
Parser *result = Ref(src);

	if(!result) return NULL;

    --src->refcount;	// refcount was inceased by Ref() ... but since we do not own src, we'll release it again
	result->destroy		= destroy_simple_parser;	// do not try to delete the proxied parser - we just lend it
	return result;
}

Parser *ForwardDeclaration(void)
{
Parser *result = parser();

	if(!result) return NULL;

	PNAME("ForwardDeclaration")

    result->parse       = unimplemented_forward_declaration;
    result->destroy     = invalid_destroy_function;
    // result->param = 0L is guranteed by parser()
    return result;
}

Parser *ForwardAssign(Parser *forward_declaration, Parser *assignment)
{
    assert(forward_declaration);
    assert(assignment);
    assert("ERROR: forward-parser already bound" && forward_declaration->param == 0L);

	if(!(forward_declaration && assignment))
	{
		printf("ForwardAssign needs valid declaration and assignment.\n");
		return NULL;
	}

	if(forward_declaration->param != 0L)
	{
		printf("ForwardAssign on already bound declaration.\n");
		return NULL;
	}

    forward_declaration->param      = assignment;
    // do not increment refcount - we normally own our declaration

    forward_declaration->parse      = parse_Ref;
    forward_declaration->destroy    = destroy_parser_proxy;
    forward_declaration->flags      |= PROXY;
    forward_declaration->flags_mask |= PROXY;

    return forward_declaration;
}

// ---------------------------------------------------------------------------


struct CaptureResults
{
    LIST_OF(struct CaptureResults);
    char *from, *to;
};
typedef struct CaptureResults CaptureResults;

// <<Abstract>>
typedef struct
{
    struct
    {
        LIST_OF(struct CaptureResults);
    } parser_results;
} CaptureResultParam;

// 'inherits' CaptureResultParam
typedef struct
{
    struct
    {
        LIST_OF(struct CaptureResults);
    } parser_results;
    Parser                  *parser;
    
	union
	{
		int                  include_endmark;	// for Capture
		int						record_complete;	// for Record
	};
} CaptureUntilParam;

static CaptureResults *capture_element(char *from, char *to)
{
CaptureResults *result = (CaptureResults *) malloc(sizeof(CaptureResults));

    assert(result);
	if(!result) return NULL;

    result->from    = from;
    result->to      = to;
    result->next = result->prev = 0L;   // illegal list header

    return result;
}

static int interpreter_CaptureResults(Parser *endmarker, void *vparam, int mode, char *from, char *to)
{
CaptureResultParam *param = (CaptureResultParam *) vparam;

    assert(endmarker);
    assert(param);
	assert(mode == TOKEN_FAILED || (from && to));

	if(!(endmarker && param))
		return 0;

	if(!(mode == TOKEN_FAILED || (from && to)))
		return 0;

    switch(mode)
    {
    case TOKEN_ENTER:
        break;

    case TOKEN_VALUE:
        if((endmarker->flags & SKIP_TOKEN) == 0) // we dont need to collect the values, if they are to be skipped
            list_append_end((list *) &param->parser_results, (list *) capture_element(from, to));
        break;

    case TOKEN_COMPLETED:
        break;

    case TOKEN_FAILED:
        break;

    default:
        assert("Illegal interpreter mode." && 0);
    }

    return 1;
}

static int parse_CaptureUntil(Parser *self, InterpreterInfo I, Region *input)
{
InterpreterInfo II;
CaptureUntilParam *param;
char *real_begin;
int result = PARSER_FAILED;

    assert(self);

	if(!self)
		return 0;

    param = (CaptureUntilParam *) self->param;

    assert(param && param->parser);

	if(!(param && param->parser))
		return 0;

    assert(input && input->begin && input->end && input->current && (input->begin <= input->current) && (input->current <= (input->end+1)) );

	if(!(input && input->begin && input->end && input->current && (input->begin <= input->current) && (input->current <= (input->end+1)) ))
		return 0;

	ADJUST_FLAGS

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

    real_begin = input->current;

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, input->begin, input->end);

	II = I;
	II.interpreter = interpreter_CaptureResults;
	II.param = param;

    while((input->current <= input->end) && (result != PARSER_MATCH))
    {
    Region view;
    CaptureResults *capture;

        init_region(&view, input->current, input->end);

        if(param->parser->parse(param->parser, II, &view))
        {
            // found the endmarker
            result = PARSER_MATCH;

            if(I.interpreter)
            {
                // post our result
                if((I.flags & SKIP_TOKEN) == 0 && (real_begin < input->current))
                {
                    I.interpreter(self, I.param, TOKEN_VALUE, real_begin, input->current-1);
                }

                // post the parser's results
                if(param->include_endmark && (param->parser->flags & SKIP_TOKEN) == 0)
                {
                    list_for_each(CaptureResults, capture, (list *) &param->parser_results)
                    {
                        I.interpreter(self, I.param, TOKEN_VALUE, capture->from, capture->to);
                    }
                }
            }
        }

        list_clear((list *) &param->parser_results, 0L);    // clear any captures

        if(!result)
		{
            if(param->parser->flags & PROVIDES_RESTART_POSITION)
				input->current = view.current;
			else
				++input->current;                               // advance to next symbol ..
        } else if(param->include_endmark)
            input->current = view.current;                  // .. or use postion after endmark
    }

	TRACE_DEC

    PARSER_STD_NOTIFY_END

    return result;
}

static void destroy_CaptureUntil(Parser *deletee)
{
CaptureUntilParam *param;

    assert(deletee);
	if(!deletee) return;

	param = (CaptureUntilParam *) deletee->param;

	if(param)
	{
        if(param->parser)
		{
			param->parser->destroy(param->parser);
			param->parser = 0;
		}

        free(param);
		deletee->param = 0;
	}

    if(--deletee->refcount == 0)
        free(deletee);
}

Parser *CaptureUntil(int include_match, Parser *p)
{
Parser *result      = parser();
CaptureUntilParam *param;

	if(!result) return NULL;

	param    = (CaptureUntilParam *) calloc(1, sizeof(CaptureUntilParam));
    assert(param);
	if(!param) return NULL;

    list_init(((list *) (&param->parser_results)));

	PNAME("CaptureUntil")

    param->parser           = p;
    param->include_endmark  = include_match;

    result->param           = param;
    result->parse           = parse_CaptureUntil;
    result->destroy         = destroy_CaptureUntil;

    return result;
}

// ---------------------------------------------------------------------------

enum
{
	RECORD_COMPLETED_FAIL = 1,
	RECORD_COMPLETED_SUCCESS
};

static int parse_Record_replay(Parser *self, InterpreterInfo I, Region *input)
{
CaptureUntilParam *param;
CaptureResults *capture;

    assert(self);
	if(!self) return PARSER_FAILED;

    param = (CaptureUntilParam *) self->param;

    assert(param && param->parser);
	if(!(param && param->parser)) return PARSER_FAILED;

    assert(input && input->begin && input->end && (input->begin <= input->end+1) && input->current && (input->begin <= input->current) && (input->current <= (input->end+1)));

	if(!(self && input))
		return 0;

    if(I.interpreter)
	{
		I.interpreter(self, I.param, TOKEN_ENTER, input->begin, input->end);

		list_for_each(CaptureResults, capture, (list *) &param->parser_results)
		{
			I.interpreter(self, I.param, TOKEN_VALUE, capture->from, capture->to);
		}

		if(param->record_complete == RECORD_COMPLETED_SUCCESS)
		{
			I.interpreter(self, I.param, TOKEN_COMPLETED, input->current, input->end);
			if((self->flags & TRACED) != 0) print("%*s%s OK\n", trace_indent, " ", self->name);
		} else {
			I.interpreter(self, I.param, TOKEN_FAILED, input->current, input->end);
			if((self->flags & TRACED) != 0) print("%*s%s FAILED\n", trace_indent, " ", self->name);
		}
	}

	return param->record_complete == RECORD_COMPLETED_SUCCESS ? PARSER_MATCH : PARSER_FAILED;
}

static int parse_Record(Parser *self, InterpreterInfo I, Region *input)
{
InterpreterInfo II;
Region view;
CaptureUntilParam *param;
char *real_begin;
int result = PARSER_FAILED;

    assert(self);
	if(!self) return result;

    param = (CaptureUntilParam *) self->param;

    assert(param && param->parser);
	if(!(param && param->parser)) return result;

    assert(input && input->current && (input->begin <= input->current) && (input->current <= (input->end+1)) );
	if(!(self && input))
		return result;

	if((I.flags & RESTART) != 0)
	{
		list_clear((list *) &param->parser_results, 0L);
		param->record_complete = 0;
	}

	if(param->record_complete)
		return parse_Record_replay(self, I, input);

	ADJUST_FLAGS

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

    real_begin = input->current;

	TRACE_ENTER

	II = I;
	II.interpreter = interpreter_CaptureResults;
	II.param = param;

	init_region(&view, input->current, input->end);
	result = param->parser->parse(param->parser, II, &view);

	param->record_complete = result ? RECORD_COMPLETED_SUCCESS : RECORD_COMPLETED_FAIL;
	input->current = view.current;
	view.current = real_begin;

	TRACE_DEC

	return parse_Record_replay(self, I, &view);
}

void restart_Record(Parser *rec)
{
CaptureUntilParam *param;

    assert(rec);
	assert("restart_Record: provided parser is not a Record-parser" && rec->parse == parse_Record);

	if(!rec)
	{
		printf("restart_Record missing recorder.\n");
		return;
	}

	if(rec->parse != parse_Record)
	{
		printf("Parameter provided to restart_Record is not a recorder.\n");
		return;
	}

    param = (CaptureUntilParam *) rec->param;
    assert(param);
	if(!param) return;

	list_clear((list *) &param->parser_results, 0L);
	param->record_complete = 0;
}

static void destroy_Record(Parser *deletee)
{
CaptureUntilParam *param;

    assert(deletee);
	if(!deletee) return;

    if(--deletee->refcount == 0)
	{
		param = (CaptureUntilParam *) deletee->param;
		list_clear((list *) &param->parser_results, 0L);
		++deletee->refcount;
		destroy_CaptureUntil(deletee);
	}
}

Parser *Record(Parser *p)
{
Parser *result      = parser();
CaptureUntilParam *param;

	if(!result) return NULL;

	param    = (CaptureUntilParam *) calloc(1, sizeof(CaptureUntilParam));

    assert(param);
	if(!param)
	{
		free(result);
		return NULL;
	}

    list_init(((list *) (&param->parser_results)));

	PNAME("Record")

    param->parser           = p;

    result->param           = param;
    result->parse           = parse_Record;
    result->destroy         = destroy_Record;
	result->flags			= ACCEPT_EMPTY_INPUT;

    return result;
}

static int parse_Print(Parser *self, InterpreterInfo I, Region *input);

void record_init_interator(RecordIterator *i, Parser *p)
{
CaptureResultParam *param;

	assert(i);
	assert(p);
	if(!(i && p)) return;

	assert("provided parser is not a Record()er" && (p->parse == parse_Record || p->parse == parse_Print));
	if(p->parse != parse_Record && p->parse != parse_Print)
	{
		printf("Parameter provided to record_init_interator is not a recorder.\n");
		return;
	}

	param = (CaptureResultParam *) p->param;
	assert(param);
	if(!param) return;

	i->head	= &param->parser_results;
	i->current = 0;
}

int record_get_next(RecordIterator *iterator, Region *target)
{
	assert(target);
	assert(iterator);

	if(!(target && iterator)) return 0;

	if(!iterator->current)
		iterator->current = ((list *) iterator->head)->next;
	else
		iterator->current = ((list *) iterator->current)->next;

	if(iterator->head == iterator->current)
		return 0;

	target->begin = target->current = ((CaptureResults *) iterator->current)->from;
	target->end = ((CaptureResults *) iterator->current)->to;

	return 1;
}

int record_get_length(Parser *p)
{
RecordIterator ri;
Region tmp;
int len = 0;

	assert(p);
	if(!p) return 0;

	record_init_interator(&ri, p);

	while(record_get_next(&ri, &tmp)) ++len;

	return len;
}

int record_iterator_at_end(RecordIterator *iterator)
{
	assert(iterator);
	if(!iterator) return 1;

	return (iterator->head == ((list *) iterator->current)->next);
}

int record_get_nth(Parser *p, int idx, Region *target)
{
RecordIterator ri;
Region tmp;

	assert(p);
	assert(idx >= 0);
	assert(target);

	if(!(p && idx >= 0 && target))
		return 0;

	record_init_interator(&ri, p);

	while(record_get_next(&ri, &tmp) && idx--)
	{
		;
	}

	if(idx >= 0)
		return 0;	// get_next failed before we reached position
	
	*target = tmp;
	return 1;
}

Parser *record_to_parser_list(Parser *data_source)
{
Parser *parser_list;
RecordIterator ri;
Region tmp;

	assert(data_source);
	if(!data_source) return NULL;

	parser_list = parser_list_header();
	record_init_interator(&ri, data_source);

	while(record_get_next(&ri, &tmp))
	{
	int len = (tmp.end - tmp.begin + 1);

		assert(len == sizeof(Parser));
        list_append_end((list *) parser_list, (list *) tmp.begin);
	}

	return parser_list;
}

// ---------------------------------------------------------------------------

typedef struct
{
	Parser		*parser;
	unsigned	index;
	unsigned	current;
	char		*from, *to;
} NthParam;

static int interpreter_Nth(Parser *endmarker, void *vparam, int mode, char *from, char *to)
{
NthParam *param = (NthParam *) vparam;

    assert(endmarker);
    assert(param);
	assert(from && to);

	if(!(endmarker && param && from && to))
		return 0;

    switch(mode)
    {
    case TOKEN_ENTER:
        break;

    case TOKEN_VALUE:
		if(param->current == param->index)
		{
			param->from	= from;
			param->to		= to;
		}
		++param->current;
		break;

    case TOKEN_COMPLETED:
        break;

    case TOKEN_FAILED:
        break;

    default:
        assert("Illegal interpreter mode." && 0);
    }

    return 1;
}

static int parse_Nth(Parser *self, InterpreterInfo I, Region *input)
{
InterpreterInfo II;
Region view;
NthParam *param;
int result = PARSER_FAILED;

    assert(self);
	if(!self) return 0;

    param = (NthParam *) self->param;

    assert(param && param->parser);
	if(!(param && param->parser)) return 0;

    assert(input && input->current && (input->begin <= input->current) && (input->current <= (input->end+1)) );
	if(!(input && input->begin && input->end && input->current && (input->begin <= input->current) && (input->current <= (input->end+1))))
		return 0;

	ADJUST_FLAGS

    // skip to next token
    if((I.flags & DONT_SKIP_SPACES) == 0)
        skip_spaces(input);

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, input->begin, input->end);

	param->from = 0;
	param->current = 0;	// start at 1st position

	II = I;
	II.interpreter = interpreter_Nth;
	II.param = param;

	init_region(&view, input->current, input->end);
	result = param->parser->parse(param->parser, II, &view);

	if(param->from)
	{
		if(I.interpreter)
			I.interpreter(self, I.param, TOKEN_VALUE, param->from, param->to);
	} else 
		result = PARSER_FAILED;

	input->current = view.current;

	TRACE_DEC

    PARSER_STD_NOTIFY_END

	return result;
}

static void destroy_Nth(Parser *deletee)
{
NthParam *param;

    assert(deletee);
	if(!deletee) return;

	param = (NthParam *) deletee->param;

	if(param)
	{
        if(param->parser)
		{
			param->parser->destroy(param->parser);
			param->parser = 0;
		}

        free(param);
		deletee->param = 0;
	}

    if(--deletee->refcount == 0)
        free(deletee);
}

Parser *Nth(unsigned index, Parser *p)
{
Parser *result = parser();
NthParam *param;

	if(!result) return NULL;

	assert("No source parser given to Nth" && p);
	if(!p)
	{
		printf("No source parser given to Nth.\n");
		return NULL;
	}

	param = (NthParam *) malloc(sizeof(NthParam));
	assert("OOM" && param);
	if(!param) return NULL;
	
	PNAME("Nth")

	param->parser	= p;
	param->index = index;

	result->param = param;
	result->parse	= parse_Nth;
	result->destroy	= destroy_Nth;
	result->flags	= ACCEPT_EMPTY_INPUT;
	
	return result;
}

// ---------------------------------------------------------------------------

typedef struct
{
    struct
    {
        LIST_OF(struct CaptureResults);
    } parser_results;
	Parser	*parser;
	Region	filler;
	char	fill_flags;
} PackParam;

#define PACK_NEW_RUN_FLAG 0x80

static int interpreter_Pack(Parser *endmarker, void *vparam, int mode, char *from, char *to)
{
Parser *base = (Parser *) vparam;
PackParam *param;

    assert(endmarker);
    assert(base);
	if(!(endmarker && base)) return 0;

    param = base->param;
    assert(param);
	if(!param) return 0;

	// HEAD is done at enter, so even empty result-sets will have it
	if(param->filler.begin && (mode == TOKEN_ENTER) && (param->fill_flags & JOIN_WITH_HEAD))
		list_append_end((list *) &param->parser_results, (list *) capture_element(param->filler.begin, param->filler.end));

    if(mode == TOKEN_VALUE)
	{
		if(param->filler.begin)
		{
			// skip the filler on the 1st token only
			if(param->fill_flags & PACK_NEW_RUN_FLAG)
				param->fill_flags &= ~PACK_NEW_RUN_FLAG;
			else
				list_append_end((list *) &param->parser_results, (list *) capture_element(param->filler.begin, param->filler.end));
		}

		list_append_end((list *) &param->parser_results, (list *) capture_element(from, to));
	}

	// TAIL is done at exit, so even empty result-sets will have it
	if(param->filler.begin && (mode == TOKEN_COMPLETED) && (param->fill_flags & JOIN_WITH_TAIL))
		list_append_end((list *) &param->parser_results, (list *) capture_element(param->filler.begin, param->filler.end));

    return 1;
}

static int parse_Pack(Parser *self, InterpreterInfo I, Region *input)
{
InterpreterInfo II;
PackParam *param;
Region view;
int result = 0;

    assert(self);
	if(!self) return 0;

    param = (PackParam *) self->param;

    assert(param && param->parser);

	if(!(param && param->parser)) return 0;

    assert(input && input->begin && input->end && input->current && (input->begin <= input->current) && (input->current <= (input->end+1)) );
	if(!(self && input))
		return 0;

	ADJUST_FLAGS

	TRACE_ENTER

    if(I.interpreter)
        I.interpreter(self, I.param, TOKEN_ENTER, input->begin, input->end);

	TRACE_INC

	init_region(&view, input->current, input->end);

	if(param->filler.begin)
		param->fill_flags |= PACK_NEW_RUN_FLAG;		// tag for 1st token

	II = I;
	II.interpreter	= interpreter_Pack;
	II.param		= self;

	if((result = param->parser->parse(param->parser, II, &view)) != 0)
	{
	CaptureResults *capture;
	char *joined, *current;
	unsigned long cap_len = 0, seg_len;

		if(I.interpreter)
		{
			list_for_each(CaptureResults, capture, (list *) &param->parser_results)
				cap_len += (capture->to - capture->from + 1);

			joined = current = parse_malloc(cap_len);

			list_for_each(CaptureResults, capture, (list *) &param->parser_results)
			{
				seg_len = (capture->to - capture->from + 1);
				memcpy(current, capture->from, seg_len);
				current += seg_len;
			}

			I.interpreter(self, I.param, TOKEN_VALUE, joined, current-1);
		}
        list_clear((list *) &param->parser_results, 0L);
		input->current = view.current;
	}

	TRACE_DEC

    PARSER_STD_NOTIFY_END

	return result;
}

static void destroy_Pack(Parser *deletee)
{
PackParam *param;

    assert(deletee);
	if(!deletee) return;

	param = (PackParam *) deletee->param;

	if(param)
	{
        if(param->parser)
		{
			param->parser->destroy(param->parser);
			param->parser = 0;
		}

        list_clear((list *) &param->parser_results, 0L);

        free(param);
		deletee->param = 0;
	}

    if(--deletee->refcount == 0)
        free(deletee);
}

Parser *Pack(Parser *p)
{
Parser *result;
PackParam *param;

	if(!p)
	{
		printf("No parser definition given to Pack.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	param = (PackParam *) malloc(sizeof(PackParam));
	assert("OOM" && param);
	if(!param)
	{
		free(result);
		return NULL;
	}

	PNAME("Pack")

	param->parser		= p;
	param->filler.begin = param->filler.end = param->filler.current = 0L;
	param->fill_flags	= 0L;
    list_init(((list *) (&param->parser_results)));

	result->parse	= parse_Pack;
	result->destroy	= destroy_Pack;
	result->param	= param;

	return result;
}

Parser *Join(Region *filler, char flags, Parser *p)
{
Parser *result = Pack(p);
PackParam *param;

	if(result)
	{
		param = (PackParam *) result->param;
		if(param)
		{
			param->filler		= *filler;
			param->fill_flags	= flags;
		}
	}

	return result;	
}
// ---------------------------------------------------------------------------

typedef struct
{
	Parser		*parser;
	InterpreterInfo I;
	Parser		*self;			// access the true self in recode-interpreter
	Editor		recode;
	void		*edit_param;
	int			failed;
} EditParam;

static int interpreter_Edit(Parser *self, void *vparam, int mode, char *from, char *to)
{
EditParam *param = (EditParam *) vparam;

    assert(self);
    assert(param && param->recode && param->self);
	assert(from);
	assert(to);

	if(!(self && param && param->recode && param->self && from && to)) return 0;	

    switch(mode)
    {
    case TOKEN_ENTER:
        if(param->I.interpreter)
            param->I.interpreter(param->self, param->I.param, TOKEN_ENTER, from, to);

        break;

    case TOKEN_VALUE:
		if(param->failed)		// we've had fail - just wait for the inner parser to be done now
			return 0;

		{
		char *_from = from, *_to = to;		// don't modify someone else's data

			if(param->recode(param->edit_param, &_from, &_to))
			{
				assert("Recoder returned illegal region" && (_from <= (_to + 1)));

				if((param->I.flags & SKIP_TOKEN) == 0)
					param->I.interpreter(param->self, param->I.param, TOKEN_VALUE, _from, _to);
			} else {
				param->failed = 1;
			}
		}

        break;

    case TOKEN_COMPLETED:
        // unusual end-of-token notification, so not the usual macro invocation
		if(param->I.interpreter)
		{
            if(param->failed)
            {
                param->I.interpreter(param->self, param->I.param, TOKEN_FAILED, from, to);
            } else {
                param->failed = !param->I.interpreter(param->self, param->I.param, TOKEN_COMPLETED, from, to);
            }
		}
		param->failed = 0;		// we're done, now we can start again
        break;

    case TOKEN_FAILED:
        if(param->I.interpreter)
            param->I.interpreter(param->self, param->I.param, TOKEN_FAILED, from, to);

		param->failed = 0;		// every begin has new opportunities
		return 0;				// ... but this one is a fail

        break;

    default:
        assert("Illegal interpreter mode." && 0);
    }

    return !param->failed;
}

static int parse_Edit(Parser *self, InterpreterInfo I, Region *input)
{
EditParam *param;
InterpreterInfo info;
int result;

    assert(self);
	if(!self) return 0;

    param = (EditParam *) self->param;

    assert(param && param->parser);
    assert(input);
	if(!(param && param->parser && input)) return 0;

	ADJUST_FLAGS

	TRACE_ENTER

	// force the source parser to use our interpreter
	info = I;
	param->I = I;
	info.interpreter	= interpreter_Edit;
	info.param			= param;

    result = param->parser->parse(param->parser, info, input);

	if( (I.flags & TRACED) != 0 )
		printf("%*s%s %s\n", trace_indent, " ", self->name, result ? "OK" : "Failed");

	return result;
}

static void destroy_Edit(Parser *deletee)
{
EditParam *param;

	assert(deletee);
	if(!deletee) return;

	param = deletee->param;

	if(param && param->parser)
	{
		param->parser->destroy(param->parser);
		param->parser = 0L;
	}

	destroy_parser_and_param(deletee);
}

Parser *Edit(Editor editor, void *editor_param, Parser *source)
{
Parser *result;
EditParam *param;

	assert(source);
	assert(editor);

	if(!(source && editor))
	{
		printf("Edit needs an editor and a source.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	param = (EditParam *) malloc(sizeof(EditParam));
	assert(param);
	if(!param)
	{
		free(result);
		return NULL;
	}

	PNAME("Edit")

	param->self			= result;
	param->parser		= source;
	param->recode		= editor;
	param->edit_param	= editor_param;
	param->failed		= 0;

	result->parse		= parse_Edit;
	result->destroy		= destroy_Edit;
	result->param		= param;
	result->flags		= ACCEPT_EMPTY_INPUT;

	return result;
}

Parser *ToIntB(int base, Parser *p) { return Edit(str2int, (void *) (long) base, p); }
Parser *ToShortB(int base, Parser *p) { return Edit(str2short, (void *) (long) base, p); }
Parser *ToLongB(int base, Parser *p) { return Edit(str2long, (void *) (long) base, p); }
Parser *Subst(char *fmt, Parser *p) { return Edit(str2str, fmt, p); }

Parser *AddToDict(Dict *d, Parser *p) { return Edit(add2dict, d, p); }

Parser *ToByte(Parser *p) { return Edit(str2byte, 0, p); }
Parser *ToInt(Parser *p) { return Edit(str2int, 0, p); }
Parser *ToShort(Parser *p) { return Edit(str2short, 0, p); }
Parser *ToLong(Parser *p) { return Edit(str2long, 0, p); }
Parser *ToDouble(Parser *p) { return Edit(str2double, 0, p); }
Parser *ToStr(Parser *p) { return Edit(region2string, 0, p); }
Parser *ToPtr(Parser *p) { return Edit(region2pointer, 0, p); }
Parser *ToRegion(Parser *p) { return Edit(region2region, 0, p); }
Parser *ToLower(Parser *p) { return Edit(map_chars, tolower, p); }
Parser *ToUpper(Parser *p) { return Edit(map_chars, toupper, p); }
Parser *ToBits(Parser *p) { return Edit(bytes2bits, 0, p); }
Parser *Htons(Parser *p) { return Edit(short_h2n, 0, p); }
Parser *Ntohs(Parser *p) { return Edit(short_n2h, 0, p); }
Parser *Htonl(Parser *p) { return Edit(long_h2n, 0, p); }
Parser *Ntohl(Parser *p) { return Edit(long_n2h, 0, p); }

// ---------------------------------------------------------------------------
typedef struct
{
    Parser  *parser;
    InterpreterInfo I;
	Parser  *self;
    Region  found;
    void *  target;
    void ** base;		// PutStruct
    size_t  size;
    size_t  offset;		// PutStruct
} PutParam;

static void destroy_Put(Parser *deletee)
{
PutParam *param;

    assert(deletee);
	if(!deletee) return;

    param = (PutParam *) deletee->param;

    if(param)
	{
		if(param->parser) 
		{
			param->parser->destroy(param->parser);
			param->parser = 0;
		}

        free(deletee->param);
        deletee->param = 0L;
	}

    if(--deletee->refcount == 0)
        free(deletee);
}

static int interpreter_Put(Parser *self, void *vparam, int mode, char *from, char *to)
{
PutParam *param = (PutParam *) vparam;

    assert(self);
    assert(param && param->parser && param->target && param->size);

	if(!(self && param && param->parser && param->target && param->size))
		return 0;

    switch(mode)
    {
    case TOKEN_ENTER:
        if(param->I.interpreter)
            param->I.interpreter(param->self, param->I.param, TOKEN_ENTER, from, to);

        return 1;

    case TOKEN_VALUE:
		// these two tests are assertations because both conditions indicated an error in the parser design
        assert("Source of Put yields multiple values" && (param->found.begin == 0));		// single token only
        assert("Source of Put yields token of invalid size" && (param->size >= to-from+1));	// sufficient space available

        param->found.begin  = param->found.current = from;
        param->found.end    = to;

        return 1;

    case TOKEN_COMPLETED:
		if(param->found.begin == 0) // did we get exactly one result?
		{
			if(param->I.interpreter)
				param->I.interpreter(param->self, param->I.param, TOKEN_FAILED, from, to);
			return 0;
		} else {
			// all ok - perform the actual copy
			memcpy(param->target, param->found.begin, param->found.end - param->found.begin + 1);
			{
			char *_from = (char *) param->target, *_to = _from + param->size - 1;

				if(param->I.interpreter)
				{
					if((param->I.flags & SKIP_TOKEN) == 0)
						param->I.interpreter(param->self, param->I.param, TOKEN_VALUE, _from, _to);

					param->I.interpreter(param->self, param->I.param, TOKEN_COMPLETED, from, to);
				}

			}
			return 1;
		}
        break;

    case TOKEN_FAILED:
        if(param->I.interpreter)
            param->I.interpreter(param->self, param->I.param, TOKEN_FAILED, from, to);

		return 0;

    default:
        assert("Illegal interpreter mode." && 0);
    }

    return 0;
}


static int parse_Put(Parser *self, InterpreterInfo I, Region *input)
{
int result = PARSER_FAILED;
PutParam *param;
InterpreterInfo info;

    assert(self);
	if(!self) return 0;

    param = (PutParam *) self->param;
    assert(param && param->parser);
    assert(input);

	if(!(param && param->parser && input)) return 0;

	ADJUST_FLAGS

	TRACE_ENTER

	// cleanup + init
	param->found.begin = param->found.end = 0L;

	info				= I;
	param->I			= I;
	info.interpreter	= interpreter_Put;
	info.param			= param;

    result = param->parser->parse(param->parser, info, input);

	if( (I.flags & TRACED) != 0 )
		printf("%*s%s %s\n", trace_indent, " ", self->name, result ? "OK" : "Failed");

    return result;
}

Parser *_Put(void *target, size_t size, Parser *source)
{
Parser *result;
PutParam *param;

	assert(target);
	assert(size);
	assert(source);
	if(!(target && size && source))
	{
		printf("Parameter missing for _Put.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	param = (PutParam *) calloc(1, sizeof(PutParam));
	assert(param);
	if(!param)
	{
		free(result);
		return NULL;
	}

	PNAME("Put")

	param->self		= result;
	param->parser	= source;
	param->target	= target;
	param->size		= size;

	param->base		= 0L;	// 2 entries used only but _PutStruct
	param->offset	= 0;

	result->param	= param;
	result->parse	= parse_Put;
	result->destroy	= destroy_Put;
	result->flags	= SKIP_TOKEN | ACCEPT_EMPTY_INPUT;	// Put consumes its token
	// *NO* result->flags_mask - 'skip' isn't propagated

	return result;
}


static int parse_PutStruct(Parser *self, InterpreterInfo I, Region *input)
{
int result = PARSER_FAILED;
PutParam *param;
InterpreterInfo info;

    assert(self);
	if(!self) return 0;

    param = (PutParam *) self->param;
    assert(param && param->parser);
    assert(input);

	if(!(param && param->parser && input)) return 0;

	ADJUST_FLAGS

	TRACE_ENTER

	// cleanup + init
	param->found.begin = param->found.end = 0L;

	info				= I;
	param->I			= I;
	info.interpreter	= interpreter_Put;
	info.param			= param;
	param->target		= ((char *) (*param->base)) + param->offset;

    result = param->parser->parse(param->parser, info, input);

	if( (I.flags & TRACED) != 0 )
		printf("%*s%s %s\n", trace_indent, " ", self->name, result ? "OK" : "Failed");

    return result;
}

Parser *_PutStruct(void **base, size_t offset, size_t size, Parser *source)
{
Parser *result;
PutParam *param;

	assert(base);
	assert(size);
	assert(source);
	if(!(base && size && source))
	{
		printf("Parameter missing for _Put.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	param = (PutParam *) calloc(1, sizeof(PutParam));
	assert(param);
	if(!param)
	{
		free(result);
		return NULL;
	}

	PNAME("Put")

	param->self		= result;
	param->parser	= source;
	param->target	= 0;
	param->base		= base;
	param->size		= size;
	param->offset	= offset;

	result->param	= param;
	result->parse	= parse_PutStruct;
	result->destroy	= destroy_Put;
	result->flags	= SKIP_TOKEN | ACCEPT_EMPTY_INPUT;	// Put consumes its token
	// *NO* result->flags_mask - 'skip' isn't propagated

	return result;
}

// ---------------------------------------------------------------------------

static int parse_Emit(Parser *self, InterpreterInfo I, Region *input)
{
Region *r;

	assert(self);
	if(!self) return 0;

	r = (Region *) self->param;
	assert(r && r->begin && r->end && r->begin <= r->end);
	assert(input);

	if(!(r && r->begin && r->end && r->begin <= r->end))
		return 0;

	TRACE_ENTER

	if(I.interpreter)
	{
		I.interpreter(self, I.param, TOKEN_ENTER, input->begin, input->end);
		I.interpreter(self, I.param, TOKEN_VALUE, r->begin, r->end);
		I.interpreter(self, I.param, TOKEN_COMPLETED, input->begin, input->end);
	}

	return 1;
}

Parser *EmitRegion(Region *r)
{
Parser *result;

	assert(r && r->begin && r->end);
	if(!(r && r->begin && r->end)) return NULL;

	result = parser();
	if(!result) return NULL;

	PNAME("Emit")

	result->param	= r;

	result->parse	= parse_Emit;
	result->destroy = destroy_parser_and_param;
	result->flags	= ACCEPT_EMPTY_INPUT;

	return result;
}

Parser *Emit(char *from, size_t len)
{
	assert(from);
	return EmitRegion(region(from, len ? from + len - 1 : 0L));
}

// ---------------------------------------------------------------------------
typedef struct 
{
	parser_callback_function function;
	void *arg_in, **arg_out;
} CallbackParam;

static int parse_Callback(Parser *self, InterpreterInfo I, Region *input)
{
CallbackParam *param;

    assert(self);
	if(!self) return 0;

    param = (CallbackParam *) self->param;
    assert(param);

	if(!(param && param->function))
		return 0;

	ADJUST_FLAGS
	TRACE_ENTER
	
	return param->function(param->arg_in, param->arg_out);
}

Parser *Callback(parser_callback_function function, void *arg_in, void **arg_out)
{
Parser *result;
CallbackParam *param;

	assert(function);
	if(!function)
		return NULL;

	param = (CallbackParam *) calloc(1, sizeof(CallbackParam));
	assert(param);

	if(!param)
		return NULL;

	result = parser();

	if(result)
	{
		param->function		= function;
		param->arg_in		= arg_in;
		param->arg_out		= arg_out;

		result->parse		= parse_Callback;
		result->destroy		= destroy_parser_and_param;
		result->param		= param;
		result->flags 	 	= ACCEPT_EMPTY_INPUT;

		PNAME("CallBack")
	}

	return result;
}

// ---------------------------------------------------------------------------

static int parse_Call(Parser *self, InterpreterInfo I, Region *input)
{
int result = PARSER_FAILED;
CallParam *param;
Parser *save;
InterpreterInfo II;

    assert(self);
	if(!self) return 0;

    param = (CallParam *) self->param;
    assert(param && param->parser && param->ctor);
    assert(input);

	if(!(param && param->parser && param->ctor && input))
		return 0;

	ADJUST_FLAGS

	TRACE_ENTER
	
	save	= Record(Foreign(param->parser));
	// save = param->parser;

	II = I0;
	II.flags = I.flags;
	II.flags_mask = I.flags_mask;
	
	result	= save->parse(save, II, input);

	if(result == PARSER_MATCH)
	{
		param->ctor(param->user_data, I, save);
	}

	// data has been consumed, so restart the recorder
	save->destroy(save);
	// restart_Record(save);

	return result;
}

void destroy_Call(Parser *deletee)
{
CallParam *param;

    assert(deletee);
	if(!deletee) return;

    param = (CallParam *) deletee->param;

    if(param)
	{
        if(param->parser)
		{
			param->parser->destroy(param->parser);
			param->parser = 0;
		}

        free(deletee->param);
        deletee->param = 0L;
	}

    if(--deletee->refcount == 0)
        free(deletee);
}

Parser *Call(ctor_function ctor, void *user_data, Parser *p)
{
Parser *result;
CallParam *param;

	assert(ctor);
	assert(p);

	if(!(ctor && p))
	{
		printf("Call needs ctor_function and source-parser.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	param = (CallParam *) calloc(1, sizeof(CallParam));
	assert(param);
	if(!param) 
	{
		free(result);
		return NULL;
	}

	PNAME("Call")

	param->ctor			= ctor;
	param->user_data	= user_data;
	// param->parser		= Record(Foreign(p));
	param->parser		= p;

	result->param		= param;
	result->parse		= parse_Call;
	result->destroy		= destroy_Call;
	result->flags		= ACCEPT_EMPTY_INPUT;

	return result;
}


// ---------------------------------------------------------------------------
typedef struct
{
	Parser 			*parser;
	void			*user;
	error_function	error;
	const char		*error_message;
} OnFailParam;

static int parse_OnFail(Parser *self, InterpreterInfo I, Region *input)
{
int result = PARSER_FAILED;
OnFailParam *param;

    assert(self);
	if(!self) return result;

    param = (OnFailParam *) self->param;
    assert(param && param->parser && param->error);
    assert(input);

	if(!(param && param->parser && param->error && input))
		return 0;

	ADJUST_FLAGS

	TRACE_ENTER
	
	result	= param->parser->parse(param->parser, I, input);

	if(result != PARSER_MATCH)
		param->error(param->user, param->error_message, param->parser, input);
	
	return result;
}

Parser *OnFail(void *user, error_function error_handler, const char *error_message, Parser *p)
{
Parser *result;
OnFailParam *param;

	assert(error_handler);
	assert(p);

	if(!(p && error_handler))
	{
		printf("OnFail needs source-parser and error_function.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	param = (OnFailParam *) calloc(1, sizeof(OnFailParam));
	assert(param);
	if(!param)
	{
		free(result);
		return NULL;
	}

	PNAME("OnFail")

	param->error			= error_handler;
	param->error_message	= error_message;
	param->parser			= p;
	param->user				= user;

	result->param			= param;
	result->parse			= parse_OnFail;
	result->destroy			= destroy_Call;
	result->flags			= ACCEPT_EMPTY_INPUT;

	return result;
}

// ---------------------------------------------------------------------------
typedef struct
{
	void			*user;
	error_function	error;
	const char		*error_message;
} FailParam;

static int parse_Fail(Parser *self, InterpreterInfo I, Region *input)
{
FailParam *param;

    assert(self);
	if(!self) return 0;

    param = (FailParam *) self->param;
    assert(param && param->error);
    assert(input);
	if(!(param && param->error && input)) return 0;

	TRACE_ENTER
	
	param->error(param->user, param->error_message, self, input);

	return PARSER_FAILED;
}

Parser *Fail(void *user, error_function error_handler, const char *error_message)
{
Parser *result;
FailParam *param;

	assert(error_handler);
	if(!error_handler)
	{
		printf("Fail requires an error_handler.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	param = (FailParam *) calloc(1, sizeof(FailParam));
	assert(param);
	if(!param)
	{
		free(result);
		return NULL;
	}

	PNAME("Fail")

	param->error			= error_handler;
	param->error_message	= error_message;
	param->user				= user;

	result->param			= param;
	result->parse			= parse_Fail;
	result->destroy			= destroy_parser_and_param;
	result->flags			= ACCEPT_EMPTY_INPUT;

	return result;
}

/// ---------------------------------------------------------------------------

static int parse_OK(Parser *self, InterpreterInfo I, Region *input)
{
    assert(self);
	if(!self) return 0;

    assert(input);
	if(!input) return 0;

	TRACE_ENTER
	
	return PARSER_MATCH;
}

Parser *OK(void)
{
Parser *result;

	result = parser();
	if(!result) return NULL;

	PNAME("OK")

	result->parse			= parse_OK;
	result->destroy			= destroy_simple_parser;
	result->flags			= ACCEPT_EMPTY_INPUT;

	return result;
}

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

static int parse_Chain(Parser *self, InterpreterInfo I, Region *input)
{
int result = PARSER_MATCH;
ParserPairParam *param;
Parser *save;
CaptureResults *capture;

    assert(self);
    assert(input);

	if(!(self && input)) return result;

    param = (ParserPairParam *) self->param;

    assert(param && param->first_parser && param->second_parser);
	if(!(param && param->first_parser && param->second_parser)) return result;

	ADJUST_FLAGS

	TRACE_ENTER
	
	save	= Record(Foreign(param->first_parser));
	result	= save->parse(save, I0, input);

	if(result == PARSER_MATCH)
	{
		list_for_each(CaptureResults, capture, (list *) &((CaptureUntilParam *) save->param)->parser_results)
		{
		Region view;

			init_region(&view, capture->from, capture->to);
			if(param->second_parser->parse(param->second_parser, I, &view) != PARSER_MATCH)
			{
				result = PARSER_FAILED;
				break;
			}
		}
	}
	
	// .. only the recorder should be freed
	save->destroy(save);

	return result;
}

Parser *Chain(Parser *producer, Parser *consumer)
{
Parser *result;
ParserPairParam *param;

	assert(producer);
	assert(consumer);

	if(!(producer && consumer))
	{
		printf("Chain needs producer and consumer.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	param = (ParserPairParam *) calloc(1, sizeof(ParserPairParam));

	assert(param);
	if(!param)
	{
		free(result);
		return NULL;
	}

	PNAME("Chain")

	param->first_parser		= producer;
	param->second_parser	= consumer;

	result->param		= param;
	result->parse		= parse_Chain;
	result->destroy		= destroy_ParserPair;
	result->flags		= ACCEPT_EMPTY_INPUT;

	return result;

}

// ---------------------------------------------------------------------------
typedef struct
{
    struct
    {
        LIST_OF(struct CaptureResults);
    } parser_results;
    Parser  *parser;
    InterpreterInfo I;
	Parser  *self;
	char *delimiter;
} PrintParam;

static void destroy_Print(Parser *deletee)
{
PrintParam *param;

    assert(deletee);
	if(!deletee) return;

    param = (PrintParam *) deletee->param;

    if(param)
	{
		if(param->parser) 
		{
			param->parser->destroy(param->parser);
			param->parser = 0;
		}

        list_clear((list *) &param->parser_results, 0L);

        free(deletee->param);
        deletee->param = 0L;
	}

    if(--deletee->refcount == 0)
        free(deletee);
}

static int parse_Print(Parser *self, InterpreterInfo I, Region *input)
{
int result = PARSER_FAILED;
PrintParam *param;
InterpreterInfo info;

    assert(self);
	if(!self) return result;

    param = (PrintParam *) self->param;
    assert(param && param->parser);
    assert(input);
	if(!(param && param->parser && input)) return result;

	ADJUST_FLAGS

	TRACE_ENTER

	// cleanup + init
	info				= I;
	param->I			= I;
	info.interpreter	= interpreter_CaptureResults;
	info.param			= param;

    result = param->parser->parse(param->parser, info, input);

	if( (I.flags & TRACED) != 0 )
		printf("%*s%s %s\n", trace_indent, " ", self->name, result ? "OK" : "Failed");

	if(result)
	{
	RecordIterator ri;
	Region r;

		record_init_interator(&ri, self);

		while(record_get_next(&ri, &r))
		{
			print_ft(r.begin, r.end);

			if(param->delimiter && (((I.flags & WEAK_MAX) == 0) || !record_iterator_at_end(&ri)))
				print("%s", param->delimiter);

			if(I.interpreter && ((I.flags & SKIP_TOKEN) == 0))
				I.interpreter(self, I.param, TOKEN_VALUE, r.begin, r.end);
		}
	}

    return result;
}

Parser *Print(char *delimiter, Parser *source)
{
Parser *result;
PrintParam *param;

	assert(source);
	if(!source)
	{
		printf("Print needs a parser definition.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	param = (PrintParam *) calloc(1, sizeof(PrintParam));
	assert(param);
	if(!param)
	{
		free(result);
		return NULL;
	}

	PNAME("Print")

	param->self		= result;
	param->parser	= source;
    list_init(((list *) (&param->parser_results)));
	param->delimiter = delimiter;

	result->param	= param;
	result->parse	= parse_Print;
	result->destroy	= destroy_Print;
	result->flags	= ACCEPT_EMPTY_INPUT | WEAK_MAX;

	return result;
}

// ---------------------------------------------------------------------------

static int parse_Lookahead(Parser *self, InterpreterInfo I, Region *input)
{
Parser *p;
Region view;
int result;

    assert(self);
	if(!self) return 0;

    p = (Parser *) self->param;

    assert(p);
    assert(input);
	if(!(p && input)) return 0;

	ADJUST_FLAGS

	TRACE_ENTER

	// Use a copy of the current region ..
	init_region(&view, input->current, input->end);

	// .. and I0 interpreter - we're just interested in the return value
    result = p->parse(p, I0, &view);

	if( (I.flags & TRACED) != 0 )
		printf("%*s%s %s\n", trace_indent, " ", self->name, result ? "OK" : "Failed");

	return result;
}


Parser *Lookahead(Parser *p)
{
Parser *result;

	assert(p);
	if(!p)
	{
		printf("Lookahead needs a parser definition.\n");
		return NULL;
	}

	result = parser();
	if(!result) return NULL;

	PNAME("Lookahead")

	result->param	= p;
	result->parse	= parse_Lookahead;
	result->destroy	= destroy_parser_proxy;

	return result;
}


static int parse_Data(Parser *self, InterpreterInfo I, Region *input)
{
int result = PARSER_FAILED;

    assert(input);
	if(!input) return 0;

	ADJUST_FLAGS

	TRACE_ENTER

	if(input->current && input->current <= input->end)
	{
		result = PARSER_MATCH;

		if(I.interpreter)
		{
			I.interpreter(self, I.param, TOKEN_ENTER, input->begin, input->end);
			if((I.flags & SKIP_TOKEN) == 0)
				I.interpreter(self, I.param, TOKEN_VALUE, input->begin, input->end);
			I.interpreter(self, I.param, TOKEN_COMPLETED, input->begin, input->end);
		}

	}

	if( (I.flags & TRACED) != 0 )
		printf("%*s%s %s\n", trace_indent, " ", self->name, result ? "OK" : "Failed");

	return result;
}


Parser *Data(void)
{
Parser *result;

	result = parser();
	if(!result) return NULL;

	PNAME("Data")

	result->parse	= parse_Data;
	result->destroy	= destroy_simple_parser;

	return result;
}








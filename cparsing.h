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

#ifndef __CPARSING_H__
#define __CPARSING_H__

/***

A recursive descent parser combinator for C - with curtsey to pyparsing.

The aim of this module is ease of use - not parsing-performance, nor resource efficency.

## When using the combinator API make sure to read about Ref() at the end of this header ##

***/

/***

ATTENTION: for use in a production environment, the library code needs to
be compiled with NDEBUG defined ('make lib' will do so), since debug assertations are used to aid
development, but you don't want to see them in library code.

***/

// some overhead when parsing in concurrent threads
#define CPARSING_MULTI_THREADED

// if expressions can spread over multiple lines, it's often useful to simply ignore
// newline characters - however, in some cases this might not be wanted.
#define NEWLINE_IS_WHITESPACE

// Our maximum recursion depth
#define CPARSING_MAX_CALL_DEPTH 	128

// --------- END OF CONFIG ------------------------------------------------------

#include <stddef.h>

#define NEWLINE							"\n"
#define WHITE_CHARS						" \t\r"
#if defined(NEWLINE_IS_WHITESPACE)
#define DEFAULT_WHITE_CHARS				WHITE_CHARS NEWLINE
#else
#define DEFAULT_WHITE_CHARS				WHITE_CHARS
#endif
#define ALPHA_LOWER						"abcdefghijklmnopqrstuvwxyz"
#define ALPHA_UPPER						"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
#define DIGIT							"01234567890"
#define ALPHA							ALPHA_LOWER ALPHA_UPPER	"_"
#define ALPHANUM						ALPHA DIGIT
#define HEX                             DIGIT "abcdef"

#ifdef CPARSING_MULTI_THREADED
#define _GNU_SOURCE
#include <unistd.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <pthread.h>
#endif

#include "cparsing_list.h"
#include "cparsing_region.h"
#include "cparsing_dict.h"

#define HAVE_ANSI_COLOR
#ifdef HAVE_ANSI_COLOR
#define YELLOW	"\x1b[33m"
#define GREEN	"\x1b[32m"
#define BLUE	"\x1b[34m"
#define WHITE	"\x1b[37m"
#define RED		"\x1b[31m"

#define YELLOWB	"\x1b[43m"
#define GREENB	"\x1b[42m"
#define BLUEB	"\x1b[44m"
#define WHITEB	"\x1b[47m"
#define REDB	"\x1b[41m"

#define NORMAL	"\x1b[0m"
#define BOLD	"\x1b[1m"
#endif

#if defined SET_PARSER_NAME
#define PNAME(XXX) result->name = XXX;
#else
#define PNAME(XXX)
#endif

// A Parser's parse-function returns TRUE if it matches a given 'Region'
struct Parser;

// Phases of the interpreter protocol; 'interpreter_function' defines the interface
// Parser may 'emit' token to their interpreter using this protocol
enum
{
    // Phase 1 called (once) every time a parser is started
    // receives: input->begin == start of whole region, input->end == end of whole region
    TOKEN_ENTER = 0,

    // Phase 2 can be called multiple times, even in case of later failure
    // receives: input->begin == token start (after skip_spaces), input->end == end of token
    TOKEN_VALUE,            // once for every token found

    // Phase 3 signals match or failure (once for every try)
    // receives: input->begin == position after parse, input->end == end of whole region
    TOKEN_COMPLETED,
    TOKEN_FAILED
    // total input consumed: TOKEN_ENTER/input->begin ... TOKEN_{COMPLETED,FAILED}/input->current (+1 after last used input)
};

typedef int (*interpreter_function)(struct Parser *self, void *interpreter_param, int phase, char *begin, char *end);

typedef struct
{
	interpreter_function	interpreter;
	void					*param;
	short					flags;
	short					flags_mask;
#ifdef CPARSING_MAX_CALL_DEPTH
	unsigned				call_depth;
#endif
} InterpreterInfo;

#ifndef FROM_CPARSING_BODY
extern const InterpreterInfo I0;	// the 'null'-interpreter doing nothing
#endif

typedef int     (*parse_function)(struct Parser *self, InterpreterInfo I, Region *input);      // 1 == parse ok, 0 == fail
typedef void    (*destroy_function)(struct Parser *self);

// Flags marked with (*) are not propagated to sub-parsers
// if a parser does not support a specific flag, the flag gets ignored
enum
{
	// client side flags
	SKIP_TOKEN			= 1,        // apply and check the parser but don't include the match as TOKEN_VALUE (*)
	IGNORE_CASE			= 2,        // apply 'to lowercase' on input symbols before actual matching
	DONT_SKIP_SPACES	= 4,        // supress automatic skipping of whitespaces
	WEAK_MAX			= 8,		// Repeat(): allow more than max occurances (but parse only up to max) - default is to fail if there are more than max occurances (*)
	TRACED				= 16,
	RESTART				= 32,			// tells a stateful parser (i.e. Record) to reset itself
	// internal flags
	COMBINATOR    		= 256,     // not a primitive parser => uses/combines other parsers
	PROXY         		= 512,     // not a primitive parser but only redirecting (dont wants to be seen itself)
	OPTIONAL          	= 1024,    // if Sequence meets an early end of input, it has to check if further parsers still need to be
											//   satisfied - a parser tags itself 'OPTIONAL', if it is ok with being skipped
	ACCEPT_EMPTY_INPUT	= 2048,		// in Sequence: if a parser is not optional, it may accept empty input at eoi (i.e. Eol, Emit)
	PROVIDES_RESTART_POSITION	= 4096		// recognized by CaptureUntil: in case of a failed match, CaptureUntil will use 
											// the resulting view.current to restart the search - this allows for advanced searches such as Boyer-Moore or KMP
											// TODO: let Literal use KMP
};

// more verbose signess parameter for Base10() parser
enum
{
	UNSIGNED          = 0,
	SIGNED            = 1
};

struct Parser
{
    LIST_OF(struct Parser);					// list link is first element, so Parser can be casted to list (element or header)
    parse_function		parse;              // the actual parser
    destroy_function	destroy;            // and its dtor
    void *				param;              // parser parameter
	char *				name;				// name of that parser (or 2nd param to interpreter); completely ignored by cparsing .. (1)
    short				flags;
    short				flags_mask;			// normally, flags are provided by caller - explicitly setting the flags on nested parsers shall overwrite, so the mask determines which local flags are to be used
    int					refcount;           // remove parser for real if refcount reaches 0 in dtor
};
typedef struct Parser Parser;

enum
{
	PARSER_FAILED = 0,
	PARSER_MATCH
};

void free_parser(Parser *p);									// calls p->destroy(p)

void set_whitespace_pattern(Parser *w);							// customize whitespaces - 'w' is either result of Word(), or 0L which resets to default-whitespaces
void free_spaces(void);											// if you want to be complete, call this before the end

// ## Basic parser
Parser *Word(const char *character_pool, int min, int max);		// matches a word made up by symbols of character_pool with given minimal and maximal len (-1 == any len including zero)

Parser *CharNotIn(void *character_pool, int min, int max);		// inverse of 'Word()'

Parser *Literal(char *string_to_match);							// exact match of given string_to_match - may not be empty
Parser *LiteralRegion(Region *region_to_match);					// as before, using a region 

Parser *StringSpec(char quote, char escape);					// parse string constants, honoring escaped quotes

Parser *String(void);											// std string is quote='"', escape='\\'

Parser *Base2(int min_value, int max_value);                    // matches numbers with given minimum and maximum _values_ to allow for quick range-checks; Number of base 2 ..
Parser *Base10(int min_value, int max_value, int is_signed);    // .. of base 10 ..
Parser *Base16(int min_value, int max_value);                   // .. and base 16, which is IGNORE_CASE by default

Parser *Eol(void);												// matches at the end of the input-line
																// ATTENTION: use CaptureUntil(1, Skipped(Eol())), if you want the rest of the line:
																// if you do not 'include_match', the parser will loop for ever on the first newline

Parser *Bytes(int size, int min, int max);						// matches byte-blocks of size 'size' in bytes - min/max is block-count with the usual '-1' for 'any'
Parser *Int8(int min, int max);									// some shortcuts to 'Bytes' for block-sizes 1 up to 32..
Parser *Int16(int min, int max);
Parser *Int24(int min, int max);
Parser *Int32(int min, int max);
Parser *Int40(int min, int max);
Parser *Int48(int min, int max);
Parser *Int56(int min, int max);
Parser *Int64(int min, int max);
Parser *Int128(int min, int max);
Parser *Int256(int min, int max);

typedef struct
{
	int min;
	int max;
	int size;
} MinMaxSize;

typedef MinMaxSize (*min_max_size_getter)(void *param);

Parser *DynBytes(min_max_size_getter f, void *param);

Parser *Bitfield(Parser *source, unsigned bit_count, ...);

// ## combinatoric parser
Parser *Sequence(Parser *p1, ...);                              // left-to-right sequence of parsers, terminated by '0L' - all given parser must match in the given order for the sequence to match
Parser *SequenceL(Parser *parser_list);

Parser *MatchAll(Parser *p1, ...);								// this is the 'And' operation: matches, if each parser matches against the whole input (left-to-right) - passes every parser's tokens
Parser *MatchAllL(Parser *parser_list);	

Parser *Or(Parser *p1, ...);									// set of alternative parsers, left-to-right: first match gets chosen
Parser *OrL(Parser *parser_list);

Parser *Not(Parser *p);											// negates p's parse-result - does not advance in input, won't ever emit any token;
																// it's purpose are pre-checks, used in the other combinatoric parsers
Parser *Len(int min_len, int max_len, Parser *producer);		// test each tokens length: -1: any - Len(0,..) == Len(-1,..) => Optional; Len(0, -1, p) == Len(-1, -1, p) == MatchAlways

#define And MatchAll

Parser *Chain(Parser *producer, Parser *consumer);
Parser *Distribute(Parser *source, Parser *p1, ...);			// call p1->parse(source token 1), p2->parse(source token 2), ...
Parser *Data(void);												// Emits its input-region => for use in Distribute's consumers

Parser *Repeat(Parser *p, int min, int max);					// checks for min..max occurances of 'p' and fails if more os less are found. '-1' for min or max means 'any including zero'
Parser *Optional(Parser *p);									// Repeat(p, 0, 1)
Parser *OneOrMore(Parser *p);									// Repeat(p, 1, -1)
Parser *ZeroOrMore(Parser *p);									// Repeat(p, 0, -1)

Parser *Until(Parser *mark, Parser *consumer);					// repeat parsing with consumer until mark is found. 'mark' will not be emitted; the input-position is updated
																// to point at the mark.

// DynRepeat fetches its intervall each time it is run
typedef struct
{
	int min;
	int max;
} MinMax;

typedef MinMax (*min_max_getter)(void *param);

Parser *DynRepeat(Parser *p, min_max_getter f, void *vparam);

#define Any ZeroOrMore											// even shorter ...
#define Some OneOrMore

Parser *ElementOf(Dict *dict);                              // looks up valid regions in 'dict' - if a match was found, dict->get_value() returns the associated value until next lookup
															// (i.e. when used as an argument to 'Edit' there is no need to repeat the query)

Parser *ForwardDeclaration(void);								// used for recursive definitions: when is parser is referred before actually being defined, 'forward declare'
																// the parser and later 'forward assign' its completed definition
																// OF COURSE this can lead to infinite loops - there should be a section discussing recursive grammars in the docu (TODO)
Parser *ForwardAssign(Parser *forward_declaration, Parser *assignment);

enum
{
	EXCLUDE_MATCH=0,
	INCLUDE_MATCH 
};
Parser *CaptureUntil(int include_match, Parser *mark);			// search for 'mark' - result is [begin, first_match][, consuming the mark if include_match is true
																// warning: current implementation is dumb linear search, repeatedly calling 'mark'

Parser *Record(Parser *p);										// capture p's results and save them on first call to 'parse' - replay the results on further calls to 'parse';
																// replaying will not change the input region / yields same result as p
																// ATTENTION: remember to Ref() your Record-parsers correctly (Record is one of the few parser that
																// need to be referenced more than once, while otherwise Ref()s are of suprisingly rare usage, 
																// so you are likely to forget about them)

Parser *Nth(unsigned index, Parser *p);							// emits the nth token of p only; fails if p emits less than index+1 token

// now our parsers are becomming less parsish, but more convertive
Parser *Pack(Parser *p);										// pack results of 'p' into one continous region

Parser *Join(Region *filler, char flags, Parser *p);			// like pack, but inserting 'filler' in between each token and optional WITH_HEAD or WITH_TAIL
enum
{
	JOIN_WITH_HEAD=0x01,	// used as bit-mask
	JOIN_WITH_TAIL=0x02
	// value 0x80 used internally
};

// An Editor may modify a given range itself by changing the frame pointers
// Called when receiving a TOKEN_VALUE (*), @returning false results in TOKEN_FAILED
typedef int (*Editor)(void *param, char **from, char **to);
// ((*) we could call the editor in TOKEN_COMPLETED, thus delaying the conversion until
// the parser succeeds, but this could result in -potentially a lot- of unnecessary parser work
// if the editor were to reject a token)
enum 
{
	EDIT_FAILED = 0,
	EDIT_OK
};

// allows for semantic checks and re-coding of token
Parser *Edit(Editor editor, void *editor_param, Parser *source);        // editor_param is relayed as 1st param to editor

// Some standard recoder aka converter for Edit:
int	str2byte(void *base_of_number, char **from, char **to);
int	char2str(void *optional_format_string, char **from, char **to);	    // format defaults to "%c"
int	str2int(void *base_of_number, char **from, char **to);              // directly cast the base into (void *) - 0 defaults to 10 == Base10
int	int2str(void *optional_format_string, char **from, char **to);	    // format defaults to "%d"
int	str2short(void *base_of_number, char **from, char **to);
int	short2str(void *optional_format_string, char **from, char **to);    // format defaults to "%ud"
int	str2long(void *base_of_number, char **from, char **to);
int	long2str(void *optional_format_string, char **from, char **to);     // format defaults to "%ld"
int	str2double(void *unused, char **from, char **to);
int	double2str(void *optional_format_string, char **from, char **to);   // format defaults to "%lf"
int	str2str(void *format, char **from, char **to);					    // NO default-format

// TODO: str2float ...

int	byte2short(void *unused, char **from, char **to);
int	byte2int(void *unused, char **from, char **to);
int	byte2long(void *unused, char **from, char **to);
int	short2int(void *unused, char **from, char **to);
int	short2long(void *unused, char **from, char **to);
int	int2long(void *unused, char **from, char **to);

int bytes2bits(void *optional_01_symbols, char **from, char **to);

int	short_h2n(void *unused, char **from, char **to);
int	short_n2h(void *unused, char **from, char **to);
int	long_h2n(void *unused, char **from, char **to);
int	long_n2h(void *unused, char **from, char **to);

// results in address of region - region is not modified, copied nor moved
int	region2pointer(void *unused, char **from, char **to);

// yep, silly name: creates Region of *from, *to and emits it
int region2region(void *unused, char **from, char **to);

// allocates parse-pool memory for region, copies its contents, terminates with '\0'
// and results in address of allocated mem as the new region
int region2string(void *unused, char **from, char **to);

// add the current token to the given dict
int	add2dict(void *dict, char **from, char **to);

// call char (*f)(char) or int (*f)(int) for every char [*from, *to], replacing input by result
int map_chars(void *function, char **from, char **to);

// helper from recoder.c: rt-alloc and print / result in *strp
int psprintf(char **strp, const char *fmt, ...);

int expand_to_size(void *param, char **from, char **to);


// Wrapper which will construct the matching Edit-parser, i.e. ToInt(p) will result in Edit(str2int, NULL, p)
Parser *ToIntB(int base,Parser *);
Parser *ToShortB(int base,Parser *);
Parser *ToLongB(int base,Parser *);
Parser *Subst(char *,Parser *);
Parser *DictSubst(Dict *substitutions);				// TODO: unify *Subst usage
Parser *AddToDict(Dict *,Parser *);
Parser *ToByte(Parser *);
Parser *ToInt(Parser *);
Parser *ToShort(Parser *);
Parser *ToLong(Parser *);
Parser *ToDouble(Parser *);
Parser *ToStr(Parser *);
Parser *ToPtr(Parser *);
Parser *ToRegion(Parser *);
Parser *ToLower(Parser *);
Parser *ToUpper(Parser *);
Parser *ToBits(Parser *);				// 'to bit-string' actually, as in "100110"
Parser *Htons(Parser *);
Parser *Ntohs(Parser *);
Parser *Htonl(Parser *);
Parser *Ntohl(Parser *);

Parser *Lookahead(Parser *p);							// runs 'p' returning its result but does not yield token and does not change input position

#define Put(T,P) _Put((T), sizeof(*(T)), P)             // Put copies parser result to given locations - parser must result in exactly 1 token
Parser *_Put(void *target, size_t size, Parser *source);	// target's size must be greater or equal the size of the received token


#define PutStruct(PTR,TYPE,MEMBER,source)	_PutStruct((void **) &PTR, offsetof(TYPE,MEMBER), sizeof(((TYPE *)0)->MEMBER), source)
Parser *_PutStruct(void **base, size_t offset, size_t size, Parser *source);

Parser *Emit(char *from, size_t len);					// even less of a parser then Edit or Put - just emits the given region and will always succeed;
															// if len is 0, strlen(from) will be used
Parser *EmitRegion(Region *r);							// same with Region

// Call() will call the given ctor with all received token of data_source accessible
typedef int (*ctor_function)(void *user_data, InterpreterInfo I, Parser *data_source);		// InterpreterInfo is provided to allow for token to be generated and be passed up

Parser *Call(ctor_function ctor, void *user_data, Parser *p);								// calls ctor if p succeeds - ctor_function's data_source is the Record-ed result of p;
																							// The call itself consumes all received token, but ctor may use the provided interpreter 'I' to inject results

typedef int (*parser_callback_function)(void *arg_in, void **arg_out);
Parser *Callback(parser_callback_function function, void *arg_in, void **arg_out);	// calls the given function providing the given args
																							// mostly used for object creation or book-keeping
																							// WARNING: since the CB-function must cast the arguments to
																							// the proper types, DOUBLE CHECK the parameter types --
																							// as we prevent the compiler from doing so.

typedef void (*error_function)(void *user, const const char *error_message, Parser *failed_parser, Region *failed_input); 
Parser *OnFail(void *user, error_function error_handler, const char *error_message, Parser *p);			// OnFail calls the error-handler only if 'p' failed..

Parser *Fail(void *user, error_function error_handler, const char *error_message);						// ..while Fail always fails (i.e. report error in required Or-choice)
Parser *OK(void);																			// OK just succeeds

Parser *Print(char *delimiter, Parser *source);				// prints all received token to stdout, separating them with 'delimiter'

// in cparsing_printf.h
// Parser *Printf(char *fmt, Parser *p1, ...);					// printf to stdout for parse-results

// ## modifying wrapper
// these wrappers just pass the given parser after applying parse options
Parser *Flagged(short flags, short mask, Parser *p);	// Assign (overwrite) client side flags (skip token, ignore case, no auto skipping of whitespaces, trace and weak max)
Parser *Caseless(Parser *p);							// set IGNORE_CASE flag
Parser *Skipped(Parser *p);								// set SKIP_TOKEN flag
Parser *Weak(Parser *p);								// set WEAK_MAX flag	(Repeat, Bytes and Word)
Parser *Strong(Parser *p);								// clear WEAK_MAX flag	(Print..)
Parser *Binary(Parser *p);								// set DONT_SKIP_SPACES flag
Parser *Traced(Parser *p);								// set TRACED flag
Parser *Named(char *name, Parser *p);					// a tool to set parser's name
Parser *Restart(Parser *p);								// set RESTART flag - ATTENTION: if applied to p itself, p will restart in every occurence - 
														// which is probably not wanted (and may easily lead to endless loops); 
														// however: using Restart(Ref(p)) will restart this occurence of p only and leave others unmodified

// service function to access Record-ed token
int _get_nth_token(void *target, size_t target_size, Editor e, void *eparam, int n, Parser *pointer_to_Record);
#define get_nth_token(TARGET,EDITOR,EPARAM,N,SOURCE) _get_nth_token((TARGET), sizeof(*(TARGET)), (EDITOR), (EPARAM), (N), (SOURCE))

// access Record'ed data with these fcns
typedef struct { void *head, *current; } RecordIterator;
void record_init_interator(RecordIterator *i, Parser *p);
int record_get_next(RecordIterator *iterator, Region *target);
int record_get_length(Parser *p);
int record_iterator_at_end(RecordIterator *iterator);
int record_get_nth(Parser *p, int idx, Region *target);
Parser *record_to_parser_list(Parser *data_source);
void restart_Record(Parser *rec);								// restart a Record()er from within a C-function - 

// Runtime memory alloction pool
// initialized automatically but needs cleanup: call cparsing_cleanup() before exiting the application -
// when used in multithreaded code, cparsing_cleanup() needs to be called once in each used thread before the termination of the thread
void *runtime_pool(void);							// pool to be used during parser-evaluation
void *detach_runtime_pool(void);					// freezes the current memory pool and returns a handle to the frozen pool
void restore_runtime_pool(void *pool);				// reactivates its argument as current parse-pool -- all allocations on the previously active pool are freed
void cparsing_cleanup(void);						// final cleanup _except_ for detached pools, who need to be freed manually by calling free_mem_pool() ..
void clear_runtime_pool(void);

// Shortcuts to be used in functions called during parse()-runs
#define parse_malloc(LEN)			pool_malloc((LEN), runtime_pool())
#define parse_calloc(AMT,LEN)		pool_calloc((AMT), (LEN), runtime_pool())
#define parse_strndup(SRC,MAX)		pool_strndup((SRC), (MAX), runtime_pool())
#define parse_memdup(SRC,LEN)		pool_memdup((SRC), (LEN), runtime_pool())
#define parse_owned_mem(PTR)		pool_owned_mem((PTR), runtime_pool())
#define parse_ftdup(FROM,TO)		pool_ftdup((FROM), (TO), runtime_pool())

// ---------------------------------------------------------------------------------------------------
//
// Parser *Ref(Parser *p):
// <<Proxy>> - // adds required management-header to a re-used parser.
//
// ---------------------------------------------------------------------------------------------------
// >>>>> Add the parser itself in one place only - // use Ref(<parser>) on any further occurances <<<<<
// ---------------------------------------------------------------------------------------------------
// i.e:
//
// Parser *p = ...;                                    // any parser definition
// Parser *q = Sequence(p, Ref(p), Ref(p));
// Parser *r = OneOrMore(Ref(p));                       // OK: first occurance of p is 'raw' while further occurances are being individually 'Ref'ed.
//
// *** ATTENTION *** THIS IS WRONG *** :
// Parser *p = ...;                                    // any parser definition
// Parsre *q = Sequence(p, p, ...);						// WRONG: 2nd occurance of p without Ref
//
// Parser *single_ref_to_p = Ref(p);
// Parser *r = Sequence(p, single_ref_to_p, .., single_ref_to_p, ..);     // DO NOT DO THIS: only one instance of Ref used to back-up two occurances of p
//
// Keep in mind that since Ref needs to act as a proxy, it needs to copy the proxied-parser's flags - overwriting these may lead into problems (restarting a Recorder() by a ref to it is OK however).
Parser *Ref(Parser *p);                         			// used in combinator definitions: make own reference to 'p'

Parser *Foreign(Parser *src);								// like Ref(), but proxied parser will not be destroyed when the proxy gets destroyed.
															// allows to (re-)use parsers in different or nested parser-trees (TODO: explain the word parser-tree)

// ************************************************************************************************************
// BTW: often, it may be easier just to #define parser-definitions for re-use - though sometimes Ref is needed, 
// i.e. most likely when using ForwardDefiniton or Record
// ************************************************************************************************************

#endif // __CPARSING_H__

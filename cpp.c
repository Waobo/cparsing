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
// #include "cparsing_meta.h"
#include "cparsing_impl.h"

#define SymbolIdentifier	Pack(Binary(Sequence(Weak(Word(ALPHA, 1, 1)), Optional(Word(ALPHANUM, 1, -1)), 0L)))
#define BindingToken		CharNotIn(DEFAULT_WHITE_CHARS "\\", 1, -1)

struct symbol_token
{
    LIST_OF(struct symbol_token);
	Region token;
};
typedef struct symbol_token symbol_token;

symbol_token *make_binding(void)
{
symbol_token *result = parse_malloc(sizeof(symbol_token));

	if(result) list_init(result);
	return result;
}

static int symbol_token_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
symbol_token *result = make_binding();
RecordIterator ri;
Region tmp;
// char *where = (char *) vdata;

	// printf("IN %s\n", where);

	if(!result) return EDIT_FAILED;

	record_init_interator(&ri, data_source);

	while(record_get_next(&ri, &tmp))
	{
	symbol_token *act = make_binding();

		if(!act) return EDIT_FAILED;
		act->token = tmp;
		list_append_end((list *) result, (list *) act);

		// print_ft(tmp.begin, tmp.end); printf("\n");
	}

	if(I.interpreter)
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(symbol_token)-1);

	return EDIT_OK;
}

typedef struct
{
	symbol_token *arguments;
	Region bindings;
} symbol_declaration;

void dump_symbol_declaration(Dict *symbols, char *name)
{
symbol_token *sym;
symbol_declaration *decl;

	printf("%s:\n", name);

	if(!dict_lookup(symbols, name, (void **) &decl))
	{
		printf("'%s' Not found.\n", name);
		return;
	}

	printf("\targs: ");
	if(list_empty((list *) decl->arguments))
		printf("<None>\n");
	else
	list_for_each(symbol_token, sym, decl->arguments)
	{
		print_ft(sym->token.begin, sym->token.end);
		printf(" ");
	}
	printf("\n");

	printf("\tbind: ");
	if(REGION_EMPTY(&decl->bindings))
		printf("<None>\n");
	else
		print_ft(decl->bindings.begin, decl->bindings.end);
	
	printf("\n");

}

static int symbol_declaration_ctor(void *vdata, InterpreterInfo I, Parser *data_source)
{
Dict *symbols = (Dict *) vdata;
symbol_declaration *result;
RecordIterator ri;
Region name;
Region tmp;

	if(!vdata) return EDIT_FAILED;

	result = malloc(sizeof(symbol_declaration));
	if(!result) return EDIT_FAILED;
	result->arguments = 0;
	result->bindings.begin = result->bindings.end = result->bindings.current = 0L;

	record_init_interator(&ri, data_source);

	(void) record_get_next(&ri, &name);

	if(record_get_next(&ri, &tmp))
		result->arguments = (symbol_token *) tmp.begin;

	if(record_get_next(&ri, &tmp))
		result->bindings = tmp;

	symbols->add(symbols, &name, result);

	return EDIT_OK;
}

#if 0
Parser *ExpandSymbols(Dict *symbols)
{
Parser *result =
	Sequence(
		Or(
			// concat
			Binary(Sequence(
				Optional(Literal("##")), 
				ToBeExpanded,
				Optional(Literal("##")),
			0L)),
			// quoted
			Binary(Sequence(
				Literal("#"), 
				ToBeExpanded,
			0L)),
			// normal expand
			ToBeExpanded,
			// no expand
			CharNotIn(DEFAULT_WHITE_CHARS, 1, 1),
			Eol(),
		0L),
	0L);

	return result;
}

#endif

void parse_and_print_string_lol(Region *txt, Parser *p);
void dump_ListDict(Dict *d);

#define has_arguments(S) (list_not_empty((list *) (S)->arguments))
#define has_bindings(S) (list_not_empty((list *) (S)->bindings))

static int expand_symbol(void *vdata, InterpreterInfo I, Parser *data_source)
{
Dict *symbols = (Dict *) vdata;
Dict *substitutions = 0L;
symbol_declaration *sym;
symbol_token *token;
RecordIterator ri;
Region name;
Region value;
Region after;
int result = EDIT_FAILED;
Parser *concret_replacement;
Region _space = INIT_STR_REGION(" ");

	if(!vdata) return EDIT_FAILED;

	record_init_interator(&ri, data_source);
	// get symbols name ..
	(void) record_get_next(&ri, &name);

	// .. and lookup its definition
	if(!dict_lookup(symbols, name.begin, (void **) &sym))
		return EDIT_FAILED;

	if(has_arguments(sym))
	{
		// build substitution for arguments
		substitutions = new_list_dict();
		list_for_each(symbol_token, token, sym->arguments)
		{
			if(!record_get_next(&ri, &value))
			{
				printf("symbol '%s' missing parameters.\n", name.begin);
				goto expand_symbol_out;
			}
			substitutions->add(substitutions, &token->token, value.begin);
		}
	}

	// end of arguments.. so data_source has to be empty now
	if(!record_iterator_at_end(&ri))
	{
		printf("bogus arguments found for symbol '%s'.\n", name.begin);
		return EDIT_FAILED;
	}

	init_region(&after, "<empty>", 0L);
	
	concret_replacement = 
		Put(&after,
			ToRegion(
				Join(&_space, 0,
					OneOrMore(
						Or(
							DictSubst(substitutions),
							CharNotIn(DEFAULT_WHITE_CHARS "\\", 1, -1),
						0L)
					)
				)
			)
		);

	// printf("Binding: "); print_ft(sym->bindings.begin, sym->bindings.end); printf("\n");

	concret_replacement->parse(concret_replacement, I0, &sym->bindings);
	
expand_symbol_out:
	if(substitutions) substitutions->destroy(substitutions);

	return result;
}

Parser *CapExpander(Dict *symbols)
{
Parser *macro_instance = 
	Call(expand_symbol, symbols, Sequence(
		ElementOf(symbols),
		Optional(Sequence(
			Skipped(Literal("(")),
			SymbolIdentifier,
			ZeroOrMore(
				Sequence(
					Skipped(Literal(",")),
					SymbolIdentifier,
				0L)
			),
			Skipped(Literal(")")),
			0L)), 
		0L)
	);

#define GLUE Optional(Binary(Literal("##")))
#define QUOTE Binary(Literal("#"))

Parser *result =
	Or(
		Sequence(GLUE, macro_instance, GLUE, 0L),
		Sequence(QUOTE, Ref(macro_instance), 0L),
	0L);


	return result;
}


Parser *SimpleExpander(Dict *symbols)
{
Parser *result =
	Call(expand_symbol, symbols, Sequence(
		ElementOf(symbols),
		Optional(Sequence(
			Skipped(Literal("(")),
			SymbolIdentifier,
			ZeroOrMore(
				Sequence(
					Skipped(Literal(",")),
					SymbolIdentifier,
				0L)
			),
			Skipped(Literal(")")),
			0L)), 
		0L)
	);

	return result;
}


Parser *SymbolDeclaration(Dict *symbols)
{
Region _space = INIT_STR_REGION(" ");
Parser *result =
	Call(symbol_declaration_ctor, symbols, Sequence(
		Skipped(Literal("#define")),
		SymbolIdentifier,
		Call(symbol_token_ctor, "1st", Or(Sequence(
			Skipped(Literal("(")),
			SymbolIdentifier,
			ZeroOrMore(
				Sequence(
					Skipped(Literal(",")),
					SymbolIdentifier,
				0L)
			),
			Skipped(Literal(")")),
			0L),
			Lookahead(Binary(CharNotIn("(", 1, -1))),
		0L)),

		// Call(symbol_token_ctor, "2nd", ZeroOrMore(BindingToken)),
		Join(&_space, 0, ZeroOrMore(BindingToken)),
	0L));

	return result;
}

// ======================================================================================

static int _expand_single_instance(void *vdata, InterpreterInfo I, Parser *data_source)
{
Dict *symbols = (Dict *) vdata;
Dict *substitutions = 0L;
symbol_declaration *sym;
symbol_token *token;
RecordIterator ri;
Region name;
Region value;
Region after;
int result = EDIT_FAILED;
Parser *concret_replacement = 0;
Region _space = INIT_STR_REGION(" ");

	if(!vdata) return EDIT_FAILED;

	record_init_interator(&ri, data_source);
	// get symbols name ..
	(void) record_get_next(&ri, &name);

	// .. and lookup its definition
	if(!dict_lookup(symbols, name.begin, (void **) &sym))
		return EDIT_FAILED;

	if(has_arguments(sym))
	{
		// build substitution for arguments
		substitutions = new_list_dict();
		list_for_each(symbol_token, token, sym->arguments)
		{
			if(!record_get_next(&ri, &value))
			{
				printf("symbol '%s' missing parameters.\n", name.begin);
				goto expand_symbol_out;
			}

			printf("* "); print_ft(value.begin, value.end); printf(" *\n");
			substitutions->add(substitutions, &token->token, parse_ftdup(value.begin, value.end));
		}
	}

	// end of arguments.. so data_source has to be empty now
	if(!record_iterator_at_end(&ri))
	{
		printf("bogus arguments found for symbol '%s'.\n", name.begin);
		return EDIT_FAILED;
	}

	//	if(substitutions) dump_ListDict(substitutions);

	init_region(&after, "<empty>", 0L);
	
	concret_replacement = 
		Put(&after,
			ToRegion(
				Join(&_space, 0,
					OneOrMore(
						Or(
							DictSubst(substitutions),
							CharNotIn(DEFAULT_WHITE_CHARS "\\", 1, -1),
						0L)
					)
				)
			)
		);

	concret_replacement->parse(concret_replacement, I0, &sym->bindings);
	
	if(I.interpreter)
		I.interpreter(data_source, I.param, TOKEN_VALUE, after.begin, after.end);

expand_symbol_out:
	if(concret_replacement) concret_replacement->destroy(concret_replacement);
	if(substitutions) substitutions->destroy(substitutions);

	return result;
}


static int _concat_substituded_1(void *vdata, InterpreterInfo I, Parser *data_source)
{
RecordIterator ri;
Region token;
static char *_space = " ";

	if(I.interpreter)
	{
	printf("_concat_substituded_1\n");
		record_init_interator(&ri, data_source);
		(void) record_get_next(&ri, &token);
		I.interpreter(data_source, I.param, TOKEN_VALUE, token.begin, token.end);

		if(!record_get_next(&ri, &token))
			I.interpreter(data_source, I.param, TOKEN_VALUE, _space, _space);
	}

	return PARSER_MATCH;
}

static int _concat_substituded_2(void *vdata, InterpreterInfo I, Parser *data_source)
{
RecordIterator ri;
Region token;
static char *_space = " ";

	if(I.interpreter)
	{
	printf("_concat_substituded_2\n");
		record_init_interator(&ri, data_source);
		(void) record_get_next(&ri, &token);

		I.interpreter(data_source, I.param, TOKEN_VALUE, _space, _space);
		I.interpreter(data_source, I.param, TOKEN_VALUE, token.begin, token.end);

		if(!record_get_next(&ri, &token))
			I.interpreter(data_source, I.param, TOKEN_VALUE, _space, _space);
	}

	return PARSER_MATCH;
}

// Call(_quote_symbol,  symbols, Binary(Sequence(Skipped(QUOTE), KEYWORD(symbols), 0L))),
static int _quote_substituded(void *vdata, InterpreterInfo I, Parser *data_source)
{
RecordIterator ri;
Region expanded;
static char *_quotes = "\"";

	if(I.interpreter)
	{
		record_init_interator(&ri, data_source);
		(void) record_get_next(&ri, &expanded);

		I.interpreter(data_source, I.param, TOKEN_VALUE, _quotes, _quotes);
		I.interpreter(data_source, I.param, TOKEN_VALUE, expanded.begin, expanded.end);
		I.interpreter(data_source, I.param, TOKEN_VALUE, _quotes, _quotes);
	}

	return PARSER_MATCH;
}

#undef GLUE
#undef QUOTE

#define GLUE Binary(Literal("##"))
#define QUOTE Binary(Literal("#"))
#define KEYWORD(D) Binary(ElementOf(D))
Parser *CppExpander(Dict *symbols)
{
Parser *substitution_parameter_assignment =	CaptureUntil(EXCLUDE_MATCH, Word(",)", 1, 1));

Parser *substitution_instanciation = 
	Call(_expand_single_instance, symbols,
		Binary(Sequence(
			ElementOf(symbols),
			Optional(Sequence(
				Skipped(Literal("(")),
				substitution_parameter_assignment,
				ZeroOrMore(Sequence(
						Skipped(Literal(",")),
						Ref(substitution_parameter_assignment),
				0L)),
				Skipped(Literal(")")),
			0L)),
		0L))
	);

	return Traced(substitution_instanciation);

#if 0
Parser *substitution_exp = 
	Or(
		Call(_concat_substituded_1, 0L, Binary(Sequence(GLUE, substitution_instanciation, Optional(GLUE), 0L))),
		Call(_concat_substituded_2, 0L, Binary(Sequence(Ref(substitution_instanciation), Optional(GLUE), 0L))),
		Call(_quote_substituded,  0L, Binary(Sequence(Skipped(QUOTE), Ref(substitution_instanciation), 0L))),
		Skipped(Eol()),
	0L);

Parser *p =
	OneOrMore(
		Sequence(
			CaptureUntil(EXCLUDE_MATCH, substitution_exp),
			Ref(substitution_exp),
		0L)
	);

	return Traced(p);
#endif
}


Parser *get_c_preprocessor_parser(Dict *definitions)
{
	return 0L;
}



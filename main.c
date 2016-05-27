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
#include "lol.h"
#include "cparsing_meta.h"
#include "cparsing_printf.h"

// lib expects a definition of 'print'
void print( const char* format, ... )
{
va_list args;

    va_start( args, format );
    vprintf( format, args );
    va_end( args );
    fflush(stdout);
}

// ---------------------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------------
// Infrastructure to convert parser-results into a tree-like structure - here a list of lists (see lol.h/lol.c).
// (stores all token found simply as strings in the leaf-nodes)

#define COL1_WIDTH 40

#define TRACE_NESTED_LIST_ACTION

int s_indent = 0;
// the main interpreter for most of the demos builds nested lists of strings to express tree structures
int to_nested_string_list(Parser *self, void *param, int mode, char *from, char *to)
{
stack *stk = (stack *) param;
ListOList *tos;
char *value;
char *msg_entry;

    assert(self);
    assert(stk);

    switch(mode)
    {
    case TOKEN_ENTER:
#ifdef TRACE_NESTED_LIST_ACTION
        { print("%*sENTER %*s >", s_indent, "", COL1_WIDTH-s_indent, self->name); print_ft(from, to); print("<\n");; s_indent++; }
#endif
        if(self->flags & COMBINATOR)
            stack_push(stk, LOL_container());
        break;

    case TOKEN_VALUE:
        value = strdup_ft(from, to);

        // print("<"); print_ft(from, to); print(">");

		// in this demo, we tag the token with the parser's name if provided
        if(self->name)
        {
            msg_entry = (char *) malloc(strlen(value) + strlen((char *) self->name) + 1 /* ':' */ + 1 /* 0 */);
            sprintf(msg_entry, "%s:%s", (char *) self->name, value);
            free(value);
        } else {
            msg_entry = value;
        }

        LOL_append_string((ListOList *) stack_tos(stk), msg_entry);
        break;

    case TOKEN_COMPLETED:
        if(self->flags & COMBINATOR)
        {
            tos = (ListOList *) stack_pop(stk);                    // the transform function may have pushed a DATA_ELEMENT here
            assert(list_not_empty((list *) stk));                  // at least 'root' on the stack

            // don't stack containers just for fun
            if((tos->type == CONTAINER_ELEMENT) && (list_len_is_one((list *) &tos->inv.children)))
            {
            ListOList *child = (ListOList *) list_remove((list *) tos->inv.children.next);

                assert(child);
                LOL_free_data(tos);
                LOL_free(tos);      // discard bogus container
                tos = child;
            }

            assert("TOS is not a container" && ((ListOList *) stack_tos(stk))->type == CONTAINER_ELEMENT);
            LOL_append((ListOList *) stack_tos(stk), tos);

        }
#ifdef TRACE_NESTED_LIST_ACTION
		{ --s_indent; print("%*sMATCH %s\n", s_indent, "", self->name); }
#endif
        break;

    case TOKEN_FAILED:
        if(self->flags & COMBINATOR)
        {
            tos = (ListOList *) stack_pop(stk);
            assert(list_not_empty((list *) stk));
            LOL_free_data(tos);
            LOL_free(tos);
        }
#ifdef TRACE_NESTED_LIST_ACTION
		{ --s_indent; print("%*sFAIL  %*s >", s_indent, "", COL1_WIDTH-s_indent, self->name); print_ft(from, to); print("<\n"); }
#endif
        break;

    default:
        assert("ERROR: to_nested_string_list: unknown mode." && 0);
    }

    return 1;
}

stack *init_nested_string_list_stack(ListOList *l, stack *s)
{
    LOL_init(l, CONTAINER_ELEMENT, 0L);
    list_init(s);
    s->data = 0L;
    stack_push(s, l);
    return s;
}


void parse_and_print_string_lol(Region *txt, Parser *p)
{
int match;
ListOList result;
stack stk, *nested_string_list_stack = init_nested_string_list_stack(&result, &stk);
InterpreterInfo I = { to_nested_string_list, nested_string_list_stack, p->flags };

    match = p->parse(p, I, txt);

    stack_pop(nested_string_list_stack);
    assert(list_empty((list *) &stk));

    print("\n");
    print_LOL(&result);
    print("%s.\n", match ? ": OK" : ": FAIL");
    print("rest: '%s'\n", txt->current);

    LOL_free_data(&result);
    LOL_clear(&result);
}

// ---------------------------------------------------------------------------------------------------------------------------
// --- Tests -----------------------------------------------------------------------------------------------------------------

void words_test(void)
{
Region *txt	= region("Hallo welt", 0L);
// Parser *p	= Word(ALPHA, 1, -1);
Parser *p	= Literal("Hallo");

    parse_and_print_string_lol(txt, p);
	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

void seq_test(void)
{
// Region *txt	= region("Hallo welt", 0L);
Region *txt	= region("Hallo welt", 0L);
Parser *p	= Sequence(Literal("Hallo"), Word(ALPHA, 1, -1), 0L);

    parse_and_print_string_lol(txt, p);
	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

void repeat_or_test(void)
{
Region *txt	= region("Hallo Welt", 0L);
Parser *p	= Some(Or(Literal("Hallo"), Caseless(Literal("welt")), 0L));

    parse_and_print_string_lol(txt, p);
	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

void matchall_test(void)
{
Region *txt	= region("Hallo Felt", 0L);
Parser *p	=	MatchAll(
					Not(CaptureUntil(EXCLUDE_MATCH, Caseless(Literal("v")))),
					OneOrMore(Word(ALPHANUM, 1, -1)),
				0L);

    parse_and_print_string_lol(txt, p);
	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

void string_test(void)
{
Region *txt	= region("\"Hallo \\\\ \\\"Welt\\\"\"", 0L);
Parser *p	= String();

    parse_and_print_string_lol(txt, p);
	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

void base_test(void)
{
Region *txt	= region("0xfe", 0L);
Parser *p	= Sequence(Optional(Literal("0x")), Base16(0, 254), 0L);

    parse_and_print_string_lol(txt, p);
	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

void forward_test(void)
{
Region *txt	= region("Hallo Welt Wie gehts", 0L);
Parser *p1	= ForwardDeclaration();
Parser *p2	= Sequence(Word(ALPHA, 1, -1), Optional(p1), 0L);
Parser *p	= ForwardAssign(p1, p2);

    parse_and_print_string_lol(txt, p);
	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

void demo_calculator(void)
{
// Calculator with exponentiation and recursion
Parser *Expression	= ForwardDeclaration();
Parser *Number		= Word(DIGIT, 1, -1);
Parser *MulOp		= Word("*/", 1, 1);
Parser *AddOp		= Word("+-", 1, 1);
Parser *Exponent    = Or(Number, Sequence(Skipped(Literal("(")), Expression, Skipped(Literal(")")), 0L), 0L);
Parser *Factor		= Sequence(Exponent, Any(Sequence(Literal("^"), Ref(Exponent), 0L)), 0L);
Parser *Term		= Sequence(Factor, Any(Sequence(MulOp, Ref(Factor), 0L)), 0L);
Parser *ExprCore    = Sequence(Term, Any(Sequence(AddOp, Ref(Term), 0L)), 0L);

    ForwardAssign(Expression, ExprCore);

Region *txt         = region("1+2*3^2*1", 0L);
// Region *txt         = region("2*(1+3)+4/1", 0L);
// Region *txt         = region("1+2+3*(1+1+1+1)", 0L);

    parse_and_print_string_lol(txt, Expression);
    Expression->destroy(Expression);
    free_region(txt, 0);
	end_scope();
}

void capture_test(void)
{
Region *txt	= region("Hallo Welt", 0L);
Parser *p	= CaptureUntil(INCLUDE_MATCH, Eol());

    parse_and_print_string_lol(txt, p);
	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

void len_test(void)
{
Region txt	= INIT_STR_REGION("a aa aaa aaaa aaaaa bbbbb");
Parser *p	= Len(1, 20, Pack(OneOrMore(Word(ALPHANUM, 1, -1))));

    parse_and_print_string_lol(&txt, p);
	p->destroy(p);
}

void pack_test(void)
{
Region *txt	= region("Hallo Welt Wie gehts", 0L);
Parser *p1	= ForwardDeclaration();
Parser *p2	= Sequence(Word(ALPHA, 1, -1), Optional(p1), 0L);
Parser *p	= Pack(ForwardAssign(p1, p2));

    parse_and_print_string_lol(txt, p);
	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

void test_hex_to_int(void)
{
Region *txt = region("17 23 404 555555555", 0L);
Parser *p = Edit(int2str, 0, Edit(str2int, (void *) 16, OneOrMore(Word(HEX, 1, -1))));

    parse_and_print_string_lol(txt, p);

	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

void edit_test(void)
{
Region *txt = region("17 23 404 555555555555555555555555555555555", 0L);
Region *txt2 = region("17 23 404 555555555", 0L);
Parser *p = Edit(int2str, "0x%x", Edit(str2int, 0L, OneOrMore(Word(DIGIT, 1, -1))));

    parse_and_print_string_lol(txt, p);
	printf("---\n");
    parse_and_print_string_lol(txt2, p);

	p->destroy(p);
	free_region(txt, 0);
	free_region(txt2, 0);
	end_scope();
}

void identifier_test(void)
{
Region *txt = region("_id", 0L);
Parser *p	= Pack(Binary(Sequence(Weak(Word(ALPHA, 1, 1)), Optional(Word(ALPHANUM, 1, -1)), 0L)));

    parse_and_print_string_lol(txt, p);
	p->destroy(p);
	free_region(txt, 0);
}


void dict_test(void)
{
char *samples[] = { "Hallo", "Welt", 0L };
void *vals[] = { "greeting", "subject", 0L };
Region *txt = region("Hallo Hallo Welt Outta", 0L);
Dict *d = new_list_dict();
Parser *p = OneOrMore(ElementOf(d));
DictIterator di;
Region key;
void *value;

	dict_add_array(d, samples, vals);

	di = d->iterator(d);

	while(di.get_next(&di, &key, &value))
		printf("=> %s: %s\n", key.begin, (char *) value);

    parse_and_print_string_lol(txt, p);

	p->destroy(p);
	d->destroy(d);
	free_region(txt, 0);
	end_scope();
}

void test_emit(void)
{
Region *txt = region("Hallo Welt", 0L);
Parser *entry = Pack(Sequence(Word(ALPHA, 1, -1), (Emit("-san", 0)), 0L));
Parser *p = Some(entry);

    parse_and_print_string_lol(txt, p);

	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

void put_test(void)
{
int  h=0, m=0;
Region *txt = region("12:59", 0L);
Parser *p = Sequence(Put(&h, ToInt(Base10(0,23,0))), Literal(":"), Put(&m, ToInt(Base10(0,59,0))), Optional(Caseless(Literal("h"))), 0L);

    // parse_and_print_string_lol(txt, p);

	if(p->parse(p, I0, txt))
		printf("=> %02d Uhr und %02d Minuten.\n", h, m);
	else
		printf("Invalides Uhrzeit-Format.\n");

	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}


typedef struct 
{
	char *user;
	char *pw;
	int uid;
	int gid;
	char comment[128];
	char *home;
	char *shell;
} passwd_entry;

void dump_passwd_entry(passwd_entry *p)
{
	printf("User:     %s\n", p->user);
	printf("Password: %s\n", p->pw);
	printf("UID:      %d\n", p->uid);
	printf("GID:      %d\n", p->gid);
	printf("Comment:  %s\n", p->comment);
	printf("Home:     %s\n", p->home);
	printf("Shell:    %s\n", p->shell);
}

InterpreterInfo no_interpreter = {0, 0, 0, 0};

void put_test2(void)
{
Region *txt = region("gnats:x:41:41:Gnats Bug-Reporting System (admin):/var/lib/gnats:/bin/sh", 0L);
passwd_entry e;
Parser *sep		= Skipped(Literal(":"));
Parser *entry	= CharNotIn(":", 1, -1);
Parser *comment_entry	= CharNotIn(":", 1, 127);
Parser *p		= Sequence(
					Put(&e.user,	ToStr(entry)),			sep, 
					Put(&e.pw,		ToStr(Ref(entry))), 	Ref(sep),
					Put(&e.uid,		ToInt(Ref(entry))), 	Ref(sep),
					Put(&e.gid,		ToInt(Ref(entry))), 	Ref(sep),
					Put(&e.comment,	comment_entry),			Ref(sep),
					Put(&e.home,	ToStr(Ref(entry))), 	Ref(sep),
					Put(&e.shell,	ToStr(Ref(entry))),
					Eol(),
					0L
				);

	memset(&e, 0, sizeof(e));

    parse_and_print_string_lol(txt, p);
	dump_passwd_entry(&e);

	/*
	if( p->parse(p, no_interpreter, txt) )
		dump_passwd_entry(&e);
	else
		printf("parse failed\n");
	*/


	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

void test_record(void)
{
Region *txt = region("Hallo Welt", 0L);
Parser *entry = Pack(Sequence(Word(ALPHA, 1, -1), (Emit("-san", 0)), 0L));
Parser *line = Record(Some(entry));
Parser *p = Sequence(line, Ref(line), Ref(line), 0L);

    parse_and_print_string_lol(txt, p);

	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

void test_nth(void)
{
Region *txt = region("Hallo Welt", 0L);
Parser *entry = Pack(Sequence(Word(ALPHA, 1, -1), (Emit("-san", 0)), 0L));
Parser *line = Record(Some(entry));
Parser *p = Sequence(Nth(1, line), Nth(0, Ref(line)), Ref(line), 0L);

    parse_and_print_string_lol(txt, p);

	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}


// ----------------------------------------
struct expr;

struct calc_operation
{
	char op;
	struct expr *L, *R;
};
typedef struct calc_operation calc_operation;

struct expr
{
	int is_value;
	union
	{
		calc_operation op;
		long value;
	};
};
typedef struct expr expr;

int expr_op(void *stk, InterpreterInfo I, Parser *source)
{
// Struct(expr.op, .OP = #1, .R = POP, .L = POP)
expr *obj = (expr *) parse_malloc(sizeof(expr));
int ok;

	ok = get_nth_token(&obj->op.op, 0, 0, 0, source);
	assert(ok);

	obj->is_value = 0;
	obj->op.R = (expr *) stack_pop(stk);
	obj->op.L = (expr *) stack_pop(stk);

	stack_push(stk, obj);

	return PARSER_MATCH;
}

int expr_value(void *stk, InterpreterInfo I, Parser *source)
{
// Struct(expr.value, .VALUE = #1)
expr *obj = (expr *) parse_malloc(sizeof(expr));
int ok;

	ok = get_nth_token(&obj->value, str2long, 0, 0, source);
	assert(ok);

	obj->is_value = 1;
	stack_push(stk, obj);

	return PARSER_MATCH;
}

void dump_expr(expr *e, int indent)
{
	if(e)
	{
		if(e->is_value)
			printf("%*s%ld\n", indent, "", e->value);
		else
		{
			printf("%*s%c\n", indent, "", e->op.op);
			dump_expr(e->op.L, indent+1);
			dump_expr(e->op.R, indent+1);
		}
	} else 
		print("%*sNIL\n", indent, "");
}

long eval_expr(expr *e)
{
	assert(e);

	if(e->is_value) return e->value;

	switch(e->op.op)
	{
	case '+':
		return eval_expr(e->op.L) + eval_expr(e->op.R);

	case '-':
		return eval_expr(e->op.L) - eval_expr(e->op.R);

	case '*':
		return eval_expr(e->op.L) * eval_expr(e->op.R);

	case '/':
		return eval_expr(e->op.L) / eval_expr(e->op.R);

	case '^':
		return (long) pow(eval_expr(e->op.L), eval_expr(e->op.R));

	default:
		assert("invalid operator in eval_expr" && 1==0);
	}
}

void calculator_ast_test(void)
{
stack stk;
expr *e;
// Calculator with exponentiation and recursion
Parser *Expression	= ForwardDeclaration();
Parser *Number		= Word(DIGIT, 1, -1);
Parser *MulOp		= Word("*/", 1, 1);
Parser *AddOp		= Word("+-", 1, 1);

Parser *Exponent    = Or(Call(expr_value, &stk, Number), Sequence(Skipped(Literal("(")), Expression, Skipped(Literal(")")), 0L), 0L);
Parser *Factor		= Sequence(Exponent, Any(Call(expr_op, &stk, Sequence(Literal("^"), Ref(Exponent), 0L))), 0L);
Parser *Term		= Sequence(Factor, Any(Call(expr_op, &stk, Sequence(MulOp, Ref(Factor), 0L))), 0L);
Parser *ExprCore    = Sequence(Term, Any(Call(expr_op, &stk, Sequence(AddOp, Ref(Term), 0L))), 0L);

    ForwardAssign(Expression, ExprCore);
	list_init(&stk);

// Region *txt         = region("17+4", 0L);
// Region *txt         = region("1+2*3^2*1", 0L);
Region *txt         = region("2*(1+3)+4/1", 0L);
// Region *txt         = region("1+2+3*(1+1+1+1)", 0L);

    parse_and_print_string_lol(txt, Expression);

	e = (expr *) stack_pop(&stk);
	dump_expr(e, 0);

	print("%s = %ld\n", txt->begin, eval_expr(e));

    Expression->destroy(Expression);
    free_region(txt, 0);
	end_scope();
}

// ----------------------------------------

MinMax exactly_n_times(void *p)
{
int n = *((int *) p);
MinMax result = {n, n};

	return result;
}

MinMax give_or_take_one(void *p)
{
int n = *((int *) p);
MinMax result = {n-1, n+1};

	return result;
}

void dyn_repeat_test(void)
{
int n;
// Region *txt	= region("3 Hallo Hallo Hallo", 0L);
Region *txt	= region("3 Hallo Hallo Hallo Hallo abc", 0L);
Parser *p	= Sequence(Put(&n, ToInt(Base10(1, 10, 0))), DynRepeat(Literal("Hallo"), give_or_take_one, &n), 0L);

    parse_and_print_string_lol(txt, p);
	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}


// ----------------------------------------

MinMaxSize my_get_mms(void *p)
{
int n = *((int *) p);
MinMaxSize result = {1, n, n};

	return result;
}

void dyn_bytes_test(void)
{
int n;
// Region *txt	= region("3 Hallo Hallo Hallo", 0L);
Region *txt	= region("3 Hallo Hallo Hallo abc", 0L);
Parser *p	= Sequence(Put(&n, ToInt(Base10(1, 10, 0))), DynBytes(my_get_mms, &n), 0L);

    parse_and_print_string_lol(txt, p);
	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}


void chain_test(void)
{
Region *txt	= region("66 Hallo Hallo Hallo abc", 0L);
Parser *inner = Bytes(2, 1, 1);
Parser *p = Chain(OneOrMore(Word(ALPHANUM, 1, -1)), inner);

    parse_and_print_string_lol(txt, p);
	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

void chain_test2(void)
{
Region *txt	= region("ABA", 0L);
Parser *p = ForwardDeclaration();
Parser *subst = OneOrMore(Or( 
					Edit(str2str, "BB", Literal("A")),
					Edit(str2str, "A", Literal("B")),
					0
				));
Parser *chain = Chain(subst, p);

	ForwardAssign(p, chain);

    parse_and_print_string_lol(txt, p);
	p->destroy(p);
	free_region(txt, 0);
	end_scope();

}

// ---------------------------------------------------------------------------------------------------------------------------

void restart_test(void)
{
Parser *mem = Record(Word(ALPHANUM, 1, -1));
// Important: Restart() is invoked at a Ref, _not_ at the parser (mem) itself, since that 
// would modify it in every occurance: 'RESTART' is just a flag in a single instance after all.
// Adding it to the Ref uses the flag-propagation mechanism instead and does not modify 'mem' by itself.
Parser *p = Repeat(Sequence(Restart(Ref(mem)), Nth(0, mem), Nth(0, Ref(mem)), 0L), 1, 5);
Region *txt = region("Hello World", 0L);

    parse_and_print_string_lol(txt, p);

	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}


// ---------------------------------------------------------------------------------------------------------------------------

void netmask_test(void)
{
Parser *bbit	= ToBits(ToByte(Base10(0, 255,0)));
Parser *pP		= Pack(Sequence(bbit, Repeat(Sequence(Skipped(Literal(".")), Ref(bbit), 0), 3, 3), 0L));
Parser *pV		= Sequence(ZeroOrMore(Literal("1")), ZeroOrMore(Literal("0")), Eol(), 0L);
Parser *p		= Chain(pP, pV);

Region *txt		= region("255.0.0.0", 0L);

    parse_and_print_string_lol(txt, p);
	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

// --------------------------------------------------------------------------------------------------

void dump_dict(Dict *d)
{
DictIterator di;
Region tmp;
void *ref;

	di = d->iterator(d);

	printf("-----------------==================---------------------\n");
	while(di.get_next(&di, &tmp, &ref))
	{
		printf("Key: "); print_ft(tmp.begin, tmp.end); printf(", Ref: %p\n", ref);
	}
	printf("-----------------==================---------------------\n");
}

void clear_dicts_dict(Dict *d)
{
DictIterator di;
Region tmp;
void *deletee;

	di = d->iterator(d);

	while(di.get_next(&di, &tmp, &deletee))
		((Dict * ) deletee)->destroy(deletee);
}

void pre_grammar_test(void)
{
Dict *d = new_list_dict();
Region test;
Parser *greeting = Some(
					Or(
						Sequence(
							Literal("!"), 
							AddToDict(d, Word("01ONF", 1, -1)), 
						0L),
						ElementOf(d), 
					0L)
				);

	init_region(&test, "!ON !0 ON ON 0 OFF 1 OFF", 0L);

	parse_and_print_string_lol(&test, greeting);

	dump_dict(d);

	greeting->destroy(greeting);
	d->destroy(d);
}

Parser *cparsing_dict_declaration(ErrorContext *ec, error_function reporter, Dict *all_dict);

void dict_declaration_test(void)
{
Dict *my_dict = new_list_dict();
Parser *ddecl = cparsing_dict_declaration(0L, 0L, my_dict);
char *g = "Dict *foo_bar_dict = new_list_dict();";
Region txt;

	init_region(&txt, g, 0L);

	parse_and_print_string_lol(&txt, ddecl);

	dump_dict(my_dict);

	ddecl->destroy(ddecl);
	my_dict->destroy(my_dict);
}

void grammar_build_test(void)
{
// Region *txt				= region("greeting = Sequence(Or(Literal(\"hallo\"), Literal(\"hello\"), 0), Word(\"edltw\", 1, -1), 0L);", 0);
// Region *txt				= region("greeting = Sequence(OneOrMore(Literal(\"Hallo\")), Word(\"Welt\", 1, 4), 0L);", 0L); // , 1, -1);", 0);
// Region *txt				= region("greeting = OneOrMore(ToUpper(Int16(1, -1)));", 0L); // , 1, -1);", 0);
// char *g					= "Parser *greeting = Some(Pack(Sequence(Caseless(Word(\"aelon01fhwt\", 1, -1)), Emit(\"-san\", 0), 0L)));";
// char *g					= "Parser *greeting = Strong(Print(\"\\n\", OneOrMore( Or( Literal(\"1\"), Literal(\"0\"), Subst(\"1\", Literal(\"ON\")), Subst(\"0\", Literal(\"OFF\")), 0L))));";
// char *g					= "Parser *greeting = Strong(Print(\"\n\", OneOrMore( Or( Literal(\"1\"), Literal(\"0\"), Subst(\"1\", Literal(\"ON\")), Subst(\"0\", Literal(\"OFF\")), 0L))));";
// char *g					= "Parser *greeting	= CaptureUntil(INCLUDE_MATCH, Eol());";
// char *g					= "Parser *greeting = Some(Or(Sequence(Literal(\"!\"), AddToDict(d, Word(\"01ONF\", 1, -1)), 0L), ElementOf(d), 0L));";

char *g					= 
	"Dict *seen = new_list_dict();"
	"Parser *token = Record(Word(\"abcdefghijklmnopqrstuvwxyz\", 1, -1));"
	"Parser *greeting = OneOrMore("
					"Sequence("
						"Chain("
							"Restart(Ref(token)),"
							"Not(ElementOf(seen))"
						"),"
						"AddToDict(seen, token),"
					"0L)"
				");";

Region *txt				= region(g, 0L);
ErrorContext ec			= {txt, 0};
Dict *parser_by_name	= new_list_dict(), *all_dicts = new_list_dict();
Dict *test_dict			= new_list_dict();
Parser *p;
Parser *the_parsed_one  = 0;
Region dict_name;

	// parse_owned_mem(test_dict);
	init_region(&dict_name, "d", 0L);
	all_dicts->add(all_dicts, &dict_name, test_dict);

	dump_dict(all_dicts);

	p = cparsing_basic_parser_definition(&ec, report_error, parser_by_name, all_dicts);

printf("------------------------------------\n");
printf("%s\n", g);
printf("------------------------------------\n");

    parse_and_print_string_lol(txt, p);

printf("-- parser_by_name ------------------\n");
	dump_dict(parser_by_name);
printf("------------------------------------\n");

	if(dict_lookup(parser_by_name, "greeting", (void **) &the_parsed_one))
	{
	Region test;

		printf("Yeah.\n");

		// init_region(&test, "Hallo Welt", 0L);
		// init_region(&test, "!ON !0 ON ON 0 OFF 1 OFF", 0L);
		init_region(&test, "eins zwei drei vier funf sechs eins sieben", 0L);
	
		parse_and_print_string_lol(&test, the_parsed_one);
		
		the_parsed_one->destroy(the_parsed_one);

	} else {
		printf("Nay.\n");
	}

	p->destroy(p);
	parser_by_name->destroy(parser_by_name);

	clear_dicts_dict(all_dicts);
	all_dicts->destroy(all_dicts);
	free_region(txt, 0);
	end_scope();
}

// ---------------------------------------------------------------------------------------------------------------------------

void not_in_dict_test(void)
{
Region txt;
Dict *seen = new_list_dict();
Parser *token = Record(Word(ALPHANUM, 1, -1));
Parser *p = OneOrMore(
				Sequence(
					Chain(
						Restart(Ref(token)),
						Not(ElementOf(seen))
					),
					AddToDict(seen, token),
				0L)
			);

	init_region(&txt, "eins zwei drei vier funf sechs eins sieben", 0L);

	parse_and_print_string_lol(&txt, p);

	dump_dict(seen);

	p->destroy(p);
	seen->destroy(seen);
}

// ---------------------------------------------------------------------------------------------------------------------------

void print_test(void)
{
Region *txt	= region("hallo welt wie gehts denn so", 0);

Parser *p = Sequence(Print("/\\", Any(Word(ALPHANUM, 1, -1))), Print("", Emit("\n\n", 0)), 0L);

	p->parse(p, I0, txt);

	p->destroy(p);
	free_region(txt, 0);
	end_scope();
}

void format_test(void)
{
format_component head;
#if 0
Parser *normal_text					= Call(fmt_ctor_text, 0L,		CharNotIn("%", 1, -1));
Parser *flags						= Call(fmt_ctor_flag, 0L,		Weak(Word(FORMAT_FLAG_CHARS, 1, 1)));
Parser *parametric					= Named("parametric",			Sequence(ToInt(Base10(1, 100, 0)), Literal("$"), 0L));
Parser *intro						= Call(fmt_ctor_intro, 0L,		Sequence(Literal("%"), Optional(parametric), 0L));
Parser *param_index					= Named("param_index",			Sequence(Literal("*"), Optional(Ref(parametric)), 0L));
Parser *width						= Call(fmt_ctor_width, 0L,		Or(ToInt(Base10(1, 100, 0)), param_index, 0L));
Parser *precision					= Call(fmt_ctor_prec, 0L,		Sequence(Skipped(Literal(".")), Optional(Or(ToInt(Word(DIGIT, 1, -1)), Ref(param_index), 0L)), 0L));
Parser *length_modifier				= Call(fmt_ctor_len, 0L,		Or(Word("hlLqjzt", 1, 1), Literal("hh"), Literal("ll"), 0L));
Parser *conversion_specifier		= Call(fmt_ctor_type, 0L,		Word("diouxXeEfFgGaAcsCSpnm", 1, 1));
Parser *conversion_specification	= Named("specification",	
										Or(
											Call(fmt_ctor_text, 0L, Subst("%%", Literal("%%"))), 	// it _is_ a substitution ..
											Call(fmt_ctor_spec, 0L, 
												Sequence(
													intro, 
													ZeroOrMore(flags), 
													Optional(width), 
													Optional(precision), 
													Optional(length_modifier), 
													conversion_specifier, 
												0L)
											),
										0L)
									);

Parser *format_string				= Call(fmt_ctor_parsed_format, &head, Binary(Sequence(Optional(normal_text), Any(Sequence(conversion_specification, Optional(Ref(normal_text)), 0L)), Eol(), 0L)));
#endif

Parser *format_string = get_format_string_parser(&head);

// Region *txt	= region("%d", 0L);
Region *txt	= region("%4$d:%5$.*1$d--%1$#+*2$d %1$s%3$d. %%   oder %2$s, \n", 0L);
// // Region *txt	= region("", 0L);

	list_init((list *) &head);

#if 0
	if(!format_string->parse(format_string, I0, txt))
		printf("Parse failed.\n");
#else
    parse_and_print_string_lol(txt, format_string);
#endif

	dump_component_list(&head);
	printf("---------------------------------------------\n");
	unify_raw_list_text(&head);
	dump_component_list(&head);

	format_string->destroy(format_string);
	free_region(txt, 0);
	end_scope();
}

void printf_test(void)
{
Region *txt = region("", 0L);

Parser *p = Printf("%1$-15s, %1$s - dies ist der %2$dte Versuch.\n Pi soetwa = %3$.2lf\n", Emit("Hallo", 0L), Emit("Welt", 0L), ToInt(Emit("3", 0L)), ToDouble(Emit("3.1415", 0L)), 0L);

    // parse_and_print_string_lol(txt, p);

#if 1
	if(p->parse(p, I0, txt))
		printf("OK.\n");
	else
		printf("Fail.\n");
#endif
	
	free_region(txt, 0);
	p->destroy(p);
}

// ---------------------------------------------------------------------------------------------------------------------------

void print_as_color(char *s)
{
	while(s && *s)
	{
		switch(*s++)
		{
			case 'A':
				printf(BLUEB " " NORMAL) ;
				break;

			case 'B':
				printf(GREENB " " NORMAL) ;
				break;

			case 'C':
				printf(YELLOWB " " NORMAL) ;
				break;

			case 'D':
				printf(REDB " " NORMAL) ;
				break;
			
			default:
				goto print_as_color_out;
		}
	}
print_as_color_out:
	printf(NORMAL);
	fflush(stdout);
	printf("                                      \n");
}


void lindenmeyer_test(void)
{
#if 0
char buffer[1024] = "A";

Parser *rules = Or(
	Subst("BBB", Literal("A")),
	Subst("C", Literal("BB")),
	Subst("ADA", Literal("B")),
	Subst("A", Literal("C")),
	Subst("C", Literal("D")),
	0L
);

char buffer[1024] = "A";
Parser *algae = Or(
	Subst("AB", Literal("A")),
	Subst("A", Literal("B")),
	0L
);

char buffer[1024] = "A";
Parser *cantor = Or(
	Subst("ABA", Literal("A")),
	Subst("BBB", Literal("B")),
	0L
);

char buffer[1024] = "F";
Parser *koch = Or(
	Subst("F+F-F-F+F", Literal("F")),
	Weak(Word("+-", 1, 1)),
	0L
);
#endif

char buffer[1024] = "A";
Parser *sierpinski = Or(
	Subst("B-A-B", Literal("A")),
	Subst("A+B+A", Literal("B")),
	Weak(Word("+-", 1, 1)),
	0L
);


Region txt;
int i;

Parser *p	= _Put(buffer, 1023, Pack(OneOrMore(sierpinski)));

	for(i=0; i<5; ++i)
	{
		init_region(&txt, buffer, 0L);
		p->parse(p, I0, &txt);
		printf("%s\n", buffer);
		// print_as_color(buffer);
	}

	p->destroy(p);
	end_scope();
}

void unify_test(void)
{
Region txt;
Parser *p = Strong(Print("\n", OneOrMore(
				Or(
					Literal("1"),
					Literal("0"),
					Subst("1", Literal("ON")),
					Subst("0", Literal("OFF")),
				0L)
			)));

	init_region(&txt, "ON 0 ON ON OFF 1 OFF", 0L);

	p->parse(p, I0, &txt);
	p->destroy(p);
}

// ---------------------------------------------------------------------------------------------------------------------------

typedef void (*test_function)(void);

test_function tests[] =
{
	words_test,
	seq_test,
	repeat_or_test,
	matchall_test,
	string_test,
	base_test,
	forward_test,
	demo_calculator,
	capture_test,
	len_test,
	pack_test,
	edit_test,
	dict_test,
	put_test,
	put_test2,
	test_emit,
	test_record,
	test_nth,
	calculator_ast_test,
	test_hex_to_int,
	dyn_repeat_test,
	netmask_test,
	grammar_build_test,
	print_test,
	// format_test,
	restart_test,
	lindenmeyer_test,
	unify_test
};
const int no_of_tests = sizeof(tests) / sizeof(test_function);


void *run_test(void *testFcn)
{
test_function f = testFcn;

	f();
	return NULL;
}

void all_test_serial(void)
{
	int i;

		for(i=0; i<no_of_tests; ++i)
		{
			printf("========================================================================\n\n");
			tests[i]();
		}
}

void all_test_parallel(void)
{
pthread_t threads[no_of_tests];
int i;

	for(i=0; i<no_of_tests; ++i)
		pthread_create(&threads[i], NULL, run_test, tests[i]);

	for(i=0; i<no_of_tests; ++i)
		pthread_join(threads[i], NULL);

	printf("\n------ ALL DONE -----\n\n");
}

void replace_from_dict(void)
{
Dict *d = new_list_dict();
char *keys[] = {"ON", "OFF", 0L};
char *vals[] = {"+", "-", 0L};
Region txt;
Parser *p = OneOrMore(Or(DictSubst(d), Word(ALPHANUM, 1, -1), 0L));

	init_region(&txt, "ON 0 OFF ON ON OFF 1 OFF", 0L);
	dict_add_array(d, keys, (void **) vals);

    parse_and_print_string_lol(&txt, p);
	p->destroy(p);
	d->destroy(d);
}

// ---------------------------------------------------------------------------------------------------------------------------
Parser *SymbolDeclaration(Dict *symbols);
Parser *SimpleExpander(Dict *symbols);
Parser *CppExpander(Dict *symbols);

void dump_symbol_declaration(Dict *symbols, char *name);

void test_cpp(void)
{
Dict *d = new_list_dict();
Region txt;
Region txt2;
// #define BindingToken		CharNotIn(DEFAULT_WHITE_CHARS "\\", 1, -1)
Parser *p = SymbolDeclaration(d);
Parser *q;
// int ok;
// Parser *p = OneOrMore(BindingToken);

	init_region(&txt, "#define ABER(YOU,ME) (DON'T YOU YOU ME)", 0L);

	if(!p->parse(p, I0, &txt))
		printf("FAILED.\n");
	else
		dump_symbol_declaration(d, "ABER");

// printf("..next..\n");

	q = CppExpander(d);	

	init_region(&txt2, "ABER(du,mich)", 0L);

    parse_and_print_string_lol(&txt2, q);
	// ok = q->parse(q, I0, &txt2);
	// printf("%s\n", ok ? "OK" : "Not jood");

	q->destroy(q);
	p->destroy(p);
}

#define GLUE Binary(Literal("##"))
#define QUOTE Binary(Literal("#"))
#define KEYWORD Binary(Literal("hallo"))
void test_quote_n_concat_pattern(void)
{
Parser *exp = 
	Or(
		Binary(Sequence(GLUE, KEYWORD, Optional(GLUE), 0L)),
		Binary(Sequence(KEYWORD, Optional(GLUE), 0L)),
		Binary(Sequence(QUOTE, KEYWORD, 0L)),
		Eol(),
	0L);

Parser *p =
	OneOrMore(
		Sequence(
			CaptureUntil(EXCLUDE_MATCH, exp),
			Ref(exp),
		0L)
	);

Region txt;

	init_region(&txt, "aber ##hallo##sie da #hallo", 0L);

	parse_and_print_string_lol(&txt, p);

	p->destroy(p);
}
// ---------------------------------------------------------------------------------------------------------------------------

#define LAST_ARRAY_ELEMENT(XXX) ((XXX)+sizeof(XXX)-1)

void bitfield_test(void)
{
char data[] = { 0xA5, 0x41, 0x42, 0x00 };
Region r;

// Parser *p = Edit(char2str, "%x", Bitfield(Bytes(1,3,3), 5, 3, 4, 4, 8, 0));
// Parser *p = Edit(short2str, "%hx", Ntohs(Edit(expand_to_size, (void *) 2, Bitfield(Bytes(1,3,3), 5, 3, 4, 4, 8, 0))));
Parser *p = Edit(short2str, "%hx", 
				Ntohs(
					Edit(expand_to_size, (void *) 2, 
						Bitfield(Bytes(1,3,3), 16, 8, 0))
			));

	init_region(&r, data, data+2);

	parse_and_print_string_lol(&r, p);

	p->destroy(p);

}

// ---------------------------------------------------------------------------------------------------------------------------
char lldp_packet[] = {
	0x01, 0x80, 0xc2, 0x00, 0x00, 0x0e, 0xec, 0xe5, 
	0x55, 0xa1, 0x42, 0x94, 0x88, 0xcc, 0x02, 0x07, 
	0x04, 0xec, 0xe5, 0x55, 0xa1, 0x42, 0x90, 0x04, 
	0x06, 0x05, 0x70, 0x6f, 0x72, 0x74, 0x34, 0x06, 
	0x02, 0x00, 0x78, 0x08, 0x18, 0x50, 0x6f, 0x72, 
	0x74, 0x3a, 0x20, 0x34, 0x20, 0x2d, 0x20, 0x31, 
	0x30, 0x2f, 0x31, 0x30, 0x30, 0x20, 0x4d, 0x62, 
	0x69, 0x74, 0x20, 0x54, 0x58, 0x0a, 0x12, 0x47, 
	0x45, 0x43, 0x4b, 0x4f, 0x2d, 0x42, 0x30, 0x34, 
	0x35, 0x34, 0x35, 0x46, 0x46, 0x46, 0x46, 0x46, 
	0x46, 0x0c, 0x09, 0x47, 0x45, 0x43, 0x4b, 0x4f, 
	0x20, 0x34, 0x54, 0x58, 0x10, 0x0c, 0x05, 0x01, 
	0x0a, 0x64, 0x66, 0x21, 0x02, 0x00, 0x00, 0x00, 
	0x04, 0x00, 0x00, 0x00
};

// -----------------

typedef struct
{
	unsigned char src[6];
	unsigned char trg[6];
	short type;
} ETHHDR;

struct LLDPTLV
{
	LIST_OF(struct LLDPTLV);
	short 			type;
	short 			len;
	Region			data;
};
typedef struct LLDPTLV LLDPTLV;

typedef struct 
{
	ETHHDR eth;	
	LLDPTLV tlvs;
} LLDPPDU;

// -----------------

int binmac_to_str(void *unused, char **from, char **to)
{
char *value, *f;
size_t len;

	assert(from && to && *from && *to);

	f = *from;

	len		= psprintf(&value, "%02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx", f[0], f[1], f[2], f[3], f[4], f[5]);
	*from	= value;
	*to		= value + len - 1;

	return EDIT_OK;
}


#define HEX_DATA_DUMP_LEN 32
int cb_dump_lldp_tlv(void *in, void **out)	// LLDPTLV *, unused
{
LLDPTLV *tlv = *((LLDPTLV **) out);
int i, dump_len, capped;

	printf("type %d -- len %d : ", tlv->type, tlv->len);
	capped = (tlv->len > HEX_DATA_DUMP_LEN);
	dump_len = (capped ? HEX_DATA_DUMP_LEN : tlv->len);

	if(tlv && tlv->data.begin)
		for(i=0; i<dump_len; ++i)
		{
			char c = tlv->data.begin[i];
			printf("%02hhx(%c) ", c, 32 <= c && c <= 126 ? c : '.' );
		}

	if(capped) printf("...");
	printf("\n");

	return PARSER_MATCH;
}

int cb_new_lldp_tlv(void *in, void **out)	// unused, LLDPTLV **
{
LLDPTLV **tlvp = (LLDPTLV **) out;
LLDPTLV *tlv = parse_calloc(1, sizeof(LLDPTLV));

	assert(tlv);
	list_init(tlv);

	assert(tlvp);
	*tlvp = tlv;

	return PARSER_MATCH;
}

int cb_append_to_list(void *lst, void **elem)	// list *, list **
{
list *l, *e;

	assert(lst && elem);
	l = (list *) lst;
	e = (list *) *elem;
	assert(e);

	list_append_end(l, e);

	return PARSER_MATCH;
}

#define PRINT_MAC(MAC) printf("%02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx", MAC[0], MAC[1], MAC[2], MAC[3], MAC[4], MAC[5]);

int cb_dump_eth_hdr(void *eth_hdr, void **out)
{
ETHHDR *hdr = (ETHHDR *) eth_hdr;

	assert(hdr);
	printf("src: ");
	PRINT_MAC(hdr->src);
	printf("  trg: ");
	PRINT_MAC(hdr->trg);
	printf("  type: %04hx\n", hdr->type);

	return PARSER_MATCH;
}

int cb_dump_lldp_pdu(void *lldp_pdu, void **out)
{
LLDPPDU *pdu = (LLDPPDU *) lldp_pdu;
LLDPTLV *tlv;

	assert(pdu);
	printf("\n");
	cb_dump_eth_hdr(&pdu->eth, 0L);

	if(list_not_empty((list *) &pdu->tlvs))
	{
		list_for_each(LLDPTLV, tlv, &pdu->tlvs)
		{
			printf("  ");
			cb_dump_lldp_tlv(0L, (void **) &tlv);
		}
	}
	printf("\n");

	return PARSER_MATCH;
}


MinMaxSize get_tlv_len(void *p)
{
LLDPTLV *tlv = *((LLDPTLV **) p);
MinMaxSize result = {tlv->len, tlv->len, 1};

	return result;
}


void lldp_test(void)
{
LLDPPDU pdu;
LLDPTLV *tlv;
Region packet;
Region lldp_mark_region;
Region lldp_eopdu;
char lldp_mark[]		= {0x88, 0xcc};
char end_of_lldpdpu[]	= {0x00, 0x00};

#define MAC_ADR				Int48(1, 1)
#define ETH_TYPE			Ntohs(Int16(1, 1))
#define ETH_TYPE_LLDP		Ntohs(LiteralRegion(&lldp_mark_region))

Parser *EthHdr = 
			Binary(Sequence(
					_Put(&pdu.eth.trg, 6, MAC_ADR), 
					_Put(&pdu.eth.src, 6, MAC_ADR), 
					_Put(&pdu.eth.type, 2, ETH_TYPE_LLDP), 
					0L
			));

Parser *LLDPTlv = 
			Binary(Sequence(
					// allocate new tlv
					Callback(cb_new_lldp_tlv, 0L, (void **) &tlv),
					// TLV Header: type -> 7 bit, len -> 9 bit
					Distribute(
						Ntohs(Edit(expand_to_size, (void *) 2, Bitfield(Int16(1,1), 7, 9, 0))),
						PutStruct(tlv, LLDPTLV, type, Data()),
						PutStruct(tlv, LLDPTLV, len, Data()),
						0L
					),
					// TLV body
					_PutStruct( (void **) &tlv, offsetof(LLDPTLV,data), sizeof(Region), ToRegion(Pack(DynBytes(get_tlv_len, &tlv)))),
					// save the result
					Callback(cb_append_to_list, &pdu.tlvs, (void **) &tlv),
					0L
			));
					
Parser *p = Binary(Sequence(
				// get Ethernet header
				EthHdr, 
				// get LLDP TLVs until end-of-PDU mark is found
				Until(LiteralRegion(&lldp_eopdu), LLDPTlv), 
				// dump the parsed result
				Callback(cb_dump_lldp_pdu, &pdu, 0L), 
				0L
			));

	memset(&pdu, 0, sizeof(LLDPPDU));
	list_init((list *) &pdu.tlvs);
	init_region(&packet, lldp_packet, LAST_ARRAY_ELEMENT(lldp_packet));
	init_region(&lldp_mark_region, lldp_mark, LAST_ARRAY_ELEMENT(lldp_mark));
	init_region(&lldp_eopdu, end_of_lldpdpu, LAST_ARRAY_ELEMENT(end_of_lldpdpu));

	parse_and_print_string_lol(&packet, p);

	p->destroy(p);
}

#if 0
void lldp_test1(void)
{
LLDPPDU pdu;
Region packet;
Region lldp_mark_region;
char lldp_mark[] = {0x88, 0xcc, 0x00};
// char end_of_lldpdpu[] = {0x00, 0x00};

#define MAC_ADR				Int48(1, 1)
#define ETH_TYPE			Ntohs(Int16(1, 1))
#define MAC_TO_STRING(p)	Edit(binmac_to_str, 0, p)
#define SHORT_TO_STRING(p)	Edit(short2str, "%hx", p)

// #define ETH_TYPE_LLDP		Literal(lldp_mark)
#define ETH_TYPE_LLDP		LiteralRegion(&lldp_mark_region)

Parser *EthHdr = 
			Binary(Sequence(
					MAC_TO_STRING(MAC_ADR), 
					MAC_TO_STRING(MAC_ADR), 
					SHORT_TO_STRING(ETH_TYPE_LLDP), 
					0L
			));

Parser *LLDPTlv = 
			Binary(Sequence(
					// TLV Header: type -> 7 bit, len -> 9 bit
					SHORT_TO_STRING(Ntohs(
						Edit(expand_to_size, (void *) 2, 
							Bitfield(Int16(1,1), 7, 9, 0)
						)
					)),
					0L
			));


Parser *p = Binary(Sequence(EthHdr, LLDPTlv, 0L));

	memset(&pdu, 0, sizeof(LLDPPDU));
	list_init((list *) &pdu.tlvs);
	init_region(&packet, lldp_packet, LAST_ARRAY_ELEMENT(lldp_packet));
	init_region(&lldp_mark_region, lldp_mark, LAST_ARRAY_ELEMENT(lldp_mark)-1);

	parse_and_print_string_lol(&packet, p);

	p->destroy(p);
}
#endif

int say_something(void *in, void **out)
{
	printf("=> %s\n", (char *) in);
	return PARSER_MATCH;
}

void distribute_test(void)
{
char data[] = { 0xA5, 0x41, 0x42, 0x00 };
Region r;

Parser *p = Sequence(
				Callback(say_something, "oans", 0L),
				Distribute(
					Bitfield(Bytes(1,3,3), 16, 8, 0), 			// source -> short -> char
					Edit(short2str, "%hx", Ntohs(Int16(1,1))), 	// receives short
					Edit(char2str, "%x", Int8(1,1)),			// receives char
					0L
				),
				Callback(say_something, "zwoa", 0L),
				0L
			);

	init_region(&r, data, data+2);

	parse_and_print_string_lol(&r, p);

	p->destroy(p);
}

// ---------------------------------------------------------------------------------------------------------------------------
void until_test(void)
{
Region r;
Parser *p = Until(
				Literal("Bob"),
				Word(ALPHANUM, 1, -1)
			);

	init_region(&r, "Sag wie geht es Dir Bob?", 0L);

	parse_and_print_string_lol(&r, p);

	p->destroy(p);
}

// ---------------------------------------------------------------------------------------------------------------------------
int main(void)
{
	// words_test();
	// seq_test();
	// repeat_or_test();
	// matchall_test();
	// string_test();
	// base_test();
	// forward_test();
	// demo_calculator();
	// capture_test();
	// len_test();
	// pack_test();
	// edit_test();
    // dict_test();
    // put_test();
    // put_test2();
	// test_emit();
	// test_record();
	// test_nth();
	// calculator_ast_test();
	// test_hex_to_int();
	// dyn_repeat_test();
	// dyn_bytes_test();
	// parser_grammar_test();
	// netmask_test();
// dict_declaration_test();
// pre_grammar_test();
// grammar_build_test();
	// chain_test();

	// not_in_dict_test();
	// print_test();
	// format_test();
	// restart_test();
	// lindenmeyer_test();
	// printf_test();
	// unify_test();
	// all_test_serial();
	// all_test_parallel();
	// identifier_test();
	// replace_from_dict();
	// test_cpp();
	// test_quote_n_concat_pattern();
	// free_spaces();

	// bitfield_test();
	// distribute_test();
	// until_test();

	lldp_test();


	cparsing_cleanup();
	return 0;
}

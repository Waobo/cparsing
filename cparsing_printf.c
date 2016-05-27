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
#include "cparsing_impl.h"
#include "cparsing_printf.h"

// ---------------------------------------------------------------------------------------------------------------------------

#if 0

"%*d", w, d

"%1$*2$d", d, w

#endif

void dump_typed(Typed *t)
{
	assert(t);

	switch(t->type)
	{
	case FMT_INVALID:
		return;

	case FMT_TEXT:
		{
			format_text *f = (format_text *) t;
			printf("FMT_TEXT: \""); print_ft(f->text.begin, f->text.end); printf("\"\n");
			// printf("FMT_TEXT: \"%s\"\n", f->text);
		}
		break;

	case FMT_INTRO:
		{
			format_intro *i = (format_intro *) t;
			printf("FMT_INTRO: param is %d\n", i->param);
		}
		break;

	case FMT_FLAG:
		{
			format_flag *a = (format_flag *) t;
			char fl = a->flag;
			printf("FMT_FLAG: %d = '%s%s%s%s%s'\n", fl, fl & 1 ? "#" : "", fl & 2 ? "0" : "", fl & 4 ? "-" : "", fl & 8 ? "_" : "", fl & 16 ? "+" : "");
		}
		break;

	case FMT_WIDTH:
		{
			format_width *w = (format_width *) t;
			printf("FMT_WIDTH: type = %d, param = %d\n", w->wtype, w->param);
		}
		break;

	case FMT_PREC:
		{
			format_precision *p = (format_precision *) t;
			printf("FMT_PREC: type = %d, param = %d\n", p->ptype, p->param);
		}
		break;

	case FMT_LENGTH:
		{
			format_length *l = (format_length *) t;
			printf("FMT_LENGTH: "); print_ft(l->param.begin, l->param.end); printf("\n");
		}
		break;

	case FMT_TYPE:
		{
			format_type *y = (format_type *) t;
			printf("FMT_TYPE: %c\n", y->spez);
		}
		break;

	case FMT_SPEC:
		{
		char fl;
		format_specification *s = (format_specification *) t;

			fl = s->f;
			printf("FMT_SPEC: at %d:\n* ", s->param_index);
			dump_typed((Typed *) &s->w);
			printf("* ");
			dump_typed((Typed *) &s->p);
			printf("* ");
			dump_typed((Typed *) &s->l);
			printf("* FLAG: %d = '%s%s%s%s%s'\n", fl, fl & 1 ? "#" : "", fl & 2 ? "0" : "", fl & 4 ? "-" : "", fl & 8 ? "_" : "", fl & 16 ? "+" : "");
			dump_typed((Typed *) &s->s);
		}
		break;

	default:
		assert("unknown type" && 1 == 0);
	}
}

static int fmt_ctor_text(void *vdata, InterpreterInfo I, Parser *data_source)
{
format_text *result = (format_text *) parse_malloc(sizeof(format_text));

	result->type = FMT_TEXT;
	record_get_nth(data_source, 0, &result->text);

	if(I.interpreter)
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(format_text)-1);

	return PARSER_MATCH;
}

#define FORMAT_FLAG_CHARS "#0- +"

static int fmt_ctor_flag(void *vdata, InterpreterInfo I, Parser *data_source)
{
format_flag *result = (format_flag *) parse_malloc(sizeof(format_flag));
Region r;

	record_get_nth(data_source, 0, &r);
	result->type = FMT_FLAG;

	switch(*r.begin)
	{
		case '#': result->flag = 1; break;
		case '0': result->flag = 2; break;
		case '-': result->flag = 4; break;
		case ' ': result->flag = 8; break;
		case '+': result->flag = 16; break;
		default: assert("grammar violation" && 1==0);
	}

	if(I.interpreter)
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(format_flag)-1);

	return PARSER_MATCH;
}

static int fmt_ctor_intro(void *vdata, InterpreterInfo I, Parser *data_source)
{
format_intro *result = (format_intro *) parse_malloc(sizeof(format_intro));
Region r;

	result->type = FMT_INTRO;
	if(record_get_nth(data_source, 1, &r))
		result->param = *((int *) r.begin);
	else
		result->param = -1;

	if(I.interpreter)
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(format_intro)-1);

	return PARSER_MATCH;
}

static int fmt_ctor_width(void *vdata, InterpreterInfo I, Parser *data_source)
{
format_width *result = (format_width *) parse_malloc(sizeof(format_width));
Region r;

	result->type = FMT_WIDTH;

	record_get_nth(data_source, 0, &r);

	if((REGION_LEN(r.begin, r.end) == 1) && (*r.begin == '*'))
	{
		if(record_get_nth(data_source, 1, &r))
			result->param = *((int *) r.begin);
		else
			result->param = 0;	// 0 means 'next'

		result->wtype = FMT_WIDTH_INDEX;
	} else {
		result->param = *((int *) r.begin);
		result->wtype = FMT_WIDTH_DIRECT;
	}

	if(I.interpreter)
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(format_intro)-1);

	return PARSER_MATCH;
}

static int fmt_ctor_prec(void *vdata, InterpreterInfo I, Parser *data_source)
{
format_precision *result = (format_precision *) parse_malloc(sizeof(format_precision));
Region r;

	assert(data_source);

	result->type = FMT_PREC;

	record_get_nth(data_source, 0, &r);

	if((REGION_LEN(r.begin, r.end) == 1) && (*r.begin == '*'))
	{
		if(record_get_nth(data_source, 1, &r))
			result->param = *((int *) r.begin);
		else
			result->param = 0;

		result->ptype = FMT_PREC_INDEX;
	} else {
		result->param = *((int *) r.begin);
		result->ptype = FMT_PREC_DIRECT;
	}

	if(I.interpreter)
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(format_precision)-1);

	return PARSER_MATCH;
}

static int fmt_ctor_len(void *vdata, InterpreterInfo I, Parser *data_source)
{
format_length *result = (format_length *) parse_malloc(sizeof(format_length));

	assert(data_source);

	result->type = FMT_LENGTH;

	record_get_nth(data_source, 0, &result->param);

	if(I.interpreter)
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(format_length)-1);

	return PARSER_MATCH;
}

static int fmt_ctor_type(void *vdata, InterpreterInfo I, Parser *data_source)
{
format_type *result = (format_type *) parse_malloc(sizeof(format_type));
Region r;

	assert(data_source);

	result->type = FMT_TYPE;

	record_get_nth(data_source, 0, &r);

	result->spez = *r.begin;

	if(I.interpreter)
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(format_type)-1);

	return PARSER_MATCH;
}

static int fmt_ctor_spec(void *vdata, InterpreterInfo I, Parser *data_source)
{
format_specification *result = (format_specification *) parse_calloc(1, sizeof(format_specification));
RecordIterator ri;
Region r;

	assert(data_source);

	result->type = FMT_SPEC;
	result->f = 0;	// no flags to start with

	record_init_interator(&ri, data_source);	

	while(record_get_next(&ri, &r))
	{
	Typed *t = (Typed *) r.begin;

		// dump_typed(t);

		switch(t->type)
		{
		case FMT_INVALID:
			break;
		case FMT_INTRO:
			result->param_index = ((format_intro *) t)->param;
			break;
		case FMT_FLAG:
			result->f |= ((format_flag *) t)->flag;
			break;
		case FMT_WIDTH:
			result->w = *((format_width *) t);
			break;
		case FMT_PREC:
			result->p = *((format_precision *) t);
			break;
		case FMT_LENGTH:
			result->l = *((format_length *) t);
			break;
		case FMT_TYPE:
			result->s = *((format_type *) t);
			break;
		default:
			assert("invalid type" && 1 == 0);
		}
	}

	// dump_typed((Typed *) result);
	// printf("----------------------------------\n");

	if(I.interpreter)
		I.interpreter(data_source, I.param, TOKEN_VALUE, ((char *) result), ((char *) result)+sizeof(format_specification)-1);

	return PARSER_MATCH;
}


static int fmt_ctor_parsed_format(void *vdata, InterpreterInfo I, Parser *data_source)
{
format_component *head = (format_component *) vdata;
Region r;
RecordIterator ri;

	assert(head);
	assert(data_source);

	record_init_interator(&ri, data_source);	

	while(record_get_next(&ri, &r))
	{
	// _real_ malloc: list of format_components is freed in destroy_Printf
	format_component *new_fc = (format_component *) calloc(1, sizeof(format_component));
	Typed *t = (Typed *) r.begin;

		switch(t->type)
		{
		case FMT_TEXT:
			{
			format_text *txt = (format_text *) t;
				new_fc->type = FMT_TEXT;
				new_fc->text =  strdup_ft(txt->text.begin, txt->text.end);
			}
			break;

		case FMT_SPEC:
			new_fc->type = FMT_SPEC;
			new_fc->spec = *((format_specification *) t);
			break;

		default:
			assert("invalid type" && 1 == 0);
		}

		list_append_end((list *) head, (list *) new_fc);
	}

	return PARSER_MATCH;
}

struct format_job
{
	LIST_OF(struct format_job);
	format_component components;
};
typedef struct format_job format_job;

format_job *new_job(void)
{
format_job *result = (format_job *)  parse_malloc(sizeof(format_job));

	list_init((list *) result);
	list_init((list *) &result->components);

	return result;
}

format_component *_join_text_component(format_component *trg, format_component *ext)
{
char *joined;
int len1;

	assert(trg && trg->type == FMT_TEXT && trg->text);
	assert(ext && ext->type == FMT_TEXT && trg->text);

#if 0
	join_regions(&trg->text.text, &ext->text.text, &trg->text.text);
	parse_owned_mem(trg->text.text.begin);
#endif

	len1 = strlen(trg->text);

	joined = malloc(len1 + strlen(ext->text) + 1);

	strcpy(joined, trg->text);
	strcpy(joined + len1, ext->text);

	free(ext->text);
	list_remove((list *) ext);

	free(trg->text);
	trg->text = joined;

	return trg;
}

void unify_raw_list_text(format_component *raw)
{
format_component *c;
format_component *last_text = 0L;

	for(c = raw->next; c != raw; c = c->next)
	{
		switch(c->type)
		{
			case FMT_TEXT:
				if(last_text)
					c = _join_text_component(last_text, c);
				else
					last_text = c;
				break;

			case FMT_SPEC:
				last_text = 0L;
				break;

			default:
				assert("invalid type" && 1 == 0);
		}
	}
}

int compose_format(format_component *c, char *buffer)
{
format_specification *s;
int i = 0;

	assert(c && c->type == FMT_SPEC);
	assert(buffer);
	s = &c->spec;

	// walk the format-spec:
	// intro..
	buffer[i++] = '%';

	// parameter-index..
	if(s->param_index)
		i += sprintf(buffer+i, "%d$", s->param_index);

	// flags..
	if(s->f)
		i += sprintf(buffer+i, "%s%s%s%s%s", s->f & 1 ? "#" : "", s->f & 2 ? "0" : "", s->f & 4 ? "-" : "", s->f & 8 ? " " : "", s->f & 16 ? "+" : "");

	// width..
	if(s->w.type == FMT_WIDTH)
	{
		switch(s->w.wtype)
		{
		case FMT_WIDTH_DIRECT:
			i += sprintf(buffer+i, "%d", s->w.param);
			break;

		case FMT_WIDTH_INDEX:
			if(s->w.param)		// explicit index given?
				i += sprintf(buffer+i, "*%d$", s->w.param);
			else
				buffer[i++] = '*';
			break;

		default:
			assert("invalid width option" && 1==0);
		}
	}

	// prec..
	if(s->p.type == FMT_PREC)
	{
		switch(s->p.ptype)
		{
		case FMT_PREC_DIRECT:
			i += sprintf(buffer+i, ".%d", s->p.param);
			break;

		case FMT_PREC_INDEX:
			if(s->p.param)		// explicit index given?
			{
				i += sprintf(buffer+i, ".*%d$", s->p.param);
			} else {
				buffer[i++] = '.';
				buffer[i++] = '*';
			}
			break;

		default:
			assert("invalid precision option" && 1==0);
		}
	}
		
	// length..
	if(s->l.type == FMT_LENGTH)
	{
		buffer[i++] = s->l.param.begin[0];
		if(s->l.param.begin != s->l.param.end)
			buffer[i++] = s->l.param.begin[1];
	}

	// ..and specifier
	buffer[i++] = s->s.spez;
	buffer[i] = 0;

	return i;
}

int fmt_component_params_required(format_component *head)
{
format_component *act; 
int result = 0;

	list_for_each(format_component, act, head)
	{
		switch(act->type)
		{
		case FMT_TEXT:
			break;

		case FMT_SPEC:
			++result;										// one for the data to be shown
			if(act->spec.w.wtype == FMT_WIDTH_INDEX) ++result;	// one, if width is given by parameter
			if(act->spec.p.ptype == FMT_PREC_INDEX) ++result;	// one, if precision is given by parameter

			break;

		default:
			assert("invalid type" && 1 == 0);
		}
	}

	return result;
}

void dump_component_list(format_component *head)
{
format_component *act; 
char spez[64];

	printf("=================================================\n");
	printf("Format requires %d parameters.\n", fmt_component_params_required(head));
	list_for_each(format_component, act, head)
	{
		switch(act->type)
		{
			case FMT_TEXT:
				printf("TXT: \"%s\"\n", act->text);
				break;

			case FMT_SPEC:
				compose_format(act, spez);
				printf(BOLD RED "%s" NORMAL " ", spez);
				dump_typed((Typed *) &act->spec);
				break;

			default:
				assert("invalid type" && 1 == 0);
		}
	}
	printf("=================================================\n");
}



#if 1

typedef enum 
{
	STORAGE_VOID = 0,
	STORAGE_CHAR,
	STORAGE_STRING,
	STORAGE_SHORT,
	STORAGE_INT,
	STORAGE_LONG,
	STORAGE_LONGLONG,
	STORAGE_FLOAT,
	STORAGE_DOUBLE,
	STORAGE_LONGDOUBLE,
	STORAGE_POINTER
} StorageFormat;

static int perform_actual_printing(char *fmt, StorageFormat stf, void *p1, int p2, int p3)
{
int ret = 0;

	printf("\n<%s>\n", fmt);

	printf(BLUEB);
	switch(stf)
	{
	case STORAGE_VOID:
		ret = printf("%s", fmt);
		break;

	case STORAGE_CHAR:
		ret = printf(fmt, *((char *) p1), p2, p2);
		break;

	case STORAGE_STRING:
		ret = printf(fmt, (char *) p1, p2, p2);
		break;

	case STORAGE_SHORT:
		ret = printf(fmt, *((short *) p1), p2, p2);
		break;

	case STORAGE_INT:
		ret = printf(fmt, *((int *) p1), p2, p2);
		break;

	case STORAGE_LONG:
		ret = printf(fmt, *((long *) p1), p2, p2);
		break;

	case STORAGE_LONGLONG:
		ret = printf(fmt, *((long long *) p1), p2, p2);
		break;

	case STORAGE_FLOAT:
		ret = printf(fmt, *((float *) p1), p2, p2);
		break;

	case STORAGE_DOUBLE:
		ret = printf(fmt, *((double *) p1), p2, p2);
		break;

	case STORAGE_LONGDOUBLE:
		ret = printf(fmt, *((long double *) p1), p2, p2);
		break;

	case STORAGE_POINTER:
		ret = printf(fmt, *((void **) p1), p2, p3);
		break;

	default:
		assert("invalid storage format" && 1==0);
	}

	printf(NORMAL);
	return ret;
}

StorageFormat get_storage_type(format_component *fc)
{
	assert(fc);
	assert(fc->type == FMT_SPEC);

	switch(fc->spec.s.spez)
	{
		case 'd':
		case 'i':
		case 'o':
		case 'u':
		case 'x':
		case 'X':
			return STORAGE_INT;		

		case 'e':
		case 'E':
		case 'f':
		case 'F':
		case 'g':
		case 'G':
		case 'a':
		case 'A':
			return STORAGE_DOUBLE;

		case 's':
			return STORAGE_STRING;

		case 'p':
			return STORAGE_POINTER;
		
		default:
			assert("Storage type not supported" && 1==0);
	}
}

static int actual_format_string(format_specification *desc, char *result)
{
int i = 0;
char pidx = '1';

	assert("invalid format component" && desc);
	assert("result buffer missing" && result);

	// intro
	result[i++] = '%';
	
	// flags
	if(desc->f & 1) result[i++] = '#';
	if(desc->f & 2) result[i++] = '0';
	if(desc->f & 4) result[i++] = '-';
	if(desc->f & 8) result[i++] = ' ';
	if(desc->f & 16) result[i++] = '+';

	// width
	if(desc->w.type == FMT_WIDTH)
	{
		if(desc->w.wtype == FMT_WIDTH_INDEX)
		{
			result[i++] = '*';
			result[i++] = pidx++;
		} else
			i += sprintf(result+i, "%d", desc->w.param);
	}

	// precision
	if(desc->p.type == FMT_PREC)
	{
		result[i++] = '.';

		if(desc->p.ptype == FMT_PREC_INDEX)
		{
			result[i++] = '*';
			result[i++] = pidx++;
		} else
			if(desc->p.param) i += sprintf(result+i, "%d", desc->p.param);
	}

	// length modifier
	if(desc->l.type == FMT_LENGTH)
	{
		result[i++] = *desc->l.param.begin;
		if(desc->l.param.end != desc->l.param.begin)
			result[i++] = *desc->l.param.end;
	}

	// conversion specifier
	result[i++] = desc->s.spez;
	result[i] = 0;

	return i;
}

static int execute_Printf(void *vdata, InterpreterInfo I, Parser *data_source)
{
format_component *head = (format_component *) vdata;
format_component *act; 
Region params[3];
int current_param = 0;
char fmt_str[32];

	assert(head);
	assert(data_source);

	// check: params required by compList vs len(data_source)

	list_for_each(format_component, act, head)
	{
		switch(act->type)
		{
		case FMT_TEXT:
			printf("%s", act->text);
			break;

		case FMT_SPEC:
			{
			int pidx = 1;
				
				if(act->spec.w.type == FMT_WIDTH && act->spec.w.wtype == FMT_WIDTH_INDEX)
				{
					if(act->spec.w.param)
						record_get_nth(data_source, act->spec.w.param, &params[pidx++]);
					else
						record_get_nth(data_source, current_param++, &params[pidx++]);
				}
				
				if(act->spec.p.type == FMT_PREC && act->spec.p.ptype == FMT_PREC_INDEX)
				{
					if(act->spec.p.param)
						record_get_nth(data_source, act->spec.p.param, &params[pidx++]);
					else
						record_get_nth(data_source, current_param++, &params[pidx++]);
				}

				// while the output-parameter's index -if present- is given first, the 
				// parameter itself is the last one used by an single format-expression if
				// its index is _not_ explicitly given
				if(act->spec.param_index >= 0)
					record_get_nth(data_source, act->spec.param_index, &params[0]);
				else
					record_get_nth(data_source, current_param++, &params[0]);

				actual_format_string(&act->spec, fmt_str);

				// printf("Printing %d paramters with format '%s'\n", pidx, fmt_str);
#if 1
				switch(pidx)
				{
				case 1:
					perform_actual_printing(fmt_str, get_storage_type(act), params[0].begin, 0, 0);
					break;

				case 2:
					perform_actual_printing(fmt_str, get_storage_type(act), params[0].begin, *((int *) params[1].begin), 0);
					break;

				case 3:
					perform_actual_printing(fmt_str, get_storage_type(act), params[0].begin, *((int *) params[1].begin), *((int *) params[2].begin));
					break;
				}
#endif
				
			}
			break;
		default:
			assert("invalid type in format description" && 1 == 0);
		}
	}

	return PARSER_MATCH;
}

static void format_component_dtor(list *deletee)
{
format_component *fc = (format_component *) deletee;

	assert(fc);

	if(fc->type == FMT_TEXT)
	{
		assert(fc->text);
		free(fc->text);
	}

	free(fc);
}

static void destroy_Printf(Parser *deletee)
{
CallParam *param;

	assert(deletee);
	
	param = (CallParam *) deletee->param;
	assert(param);
	assert(param->user_data);

	list_clear((list *) param->user_data, format_component_dtor);
	free(param->user_data);
	
	destroy_Call(deletee);
}

Parser *PrintfL(char *fmt, Parser *parser_list)
{
format_component *fmt_comp = malloc(sizeof(format_component));
Region fmt_region;
Parser *format_string = get_format_string_parser(fmt_comp);
Parser *result;
int fmt_result;

	assert(parser_list && list_not_empty((list *) parser_list));
	PNAME("Printf")

	list_init((list *) fmt_comp);
	init_region(&fmt_region, fmt, 0L);

	fmt_result = format_string->parse(format_string, I0, &fmt_region);
	format_string->destroy(format_string);

	if(!fmt_result)
	{
		assert("not a valid format description." && 1==0);
		return 0L;
	}

	if(list_len_is_one((list *) parser_list))
	{
		result = Call(execute_Printf, fmt_comp, parser_list->next);
		list_clear((list *) parser_list, 0L);
	} else {
		result = Call(execute_Printf, fmt_comp, SequenceL(parser_list));
	}

	result->destroy = destroy_Printf;
	result->flags	= ACCEPT_EMPTY_INPUT;

    return result;
}

Parser *Printf(char *fmt, Parser *p1, ...)
{
va_list argp;
Parser *p = 0;
Parser *parser_list = parser_list_header();

    assert(fmt);
    assert(p1);
    va_start(argp, p1);

    p = p1;
    do
    {
        list_append_end((list *) parser_list, (list *) p);
    } while( (p = va_arg(argp, Parser *)) != 0L );
	va_end(argp);

	return PrintfL(fmt, parser_list);
}


#endif

Parser *get_format_string_parser(format_component *head)
{
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

	return Call(fmt_ctor_parsed_format, head, Binary(Sequence(Optional(normal_text), Any(Sequence(conversion_specification, Optional(Ref(normal_text)), 0L)), Eol(), 0L)));
}



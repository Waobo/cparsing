#ifndef __CPARSING_IMPL_H__
#define __CPARSING_IMPL_H__

#define PARSER_CONTAINER_INTRO va_list argp;\
Parser *p = 0;\
Parser *parser_list = parser_list_header();\
    assert(p1);\
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

Parser *parser_list_header(void);

typedef struct
{
	Parser 			*parser;
	ctor_function	ctor;
	void			*user_data;
} CallParam;

void destroy_Call(Parser *deletee);

int unimplemented_forward_declaration(struct Parser *self, InterpreterInfo I, Region *input);

#endif // __CPARSING_IMPL_H__

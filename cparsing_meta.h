#ifndef __CPARSING_META_H__
#define __CPARSING_META_H__

typedef struct
{
	Region *all_input;
	char *error;
	TextLocation where;
} ErrorContext;

// Parser *cparsing_basic_parser_grammar(ErrorContext *ec, error_function reporter, Dict *parser_by_name_storage);
Parser *cparsing_basic_parser_grammar(ErrorContext *ec, error_function reporter, Dict *parser_by_name_storage, Dict *dicts);

// Parser *cparsing_basic_parser_definition(ErrorContext *ec, error_function reporter, Dict *parser_by_name_storage);
Parser *cparsing_basic_parser_definition(ErrorContext *ec, error_function reporter, Dict *parser_by_name_storage, Dict *all_dict);

// simple print-error-to-console function
void report_error(void *user_data, const char *msg, Parser *src, Region *r);

#endif // __CPARSING_META_H__

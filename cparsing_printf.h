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

#ifndef __CPARSING_PRINTF_H__
#define __CPARSING_PRINTF_H__

enum
{
	FMT_INVALID = 0,
	FMT_TEXT,
	FMT_INTRO,
	FMT_FLAG,
	FMT_WIDTH,
	FMT_WIDTH_DIRECT,
	FMT_WIDTH_INDEX,
	FMT_PREC,
	FMT_PREC_DIRECT,
	FMT_PREC_INDEX,
	FMT_LENGTH,
	FMT_TYPE,
	FMT_SPEC
};

typedef struct { int type; } Typed; 

typedef struct
{
	int		type;
	int		param;
} format_intro;

typedef struct
{
	int		type;
	char	flag;
} format_flag;

typedef struct
{
	int		type;
	Region	text;
} format_text;

typedef struct
{
	int		type;
	int		wtype;
	int		param;
} format_width;

typedef struct
{
	int		type;
	int		ptype;
	int		param;
} format_precision;

typedef struct
{
	int		type;
	Region	param;
} format_length;

typedef struct
{
	int		type;
	char	spez;
} format_type;

typedef struct
{
	int		type;
	int					param_index;
	format_width		w;	
	format_precision	p;
	format_length		l;
	char				f;
	format_type			s;
} format_specification;

struct format_component
{
	LIST_OF(struct format_component);
	int type;
	union
	{
		char					*text;
		format_specification	spec;
	};
};
typedef struct format_component format_component;

Parser *get_format_string_parser(format_component *head);
void dump_component_list(format_component *head);
void unify_raw_list_text(format_component *raw);

Parser *Printf(char *fmt, Parser *p1, ...);					// printf to stdout for parse-results


#endif // __CPARSING_PRINTF_H__

# cparsing - a recusive descent parser library for C

CONTENTS
--------
	- MOTIVATION
	- ABOUT
	- DOCUMENTATION
		-- Regions, Parsers and Interpreter
			* Region
			* Parsers
			* Interpreter
		-- Memory management
		-- Flags
		-- Text vs Binary mode
	- API REFERENCE
		-- Basic Parsers
		-- Combinatoric Parsers
		-- Recoders and Callbacks
		-- Error-handling in grammars
		-- Interpreter Protocol
		-- Memory Pools
	- LICENSE


NOTE: this is still work in progress. For example, the meta-parser does
not know about some newer parsers and the C-parser never had a in-depth test
and also still misses the typedef component -- even this readme is far 
from complete. To make things worse, I'm working on this project very
erratically. The currently available functionality is however already useful.

You'll find statements preceeded with 'PITFALL: ' - these are well meant
warnings about misusages that are (unfortunally) (all too) easy to
introduce and will most likely prevent the code from doing what was intended.


TODO: add more examples - currently, the only examples can be found in the
code (=> **main.c**).


MOTIVATION
----------

So, you code in C and that means that every time you open a file, receive
a packet or even get that commandline parameter - there it is again: the
need to validate your input.

This can be quite obstrusive, since to do it right you often need to verify
more than one property of any given value - like size, range and/or format of
the data or its need for type-conversion.

This is where a parser can be a great help: define a grammar for the input
and let the parser validate the grammar against the input, reforming the
input to your need.

Once this is done, it is guaranteed that every piece of the input conforms
to the grammar and no further tests are necessary: you now have trusted
input data.

Of course, this is of help only if the definition of the grammar isn't 
even more complicate then writing all these tests and conversions by hand...


ABOUT
-----
CParsing is a combinatoric recursive descent parser (2) library written in C.

Its primary aim is ease-of-use in small to medium size parse tasks - if you
need to parse large portions of data or are depending on performance, you should
probably be looking for a more sophisticated and optimized parser solution.

CParsing was heavily inspired by the python Pyparsing (3) module, but differs
in many respects not only owed to the environment of the language C.

It supports validation, modification and extraction of data from textual or
binary form and offers service functionality to generate internal data models
out of the processed input.

The current status of this library is early alpha. It is however useful enough
to present it for discussion and development.

The parser code itself is OS-independend, however the current code used when
configured with -DCPARSING\_MULTI\_THREADED is not, as it is based on pthreads.
This needs to be addressed in a future version. There is also the chance that
your build environment is missing some functions cparsing uses (for example
strncpy() not being defined), but there shouldn't be many.

Since 'easy usage' includes that the library should not be its own hazard in
bugs and leaks, every build and every test was run against valgrind and the
code gets routinously checked against static code analysis. There are no
currently known bugs, but also no guarantee that you won't find some.
Please see the included copyright and disclaimer.

DOCUMENTATION
-------------
Currently this text is the main documentation for the lib. Other then that
there is at least one commented example/test demonstrating the use of
every component.

Of course, the code of the library can also be examined -- **main.c** i.e. holds
some tests, that also demostrate the usage.

This documentation assumes knowledge of the programming language C and its
required tools.

To use any of the functions and types below in your own code, you'll need to
include \<cparsing.h\> and link against libcparsing - there's no need to call an
init-function. 

##############################################################################

INTRODUCTION
------------
-- Regions, Parsers and Interpreter

* Region

A region in cparsing is a block of data defined by two pointers 'begin' and 'end'. It is also
the basic data unit most other functions produce and/or consume.

	----
	-> This concept differs from most parsing environments, which instead use the abstraction
	of a stream of data, often represented by a get_next_token() function. It is also
	one reason why CParsing is better suited for amounts of data that easily fit into memory.
	However, accessing a region as a unit allows for way easier combination of parser-steps as 
	well as for far easier modification of the processed data, as you will see later.
	(It is also more efficient, since seeking and copying can be reduced, but thats only of
	minor interest.)
	----

For a valid region, 'begin' and 'end' refer to the first and the last byte inclusively.

	increasing memory addresses->
	-------------------------------
	|    |    |    |    |    |    |
	-------------------------------
	   ^                        ^
	   |                        |
	 begin                     end


- for a region of one byte: 'begin' == 'end'.
- a region is empty if 'begin' == 'end+1'.
- in general, a valid region obeys 'begin' <= 'end+1' (the region invariant)
- additionally, a region carries a 'current' position with 'begin' <= 'current' <= 'end+1'

The term 'byte' is used because CParsing is not limited to the processing of text, but may
also be applied to binary data.

Lets declare a region or two:

	----- code ------

	// local region
	Region greeting = INIT_STR_REGION("Hello Region");

	// re-initialization: if 2nd pointer ('end') is 0L: assume a string
		init_region(&greeting, "Hello World", 0L);	

		// No clean up required for local regions as such
	
	// dynamically allocated
	Region *ptr_to_text = region("Lorem ipsum", 0L);

			...

		// There is a set of functions offering services like print or copy 
		// based on start and end address
		print_ft(ptr_to_text->begin+1, ptr_to_text->end-2);

			...

		// later: clean up the region only, as the text was a constant
		free_region(ptr_to_text);
	
	----- end ------



* Parser
--------

Basically, a parser gets a region as its input and outputs a boolean indicating whether the
parser matches the region (aka 'accepts' the input) or not (rejects the input). If it did match,
it also updates the 'current' of a region to the first byte after the match. Its pretty easy to
extend the library with your own parsers btw.


In the scope of this library it is save to say that "a parser definition implements a grammar"
and often save to use both words synonymously.

The library's pre-made parsers can be categorized with two categories being:

- basic parsers ensure a certain property, and
- combinatoric parsers that combine other parsers into more complex constructs

Here is a simple example using the basic 'Literal' parser:

	----- code ------
	// below, you'll notice some 'I0' mentioned - just ignore these for now

	// Literal matches exact string identity
	Parser *hello_matcher = Literal("Hello");

	// Caseless modifies Literal to ignore the case when matching
	Parser *world_match = Caseless(Literal("world"));

	// assuming the regions from the example above..
	if(hello_matcher->parse(hello_matcher, I0, &greeting))
		printf("Yep, 'Hello' is the first word so its a match and this line gets printed.\n");

	// ..we get a match here, even if there's still some input left that was not processed.

	// greeting's 'current' got updated to point just behind 'Hello' and since spaces are skipped by default, 
	// 'World' is now the next part 
	if(world_matcher->parse(world_matcher, I0, &greeting))
		printf("This line gets printed too.\n");

	// While regions are consumed by parsing and must be reset by assiging r.current = r.begin, 
	// parsers can readily be re-used.
	// 'greeting' is now exhausted, but luckily we have a second region:
	if(hello_matcher->parse(hello_matcher, I0, ptr_to_text))
		printf("This message will not be printed, because the region does not start with 'Hello' but with 'Lorem'\n");

		...

	// At the end: two separate parsers have been used and need to be cleaned up
	free_parser(hello_matcher);
	free_parser(world_matcher);

	----- end ------


The next snippet is an example of how parsers can be combined. It introduces
the probably most often used combinator - the 'Sequence':

	----- code ------
	// This declaration can be read as "match on 'Hello' followed by 'world' (ignoring the case of the characters)"
	Parser *full_greeting_matcher = 
		Sequence(
			Literal("Hello"),
			Caseless(Literal("world")),
		0L); // <- BEWARE! The 0L (NULL) at the end is _required_ for all parsers accepting a variable amount of parameters
			 // as it marks the end-of-parameters. Forget this zero and be doomed.

	Region greeting = INIT_STR_REGION("Hello World");
	
	if(full_greeting_matcher->parse(full_greeting_matcher, I0, &greeting))
		printf("Hello earthling.\n");	// this line gets printed

	// although we defined three parsers in one line, only the top-most parser
	// needs to be freed:
	free_parser(full_greeting_matcher);

	----- end ------


* Interpreter
-------------

So far we have just tested whether a certain parser matches against some region, but we have done nothing
with the actual token that were identified during the parse run.

To gain access to these token we provide the parse-run with an 'Interpreter' as the second argument
of the call.

The parser will call the interpreter when it becomes active, whenever it identifies
a token and whether the parse succeeded or failed. 

In fact we did supply a special interpreter named 'I0', which just ignores all messages it receives.

The detailed procedure is discussed in the API reference, as you seldom need to actually write an
interpreter yourself when using the lib. The cparsing library provides higher abstractions that are
more easily accessible for extraction and modification of the token data.

Internally however, the cparsing library uses this mechanism quite often and many parser also consist of
an interpreter part.

For the start, you can just imagine that every parser can be a source of token if a match was found. 

There are 'parsers' that can act with these tokens and its probably best to show an example before
diving deeper.

So, this is not much of an example about interpreters, but about the gain obtained through them:


	----- code ------

	// In the previous example, the word 'world' was accepted independently of the case of its letters.
	// Now we want to save how this word was actually written.

	Region greeting = INIT_STR_REGION(" Hello	wOrLd   ");

	// some buffer for our result
	char buffer[32];

	Parser *extracting_greeting_matcher = 
		Sequence(
			Literal("Hello"),
			Put(buffer, Caseless(Literal("world"))),	// (*)
		0L);

	memset(buffer, 0, sizeof(buffer));

	if(extracting_greeting_matcher->parse(extracting_greeting_matcher, I0, &greeting))
		printf("He said 'Hello' to '%s'.\n", buffer);

	// Output is "He said 'Hello' to 'wOrLd'."

	free_parser(extracting_greeting_matcher);

	----- end ------

(\*) 'Put(target, parser)' is actually a macro calling '\_Put(target, sizeof(target), parser)'
providing 'buffer' and its size, limiting the the buffer-write to this maximum size - here 32.

Other then 'Put/\_Put' to extract single values there are other interpreter-based parsers,
for example 'Edit' which allows for modification of single token, or 'Call' which calls custom
functions, providing verified tokens of entire rules for further processing (often used to construct
internal data-structures like abstract syntax trees). These and others are discussed in
the API-reference.

We now can also classify a parser into being retentive of its input or modifying,
in which case it may generate arbitary tokens based on its parse-results.

There is one consequence of this to keep in mind: while the region invariant (begin <= end+1)
of each token is guaranteed, consecutive token are \_not\_ guaranteed to be stored in increasing
memory locations. 


* Memory management
-------------------

This is C, so we have ..erm.. full control over memory allocations and releases.

As you have seen in the examples, regions and parsers are handled as individual objects, potentially
requiring memory release after usage. While regions may be declared locally and as such do not
necessarily need to be freed, parsers will always be allocated dynamically which forces us to free them.

Defining a complex parser/grammar results in a data structure we can call "a tree of parsers"
(or rather shorter: a parsetree), having a single top-most root.

When a parser 'p' is released by calling free\_parser(p), all of its included (and owned)
parsers are also released, so normally you'll only need to call free\_parser() once on the root of the
parsetree.

Things are getting a bit more complicated when you are working with more than one grammar at a time,
using partial parsers in more then one parsetree. This is discussed in depth in the API-
reference under 'Ownership'.


But there is more. It has already been mentioned that there are parsers that modify or even create
token. To be able to deal with the allocations required to perfrom an actual parser run, cparsing
uses **memory pools** to maintain these runtime allocations.

Each thread has its own **runtime\_pool()** that is used to store all intermediate token and data.
Memory on the runtime\_pool is allocated soley during parser runs and is kept until 
**clear\_runtime\_pool()** gets called, which in case releases all pool entries in one go. One
of the last calls of a program that uses cparsing is usually to call **cparsing\_cleanup()** which
releases all memory pools of all threads.

Because parse results can be held in pool memory it may be necessary to dup data out of tokens into
otherwise allocated memory to ensure their storage.

Other features of the memory pools are being discussed in the API documentation under 'Memory Pools'.



* Flags
-------------------

Flags are used to fine-tune a parsers operation. There are currently six flags for use by client
side code and those are supported by all parsers that 'make sense' for the specific flag.

The flags can be set/reset through a number of functions that comply to the parser interface
and can thus be used in grammar definitions, but in reality these only modify a parser's flags.

Flags are generally propagated from the root down to the leaf-parsers (so that when you
set a flag on a sequnce of parsers, all included parser will have the flag set), but you may
also overwrite them specifically deeper down the definition.

Here is the list of flags and their functions:

	SKIP_TOKEN		- apply and check the parser but don't include the match as TOKEN_VALUE
	IGNORE_CASE		- apply 'to lowercase' on input symbols before actual matching
	DONT_SKIP_SPACES- supress automatic skipping of whitespaces
	WEAK_MAX		- Repeat(): allow more than max occurances (but parse only up to max) 
					- default is to fail if there are more than max occurances
	TRACED          - print debug info about the parse-process
	RESTART			- tells a stateful parser (i.e. Record) to reset itself

and this is a list of wrappers provided to manipulate these flags:

	Parser *Flagged(short flags, short mask, Parser *p);	// Assign (overwrite) client side flags (skip token, ignore case, no auto skipping of whitespaces, trace and weak max)
	Parser *Caseless(Parser *p);							// set IGNORE_CASE flag
	Parser *Skipped(Parser *p);								// set SKIP_TOKEN flag
	Parser *Weak(Parser *p);								// set WEAK_MAX flag	(Repeat, Bytes and Word)
	Parser *Strong(Parser *p);								// clear WEAK_MAX flag	(Print..)
	Parser *Binary(Parser *p);								// set DONT_SKIP_SPACES flag
	Parser *Traced(Parser *p);								// set TRACED flag
	Parser *Restart(Parser *p);								// set RESTART flag - ATTENTION: if applied to p itself, p will restart in every occurence - 
															// which is probably not wanted (and may easily lead to endless loops); 
															// however: using Restart(Ref(p)) will restart this occurence of p only and leave others unmodified
	Parser *Named(char *name, Parser *p);					// not a flag, but a wrapper: a tool to set parser's name


* Text vs. Binary mode
-------------------

The only difference between these 'modes' is the value of the DONT\_SKIP\_SPACES flag:
if it is set, no parser will try to autoskip characters that are considered white space.

Per default, space, tablulator, newline and linefeed are considered white spaces, but 
you can define your own spacing using:

	void set_whitespace_pattern(Parser *w)
	
where 'w' is either result of Word(), or 0L which resets to default-whitespaces.


---
* API Reference
---------------

#Ownership

Ownership defines the scope of existence of any parser. Destroying a parser also
destroys all parsers owned by the root parser or its children.

Each parser holds management information that allows its usage and cleanup in a parser tree.

However, once a parser is **used more than once in a grammer** additional housekeeping data is required.

The rule is: 

	Each parser instance is to be used exactly once in any grammar by itself.
	All further occurences of the parser instance are to be proxied using
	Ref() or Foreign() instances. Any Ref()- or Foreign()-instance can only
	be used once.

Here are some examples to that rule:

	// --------------------------------------------------------------------------------------------
	// *** An 'OK' example
	Parser *p = ...;                                    // any parser definition
	Parser *q = Sequence(p, Ref(p), Ref(p), 0L);
	Parser *r = OneOrMore(Ref(p));                      
	// OK: first occurance of p is 'raw' while further occurances are being individually 'Ref'ed.
	
	// --------------------------------------------------------------------------------------------
	// *** ATTENTION *** THIS IS WRONG *** :
	Parser *p = ...;                                    // any parser definition
	Parser *q = Sequence(p, p, ...);					
	// WRONG: 2nd occurance of p without Ref
	
	// *** ATTENTION *** THIS IS WRONG *** :
	Parser *single_ref_to_p = Ref(p);
	Parser *r = Sequence(p, single_ref_to_p, .., single_ref_to_p, ..);  
	// DO NOT DO THIS: only one instance of Ref used to back-up two occurances of p


Fortunately, most of the times we can just define new instances of parsers to entirely prevent this
problematic. Preprocessor definitions are of good use here:

	#define NUMBER		Word(DIGIT, 1, -1)
	#define OPEN		Skipped(Literal("("))
	#define CLOSE 		Skipped(Literal(")"))
	#define COMMA 		Skipped(Literal(","))
	#define TUPEL		Sequence(OPEN, NUMBER, ZeroOrMore(Sequence(COMMA, NUMBER, 0L)), CLOSE, 0L)

	// now we can use TUPEL in grammar definitions and are always getting
	// everything fresh - no use of Ref() at all...

Alas, some parsers require to be used in more then one place, i.e. ForwardDeclaration() or Record().

---
### Parser \*Ref(Parser \*p)

Create a new reference to 'p', claiming ownership: if Ref is destroyed also reduce p's 
reference count, freeing p once it reaches zero.


---
### Parser \*Foreign(Parser \*p)

Create a new reference to 'p', **but do not** claim ownership: destroying the
foreign-reference has no impact on p's reference count.

Used when a parser is required in more than one parse tree and allows to
destroy the grammar containg the foreign-reference with no effect to the source grammar.


#Basic Parsers

---
### Parser \*Literal(char \*string\_to\_match)

Test for exact match of the given string\_to\_match; the provided text may not be empty.


---
### Parser \*LiteralRegion(Region \*region\_to\_match)

Test for exact match of the given region\_to\_match; the provided region may not be empty.


---
### Parser \*Word(const char \*character\_pool, int min, int max)

Matches a word made up by symbols of character\_pool with given minimal and maximal len. 
Using -1 as minimum or maximum len means 'any len including zero'.

Depending on the WEAK\_MAX flag, this parser either fails if there are more than 'max'
matches (STRONG), or succeed after 'max' bytes leaving further matches in the input (WEAK).

---
### Parser \*CharNotIn(void \*character\_pool, int min, int max)

Complement to 'Word'-parser: matches a word **not** made up by symbols of character\_pool 
with given minimal and maximal len. 
Using -1 as minimum or maximum len means 'any len including zero'.


---
### Parser \*StringSpec(char quote, char escape)

Matches a string enclosed by 'quote's, honoring 'escape' character semantics; i.e.
with quote='"' and escape='\' the text '\"' will be interpreted as a literal quotation mark.

Note: use the Len-parser to limit StringSpec's results if required.


---
### Parser \*String(void)

Shorthand for StringSpec being called with quote='"' and escape='\'.


---
### Parser \*Base2(int min\_value, int max\_value)
Parser \*Base10(int min\_value, int max\_value, int is\_signed)
Parser \*Base16(int min\_value, int max\_value)

Matches text representing numbers in duodecimal-, decimal- or hexadecimal systems.
A '+' occuring directly before the number is accepted; in Base10 mode with 'is\_signed' set to
true, a leading '-'-sign is also accepted.

The Base16-parser is configured to ignore\_case by default.

If matched, the result is interpreted as a number of the given base and tested against the
range  [min\_value, max\_value]. The value is rejected if it is found to lay outside of that region.


---
### Parser \*Eol(void)

Matches at the end of an input-line, where end-of-line is indicated by either
* an exhausted input region, or
* a string terminator ('\0' character) is found, or
* a newline ('\n' character) is found.

PITFALL: use CaptureUntil(1, Skipped(Eol())), if you want the rest of the line:
if you do not 'include\_match' with CaptureUntil, the parser will loop for ever on the first newline.


---
### Parser \*Bytes(int size, int min, int max)

Likewise: Int8(int min, int max), Int16(..), Int24(..), Int32(..), Int40(..), Int48(..), Int56(..),
Int64(..), Int128(..), Int256(..)
with the number indicating the width of the data given as bit-count.

Matches byte-blocks of size 'size' in bytes - min/max is block-count with the usual '-1' for 'any'.
No content check is being performed; the only criteria is, that there are enough bytes left in the input.


---
### Parser \*DynBytes(min\_max\_size\_getter f, void \*param)

Get the number of bytes to be read from the input by calling the min\_max\_size\_getter each time prior to
running the parser.

Here min\_max\_size\_getter is defined as 

	typedef MinMaxSize (*min_max_size_getter)(void *param);

with

	typedef struct
	{
		int min;
		int max;
		int size;
	} MinMaxSize;

The parameter 'param' is passed through to the min\_max\_size\_getter function.

Note: this parser and the more generic DynRepeat parser are mostly used with length
information extracted out of the data that is being parsed.

Depending on the WEAK\_MAX flag, this parser either fails if there are more than 'max'
matches (STRONG), or succeed after 'max' bytes leaving further matches in the input (WEAK).

---
### Parser \*ElementOf(Dict \*dict);                              

Looks up valid regions in 'dict' - if a match was found, dict-\>get\_value() returns the associated value until next lookup



---
#Combinatoric Parsers

---
### Parser \*Sequence(Parser \*p1, ...)

The Sequence is one of the most basic combinators. It is parametrized with a left-to-right sequence of parsers, 
terminated by '0L', and requires all given parser to match in the given order for the sequence to match.
The Sequence-parser honors the 'optional' flag of enclosed parsers.

PITFALL: if you forget the terminating 0L at the end of the list of parsers, your program
will die a horrible death.

PITFALL: Optional() or ZeroOrMore() and the like at the beginning of a sequence tends to terminate too early,
because a fail of the inner parser will actually result in a match \_because\_ the inner parser was optional.
Rule of Thumb: do not prefix mandatory parser with optional parsers (use explicit alternatives instead).


---
### Parser \*Or(Parser \*p1, ...)

Process a set of alternative parsers left-to-right: first match gets chosen. The list of parsers must
be terminated by 0L.

PITFALL: if you forget the terminating 0L at the end of the list of parsers, your program
will die a horrible death.

PITFALL: declaration order within a grammar: a pattern-based parser may soak up input for a
more specific parser - i.e:

	Or(Word(ALPHANUM, 1, -1), Literal("foo"), 0L)

will always match on Word(), never reaching the Literal() parser
Rule of Thumb: put the most specific parser first


---
### Parser \*Repeat(Parser \*p, int min, int max)
	Parser *Optional(Parser *p)
	Parser *OneOrMore(Parser *p)
	Parser *ZeroOrMore(Parser *p)

Checks for min..max occurances of 'p' and fails if more os less are found. 
'-1' for min or max means 'any, including zero' occurences.

	Optional(Parser *p) is equivalent to Repeat(p, 0, 1)
	OneOrMore(Parser *p) is equivalent to Repeat(p, 1, -1)
	ZeroOrMore(Parser *p) is equivalent to Repeat(p, 0, -1)

Depending on the WEAK\_MAX flag, this parser either fails if there are more than 'max'
matches (STRONG), or succeed after 'max' bytes leaving further matches in the input (WEAK).

---
### Parser \*DynRepeat(Parser \*p, min\_max\_getter f, void \*vparam)

Get the number of times parser 'p' is being run by calling 'min\_max\_getter' directly
before each parse attempt.

Here the getter is defined as

	typedef MinMax (*min_max_getter)(void *param);

with

	typedef struct
	{
		int min;
		int max;
	} MinMax;

As usual, a value of -1 means any amount of occurences including zero.

Depending on the WEAK\_MAX flag, this parser either fails if there are more than 'max'
matches (STRONG), or succeed after 'max' bytes leaving further matches in the input (WEAK).

---
### Parser \*MatchAll(Parser \*p1, ...)

This is the 'And' operation: it matches if each parser matches against the whole input (left-to-right).
MatchAll passes every parser's tokens to its interpreter.

PITFALL: if you forget the terminating 0L at the end of the list of parsers, your program
will die a horrible death.


---
### Parser \*Until(Parser \*mark, Parser \*consumer)
Repeat parsing with 'consumer' until 'mark' is found. 'mark' will not be emitted.
If the mark is found, the input-position is updated to point at the mark.
If the mark is not found the parser will reject its input.


---
### Parser \*CaptureUntil(int include\_match, Parser \*mark)
Search for 'mark' - result is the provided input, consuming the mark if include\_match is true
(INCLUDE\_MATCH), excluding the mark (EXCLUDE\_MATCH) else. CaptureUntil fails if the mark
can not be found.

Warning: current implementation is dumb linear search, repeatedly calling 'mark'


---
### Parser \*Not(Parser \*p)

Negates p's parse-result - does not advance in input and won't ever emit any token.
It's purpose are pre-checks used in the other combinatoric parsers.


---
### Parser \*Lookahead(Parser \*p)

Runs 'p' returning its result but does not yield token and does not change input position.


---
### Parser \*Len(int min\_len, int max\_len, Parser \*producer);
Test each of producer's generated tokens for it's length and fail if one
token-len is outside of [min\_len, max\_len].

Len(0,..) == Len(-1,..) => optional

Len(0, -1, p) == Len(-1, -1, p) => match always


---
### Parser \*ForwardDeclaration(void)
### Parser \*ForwardAssign(Parser \*forward\_declaration, Parser \*assignment)

Used for recursive grammars: when a parser is referred to before actually being defined, 'forward declare'
the parser and later 'forward assign' its completed definition.

PITFALL: OF COURSE, this can lead to infinite loops - there should be a section discussing
recursive grammars in the docu (TODO).

In (E)BNF, recursion is often used just to express repetition; most of the time, it is recommended to
replace these with (optional) Repeats, such as ZeroOrMore(). Using recursive grammar when
you don't have to is a pretty good way to run into trouble.


---
### Parser \*Nth(unsigned index, Parser \*p)

Emits the nth token of p only, counting from 0. 
Nth fails if p emits less than index+1 token.


---
### Parser \*Chain(Parser \*producer, Parser \*consumer)

Call 'consumer' once for each token produced by 'producer', providing the token
as input to 'consumer'.

The consumer will be called only if the producer accepts its input.


---
### Parser \*Distribute(Parser \*source, Parser \*p1, ...)
### Parser \*Data(void)			

Distribute calls p1->parse(source token 1), p2->parse(source token 2), ...
thus distributing tokens produced by 'source' in order of creation and
assigns them one by one as input to the provided list of parsers.

This can be useful if parsed input is to be processed further, but in
different manners depending on the tokens position in the input.

Distribute will fail if the source produces less token than required to assign
each parser its input, but will accept and pass token that exceed the requirement.

'Data' emits its input-region and is intended for use in Distribute's consumers
to receive their assigned token.

PITFALL: if you forget the terminating 0L at the end of the list of parsers, your program
will die a horrible death.


---
### Parser \*Record(Parser \*p)
Capture p's results and save them on first call to 'parse' - replay the results on further calls to 'parse';
Replaying will not change the input region / it yields the same results as p.

The record-parser can be reset to capture mode by including

	Reset(Ref(*reference to the recorder*))

at the required location in the grammar definition.

PITFALL: you need to apply 'Reset' to a 'Ref'erence of the recoder, not to the recorder itself if you
ever want to use the replay-mode.


---
#Recoders aka modifying parsers

---
### Parser \*Emit(char \*from, size\_t len)
### Parser \*EmitRegion(Region \*r)

Emits the given region and will always succeed.
If -in Emit- len is 0, strlen(from) will be used.


---
### Parser \*Pack(Parser \*p)

Packs token emitted by 'p' into one continous region (in the order of the creation of the token) and
emits the packed region.


---
### Parser \*Join(Region \*filler, char flags, Parser \*p)

Like pack, but inserting 'filler' in between each token and optional 
in front of the first token (flags & JOIN\_WITH\_HEAD != 0) and/or after the
last token (flags & JOIN\_WITH\_TAIL != 0).


---
### Parser \*Edit(Editor editor, void \*editor\_param, Parser \*source)

Calls 'editor' defined as

	typedef int (*Editor)(void *editor_param, char **from, char **to)

for every token of 'source'. The parameter 'editor\_param' is just
passed over to the edit-function, while 'from' and 'to' are both
input and output to the function.

When the editor-function gets called, \*\*from is the first char of the input and \*\*to is
its last.

The edit-function can change \*from and \*to, to return abitrary modified token. 
If \*from and \*to are changed, the region invariant (\*from <= \*to +1) must be obeyed.

The return value of the edit function is either PARSER\_MATCH (== EDIT\_OK) or PARSER\_FAILED (== EDIT\_FAILED), 
so to inform the lib, whether the token was accepted or rejected.

There are some pre-made wrappers to Edit, providing often used conversions:

	Parser *ToIntB(int base,Parser *);		// to type 'int' using 'base' as conversion base
	Parser *ToShortB(int base,Parser *);	// to type 'short' using 'base' as conversion base
	Parser *ToLongB(int base,Parser *);		// to type 'long' using 'base' as conversion base
	Parser *ToByte(Parser *);				// to type 'char', assuming base is 10
	Parser *ToInt(Parser *);				// to type 'int', assuming base is 10
	Parser *ToShort(Parser *);				// to type 'short', assuming base is 10
	Parser *ToLong(Parser *);				// to type 'long', assuming base is 10
	Parser *ToDouble(Parser *);				// to type 'double', assuming base is 10
	Parser *ToPtr(Parser *);				// to type 'char *', allocating the required memory from parse pool
	Parser *ToRegion(Parser *);				// to type 'Region', allocating the required memory from parse pool
	Parser *ToLower(Parser *);				// to lowercase text
	Parser *ToUpper(Parser *);				// to uppercase text
	Parser *ToBits(Parser *);				// 'to bit-string' actually, as in "100110"
	Parser *Htons(Parser *);				// 'host to net' working on type 'short'
	Parser *Ntohs(Parser *);				// 'net to host' working on type 'short'
	Parser *Htonl(Parser *);				// 'host to net' working on type 'long'
	Parser *Ntohl(Parser *);				// 'net to host' working on type 'long'
	Parser *DictSubst(Dict *substitutions);	// replace with associated value, if found in 'substitutions'
	Parser *AddToDict(Dict *,Parser *);		// add tokens to the dict

PITFALL: while Edit can produce token of all possible types, token do not contain any type info but are just a block
of bytes. If you emit token of a different kind then text (for example after converting a textual representation of 
a floating point number into a C double using ToDouble()), you have to ensure yourself that other parsers who receive
these token confirm to the token's types. The cparsing lib is currently performing size tests only.

---
### Parser \*\_Put(void \*target, size\_t size, Parser \*source)

Place size bytes of source's token at address 'target'.
Of course, target's size must be greater or equal the size of the received token.

\_Put will fail, if more than one token is emitted by the source parser.

It's called \_Put, because the following macro is actually used more often

	#define Put(T,P) _Put((T), sizeof(*(T)), P)

since it quietly determines the available buffer based on the target's true type.


---
### Parser \*\_PutStruct(void \*\*base, size\_t offset, size\_t size, Parser \*source)

Place size bytes of source's token at the address indicated by adding 'offset' bytes to \*base.

\_PutStruct will fail, if more than one token is emitted by the source parser.

Again, there is a more comfortable Version defined with

	PutStruct(PTR_TO_STRUCT, TYPE_OF_STRUCT, STRUCT_MEMBER, source)	

which calculates the required information based on a pointer to a struct, the type of the
struct and the name of the desired target member.

This usage also explains the parsers name: "put the token into dat struct"..


---
### Parser \*Call(ctor\_function ctor, void \*user\_data, Parser \*p)

Calls 'ctor' if and only if p succeeds, providing all token emitted by p - usually to construct 
some data structure.

The callback's prototype is given by

	typedef int (*ctor_function)(void *user_data, InterpreterInfo I, Parser *data_source)

Here, 'user\_data' is a passed-through pointer, 'I' the currently active interpreter
(see interpreter protocol) and 'data\_source' is the Record'ed result of the parser 'p'.

The Call itself consumes all received token, but the callback may use the provided interpreter
'I' to inject results into the parse process.


---
### Parser \*Callback(parser\_callback\_function function, void \*arg\_in, void \*\*arg\_out)

Simpler then Call, Callback just calls some function from within the parse-process.
Only two parameters are available, with only 'arg\_out' being modifiable (due to double indirection).

The callback's prototype is given as 

	typedef int (*parser_callback_function)(void *arg_in, void **arg_out);

Can be used to maintain lists or stacks during the parse process.


---
#Error-handling in grammars

.. is done throught parsers that call a dedicated error\_handler in case of a failure.

The error-handlers are defined as
	
	typedef void (*error_function)(
		void *user_data, 
		const const char *error_message, 
		Parser *failed_parser, 
		Region *failed_input
	); 

The 'user\_data' and 'error\_message' fields are provided in the grammar definition, 'failed\_parser'
and 'failed\_input' are added by the lib.

TODO: as of yet, no line number information about the input is provided. This has to be added 
at some point in the future.

---
### Parser \*Fail(void \*user\_data, error\_function error\_handler, const char \*error\_message)

This parser fails and calls its error\_handler, forwarding error\_message and user\_data.

Its primary use is to notify that no alternative of an Or-parser had been found.


---
### Parser \*OnFail(void \*user, error\_function error\_handler, const char \*error\_message, Parser \*p)

OnFail fails and calls its error\_handler only if 'p' failed. 'error\_message' and 'user\_data' are
forwarded to the error handler.


---
#Interpreter Protocol

A parser's parse() function is defined as


	typedef int (*parse_function)(struct Parser *self, InterpreterInfo I, Region *input);

The first argument is the parser itself to provide an object pointer; the third
argument is the region to be parsed.

The second argument is a description of an interpreter, build to receive generated tokens
and events (called 'Phases') of that parse-run.

The InterpreterInfo contains an private data pointer, a flags field and an associated mask field
and a function reference defined by

	typedef int (*interpreter_function)(struct Parser *self, void *interpreter_param, int phase, char *begin, char *end);

which is called for each 'phase' for the current input [begin, end]. The phases are:
	
    TOKEN_ENTER
    // Phase 1 called (once) every time a parser is started.
    // receives: input->begin == start of whole region, input->end == end of whole region

    TOKEN_VALUE            // once for every token found
    // Phase 2 can be called multiple times, even in case of later failure.
    // receives: input->begin == token start (after skip_spaces), input->end == end of token

    TOKEN_COMPLETED or
    TOKEN_FAILED
    // Phase 3 signals match or failure (once for every parser activation).
	// If a TOKEN_FAILED was received, all token received during TOKEN_VALUE are to be discarded.
    // receives: input->begin == position after parse, input->end == end of whole region

Writing your own interpreter is easy: you get notified once the parser starts, collect values during
TOKEN\_VALUE and are save to use them (in case of TOKEN\_COMPLETED) or have to ignore them (TOKEN\_FAILED).

Assign this function to InterpreterInfo.interpreter, add InterpreterInfo.param if needed and provided
said info to a parser's parse() call.

There's a pre-defined interpreter that just does nothing: I0 (IZero).

---
#Memory Pools

Parsers are created when writing grammars and are being destroyed by calling destroy\_parser().

All allocations required during a parse-run however are done in the current runtime\_pool().

A pool is just list of allocations, that are altogether thrown away by a call to

	void clear_runtime_pool(void);
	
So you can simply throw away all objects created during a parser's parse()-run by
calling this function afterwards.

You can also 'put away' an entire memory pool for later clean up by calling

	void *detach_runtime_pool(void);		// freezes the current memory pool and returns a handle to the frozen pool

which returns a handle to the last active pool and creates a new active pool, and then, later call

	void restore_runtime_pool(void *pool);	// reactivates its argument as current parse-pool -- all allocations on the previously active pool are freed

to restore the detached pool again.

There are some service functions to allocate pool-memory:

	parse_malloc(LEN)			== pool_malloc(LEN, runtime_pool())
	parse_calloc(AMOUNT,LEN)	== pool_calloc(AMOUNT, LEN, runtime_pool())
	parse_strndup(SRC,MAXLEN)	== pool_strndup(SRC, MAXLEN, runtime_pool())
	parse_memdup(SRC,LEN)		== pool_memdup(SRC, LEN, runtime_pool())
	parse_owned_mem(PTR)		== pool_owned_mem(PTR, runtime_pool())
	parse_ftdup(FROM,TO)		== pool_ftdup(FROM, TO, runtime_pool())


LICENSE
-------
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


---
...... TO BE CONTINUED .....




---
[(1) Chomsky hierarchy] (http://en.wikipedia.org/wiki/Chomsky_hierarchy) for a short overview.
Formal languages are part of the field of theoretical computer science.
[(2) Recursive Descent Parser] (http://en.wikipedia.org/wiki/Recursive_descent_parser)
[(3) Pyparsing] (http://pyparsing.wikispaces.com/home)

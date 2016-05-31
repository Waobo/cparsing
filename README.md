# cparsing

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


NOTE: this is still work in progress. For example, the meta-parser does
not know about some newer parsers and the C-parser never had a in-depth test
and also still misses the typedef component -- even this readme is far 
from complete. To make things worse, I'm working on this project very
erratically. The currently available functionality is however already useful.


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
configured with -DCPARSING_MULTI_THREADED is not, as it is based on pthreads.
This needs to be addressed in a future version. There is also the chance that
your build environment is missing some functions cparsing uses (for example
strncpy() not being defined), but there shouldn't be many.

Since 'easy usage' includes that the library should not be its own hazard in
bugs and leaks, every build and every test was run against valgrind and the
code gets routinously checked against static code analyses. There are no
currently known bugs, but also no guarantee that you won't find some.
Please see the included copyright and disclaimer.

DOCUMENTATION
-------------
Currently this text is the main documentation for the lib. Other then that
there is at least one commented example/test demonstrating the use of
every component.

Of course, the code of the library can also be examined -- *main.c* i.e. holds
some tests, that also demostrate the usage.

This documentation assumes knowledge of the programming language C and its
required tools.

To use any of the functions and types below in your own code, you'll need to
include <cparsing.h> and link against libcparsing - there's no need to call an
init-function. 

###############################################################################

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
	minor intrest.)
	----

For a valid region, 'begin' and 'end' refer to the first and the last byte inclusively.

```
increasing memory addresses->
-------------------------------
|    |    |    |    |    |    |
-------------------------------
   ^                        ^
   |                        |
 begin                     end
```


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

Basically, a parser gets a region as its input and outputs a boolean indicating wether the
parser matches the region or not. If it did match, it also updates the 'current' of a region
to the first byte after the match. Its pretty easy to extend the library with your own parsers btw.

In the scope of this library it is save to say that "a parser definition implements a grammar"
and often save to use both words synonymously.

The library's pre-made parsers fall into two categories:

- basic parsers ensure a certain property, and
- combinatoric parsers that combine other parsers into more complex constructs

Here is a simple example using the basic 'Literal' parser:

	----- code ------
	// below, you'll notice some 'I0' mentioned - just ignore these for now

	// Literal matches exact string identity
	Parser *hello_matcher = Literal("Hello");

	// Caseless modifies Literal to ignore the case when matching
	Parser *world_match = Caseless(Literal("world"));

	// assuming the regions from the example above
	if(hello_matcher->parse(hello_matcher, I0, &greeting))
		printf("Yep, 'Hello' is the first word so its a match and this line gets printed.\n");

	// greeting's 'current' got updated to point just behind 'Hello' and since spaces are skipped by default, 
	// 'World' is now the next part 
	if(world_matcher->parse(world_matcher, I0, &greeting))
		printf("This line gets printed too.\n");

	// while regions are consumed by parsing, parsers can be re-used.
	// 'greeting' is now exhausted, but luckily we have a second region:
	if(hello_matcher->parse(hello_matcher, I0, ptr_to_text))
		printf("This message will not be printed, because the region does not start with 'Hello' but with 'Lorem'\n");

		...

	// At the end: two separate parsers were used and need to be cleaned up
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

So far we have just tested wether a certain parser matches against some region, but we have done nothing
with the actual token that were identified during the parse run.

To gain access to these token we provide the parse-run with an 'Interpreter' as the second argument
of the call.

The parser will call the interpreter when it becomes active, whenever it identifies
a token and wether the parse succeeded or failed. 

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

(*) 'Put(target, parser)' is actually a macro calling '_Put(target, sizeof(target), parser)'
providing 'buffer' and its size, limiting the the buffer-write to this maximum size - here 32.

Other then 'Put/_Put' to extract single values there are other interpreter-based parsers,
for example 'Edit' which allows for modification of single token, or 'Call' which calls custom
functions, providing verified tokens of entire rules for further processing (often used to construct
internal data-structures like abstract syntax trees). These and others are discussed in
the API-reference.

We now can also classify a parser into being retentive of its input or modifying,
in which case it may generate arbitary tokens based on its parse-results.

There is one consequence of this to keep in mind: while the region invariant (begin <= end+1)
of each token is guaranteed, consecutive token are _not_ guaranteed to be stored in increasing
memory locations. 


* Memory management
-------------------

This is C, so we have erm.. full control over memory allocations and releases.

As you have seen in the examples, regions and parsers are handled as individual objects, potentially
requiring memory release after usage. While regions may be declared locally and as such do not
necessarily need to be freed, parsers will always be allocated dynamically which forces us to free them.

Defining a complex parser/grammar results in a data structure we can call "a tree of parsers"
(or rather shorter: a parsetree), having a single top-most root.

When a parser 'p' is released by calling free_parser(p), all of its included (and owned)
parsers are also released, so normally you'll only need to call free_parser() once on the root of the
parsetree.

Things are getting a bit more complicated when you are working with more than one grammar at a time,
using partial parsers in more then one parsetree. This is discussed in depth in the API-
reference under Ref() and Foreign().

But there is more. It has already been mentioned that there are parsers that modify or even create
token. To be able to deal with the allocations required to perfrom an actual parser run, cparsing
uses memory pools to maintain these runtime allocations.

Each thread has its own runtime_pool() that is used to store all intermediate token and data.
Memory on the runtime_pool is allocated soley during parser runs and is kept until 
clear_runtime_pool() gets called, which in case releases all pool entries in one go. One
of the last calls of a program that uses cparsing is usually to call cparsing_cleanup() which
releases all memory pools of all threads.

Because parse results can be held in pool memory it may be necessary to dup data out of tokens into
otherwise allocated memory to ensure their storage.

Other features of the memory pools are being discussed in the API documentation under runtime_pool().



...... TO BE CONTINUED .....




(1) http://en.wikipedia.org/wiki/Chomsky_hierarchy for a short overview. Formal languages are part
of the field of theoretical computer science.
(2) http://en.wikipedia.org/wiki/Recursive_descent_parser
(3) http://pyparsing.wikispaces.com/home


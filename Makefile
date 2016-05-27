CC=gcc
# LDFLAGS= -lm -g
LDFLAGS= -lm -g -lpthread
LIB_SOURCES=cparsing.c cparsing_list.c cparsing_region.c scope_malloc.c recoder.c cparsing_dict.c cparsing_meta.c cparsing_printf.c cpp.c
LIB_HEADERS=cparsing_dict.h cparsing.h cparsing_list.h cparsing_region.h lol.h scope_malloc.h
LIB_LDFLAGS=$(LDFLAGS) -fPIC -shared -O0 -g

SOURCES_FILES=$(LIB_SOURCES) lol.c main.c # c.c
DEPENDENCIES=$(SOURCES_FILES:.c=.d) 
SOURCES= $(SOURCES_FILES) 

OBJECTS=$(SOURCES_FILES:.c=.o)
LIB_OBJECTS=$(LIB_SOURCES:.c=.o)

EXECUTABLE=cparsing
LIBRARY=libcparsing.so

all: devel $(SOURCES) $(EXECUTABLE) $(LIBRARY)
	@echo "DEVELOPER build complete -- use 'make lib' to build for release"

lib: clean production $(SOURCES) $(EXECUTABLE) $(LIBRARY)
	@echo "RELEASE build complete"

$(EXECUTABLE): $(OBJECTS) 
	$(CC) $(OBJECTS) -o $@ $(LDFLAGS) 

$(LIBRARY): $(LIB_SOURCES)
	$(CC) -o $@ $(LIB_LDFLAGS) $(LIB_SOURCES) 

production:
	$(eval CFLAGS := -c -Wall -DNDEBUG)

devel:
	$(eval CFLAGS := -c -Wall -g)

%.d: %.c
	@set -e; rm -f $@; \
	$(CC) -MM $(CPPFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

-include $(DEPENDENCIES)

.c.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	-rm *.o cparsing *.so *.d

scope_malloc_test.o: scope_malloc_test.c

scope_test: scope_malloc.c scope_malloc.h scope_malloc_test.o cparsing_list.o
	$(CC) scope_malloc.o scope_malloc_test.o cparsing_list.o -o $@ $(LDFLAGS) 

tags:
	ctags -R
	cscope -Rb

.PHONY: clean tags devel production

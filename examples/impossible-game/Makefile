ECLISP := ../../eclisp
QUIET := @
CC = gcc
CFLAGS := -g3 -Wall -Wextra $(shell pkg-config --cflags sdl2)
LDFLAGS := $(shell pkg-config --libs sdl2)
.SUFFIXES: .eclisp .eclisph

.eclisph.h:
	$(QUIET)$(ECLISP) $< > $@

.eclisp.c:
	$(QUIET)$(ECLISP) $< > $@

all: ig
	rm -rf *.c *.h

ig: ig.o
	gcc $(CLAGS) $< $(LDFLAGS) -o $@

clean:
	rm -rf ig ig.o

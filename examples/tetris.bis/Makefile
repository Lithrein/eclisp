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

all: tetris
	rm -rf *.c *.h

tetris: tetris.o
	gcc $(CLAGS) $< $(LDFLAGS) -o $@

macros.h: ../../include/macros.eclisph
	$(QUIET)$(ECLISP) $< > $@

tetris.c: macros.h

clean:
	rm -rf tetris tetris.o

ECLISP := ../../eclisp
QUIET := @
CC = gcc
CFLAGS := -g3 -Wall -Wextra $(shell pkg-config --cflags sdl2)
LDFLAGS := $(shell pkg-config --libs sdl2)
.SUFFIXES: .eclisp

.eclisp.c:
	$(QUIET)cat macros.eclisph $< | $(ECLISP) > $@

tetris: tetris.o
	gcc $(CLAGS) $< $(LDFLAGS) -o $@

clean:
	rm -rf tetris tetris.o

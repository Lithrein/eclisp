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

all: breakout
	rm -rf *.c *.h

breakout: breakout.o
	gcc $(CLAGS) $< $(LDFLAGS) -o $@

clean:
	rm -rf breakout breakout.o

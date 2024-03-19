ECLISP := ../../eclisp
QUIET := @
CC = gcc
CFLAGS := -O2 -g3 -Wall -Wextra
LDFLAGS := -lm
.SUFFIXES: .eclisp .eclisph

.eclisph.h:
	$(QUIET)$(ECLISP) $< > $@

.eclisp.c:
	$(QUIET)$(ECLISP) $< > $@

all: rt
	rm -rf *.c *.h

rt: rt.o utils.o
	gcc $(CLAGS) $^ $(LDFLAGS) -o $@

rt.c: macros.h include/ray-tracer/utils.h
%.c: %.h

clean:
	rm -rf rt rt.o

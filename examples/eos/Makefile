ECLISP := ../../eclisp
QUIET := @
CC = gcc
CFLAGS := -g0 -Wall -Wextra -O3 -fdump-tree-optimized
LDFLAGS :=
.SUFFIXES: .eclisp .eclisph

.eclisph.h:
	$(QUIET)$(ECLISP) $< > $@

.eclisp.c:
	$(QUIET)$(ECLISP) $< > $@

all: eos
	rm -rf *.c *.h

eos: eos.o
	gcc $(CLAGS) $< $(LDFLAGS) -o $@

macros.h: ../../include/macros.eclisph
	$(QUIET)$(ECLISP) $< > $@

clean:
	rm -rf eos eos.o

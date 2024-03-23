ECLISP := ../../eclisp
QUIET := @
CC = gcc
CFLAGS := -O2 -g3 -Wall -Wextra
LDFLAGS := -lm
.SUFFIXES: .eclisp

.eclisp.c:
	$(QUIET)cat macros.eclisph $< | $(ECLISP) > $@

rt: rt.o
	gcc $(CLAGS) $< $(LDFLAGS) -o $@

clean:
	rm -rf rt rt.o

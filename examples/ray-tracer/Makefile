ECLISP := ../../eclisp
QUIET := @
CFLAGS := -O2 -g3 $(shell pkg-config --cflags sdl2)
LDFLAGS := -lm $(shell pkg-config --libs sdl2)
.SUFFIXES: .eclisp

.eclisp.c:
	$(QUIET)cat macros.eclisph $< | $(ECLISP) > $@

rc: rc.o
	gcc $(CLAGS) $< $(LDFLAGS) -o $@

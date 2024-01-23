ECLISP := ../../eclisp
QUIET := @
CFLAGS := -g3 $(shell pkg-config --cflags sdl2)
LDFLAGS := $(shell pkg-config --libs sdl2)
.SUFFIXES: .eclisp

.eclisp.c:
	$(QUIET)cat macros.eclisph $< | $(ECLISP) > $@

tetris: tetris.o
	gcc $(CLAGS) $< $(LDFLAGS) -o $@

clean:
	rm tetris tetris.o

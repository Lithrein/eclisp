ECLISP := ../../eclisp
QUIET := @
CFLAGS := -O2 -g3
LDFLAGS := -lm
.SUFFIXES: .eclisp

.eclisp.c:
	$(QUIET)cat macros.eclisph $< | $(ECLISP) > $@

rt: rt.o
	gcc $(CLAGS) $< $(LDFLAGS) -o $@

clean:
	rm rt rt.o

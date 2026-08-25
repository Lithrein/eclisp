ECLISP := ../../eclisp
QUIET := @
CC = gcc
CFLAGS := -O0 -g3 -Wall -Wextra $(shell pkg-config --cflags sdl2)
LDFLAGS := -lm $(shell pkg-config --libs sdl2)
.SUFFIXES: .eclisp .eclisph

SOURCES = \
	simple.eclisp

X_FILES = $(addprefix build/,$(patsubst %.eclisp,%.X,$(SOURCES)))

.INTERMEDIATE: $(X_FILES)

.eclisph.h:
	$(QUIET)$(ECLISP) $< > $@

.eclisp.c:
	$(QUIET)$(ECLISP) $< > $@

all: simple

simple: build/simple.o
	$(CC) $(CLAGS) $^ $(LDFLAGS) -o $@

build/simple.c: \
	build/simple.h

%.c: %.X
	@sed -e 's/#include/%include/' $< > $@
	@cpp -Ibuild -C -ffreestanding $@ -o $@.i
	@sed -i -e 's/^# [0-9]\{1,\}.*//' $@.i
	@mv $@.i $@
	@sed -i 's/%include/#include/' $@
	@clang-format -i $@
	@awk 'NF {p=1} p' $@ > $@.i && mv $@.i $@

%.h: %.X
	@sed -e 's/#include/%include/' $< > $@
	@cpp -Ibuild -C -DPROCESS_HEADER -ffreestanding $@ -o $@.ih
	@sed -i -e 's/^# [0-9]\{1,\}.*//' $@.ih
	@mv $@.ih $@
	@sed -i '1i#ifndef $(basename $(notdir $@))_H\n' $@
	@sed -i '2i#define $(basename $(notdir $@))_H\n' $@
	@echo '#endif' >> $@
	@sed -i 's/%include/#include/' $@
	@clang-format -i $@

make-build-dir:
	@mkdir -p build

build/%.X: %.eclisp | make-build-dir
	$(QUIET)$(ECLISP) $^ > $@

build/%.h: %.eclisph | make-build-dir
	$(QUIET)$(ECLISP) $^ > $@

clean:
	rm -rf build/

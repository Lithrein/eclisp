ECLISP := ../../eclisp
QUIET := @
CC = gcc
CFLAGS := -O2 -g3 -Wall -Wextra
LDFLAGS := -lm
.SUFFIXES: .eclisp .eclisph

SOURCES = \
	rt.eclisp \
	utils.eclisp \
	vec3.eclisp

X_FILES = $(addprefix build/,$(patsubst %.eclisp,%.X,$(SOURCES)))

.INTERMEDIATE: $(X_FILES)

.eclisph.h:
	$(QUIET)$(ECLISP) $< > $@

.eclisp.c:
	$(QUIET)$(ECLISP) $< > $@

all: rt

rt: build/utils.o build/rt.o build/vec3.o
	gcc $(CLAGS) $^ $(LDFLAGS) -o $@

build/rt.c: build/rt.h build/utils.h build/vec3.h
build/utils.c: build/utils.h
build/vec3.c: build/vec3.h

%.c: %.X
	sed -e 's/#include/%include/' $< > $@
	cpp -Ibuild -C $@ -o $@.i
	sed -i '1,/# [0-9]\{1,\} "<command-line>" 2/d' $@.i
	sed -i -e 's/^# [0-9]\{1,\}.*//' $@.i
	mv $@.i $@
	sed -i 's/%include/#include/' $@
	clang-format -i $@

%.h: %.X
	sed -e 's/#include/%include/' $< > $@
	cpp -Ibuild -C -DPROCESS_HEADER $@ -o $@.ih
	sed -i '3,/# 1 "<command-line>" 2/d' $@.ih
	sed -i -e 's/^# [0-9]\{1,\}.*//' $@.ih
	mv $@.ih $@
	sed -i '1i#ifndef $(basename $(notdir $@))_H\n' $@
	sed -i '2i#define $(basename $(notdir $@))_H\n' $@
	echo '#endif' >> $@
	sed -i 's/%include/#include/' $@
	clang-format -i $@

build/%.X: %.eclisp
	$(ECLISP) $^ > $@

build/%.h: %.eclisph
	$(ECLISP) $^ > $@

clean:
	rm -rf build/

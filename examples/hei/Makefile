ECLISP := ../../eclisp
QUIET := @
CC = gcc
CFLAGS := -O2 -g3 -Wall -Wextra
LDFLAGS := -lm
.SUFFIXES: .eclisp .eclisph

SOURCES = \
	main.eclisp

UC = $(shell echo '$1' | tr '[:lower:]' '[:upper:]')

X_FILES = $(addprefix build/,$(patsubst %.eclisp,%.X,$(SOURCES)))

.INTERMEDIATE: $(X_FILES)

.eclisph.h:
	$(QUIET)$(ECLISP) $< > $@

.eclisp.c:
	$(QUIET)$(ECLISP) $< > $@

all: hei

hei: build/main.o
	gcc $(CLAGS) $^ $(LDFLAGS) -o $@

build/main.c: \
	build/main.h 


%.c: %.X
	sed -e 's/#include/%include/;s/#define/%define/' $< > $@
	cpp -Ibuild -C $@ -o $@.i
	sed -i '1,/# [0-9]\{1,\} "<command-line>" 2/d' $@.i
	sed -i -e 's/^# [0-9]\{1,\}.*//' $@.i
	mv $@.i $@
	sed -i 's/%include/#include/;s/%define/#define/' $@
	clang-format -i $@
	awk 'NF {p=1} p' $@ > $@.i && mv $@.i $@

%.h: %.X
	sed -e 's/#include/%include/;s/#define/%define/' $< > $@
	cpp -Ibuild -C -DPROCESS_HEADER $@ -o $@.ih
	sed -i '3,/# 1 "<command-line>" 2/d' $@.ih
	sed -i -e 's/^# [0-9]\{1,\}.*//' $@.ih
	mv $@.ih $@
	sed -i '1i#ifndef $(call UC,$(basename $(notdir $@)))_H\n' $@
	sed -i '2i#define $(call UC,$(basename $(notdir $@)))_H\n' $@
	echo '#endif' >> $@
	sed -i 's/%include/#include/;s/%define/#define/' $@
	clang-format -i $@

make-build-dir:
	mkdir -p build

build/%.X: %.eclisp | make-build-dir
	$(QUIET)$(ECLISP) $^ > $@

build/%.h: %.eclisph | make-build-dir
	$(QUIET)$(ECLISP) $^ > $@

clean:
	rm -rf build/

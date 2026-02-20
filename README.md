# Eclisp

## Rationale

C is the best language out there.  C has the best compilers.
C is only lacking a good preprocessor.

Eclisp is a transpiler from C as s-expressions to regular C, while taking some
idea from common-lisp such as the possiblity to attach documentation to
symbols.  The advantages of s-expressions is among others, the fact that code
can be easily crafted at compile time through macros acting directly on the
AST, since the code is the AST and vice-versa.  On the other hand, Prefix
notation allows to consider any binary operators as a n-ary operator.

The syntax is still highly experimental as I try to find what works the best
while allowing a 1-1 mapping between C programs and Eclisp programs.  The fact
that invalid programs C can be created from Eclisp is intentional. Eclisp only
translate mindlessly "C as s-exp" to traditional C.

Currently, the code is pretty much hot garbage and I know it.  I'll keep it
this way until the syntax has stabilized, at this point I'll do a complete
rewrite, add macros (and gensym).

## How to install

`eclisp` depends on `sbcl` as well as the `asdf` and `ppcre` libraries. If you
wish to run the testsuite as well, you will also need the `fiveam` library.

It should work with other Common Lisp implementation but you are on you own.

On a typical Debian install, install the dependencies:
```sh
$ sudo apt install sbcl cl-asdf cl-ppcre
````

You should then be able to build eclisp.
```sh
$ make
```

## How to run the tests

Make sure you have the `fiveam` library installed.
```sh
$ sudo apt install cl-fiveam
```

You can run the testsuite using make.
```sh
$ make check
```

## How to use

The eclisp executable reads its input from stdin and outputs its result to stdout.

You can also pass the input file name as an argument.
```sh
$ eclisp file.eclisp
```

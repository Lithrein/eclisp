all:
	sbcl --eval "(asdf:load-asd \"$$PWD/eclisp.asd\")" \
	     --eval '(ql:quickload :eclisp :verbose t)' \
	     --eval '(asdf:make :eclisp)' \
	     --eval '(quit)'

check:
	sbcl --eval "(asdf:load-asd \"$$PWD/eclisp.asd\")" \
	     --eval '(ql:quickload :eclisp :verbose t)' \
	     --eval '(asdf:test-system :eclisp)' \
	     --eval '(quit)'

doc: doc/eclisp.info
doc/eclisp.info: doc/eclisp.texi
	makeinfo $^ -o $@

html: doc/eclisp.html
doc/eclisp.html: doc/eclisp.texi
	makeinfo $^ -o $@ --html --no-split --css-ref=https://www.gnu.org/software/gnulib/manual.css

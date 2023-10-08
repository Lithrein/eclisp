all:
	sbcl --eval "(asdf:load-asd \"$$PWD/eclisp.asd\")" \
	     --eval '(ql:quickload :eclisp)' \
	     --eval '(asdf:make :eclisp)' \
	     --eval '(quit)'

check:
	sbcl --eval "(asdf:load-asd \"$$PWD/eclisp.asd\")" \
	     --eval '(ql:quickload :eclisp)' \
	     --eval '(asdf:test-system :eclisp)' \
	     --eval '(quit)'

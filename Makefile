all:
	sbcl --eval "(asdf:load-asd \"$$PWD/clisp.asd\")" \
	     --eval '(ql:quickload :clisp)' \
	     --eval '(asdf:make :clisp)' \
	     --eval '(quit)'

check:
	sbcl --eval "(asdf:load-asd \"$$PWD/clisp.asd\")" \
	     --eval '(ql:quickload :clisp)' \
	     --eval '(asdf:test-system :clisp)' \
	     --eval '(quit)'

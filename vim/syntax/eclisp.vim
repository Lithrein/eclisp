if exists("b:current_syntax")
    finish
endif

let b:current_syntax = "eclisp"


syntax match eclispNum "\v<[-+]?[0-9]+\.?[0-9]*>"

syntax match   eclispTyp "\v\-\>"
syntax keyword eclispTyp array
syntax keyword eclispTyp char
syntax keyword eclispTyp const
syntax keyword eclispTyp double
syntax keyword eclispTyp float
syntax keyword eclispTyp int
syntax keyword eclispTyp long
syntax keyword eclispTyp ptr
syntax keyword eclispTyp register
syntax keyword eclispTyp signed
syntax keyword eclispTyp static
syntax keyword eclispTyp struct
syntax keyword eclispTyp typedef
syntax keyword eclispTyp union
syntax keyword eclispTyp unisgned
syntax keyword eclispTyp void
syntax keyword eclispTyp volatile

syntax keyword eclispKwd auto
syntax keyword eclispKwd addr
syntax keyword eclispKwd break
syntax keyword eclispKwd cast
syntax keyword eclispKwd continue
syntax keyword eclispKwd default
syntax keyword eclispKwd deref
syntax keyword eclispKwd do
syntax keyword eclispKwd else
syntax keyword eclispKwd enum
syntax keyword eclispKwd extern
syntax keyword eclispKwd for
syntax keyword eclispKwd goto
syntax keyword eclispKwd if
syntax keyword eclispKwd prog
syntax keyword eclispKwd prog*
syntax keyword eclispKwd return
syntax keyword eclispKwd sizeof
syntax keyword eclispKwd switch
syntax keyword eclispKwd short
syntax keyword eclispKwd while

syntax match   eclispOpe "\v\."
syntax match   eclispOpe "\v\+"
syntax match   eclispOpe "\v\*"
syntax match   eclispOpe "\v\/"
syntax match   eclispOpe "\v\-"
syntax match   eclispOpe "\v\&"
syntax match   eclispOpe "\v\="
syntax match   eclispOpe "\v\|"
syntax match   eclispOpe "\v\^"
syntax match   eclispOpe "\v\%"
syntax match   eclispOpe "\v\<"
syntax match   eclispOpe "\v\>"
syntax match   eclispOpe "\v\<\="
syntax match   eclispOpe "\v\>\="

syntax match eclispLbl "\v[( ]\zs[\.:][a-zA-Z_]+"

syntax keyword eclispKwd aref
syntax keyword eclispKwd def
syntax keyword eclispKwd macro
syntax keyword eclispKwd macrofn
syntax keyword eclispKwd macrolet

syntax match eclispPrP "\v\%:include"
syntax match eclispPrP "\v\%:define"
syntax match eclispPrP "\v\%:if"
syntax match eclispPrP "\v\#endif"
syntax match eclispKwd "\v\%funcall"
syntax match eclispKwd "\v\%type"
syntax match eclispKwd "\v\&body"

syntax match eclispComment "\v;.*$"

syntax region eclispString start=/"/ skip=/\\\\\|\\"/ end=/"/
syntax match eclispString "<[^ ]*>"

highlight default link eclispString  String
highlight default link eclispComment Comment
highlight default link eclispFn      Function
highlight default link eclispKwd     Keyword
highlight default link eclispLbl     Label
highlight default link eclispTyp     Type
highlight default link eclispNum     Number
highlight default link eclispOpe     Operator
highlight default link eclispPrP     PreProc

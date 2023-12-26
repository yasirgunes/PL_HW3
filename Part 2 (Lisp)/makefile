LISP = clisp

FILE = gpp_lexer

INPUTFILE = input.txt
OUTPUTFILE = output.txt

flex: ${FILE}.lisp
	@clear
	@${LISP} ${FILE}.lisp


input:
	@${LISP} ${FILE}.lisp < ${INPUTFILE}

output:
	@${LISP} ${FILE}.lisp < ${INPUTFILE} > ${OUTPUTFILE}

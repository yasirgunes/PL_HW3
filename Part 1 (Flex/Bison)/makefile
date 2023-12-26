all:
	@rm -f lex.yy.c gpp_interpreter.tab.c gpp_interpreter.tab.h
	@flex gpp_interpreter.l
	@bison -d -t gpp_interpreter.y 2>/dev/null
	@gcc -w -o gpp_interpreter gpp_interpreter.tab.c lex.yy.c -lfl -lm
	@./gpp_interpreter
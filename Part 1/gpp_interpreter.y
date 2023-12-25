/* {% 
 //definitions
%} */

// rules
%union{
char string [20];
char symbol;
float valuef;
}

/* operators */
%token <symbol> OP_OP
%token <symbol> OP_CP 
%token <symbol> OP_PLUS
%token <symbol> OP_MINUS
%token <symbol> OP_MULT
%token <symbol> OP_DIV

/* keywords */
%token <string> KW_EXIT
%token <string> KW_DEF

/* identifier */
%token <string> IDENTIFIER

/* value */
%token <valuef> VALUEF

%type <valuef> EXP


// rules
%% 

START:
    EXP
    | FUNCTION
    | OP_OP KW_EXIT OP_CP
    ;   
EXP: /* An expression always returns a fraction */
      OP_OP OP_PLUS EXP EXP OP_CP { $$ = $3 + $4; printf("%f\n", $$); }
    | OP_OP OP_MINUS EXP EXP OP_CP 
    | OP_OP OP_MULT EXP EXP OP_CP 
    | OP_OP OP_DIV EXP EXP OP_CP 
    | OP_OP IDENTIFIER EXP
    | OP_OP IDENTIFIER EXP EXP 
    | OP_OP IDENTIFIER EXP EXP EXP
    | IDENTIFIER
    | VALUEF
    ;

/* 
 Parameter passing by value – Function definition returns a function value – Function application will return the value of the 
 expression evaluated
*/
FUNCTION:
    OP_OP KW_DEF IDENTIFIER EXP OP_CP
    | OP_OP KW_DEF IDENTIFIER IDENTIFIER EXP OP_CP
    | OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP
    ;


%%

int main() {
    yyparse();
    return 0;
}

int yyerror(char *s) {
    fprintf(stderr, "error: %s\n", s);
    return 0;
}
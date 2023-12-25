%{ 
 //definitions
 #include <stdio.h>
 #include <string.h>
 char* add_fractions(char* frac1, char* frac2);
%}


// rules
%union{
char string [20];
char symbol;
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
%token <string> VALUEF

%type <string> EXP


// rules
%% 

START:
    EXP START
    | FUNCTION
    | OP_OP KW_EXIT OP_CP
    |
    ;   
EXP: /* An expression always returns a fraction */
      OP_OP OP_PLUS EXP EXP OP_CP   {
         char* result = add_fractions($3, $4);
         strcpy($$, result);
         printf("%s\n", $$); 
         }                          
    | OP_OP OP_MINUS EXP EXP OP_CP  
    | OP_OP OP_MULT EXP EXP OP_CP   
    | OP_OP OP_DIV EXP EXP OP_CP    
    | OP_OP IDENTIFIER EXP          
    | OP_OP IDENTIFIER EXP EXP      
    | OP_OP IDENTIFIER EXP EXP EXP  
    | IDENTIFIER                    
    | VALUEF                        
    |
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

char* add_fractions(char* frac1, char* frac2) {
    // Extract numerators and denominators from frac1 and frac2
    int num1, denom1, num2, denom2;
    sscanf(frac1, "%db%d", &num1, &denom1);
    sscanf(frac2, "%db%d", &num2, &denom2);

    // Calculate the sum as fractions (assuming same denominator for simplicity)
    int common_denom = denom1 * denom2;
    int result_num = (num1 * denom2) + (num2 * denom1);

    // Allocate memory for the result
    char* result = (char*)malloc(20 * sizeof(char));
    if (result != NULL) {
        sprintf(result, "%db%d", result_num, common_denom);
    }
    return result; // Return the dynamically allocated result
}
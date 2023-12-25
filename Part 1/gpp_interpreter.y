%{ 
 //definitions
 #include <stdio.h>
 #include <string.h>
 char* add(char* frac1, char* frac2);
 char* subtract(char* frac1, char* frac2);
 char* multiply(char* frac1, char* frac2);
 char* divide(char* frac1, char* frac2);
 int gcd(int a, int b);
 void simplify_fraction(int *numerator, int *denominator);
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
    | OP_OP KW_EXIT OP_CP         { printf("Exiting...\n"); exit(0); }
    |
    ;   
EXP: /* An expression always returns a fraction */
      OP_OP OP_PLUS EXP EXP OP_CP   {
         char* result = add($3, $4);
         strcpy($$, result);
         printf("%s\n", $$); 
         }                          
    | OP_OP OP_MINUS EXP EXP OP_CP  {
        char* result = subtract($3, $4);
        strcpy($$, result);
        printf("%s\n", $$); 
    }
    | OP_OP OP_MULT EXP EXP OP_CP   {
        char* result = multiply($3, $4);
        strcpy($$, result);
        printf("%s\n", $$); 
    }
    | OP_OP OP_DIV EXP EXP OP_CP    {
        char* result = divide($3, $4);
        strcpy($$, result);
        printf("%s\n", $$); 
    }
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

int gcd(int a, int b) {
    while (b != 0) {
        int t = b;
        b = a % b;
        a = t;
    }
    return a;
}

void simplify_fraction(int *numerator, int *denominator) {
    int common_divisor = gcd(*numerator, *denominator);
    *numerator /= common_divisor;
    *denominator /= common_divisor;
}


char* add(char* frac1, char* frac2) {
    // Extract numerators and denominators from frac1 and frac2
    int num1, denom1, num2, denom2;
    sscanf(frac1, "%db%d", &num1, &denom1);
    sscanf(frac2, "%db%d", &num2, &denom2);

    // Calculate the sum as fractions (assuming same denominator for simplicity)
    int common_denom = denom1 * denom2;
    int result_num = (num1 * denom2) + (num2 * denom1);

    // simplify the fraction
    simplify_fraction(&result_num, &common_denom);

    // Allocate memory for the result
    char* result = (char*)malloc(20 * sizeof(char));
    if (result != NULL) {
        sprintf(result, "%db%d", result_num, common_denom);
    }
    return result; // Return the dynamically allocated result
}

char* subtract(char* frac1, char* frac2) {
    // Extract numerators and denominators from frac1 and frac2
    int num1, denom1, num2, denom2;
    sscanf(frac1, "%db%d", &num1, &denom1);
    sscanf(frac2, "%db%d", &num2, &denom2);

    // Calculate the difference as fractions (assuming same denominator for simplicity)
    // Here, simply subtract the second numerator from the first after adjusting for common denominator
    int common_denom = denom1 * denom2;
    int result_num = (num1 * denom2) - (num2 * denom1);

    // simplify the fraction
    simplify_fraction(&result_num, &common_denom);

    // Allocate memory for the result
    char* result = (char*)malloc(20 * sizeof(char));
    if (result != NULL) {
        sprintf(result, "%db%d", result_num, common_denom);
    }
    return result; // Return the dynamically allocated result
}

char* multiply(char* frac1, char* frac2) {
    // Extract numerators and denominators from frac1 and frac2
    int num1, denom1, num2, denom2;
    sscanf(frac1, "%db%d", &num1, &denom1);
    sscanf(frac2, "%db%d", &num2, &denom2);

    // Calculate the product as fractions
    int result_num = num1 * num2;
    int result_denom = denom1 * denom2;

    // simplify the fraction
    simplify_fraction(&result_num, &result_denom);

    // Allocate memory for the result
    char* result = (char*)malloc(20 * sizeof(char));
    if (result != NULL) {
        sprintf(result, "%db%d", result_num, result_denom);
    }
    return result; // Return the dynamically allocated result
}

char* divide(char* frac1, char* frac2) {
    // Extract numerators and denominators from frac1 and frac2
    int num1, denom1, num2, denom2;
    sscanf(frac1, "%db%d", &num1, &denom1);
    sscanf(frac2, "%db%d", &num2, &denom2);

    // Calculate the quotient as fractions
    int result_num = num1 * denom2;
    int result_denom = denom1 * num2;

    // simplify the fraction
    simplify_fraction(&result_num, &result_denom);

    // Allocate memory for the result
    char* result = (char*)malloc(20 * sizeof(char));
    if (result != NULL) {
        sprintf(result, "%db%d", result_num, result_denom);
    }
    return result; // Return the dynamically allocated result
}

%{ 
 //definitions
 #include <stdio.h>
 #include <string.h>
 #include <stdlib.h>

 char* add(char* frac1, char* frac2);
 char* subtract(char* frac1, char* frac2);
 char* multiply(char* frac1, char* frac2);
 char* divide(char* frac1, char* frac2);
 
 int gcd(int a, int b);
 void simplify_fraction(int *numerator, int *denominator);

typedef struct variable {
    char var_name[20];
    char var_value[20];
} variable;

// 100 variables
 variable variables[100];
 int variable_count = 0;

// function prototype
typedef struct function {
    char func_name[20];
    char exp_type[20];
} function;

// 100 functions
 function functions[100];
 int function_count = 0;

 int inFunction = 0; // flag to check if we are in a function => for scoping

// symbol table functions
void add_variable(char* var_name, char* var_value);
char* get_variable_value(char* var_name);
void set_variable_value(char* var_name, char* var_value);
%}

// rules
%union{
char string [20]; // valuef
char name [20]; // identifier
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
%token <string> KW_SET

/* identifier */
%token <name> IDENTIFIER

/* value */
%token <string> VALUEF

%type <string> EXP


// rules
%% 

START:
    EXP START
    | FUNCTION START
    | OP_OP KW_EXIT OP_CP         { printf("Exiting...\n"); exit(0); }
    | SET START
    |
    ;   
EXP: /* An expression always returns a fraction */
      OP_OP OP_PLUS EXP EXP OP_CP   {
        if(inFunction == 0) {
            char exp3_value[20];
            char exp4_value[20];
            strcpy(exp3_value, $3);
            strcpy(exp4_value, $4);

            // if exp is an identifier like x. handle it.
            for(int i = 0; i < variable_count; i++) {
                if(strcmp(variables[i].var_name, $3) == 0) {
                    strcpy(exp3_value, variables[i].var_value);
                }
            }
            for(int i = 0; i < variable_count; i++) {
                if(strcmp(variables[i].var_name, $4) == 0) {
                    strcpy(exp4_value, variables[i].var_value);
                }
            }
            char* result = add(exp3_value, exp4_value);
            strcpy($$, result);
            printf("%s\n", $$); 
        }
        else {
            strcpy(functions[function_count].exp_type, "add");
        }
    }                          
    | OP_OP OP_MINUS EXP EXP OP_CP  {
        if(inFunction == 0) {
            char exp3_value[20];
            char exp4_value[20];
            strcpy(exp3_value, $3);
            strcpy(exp4_value, $4);

            // if exp is an identifier like x. handle it.
            for(int i = 0; i < variable_count; i++) {
                if(strcmp(variables[i].var_name, $3) == 0) {
                    strcpy(exp3_value, variables[i].var_value);
                }
            }
            for(int i = 0; i < variable_count; i++) {
                if(strcmp(variables[i].var_name, $4) == 0) {
                    strcpy(exp4_value, variables[i].var_value);
                }
            }
            char* result = subtract(exp3_value, exp4_value);
            strcpy($$, result);
            printf("%s\n", $$); 
        }
        else {
            strcpy(functions[function_count].exp_type, "subtract");
        }
    }
    | OP_OP OP_MULT EXP EXP OP_CP   {
        if(inFunction == 0) {
            char exp3_value[20];
            char exp4_value[20];
            strcpy(exp3_value, $3);
            strcpy(exp4_value, $4);

            // if exp is an identifier like x. handle it.
            for(int i = 0; i < variable_count; i++) {
                if(strcmp(variables[i].var_name, $3) == 0) {
                    strcpy(exp3_value, variables[i].var_value);
                }
            }
            for(int i = 0; i < variable_count; i++) {
                if(strcmp(variables[i].var_name, $4) == 0) {
                    strcpy(exp4_value, variables[i].var_value);
                }
            }
            char* result = multiply(exp3_value, exp4_value);
            strcpy($$, result);
            printf("%s\n", $$); 
        }
        else {
            strcpy(functions[function_count].exp_type, "multiply");
        }
    }
    | OP_OP OP_DIV EXP EXP OP_CP    {
        if(inFunction == 0) {
            char exp3_value[20];
            char exp4_value[20];
            strcpy(exp3_value, $3);
            strcpy(exp4_value, $4);

            // if exp is an identifier like x. handle it.
            for(int i = 0; i < variable_count; i++) {
                if(strcmp(variables[i].var_name, $3) == 0) {
                    strcpy(exp3_value, variables[i].var_value);
                }
            }
            for(int i = 0; i < variable_count; i++) {
                if(strcmp(variables[i].var_name, $4) == 0) {
                    strcpy(exp4_value, variables[i].var_value);
                }
            }
            char* result = divide(exp3_value, exp4_value);
            strcpy($$, result);
            printf("%s\n", $$); 
        }
        else {
            strcpy(functions[function_count].exp_type, "divide");
        }
    }
    | OP_OP IDENTIFIER EXP          
    | OP_OP IDENTIFIER EXP EXP       
    | OP_OP IDENTIFIER EXP EXP EXP 
    //function part
    | OP_OP IDENTIFIER EXP EXP OP_CP {
        char exp3_value[20];
        char exp4_value[20];
        strcpy(exp3_value, $3);
        strcpy(exp4_value, $4);

        // if exp is an identifier like x. handle it.
        for(int i = 0; i < variable_count; i++) {
            if(strcmp(variables[i].var_name, $3) == 0) {
                strcpy(exp3_value, variables[i].var_value);
            }
        }
        for(int i = 0; i < variable_count; i++) {
            if(strcmp(variables[i].var_name, $4) == 0) {
                strcpy(exp4_value, variables[i].var_value);
            }
        }

        // calculate the result based on the function's exp_type
        char exp_type[20];
        for(int i = 0; i < function_count; i++) {
            if(strcmp(functions[i].func_name, $2) == 0) {
                strcpy(exp_type, functions[i].exp_type);
            }
        }
        if(strcmp(exp_type, "add") == 0) {
            char* result = add(exp3_value, exp4_value);
            strcpy($$, result);
            printf("%s\n", $$); 
        }
        else if(strcmp(exp_type, "subtract") == 0) {
            char* result = subtract(exp3_value, exp4_value);
            strcpy($$, result);
            printf("%s\n", $$); 
        }
        else if(strcmp(exp_type, "multiply") == 0) {
            char* result = multiply(exp3_value, exp4_value);
            strcpy($$, result);
            printf("%s\n", $$); 
        }
        else if(strcmp(exp_type, "divide") == 0) {
            char* result = divide(exp3_value, exp4_value);
            strcpy($$, result);
            printf("%s\n", $$); 
        }
    }
    | IDENTIFIER {
        // if the variable exists do nothing else add it to the symbol table
        int exists = 0; 
        for(int i = 0; i < variable_count; i++) {
            if(strcmp(variables[i].var_name, $1) == 0) {
                exists = 1;
                break;
            }
        }
        if(exists == 0) {
            add_variable($1, "0b1");
        }
    }                    
    | VALUEF  
    |
    ;

/* 
 Parameter passing by value – Function definition returns a function value – Function application will return the value of the 
 expression evaluated
*/
FUNCTION:
    OP_OP KW_DEF IDENTIFIER EXP OP_CP {
        printf("Function %s defined\n", $3);
    }
    | OP_OP KW_DEF IDENTIFIER IDENTIFIER EXP OP_CP
    {
        printf("Function %s defined\n", $3);
    }
    | OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP 
    {
        inFunction = 0;
        strcpy(functions[function_count].func_name, $3);
        function_count++;
        printf("Function %s defined\n", $3);
    }
    ;

SET:
    OP_OP KW_SET IDENTIFIER EXP OP_CP {
        set_variable_value($3, $4);
        printf("Variable %s set to %s\n", $3, $4);
    }
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

// symbol table functions
void add_variable(char* var_name, char* var_value) {
    strcpy(variables[variable_count].var_name, var_name);
    strcpy(variables[variable_count].var_value, var_value);
    variable_count++;
}

char* get_variable_value(char* var_name) {
    for (int i = 0; i < variable_count; i++) {
        if (strcmp(variables[i].var_name, var_name) == 0) {
            return variables[i].var_value;
        }
    }
    return "0b1";
}

void set_variable_value(char* var_name, char* var_value) {
    for (int i = 0; i < variable_count; i++) {
        if (strcmp(variables[i].var_name, var_name) == 0) {
            strcpy(variables[i].var_value, var_value);
            return;
        }
    }
    add_variable(var_name, var_value);
}

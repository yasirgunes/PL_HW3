%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gpp_interpreter.tab.h"
extern int inFunction;
%}


%%
        /* keywords */
and           {     }
or            {       }
not           {      }
equal         {    }
less          {     }
nil           {      }   
list          {     }
append        {   }
concat        {   }
set           {printf("KW_SET\n"); return KW_SET; }
def           {inFunction = 1; return KW_DEF; }
for           {     }
if            {       }
exit          {  
                return KW_EXIT; }
load          { printf("KW_LOAD\n");    }
display       { printf("KW_DISPLAY\n"); }
true          { printf("KW_TRUE\n");    }
false         { printf("KW_FALSE\n");   }
        /* operators */
[+]           { return(OP_PLUS);}
[-]           { return(OP_MINUS);}
[/]           { return(OP_DIV);}
[*]           { return(OP_MULT);}
[(]           { return(OP_OP);}
[)]           { return(OP_CP);}
[,]           { }

        /* literal comment identifier */
[0-9]+b[0-9]+           {strcpy(yylval.string, yytext); return VALUEF;}

[;]{2}.*                {}

[a-zA-Z][a-zA-Z0-9]*    {  strcpy(yylval.name, yytext);  return IDENTIFIER;}

[ \t\n]+                { /* ignore tab, whitespace and newline */ ;}

[0-9]+[a-zA-Z]+         { printf("SYNTAX_ERROR an identifier should start with a letter.\n");}

.                       { if((int)yytext[0] != 13) { /* don't print carriage return */
                            printf("SYNTAX_ERROR %s cannot be tokenized\n", yytext);
                          }
                        }

%%
/* */
int yywrap() {}

// int main(int argc, char **argv) {
//     if (argc == 1) {
//         printf("Type exit to quit,\n");
//         printf("Enter the string to be parsed: ");
//         yylex();
//     }
//     else if (argc == 2) {
//         yyin = fopen(argv[1], "r");
//         yylex();
//         fclose(yyin);
//     }
//     else {
//         printf("Too many arguments!\n");
//         return 0;
//     }
//     return 0;
// }

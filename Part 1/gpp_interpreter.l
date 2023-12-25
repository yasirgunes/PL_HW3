%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gpp_interpreter.tab.h"
%}


%%
        /* keywords */
and           { printf("KW_AND\n");     }
or            { printf("KW_OR\n");      }
not           { printf("KW_NOT\n");     }
equal         { printf("KW_EQUAL\n");   }
less          { printf("KW_LESS\n");    }
nil           { printf("KW_NIL\n");     }   
list          { printf("KW_LIST\n");    }
append        { printf("KW_APPEND\n");  }
concat        { printf("KW_CONCAT\n");  }
set           { printf("KW_SET\n");     }
def           { printf("KW_DEF\n");     }
for           { printf("KW_FOR\n");     }
if            { printf("KW_IF\n");      }
exit          { printf("KW_EXIT\n"); 
                printf("Exiting...\n");
                return 0;               }
load          { printf("KW_LOAD\n");    }
display       { printf("KW_DISPLAY\n"); }
true          { printf("KW_TRUE\n");    }
false         { printf("KW_FALSE\n");   }
        /* operators */
[+]           { printf("OP_PLUS\n");    return(OP_PLUS);}
[-]           { printf("OP_MINUS\n");   return(OP_MINUS);}
[/]           { printf("OP_DIV\n");     return(OP_DIV);}
[*]           { printf("OP_MULT\n");    return(OP_MULT);}
[(]           { printf("OP_OP\n");      return(OP_OP);}
[)]           { printf("OP_CP\n");      return(OP_CP);}
[,]           { printf("OP_COMMA\n");   }

        /* literal comment identifier */
[0-9]+b[0-9]+           { printf("VALUEF\n"); }

[;]{2}.*                { printf("COMMENT\n");    }

[a-zA-Z][a-zA-Z0-9]*    { printf("IDENTIFIER\n"); strcpy(yylval.string, yytext); return IDENTIFIER;}

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
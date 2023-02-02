#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#define LINE_SIZE 256

//token enum
enum Token{
    DUMMY,FOR,LPARANTH,RPARANTH,LBRACKET,
    RBRACKET,LBRACES,RBRACES,IN,
    COLUMN,EQUALS,ASTERIKS,PLUS,
    MINUS,COMMA,MATRIX,SCALAR,
    VECTOR,SQRT,TRANSPOSE,CHOOSE,
    PRINT,PRINTSEP,VARIABLE,NUMBER,
    ERROR,INTEGER,FLOAT
};

int variableIndex = 0;
int *tokens;
int inFor = 0;

struct expression *expr(int,int);
struct expression *term(int,int);
struct expression *moreterms(int,int);
struct expression *factor(int,int);
struct expression *morefactors(int,int);

struct variable **variables;
char **words;
//struct of variable
struct variable{
    char* name;
    int type;
    int value;
    int dimension1;
    int dimension2;
};
//struct of expression
struct expression{
    int state;
    int type;
    int value;
    int dimension1;
    int dimension2;
    char *string;
};

// Checks whether a token is number
int isNum(char* token){
    int dotCount = 0;
    while( *token != '\0'){
        if( ( isdigit(*token) == 0 && *token != '.' ) || dotCount > 1){
            return 0;
        }
        if(*token == '.'){
            dotCount++;
        }
        token++;
    }
    return 1;
}
// Checks whether a token is variable
int isVariable(char* token){
    if( isdigit(*token++) == 1 ){
        return (0);
    }
    while( *token != '\0'){
        if( isalnum(*token) == 0 && isdigit(*token) == 0 && *token != '_'){
            return (0);
        }
        token++;
    }
    return 1;
}
//check a tokens type
int tokenFind(char* token){
    if( strcmp(token,"for") == 0 ){
        return(FOR);
    }
    else if(strcmp(token,"(") == 0 ){
        return(LPARANTH);
    }
    else if(strcmp(token,")") == 0 ){
        return(RPARANTH);
    }
    else if(strcmp(token,"[") == 0 ){
        return(LBRACKET);
    }
    else if(strcmp(token,"]") == 0 ){
        return(RBRACKET);
    }
    else if(strcmp(token,"{") == 0 ){
        return(LBRACES);
    }
    else if(strcmp(token,"}") == 0 ){
        return(RBRACES);
    }
    else if(strcmp(token,"in") == 0 ){
        return(IN);
    }
    else if(strcmp(token,":") == 0 ){
        return(COLUMN);
    }
    else if(strcmp(token,"=") == 0 ){
        return(EQUALS);
    }
    else if(strcmp(token,"*") == 0 ){
        return(ASTERIKS);
    }
    else if(strcmp(token,"+") == 0 ){
        return(PLUS);
    }
    else if(strcmp(token,"-") == 0 ){
        return(MINUS);
    }
    else if(strcmp(token,",") == 0 ){
        return(COMMA);
    }
    else if(strcmp(token,"matrix") == 0 ){
        return(MATRIX);
    }
    else if(strcmp(token,"scalar") == 0 ){
        return(SCALAR);
    }
    else if(strcmp(token,"vector") == 0 ){
        return(VECTOR);
    }
    else if(strcmp(token,"sqrt") == 0 ){
        return(SQRT);
    }
    else if(strcmp(token,"tr") == 0 ){
        return(TRANSPOSE);
    }
    else if(strcmp(token,"choose") == 0 ){
        return(CHOOSE);
    }
    else if(strcmp(token,"print") == 0 ){
        return(PRINT);
    }
    else if(strcmp(token,"printsep") == 0 ){
        return(PRINTSEP);
    }
    else if(isNum(token) == 1){
        return(NUMBER);
    }
    else if(isVariable(token) == 1){
        return(VARIABLE);
    }
    else{
        return(ERROR);
    }

}
// scans line
int* scanLine(){
    int* tokenList;
    int* tokenListStart;
    tokenList = malloc(256*sizeof(int));
    tokenListStart = tokenList;

    int wordIndex = 0;
    while( words[wordIndex] != NULL){
        *tokenList++ = tokenFind(words[wordIndex]);
        wordIndex++;
    }
    return tokenListStart;
}
// spaces between special characters and returns spaced string
char* spacer(char* defaultLine){
    if(defaultLine[0] == '#' || defaultLine[0] == '\n'){
        return "";
    }
    char *newline;
    newline = malloc(256 * sizeof(char));
    int currentInNew;
    int currentInOld;
    for(currentInOld = 0,currentInNew = 0; defaultLine[currentInOld] != '\0' && defaultLine[currentInOld] != '\n'; currentInNew++,currentInOld++){
        if(isalnum(defaultLine[currentInOld]) || defaultLine[currentInOld] == '.' || defaultLine[currentInOld] == ' '){
            newline[currentInNew] = defaultLine[currentInOld];
        }
        else{
            newline[currentInNew++] = ' ';
            newline[currentInNew++] = defaultLine[currentInOld];
            newline[currentInNew] = ' ';
        }
    }
    return newline;
}
//reads file
FILE* fileRead(int argc,char *argv[]){
    if (argc != 2) {
        //printf("Give filename as command line argument\n") ;
        return(NULL);
    }

    FILE *fp;
    fp = fopen(argv[1], "r");
    if(fp == NULL) {
        //printf("Cannot open %s\n",argv[1]);
        return(NULL);
    }
    return fp;
}
// writes file
FILE* fileWrite(){
    FILE *fp;
    fp = fopen("file.c", "w");
    if(fp == NULL) {
        //printf("Cannot open\n");
        return(NULL);
    }
    return fp;
}
// state enum
enum state{
    FAILURE,SUCCESS
};
// checks whether a word is int
int isInt(char *word){
    while( *word != '\0'){
        if( *word == '.'){
            return FAILURE;
        }
        word++;
    }
    return SUCCESS;
}
// finds whether a variable is declared before and returns it's struct if it exists
struct variable *findVariable(char *varName){
    for(int i = 0; variables[i] != NULL; i++){
        if(strcmp(variables[i] -> name, varName) == 0){
            return variables[i];
        }
    }
    return NULL;
}
// checks the dimension of expression
char *expressionDimensionCheck(struct expression *expression){
    char *temp = malloc(10 * sizeof(char));
    if(expression -> type == MATRIX || expression -> type == VECTOR){
        strcat(temp,"[0]");
    }
    return temp;
}

// compares term expressions
void termExpressionComp( struct expression *left, struct expression *right,struct expression *current,int operator){
    if( left -> state == FAILURE || right -> state == FAILURE){
        current -> state = FAILURE;
        
        return;
    }
    if(left -> dimension1 == right -> dimension1 && left -> dimension2 == right -> dimension2){
        current -> dimension1 = left -> dimension1;
        current -> dimension2 = left -> dimension2;
        current -> state = SUCCESS;

        char *temp = malloc(256*sizeof(char));
        if(left -> dimension1 == 1 && left -> dimension2 == 1 && right -> dimension1 && right -> dimension2 == 1){
            strcat(temp,left -> string);
            strcat(temp,expressionDimensionCheck(left));
            if(operator == PLUS){
                strcat(temp," + ");
            }else{
                strcat(temp," - ");
            }
            strcat(temp,right -> string);
            strcat(temp,expressionDimensionCheck(right));
            current -> type = SCALAR;
        }
        else{
            if(operator == PLUS){
                sprintf(temp,"matrixPlus( %s, %s, %d, %d)",left -> string,right -> string,left -> dimension1,left -> dimension2);
            }else{
                sprintf(temp,"matrixMinus( %s, %s, %d, %d)",left -> string,right -> string,left -> dimension1,left -> dimension2);
            }
            current -> type = MATRIX;
        }
        current -> string = temp;
        return;
    }
    current -> state = FAILURE;
}
// updates expression
void updateExpression( struct expression *old,struct expression *current){
    if(old -> state == FAILURE){
        current -> state = FAILURE;
    }
    else{
        current -> state = SUCCESS;
        current -> type = old -> type;
        current -> dimension1 = old -> dimension1;
        current -> dimension2 = old -> dimension2;
        current -> string = old -> string;
    }
}
// expression function
struct expression *expr(int leftIndex, int rightIndex){
    // term moreTerms
    struct expression *exprState = malloc( sizeof(struct expression) );
    if(leftIndex > rightIndex){
        exprState -> state = FAILURE;
        return exprState;
    }
    int lparan = 0, rparan = 0,lbracket = 0,rbracket = 0;
    for(int i = leftIndex; i <= rightIndex; i++){
        if(tokens[i] == LPARANTH){
            lparan++;
        }
        else if(tokens[i] == RPARANTH){
            rparan++;
        }
        else if( tokens[i] == LBRACKET){
            lbracket++;
        }
        else if( tokens[i] == RBRACKET){
            rbracket++;
        }
        else if((tokens[i] == PLUS || tokens[i] == MINUS) && lparan == rparan && lbracket == rbracket){
            struct expression *termExpr = term(leftIndex, i-1);
            struct expression *moretermsExpr = moreterms(i+1, rightIndex);
            
            if(tokens[i] == PLUS){
                termExpressionComp(termExpr, moretermsExpr, exprState, PLUS);
            }else{
                termExpressionComp(termExpr, moretermsExpr, exprState, MINUS);
            }
            return exprState;
        }
    }
    struct expression *termExpr = term(leftIndex, rightIndex);
    updateExpression(termExpr, exprState);
    return exprState;
}
// compares factor expressions
void factorExpressionComp( struct expression *left, struct expression *right,struct expression *current){
    if( left -> state == FAILURE || right -> state == FAILURE){
        current -> state = FAILURE;
        return;
    }
    if(left -> dimension2 == right -> dimension1){
        current -> dimension1 = left -> dimension1;
        current -> dimension2 = right -> dimension2;
        current -> state = SUCCESS;

        char *temp = malloc(256 * sizeof(char));
        if(left -> dimension1 != 1 || left -> dimension2 != 1 || right -> dimension1 != 1 || right -> dimension2 != 1){
            char *buffer = malloc(256*sizeof(char));
            sprintf(buffer,"matrixMultiplication((float *) %s, (float *)%s, %d, %d, %d, %d) ",left -> string,right -> string,left -> dimension1,left -> dimension2,right -> dimension1,right -> dimension2);
            strcat(temp,buffer);
            current -> type = MATRIX;
        }else{
            strcat(temp,left->string);
            strcat(temp,expressionDimensionCheck(left));
            strcat(temp," * ");
            strcat(temp,right->string);
            strcat(temp,expressionDimensionCheck(right));
            current -> type = SCALAR;
        }
        current -> string = temp;
        return;
    }
    current -> state = FAILURE;
}
// term function
struct expression *term(int leftIndex, int rightIndex){
    // factor moreFactors
    struct expression *termState = malloc( sizeof(struct expression) );
    if(leftIndex > rightIndex){
        termState->state = FAILURE;
        return termState;
    }
    int lparan = 0, rparan = 0, lbracket = 0, rbracket = 0;
    for(int i = leftIndex; i <= rightIndex; i++){
        if(tokens[i] == LPARANTH){
            lparan++;
        }
        else if(tokens[i] == RPARANTH){
            rparan++;
        }
        else if( tokens[i] == LBRACKET){
            lbracket++;
        }
        else if( tokens[i] == RBRACKET){
            rbracket++;
        }
        if(tokens[i] == ASTERIKS && lparan == rparan && lbracket == rbracket){
            struct expression *factorExpr = factor(leftIndex, i-1);
            struct expression *morefactorsExpr = morefactors(i+1, rightIndex);
            factorExpressionComp(factorExpr, morefactorsExpr, termState);
            return termState;
        }
    }
    struct expression *factorExpr = factor(leftIndex, rightIndex);
    updateExpression(factorExpr, termState);
    return termState;
}
// moreterm function
struct expression *moreterms(int leftIndex, int rightIndex){
    struct expression *termState = malloc( sizeof(struct expression) );
    if(leftIndex > rightIndex){
        termState->state = FAILURE;
        return termState;
    }
    int lparan = 0, rparan = 0, lbracket = 0, rbracket;
    for(int i = leftIndex; i <= rightIndex; i++){
        if(tokens[i] == LPARANTH){
            lparan++;
        }
        else if(tokens[i] == RPARANTH){
            rparan++;
        }
        else if( tokens[i] == LBRACKET){
            lbracket++;
        }
        else if( tokens[i] == RBRACKET){
            rbracket++;
        }
        else if((tokens[i] == PLUS || tokens[i] == MINUS) && lparan == rparan && lbracket == rbracket){
            struct expression *termExpr = term(leftIndex, i-1);
            struct expression *moretermsExpr = moreterms(i+1, rightIndex);
            
            if(tokens[i] == PLUS){
                termExpressionComp(termExpr, moretermsExpr, termState,PLUS);
            }else{
                termExpressionComp(termExpr, moretermsExpr, termState,MINUS);
            }

            return termState;
        }
    }
    struct expression *termExpr = term(leftIndex, rightIndex);
    updateExpression(termExpr, termState);
    return termState;
}
// morefactors function
struct expression *morefactors(int leftIndex, int rightIndex){
    struct expression *factorState = malloc( sizeof(struct expression) );
    if(leftIndex > rightIndex){
        factorState->state = FAILURE;
        return factorState;
    }
    int lparan = 0, rparan = 0,lbracket = 0,rbracket = 0;
    for(int i = leftIndex; i <= rightIndex; i++){
        if(tokens[i] == LPARANTH){
            lparan++;
        }
        else if(tokens[i] == RPARANTH){
            rparan++;
        }
        else if( tokens[i] == LBRACKET){
            lbracket++;
        }
        else if( tokens[i] == RBRACKET){
            rbracket++;
        }
        if(tokens[i] == ASTERIKS && lparan == rparan && lbracket == rbracket){
            struct expression *factorExpr = factor(leftIndex, i-1);
            struct expression *morefactorsExpr = morefactors(i+1, rightIndex);
            factorExpressionComp(factorExpr, morefactorsExpr, factorState);
            return factorState;
        }
    }
    struct expression *factorExpr = factor(leftIndex, rightIndex);
    updateExpression(factorExpr, factorState);
    return factorState;
}
// finds first comma between indexes
int findComma(int leftIndex,int rightIndex){
    int lparan=0,rparan=0,lbracket = 0,rbracket = 0;
    for(int i = leftIndex; i <= rightIndex; i++){
        if(tokens[i] == LPARANTH){
            lparan++;
        }
        else if (tokens[i] == RPARANTH){
            rparan++;
        }
        else if (tokens[i] == LBRACKET){
            lbracket++;
        }
        else if (tokens[i] == RBRACKET){
            rbracket++;
        }
        else if(tokens[i] == COMMA && lbracket == rbracket && lparan == rparan){
            return i;
        }
    }
    return -1;
}
// updates factor expression by variable
void updateFactorExprByVariable(int index,struct expression *current){
    char *varName = words[index];
    struct variable *var = findVariable(varName);
    if(var == NULL){
        current -> state = FAILURE;
        return;
    }
    current -> state = SUCCESS;
    current -> type = SCALAR;
    current -> dimension1 = var -> dimension1;
    current -> dimension2 = var -> dimension2;
    current -> string = strdup(var -> name); 
}
// sqrt expression function
void sqrtExpression(struct expression *old,struct expression *current){
    if(old -> state == FAILURE){
        current -> state = FAILURE;
        return;
    }
    if (old -> dimension1 == 1 && old -> dimension2 == 1){
        current -> state = SUCCESS;
        current -> type = SCALAR;
        current -> dimension1 = 1;
        current -> dimension2 = 1;

        char *temp = malloc(256*sizeof(char));
        strcat(temp,"sqrt(");
        strcat(temp,old -> string);
        strcat(temp,expressionDimensionCheck(old));
        strcat(temp,")");
        current -> string = temp; 
        return;
    }
    current -> state = FAILURE;
}
// transpose expression function
void transposeExpression(struct expression *old,struct expression *current){
    if(old -> state == FAILURE){
        current -> state = FAILURE;
        return;
    }
    else{
        current -> state = SUCCESS;
        current -> type = old -> type;
        current -> dimension1 = old -> dimension2;
        current -> dimension2 = old -> dimension1;
        
        char *temp = malloc(256*sizeof(char));
        char *buffer = malloc(256*sizeof(char));
        if( current -> dimension1 == 1 && current -> dimension2 == 1){
            strcat(temp,old -> string);
            strcat(temp,expressionDimensionCheck(old));
            current -> type = SCALAR;
        }
        else{
            sprintf(buffer,"transpose( (float *) %s, %d, %d )",old -> string,old->dimension1,old->dimension2);
            strcat(temp,buffer);
            current -> type = MATRIX;
        }
        current -> string = temp; 
        
        return;
    }
}
// number expression function
void numberExpression(int index,struct expression *current){
    current -> type = SCALAR;
    current -> state = SUCCESS;
    current -> dimension1 = 1;
    current -> dimension2 = 1;
    
    char *temp = malloc(256*sizeof(char));
    strcat(temp,words[index]);
    current -> string = temp;

}
// compares matrix variables
void variableMatrixComp(int index, struct expression *expr1,struct expression *expr2,struct expression *current){
    if(expr1 -> state == FAILURE || expr2 -> state == FAILURE){
        current -> state = FAILURE;
        return;
    }
    //variable must be matrix
    char *varName = words[index];
    struct variable *var = findVariable(varName);
    if(var == NULL){
        current -> state = FAILURE;
        return;
    }
    if(expr1 -> dimension1 == 1 && expr1 -> dimension2 == 1 && expr2 -> dimension1 == 1 && expr2 -> dimension2 == 1 && var -> type == MATRIX){
        current -> state = SUCCESS;
        current -> type = expr1 -> type;
        current -> dimension1 = 1;
        current -> dimension2 = 1;
        current -> type = SCALAR;
        char *temp = malloc(256*sizeof(char));
        strcat(temp," ");
        strcat(temp, var -> name);
        strcat(temp,"[rev(");
        strcat(temp,expr1 -> string);
        strcat(temp,expressionDimensionCheck(expr1));
        strcat(temp,") - 1][rev(");
        strcat(temp,expr2 -> string);
        strcat(temp,expressionDimensionCheck(expr2));
        strcat(temp,") - 1]");
        current -> string = temp;
        return;
    }
    current -> state = FAILURE;
}
// compares vector variables
void variableVectorComp(int index,struct expression *expr1,struct expression *current){
    if(expr1 -> state == FAILURE){
        current -> state = FAILURE;
        return;
    }
    //variable must be vector or matrix
    char *varName = words[index];
    struct variable *var = findVariable(varName);
    if(var == NULL){
        current -> state = FAILURE;
        return;
    }
    if(expr1 -> dimension1 == 1 && expr1 -> dimension2 == 1 && (var -> type == MATRIX || var -> type == VECTOR) ){
        current -> state = SUCCESS;
        current -> dimension1 = 1;
        current -> dimension2 = var -> dimension2;

        //patlar
        if( var -> dimension2 == 1){
            current -> type = SCALAR;
        }
        else{
            current -> type = MATRIX;
        } 

        char *temp = malloc(256*sizeof(char));
        strcat(temp, var -> name);
        strcat(temp,"[rev(");
        strcat(temp,expr1 -> string);
        strcat(temp,expressionDimensionCheck(expr1));
        strcat(temp,")- 1] ");
        current -> string = temp;
        
        return;
    }
    current -> state = FAILURE;
}
//chooses between expressions
void chooseComp(struct expression *expr1,struct expression *expr2,struct expression *expr3,struct expression *expr4,struct expression *current){
    if(expr1 -> state == FAILURE || expr2-> state == FAILURE || expr3->state == FAILURE || expr4->state == FAILURE){
        current -> state = FAILURE;
        return;
    }
    else if(expr1 -> dimension1 == 1 && expr1 -> dimension2 == 1 
    && expr2 -> dimension1 == 1 && expr2 -> dimension2 == 1
    && expr3 -> dimension1 == 1 && expr3 -> dimension2 == 1
    && expr4 -> dimension1 == 1 && expr4 -> dimension2 == 1){
        current -> state = SUCCESS;
        current -> type = SCALAR;
        current -> dimension1 = 1;
        current -> dimension2 = 1;

        char *temp = malloc(256*sizeof(char));
        strcat(temp,"choose( ");
        strcat(temp, expr1 -> string);
        strcat(temp,expressionDimensionCheck(expr1));
        strcat(temp,", ");
        strcat(temp, expr2 -> string);
        strcat(temp,expressionDimensionCheck(expr2));
        strcat(temp,", ");
        strcat(temp, expr3 -> string);
        strcat(temp,expressionDimensionCheck(expr3));
        strcat(temp,", ");
        strcat(temp, expr4 -> string);
        strcat(temp,expressionDimensionCheck(expr4));
        strcat(temp,")");
        current -> string = temp;

        return;
    }
    current -> state == FAILURE;
}

//factor function
struct expression *factor(int leftIndex, int rightIndex){
    struct expression *factorState = malloc( sizeof(struct expression) );
    if(leftIndex > rightIndex){
        factorState->state = FAILURE;
        return factorState;
    }
    if(tokens[leftIndex] == LPARANTH && tokens[rightIndex] == RPARANTH && rightIndex-leftIndex >= 2){
        struct expression *exprExpr = expr(leftIndex+1, rightIndex-1);
        updateExpression(exprExpr, factorState);
        
        char *temp = malloc(256*sizeof(char));
        strcat(temp,"(");
        strcat(temp,factorState->string);
        strcat(temp,")");
        factorState -> string = temp;

        return factorState;
    }
    else if(tokens[leftIndex] == VARIABLE){
        if(leftIndex == rightIndex){
            updateFactorExprByVariable(leftIndex, factorState);
            return factorState;
        }
        else if( rightIndex - leftIndex >= 3){
            if(tokens[leftIndex+1] == LBRACKET && tokens[rightIndex] == RBRACKET){
                int comma = findComma(leftIndex+2, rightIndex-1);
                if(comma != -1){
                    struct expression *exprExpr1 = expr(leftIndex+2, comma-1);
                    struct expression *exprExpr2 = expr(comma+1, rightIndex-1);
                    variableMatrixComp(leftIndex,exprExpr1,exprExpr2,factorState);
                    return factorState;
                }
                else{
                    struct expression *exprExpr1 = expr(leftIndex+2, rightIndex-1);
                    variableVectorComp(leftIndex,exprExpr1,factorState);
                    return factorState;
                }
            }
        }
    }
    else if(tokens[leftIndex] == SQRT){
        if(rightIndex - leftIndex >= 3){
            if(tokens[leftIndex+1] == LPARANTH && tokens[rightIndex] == RPARANTH){
                struct expression *sqrtExpr = expr(leftIndex+2, rightIndex-1);
                sqrtExpression(sqrtExpr, factorState);
                return factorState;
            }
        }
    }
    else if(tokens[leftIndex] == TRANSPOSE){
        if(rightIndex - leftIndex >= 3){
            if(tokens[leftIndex+1] == LPARANTH && tokens[rightIndex] == RPARANTH){
                struct expression *trExpr = expr(leftIndex +2, rightIndex-1);
                transposeExpression(trExpr, factorState);
                return factorState;
            }
        }
    }
    else if(tokens[leftIndex] == CHOOSE){
        if(rightIndex - leftIndex >= 9){
            if(tokens[leftIndex+1] == LPARANTH && tokens[rightIndex] == RPARANTH){
                int comma1 = findComma(leftIndex+2, rightIndex-1);
                if (comma1 != -1){
                    int comma2 = findComma(comma1+1, rightIndex-1);
                    if(comma2 != -1){
                        int comma3 = findComma(comma2+1, rightIndex-1);
                        if(comma3 != -1){
                            chooseComp(expr(leftIndex +2,comma1-1),
                            expr(comma1 +1 , comma2-1),
                            expr(comma2 +1 , comma3-1),
                            expr(comma3 +1 , rightIndex-1),
                            factorState);
                            return factorState;
                        }
                    }
                }
            }
        }
    }
    else if(tokens[leftIndex] == NUMBER && leftIndex == rightIndex){
        numberExpression(leftIndex, factorState);
        return factorState;
    }
    factorState -> state = FAILURE;
    return factorState;
}
// scalar syntax check
int scalarCheck(char *convertedCode){
    int index = 1;
    for(; tokens[index] != '\0'; index++){
        if(index > 2){
            return FAILURE;
        }
        char *word = words[index];
        if(index == 1){
                if(tokens[index] != VARIABLE){
                    return FAILURE;
                }
                if(findVariable(word) != NULL){
                    return FAILURE;
                }else{
                    struct variable *newVar = malloc(sizeof(struct variable));
                    newVar -> name = strdup(word);
                    newVar -> type = SCALAR;
                    newVar -> dimension1 = 1;
                    newVar -> dimension2 = 1;
                    variables[variableIndex++] = newVar;
                }
        }
    }
    if(index < 1){
        return FAILURE;
    }

    char *line = malloc(256*sizeof(char));
    strcat(line,"\tfloat ");
    strcat(line,words[1]);
    strcat(line,";\n");
    strcat(convertedCode,line);
    return SUCCESS;
}
//vector syntax check
int vectorCheck(char *convertedCode){
    int index = 1;
    struct variable *newVar = malloc(sizeof(struct variable));
    for(; tokens[index] != '\0'; index++){
        if(index > 5){
            return FAILURE;
        }
        char *word = words[index];
        if(index == 1){
                if(tokens[index] != VARIABLE){
                    return FAILURE;
                }
                if(findVariable(word) != NULL){
                    return FAILURE;
                }else{
                    newVar -> name = word;
                }
        }
        else if(index == 2 && tokens[index] != LBRACKET){
            return FAILURE;
        }
        else if(index == 3){
            if(tokens[index] != NUMBER || isInt(words[index]) == FAILURE ){
                return FAILURE;
            }
            else{
                newVar -> dimension1 = atoi(words[index]);
                newVar -> type = VECTOR;
                newVar -> dimension2 = 1;
            }
        }
        else if(index == 4 && tokens[index] != RBRACKET){
            return FAILURE;
        }
    }
    if(index < 4){
        return FAILURE;
    }
    variables[variableIndex++] = newVar;

    char *line = malloc(256*sizeof(char));
    strcat(line,"\tfloat ");
    strcat(line,words[1]);
    strcat(line,"[");
    strcat(line,words[3]);
    strcat(line,"];\n");
    strcat(convertedCode,line);
    return SUCCESS;
}
//matrix syntax check
int matrixCheck(char *convertedCode){
    int index = 1;
    struct variable *newVar = malloc(sizeof(struct variable));
    for(; tokens[index] != '\0'; index++){
        if(index > 6){
            return FAILURE;
        }
        char *word = words[index];
        if(index == 1){
                if(tokens[index] != VARIABLE){
                    return FAILURE;
                }
                if(findVariable(word) != NULL){
                    return FAILURE;
                }else{
                    newVar -> name = word;
                }
        }
        else if(index == 2 && tokens[index] != LBRACKET){
            return FAILURE;
        }
        else if(index == 3){
            if(tokens[index] != NUMBER || isInt(words[index]) == FAILURE){
                return FAILURE;
            }
            else{
                newVar -> dimension1 = atoi(words[index]);
            }
        }
        else if(index == 4 && tokens[index] != COMMA){
            return FAILURE;
        }
        else if(index == 5){
            if(tokens[index] != NUMBER || isInt(words[index]) == FAILURE){
                return FAILURE;
            }
            else{
                newVar -> dimension2 = atoi(words[index]);
            }
        }
        else if(index == 6 && tokens[index] != RBRACKET){
            return FAILURE;
        }
    }
    if(index < 6){
        return FAILURE;
    }
    newVar -> type = MATRIX;
    variables[variableIndex++] = newVar;


    char *line = malloc(256*sizeof(char));
    strcat(line,"\tfloat ");
    strcat(line,words[1]);
    strcat(line,"[");
    strcat(line,words[3]);
    strcat(line,"][");
    strcat(line,words[5]);
    strcat(line,"];\n");
    strcat(convertedCode,line);
    return SUCCESS;
}
// updates variable
int updateVariable(char *varName,struct expression *update){
    struct variable *curVar = findVariable(varName);
    if(curVar == NULL){
        //printf("Variable %s not found\n", varName);
        return FAILURE;
    }
    if(curVar -> dimension1 != update -> dimension1 || curVar -> dimension2 != update -> dimension2){
        //printf("Dimension mismatch\n");
        return FAILURE;
    }
    curVar -> value = update -> value;
    return SUCCESS;
}
// find size of line
int findSize(){
    int index = 0;
    while(tokens[index]!='\0'){
        index++;
    }
    return index;
}
//array syntax check
int arrayCheck(int leftIndex,int rightIndex, char *line){
    strcat(line," { ");
    for(int i = leftIndex; i <= rightIndex; i++){
        if(tokens[i] == NUMBER){
            strcat(line,words[i]);
            if(i != rightIndex) strcat(line,", ");
        }
        else{
            return FAILURE;
        }
    }
    strcat(line," } ");
    return SUCCESS;
}

int assignmentCount = 0;
// assignment syntax check
int assignmentCheck(char *convertedCode){
    int index = 0;
    char *line = malloc(256*sizeof(char));
    char *buffer = malloc(256*sizeof(char));
    strcat(line,"\t");
    
    struct variable *curVar = findVariable(words[index]);
    if( curVar == NULL){
        return FAILURE;
    }
    for(; tokens[index] != '\0' && index <= 2; index++){
        if(index == 1 && tokens[index] != EQUALS){
            return FAILURE;
            
        }
        else if(index == 2){
            int size = findSize();

            if( curVar -> dimension1 == 1 && curVar -> dimension2 == 1){
                struct expression *assignExpr = expr(index,size-1);
                if(assignExpr -> state == FAILURE){
                    return FAILURE;
                }
                else{
                    int result = updateVariable(words[0],assignExpr);
                    if(result == FAILURE){
                        return FAILURE;
                    }
                    strcat(line,curVar -> name);
                    strcat(line," = ");
                    strcat(line,assignExpr -> string);
                    strcat(line,expressionDimensionCheck(assignExpr));
                }
            }
            else {
                if( tokens[index] == LBRACES && tokens[size-1] == RBRACES){
                    strcat(line,"float temp");
                    sprintf(buffer,"%d",assignmentCount);
                    strcat(line,buffer);
                    strcat(line,"[] = ");
                    int result = arrayCheck(index + 1,size-2,line);
                    if(result == FAILURE){
                        return FAILURE;
                    }else{
                        strcat(line,";\n");
                        if(inFor)for(int i=0;i<inFor;i++) strcat(convertedCode,"\t");
                        strcat(line,"\tassign( (float *)temp");
                        strcat(line,buffer);
                        strcat(line,", (float *)");
                        strcat(line,curVar -> name);
                        strcat(line,", ");
                        sprintf(buffer,"%d",curVar->dimension1);
                        strcat(line,buffer);
                        strcat(line,", ");
                        sprintf(buffer,"%d",curVar->dimension2);
                        strcat(line,buffer);
                        strcat(line,")");
                    }
                }
                else {
                    struct expression *assignExpr = expr(index,size-1);
                    if(assignExpr -> state == FAILURE){
                        return FAILURE;
                    }
                    else{
                        int result = updateVariable(words[0],assignExpr);
                        if(result == FAILURE){
                            return FAILURE;
                        }
                        strcat(line,"assign( (float *)");
                        strcat(line,assignExpr -> string);
                        strcat(line,", (float *)");
                        strcat(line,curVar -> name);
                        strcat(line,", ");
                        sprintf(buffer,"%d",curVar->dimension1);
                        strcat(line,buffer);
                        strcat(line,", ");
                        sprintf(buffer,"%d",curVar->dimension2);
                        strcat(line,buffer);
                        strcat(line,")");
                    }
                }
            }
        }
    }
    if(index < 2){
        return FAILURE;
    }
    strcat(convertedCode,line);
    strcat(convertedCode,";\n");
    assignmentCount++;
    return SUCCESS;
}
// finds first column between indexes
int findColumn(int leftIndex,int rightIndex){
    for(int i = leftIndex; i <= rightIndex; i++){
         if(tokens[i] == COLUMN){
            return i;
        }
    }
    return -1;
}
//finds expression in for
int findExpressionInFor(int leftIndex, int rightIndex,char *varName,char *convertedCode){
    int firstColumnIndex = findColumn(leftIndex,rightIndex);
    if(firstColumnIndex != -1){
        int secondColumnIndex =findColumn(firstColumnIndex+1, rightIndex);
        if(secondColumnIndex != -1){
            struct expression *expr1 = expr(leftIndex,firstColumnIndex-1);
            if(expr1 -> state == FAILURE || expr1 -> dimension1 !=1 || expr1 -> dimension2 !=1 ){
                return FAILURE;
            }   
            struct expression *expr2 = expr(firstColumnIndex+1,secondColumnIndex-1);
            if(expr2 -> state == FAILURE || expr2 -> dimension1 !=1 || expr2 -> dimension2 !=1 ){
                return FAILURE;
            }
            struct expression *expr3 = expr(secondColumnIndex+1,rightIndex);
            if(expr3 -> state == FAILURE || expr3 -> dimension1 !=1 || expr3 -> dimension2 !=1 ){
                return FAILURE;
            }else{
                char *line = malloc(256*sizeof(char));
                if(inFor > 0){
                    for(int i=0;i<inFor;i++) strcat(line,"\t");
                }
                strcat(line,"\tfor( ");
                strcat(line,varName);
                strcat(line," = ");
                strcat(line,expr1->string);
                strcat(line,"; ");
                strcat(line,varName);
                strcat(line," <= ");
                strcat(line,expr2->string);
                strcat(line,"; ");
                strcat(line,varName);
                strcat(line," += ");
                strcat(line,expr3->string);
                strcat(line,"){\n");
                strcat(convertedCode,line);
                inFor += 1;
                return SUCCESS;
            }
        }
        else{
            return FAILURE; 
        }
    }else{
        return FAILURE;   
    }
}
// syntax check of for
int forCheck(char *convertedCode){
    int index = 1;
    int size = findSize();
    if(size <11){
        return FAILURE;
    }
    char *line = malloc(256*sizeof(char));
    strcat(line,"\t");

    for(; tokens[index] != '\0'; index++){
        if(index ==1 && tokens[index]!= LPARANTH){
            return FAILURE;
        }
        else if(index ==2){
            struct variable *temp = findVariable(words[index]);
            if(temp==NULL || temp->dimension1!=1 || temp->dimension2!=1){
                return FAILURE;
            }
        }
        else if(index == 3 && tokens[index] == IN){   
            if(findExpressionInFor(4,size - 3,words[index-1],convertedCode) == FAILURE){
                return FAILURE;
            }
            if(tokens[size-2]!= RPARANTH || tokens[size-1]!= LBRACES){
                return FAILURE;
            }
        }
        else if(index ==3 && tokens[index] == COMMA){
            struct variable *temp= findVariable(words[index+1]);
            if(temp==NULL || temp->dimension1!=1 || temp->dimension2!=1){
                return FAILURE;
            }
            
            if(tokens[index+2] != IN){
                return FAILURE;
            }
            
            int commaIndex = findComma(6,size-3);
            if(commaIndex != -1){
                if(findExpressionInFor(6,commaIndex-1,words[index-1],convertedCode) ==FAILURE){
                    return FAILURE;
                }else if(findExpressionInFor(commaIndex+1,size-3,words[index+1],convertedCode) == FAILURE){
                    return FAILURE;
                }
            }  
        }      
    }
    return SUCCESS;
}
// syntax check of print
int printCheck(char *convertedCode){
    int size = findSize();
    if(size < 3){
        return FAILURE;
    }
    if(tokens[1] != LPARANTH){
        return FAILURE;
    }
    if( tokens[size - 1]!= RPARANTH){
        return FAILURE;
    }
    struct expression *temp;
    temp = expr(2,size-2);

    if(temp -> state == FAILURE){
        return FAILURE;
    }
    char *line = malloc(256*sizeof(char));
    if(temp -> dimension1 == 1 && temp -> dimension2 == 1){
        sprintf(line,"\tdecimalCheckAndPrint( %s );\n",temp->string);
    }
    else{
            
        char *buffer = malloc(256*sizeof(char));
        sprintf(buffer,"\tcustomPrint((float *) %s, %d, %d);\n",temp -> string,temp -> dimension1,temp -> dimension2);
        strcat(line,buffer);
    }
    strcat(convertedCode,line);
    return SUCCESS;
}
//syntax check of printsep
int printSepCheck( char *convertedCode){
    int index =1;
    for(;tokens[index] != '\0'; index++){
        if(index==1 && tokens[index]!= LPARANTH){
            return FAILURE;
        }
        if(index==2 && tokens[index]!= RPARANTH){
            return FAILURE;
        }
    }
    char *line = malloc(256*sizeof(char));
    strcat(line,"\tprintf(");
    strcat(line,"\"------------\\n\"");
    strcat(line,");\n");
    strcat(convertedCode,line);
    return SUCCESS;
}

int tokenConverter(char* convertedCode){

    if(*tokens == '\0'){
        return SUCCESS; 
    }
    int lineType = *tokens;

    int result;
    if(lineType == SCALAR){
        if(inFor)for(int i=0;i<inFor;i++) strcat(convertedCode,"\t");
        result = scalarCheck(convertedCode);
    }
    else if(lineType == VECTOR){
        if(inFor)for(int i=0;i<inFor;i++) strcat(convertedCode,"\t");
        result = vectorCheck(convertedCode);
    }
    else if(lineType == MATRIX){
        if(inFor)for(int i=0;i<inFor;i++) strcat(convertedCode,"\t");
        result = matrixCheck(convertedCode);
    }
    else if(lineType == VARIABLE){
        if(inFor)for(int i=0;i<inFor;i++) strcat(convertedCode,"\t");
        result = assignmentCheck(convertedCode);
    }
    else if(lineType == FOR && inFor == 0){
        result = forCheck(convertedCode);
    }
    else if(lineType == RBRACES && inFor >= 1 && tokens[1] == '\0'){
        inFor--;
        if(inFor){
            for(int i=0;i<inFor;i++) strcat(convertedCode,"\t");
            strcat(convertedCode,"\t}\n");
        }
        inFor = 0;
        strcat(convertedCode,"\t}\n");
        result = SUCCESS;
    } 
    else if(lineType == PRINT){
        for(int i=0;i<inFor;i++) strcat(convertedCode,"\t");
        result = printCheck(convertedCode);
    }
    else if(lineType == PRINTSEP){
        for(int i=0;i<inFor;i++) strcat(convertedCode,"\t");
        result = printSepCheck(convertedCode);
    }
    else{
        return FAILURE;
    }
    if(result == FAILURE){
       // printf("fail");
        return FAILURE;
    }
    //printf("oldu\n");
    return SUCCESS;
}

int writeTokens(){
    int *abc = tokens;
    while(*abc != '\0'){
        printf(" %d ",*abc++);
    }
    printf("\n");
}

char** wordByWordSplit(char* spacedLine){
    words = malloc(256*sizeof(char*));
    int wordIndex = 0;
    char *ptr,*word;
    ptr = strdup(spacedLine);
    word = strtok(ptr," ");
    while(word != NULL){
        words[wordIndex] = malloc(256*sizeof(char));
        strcpy(words[wordIndex++],word);
        word = strtok(NULL," ");
    }
    return words;
}

void matrixMultTemplate(char *convertedCode){
    char *matrixMult1 = "float *matrixMultiplication(float *left,float *right,int leftDimension1, int leftDimension2,int rightDimension1, int rightDimension2){\n";
    char *matrixMult2 = "    float *newMatrix = malloc(leftDimension1*rightDimension2 * sizeof(float));\n";
    char *matrixMult3 = "    for( int i = 0; i < leftDimension1 ; i++){\n        for( int j = 0; j < rightDimension2; j++){\n";
    char *matrixMult4 ="            float sum = 0;\n";
    char *matrixMult5 = "            for(int k = 0; k < leftDimension2; k++){\n                sum += left[i*leftDimension2+k] * right[k*rightDimension2+j] ;\n";
    char *matrixMult6 = "            }\n            newMatrix[i*rightDimension2+j] = sum;\n        }\n    }\n    return newMatrix;\n}\n\n";
    strcat(convertedCode,matrixMult1);
    strcat(convertedCode,matrixMult2);
    strcat(convertedCode,matrixMult3);
    strcat(convertedCode,matrixMult4);
    strcat(convertedCode,matrixMult5);
    strcat(convertedCode,matrixMult6);
}

void updateMatrixTemplate(char *convertedCode){
    char *updateMatrixTemplate1 = "void updateMatrix(float *cur,float *new,int dimension1, int dimension2){\n";
    char *updateMatrixTemplate2 = "\tfor( int i = 0; i < dimension1 ; i++){\n        for( int j = 0; j < dimension2; j++){\n";
    char *updateMatrixTemplate3 ="\t\t\tcur[i*dimension2 + j] = new[i*dimension2 + j];\n        }\n    }\n}\n\n";
    strcat(convertedCode,updateMatrixTemplate1);
    strcat(convertedCode,updateMatrixTemplate2);
    strcat(convertedCode,updateMatrixTemplate3);
}

void transposeTemplate(char *convertedCode){
    char *trans1 = "float *transpose(float *matrix,int dim1,int dim2){\n\tfloat *newMatrix = malloc(dim1 * dim2 * sizeof(float));\n";
    char *trans2 = "\tfor( int i = 0; i < dim1 ; i++){\n\t\tfor( int j = 0; j < dim2; j++){\n\t\t\tnewMatrix[j*dim1 + i] = matrix[i*dim2 + j];\n";
    char *trans3 = "\t\t}\n\t}\n\treturn newMatrix;\n}\n\n";
    strcat(convertedCode,trans1);
    strcat(convertedCode,trans2);
    strcat(convertedCode,trans3);
}

void chooseTemplate(char *convertedCode){
    char *choose1 = "float choose(float expr1,float expr2,float expr3,float expr4){\n";
    char *choose2 = "\tif( abs(expr1) < E){\n\t\treturn expr2;\n\t}\n\telse if(expr1 > 0){\n\t\treturn expr3;\n\t}\n\telse{\n\t\treturn expr4;\n\t}\n}\n\n";
    strcat(convertedCode,choose1);
    strcat(convertedCode,choose2);
}

void assignTemplate(char *convertedCode){
    char *assign1 = "void assign(float *old,float *cur,int dim1,int dim2){\n";
    char *assign2 = "\tfor(int i=0;i<dim1;i++){\n\t\tfor(int j=0;j<dim2;j++){\n\t\t\tcur[i*dim2+j] = old[i*dim2+j];\n\t\t}\n\t}\n}\n\n";
    strcat(convertedCode,assign1);
    strcat(convertedCode,assign2);
}

void decimalCheckAndPrintTemplate(char *convertedCode){
    char *decimalCheck1 = "void decimalCheckAndPrint(float x){\n";
    char *decimalCheck2 = "\tif( x - (int) x  < E){\n\t\tprintf(\"%d\\n\",(int) x);\n\t}\n\telse{\n\t\tprintf(\"%.6f\\n\" , x);\n\t}\n}\n\n";
    strcat(convertedCode,decimalCheck1);
    strcat(convertedCode,decimalCheck2);
}

void customPrintTemplate(char *convertedCode){
    char *customPrint1 = "void customPrint(float *matrix,int dim1,int dim2){\n";
    char *customPrint2 = "\tfor(int i=0;i<dim1;i++){\n\t\tfor(int j=0;j<dim2;j++){\n\t\t\tdecimalCheckAndPrint(matrix[i*dim2+j]);\n\t\t}\n\t}\n}\n\n";
    strcat(convertedCode,customPrint1);
    strcat(convertedCode,customPrint2);
}

void matrixPlusTemplate(char *convertedCode){
    char *matrixPlus1 = "float *matrixPlus(float *left,float *right,int dim1,int dim2){\n\tfloat *newMatrix = malloc(dim1 * dim2 * sizeof(float));\n";
    char *matrixPlus2 = "\tfor(int i=0;i<dim1;i++){\n\t\tfor(int j=0;j<dim2;j++){\n\t\t\tnewMatrix[i*dim2+j] = left[i*dim2+j] + right[i*dim2+j];\n\t\t}\n\t}\n\treturn newMatrix;\n}\n\n";
    strcat(convertedCode,matrixPlus1);
    strcat(convertedCode,matrixPlus2);
}

void matrixMinusTemplate(char *convertedCode){
    char *matrixPlus1 = "float *matrixMinus(float *left,float *right,int dim1,int dim2){\n\tfloat *newMatrix = malloc(dim1 * dim2 * sizeof(float));\n";
    char *matrixPlus2 = "\tfor(int i=0;i<dim1;i++){\n\t\tfor(int j=0;j<dim2;j++){\n\t\t\tnewMatrix[i*dim2+j] = left[i*dim2+j] - right[i*dim2+j];\n\t\t}\n\t}\n\treturn newMatrix;\n}\n\n";
    strcat(convertedCode,matrixPlus1);
    strcat(convertedCode,matrixPlus2);
}
void revTemplate(char *convertedCode){
    strcat(convertedCode,"int rev(float expr){\n\tif(expr-(int)expr<E && expr>=1){\n\t\treturn (int)expr;\n\t}\n\telse{\n\t\treturn INT_MAX;\n\t}\n}\n\n");
}
void convertedCodeTemplate(char *convertedCode){
    char *template = "";
    char *includeTemplate = "#include <stdio.h>\n#include <stdlib.h>\n#include <math.h>\n#include <string.h>\n#include <ctype.h>\n#include <limits.h>\n#define E 0.00000001\n";
    char *updateScalarTemplate = "void updateScalar(float *cur,float *new){\n\t*cur = *new;\n}\n\n";
    char *mainTemplate ="int main(){\n";
    strcat(convertedCode,includeTemplate);
    strcat(convertedCode,updateScalarTemplate);
    updateMatrixTemplate(convertedCode);
    matrixMultTemplate(convertedCode);
    transposeTemplate(convertedCode);
    chooseTemplate(convertedCode);
    assignTemplate(convertedCode);
    decimalCheckAndPrintTemplate(convertedCode);
    customPrintTemplate(convertedCode);
    matrixPlusTemplate(convertedCode);
    matrixMinusTemplate(convertedCode);
    revTemplate(convertedCode);
    strcat(convertedCode,mainTemplate);
}



int main (int argc,char *argv[]) {

    FILE *file,*outputFile;
    file = fileRead(argc,argv);
    outputFile = fileWrite();
    if(file == NULL){
        return(1);
    }

    variables = malloc (256 * sizeof(struct variable));
    char *convertedCode = malloc (256* 256 * sizeof(char));
    convertedCodeTemplate(convertedCode);

    int lineCount = 1;
    char line[LINE_SIZE];
    while( fgets(line,LINE_SIZE,file) != NULL ) {
        //printf("%d ",lineCount);
        char* spacedLine = spacer(line);
        words = wordByWordSplit(spacedLine);
        tokens = scanLine();
        int tokensState = tokenConverter(convertedCode);
        if(tokensState == FAILURE){
            printf("Error (Line %d)",lineCount);
            fclose(file);
            fclose(outputFile);
            return(0);
        }
        lineCount++;
    }
    if(inFor == 1){
        printf("Error (Line %d)",lineCount);
        return 0;
    }
    strcat(convertedCode,"\treturn 0;\n}");
    fputs(convertedCode,outputFile);
    fclose(file);
    fclose(outputFile);
    return(0);
}



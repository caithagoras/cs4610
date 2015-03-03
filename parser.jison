
%token AT CASE CLASS COLON COMMA DIVIDE DOT ELSE EQUALS ESAC FALSE FI IDENTIFIER IF IN INHERITS INTEGER ISVOID LARROW LBRACE LE LET LOOP LPAREN LT MINUS NEW NOT OF PLUS POOL RARROW RBRACE RPAREN SEMI STRING THEN TILDE TIMES TRUE TYPE WHILE

%right LARROW
%left NOT
%nonassoc LT LE EQ
%left PLUS MINUS
%left TIMES DIVIDE
%left ISVOID
%left TILDE
%left AT
%left DOT
%start program

%%
program
    :   class_list EOF                          { return $1; }
    ;
    
class_list
    :   class_list class SEMI                   { $$ = $1.concat([$2]); }
    |   class SEMI                              { $$ = [$1]; }
    ;

class
    :   CLASS type LBRACE feature_list RBRACE
                                                { $$ = [$2, undefined, $4]; }
    |   CLASS type INHERITS type LBRACE feature_list RBRACE
                                                { $$ = [$2, $4, $6]; }
    ;

feature_list
    :   feature_list feature SEMI               { $$ = $1.concat([$2]);}
    |   /* empty */                             { $$ = []; }
    ;

feature
    :   id LPAREN formal_list RPAREN COLON type LBRACE expr RBRACE
                                                { $$ = ["method", $1, $3, $6, $8]; }
    |   id COLON type                           { $$ = ["attribute_no_init", $1, $3]; }
    |   id COLON type LARROW expr               { $$ = ["attribute_init", $1, $3, $5]; }
    ;

formals
    :   formal_list                             { $$ = $1; }
    |   /* empty */                             { $$ = []; }
    ;

formal_list
    :   formal_list COMMA formal                { $$ = $1.concat([$3]); }
    |   formal                                  { $$ = [$1]; }
    ;

formal
    :   id COLON type                           { $$ = [$1, $3]; }
    ;

expr
    :   id LARROW expr                          { $$ = [@1.first_line, "assign", $1, $3]; }
    |   expr AT type DOT id LPAREN actuals RPAREN
                                                { $$ = [@1.first_line, "static_dispatch", $1, $3, $5, $7 ] }
    |   expr DOT id LPAREN actuals RPAREN       { $$ = [@1.first_line, "dynamic_dispatch", $1, $3, $5] }
    |   ID LPAREN actuals RPAREN                { $$ = [@1.first_line, "self_dispatch", $1, $3] }
    |   IF expr THEN expr ELSE expr FI          { $$ = [@1.first_line, "if", $2, $4, $6]; }
    |   WHILE expr LOOP expr POOL               { $$ = [@1.first_line, "while", $2, $4]; }
    |   LBRACE expr_list RBRACE                 { $$ = [@1.first_line, "block", $2 ]; }
    |   NEW type                                { $$ = [@1.first_line, "new", $2]; }
    |   ISVOID expr                             { $$ = [@1.first_line, "isvoid", $2]; }
    |   expr PLUS expr                          { $$ = [@1.first_line, "plus", $1, $3];}
    |   expr MINUS expr                         { $$ = [@1.first_line, "minus", $1, $3]; }
    |   expr TIMES expr                         { $$ = [@1.first_line, "times", $1, $3]; }
    |   expr DIVIDE expr                        { $$ = [@1.first_line, "divide", $1, $3]; }
    |   TILDE expr                              { $$ = [@1.first_line, "negate", $2]; }
    |   expr LT expr                            { $$ = [@1.first_line, "lt", $1, $3]; }
    |   expr LE expr                            { $$ = [@1.first_line, "le", $1, $3]; }
    |   expr EQ expr                            { $$ = [@1.first_line, "eq", $1, $3]; }
    |   NOT expr                                { $$ = [@1.first_line, "not", $2]; }
    |   LPAREN expr RPAREN                      { $$ = $2; }    
    |   id                                      { $$ = [@1.first_line, "identifier", $1]; }
    |   INTEGER                                 { $$ = [@1.first_line, "integer", Number(yytext)]; }
    |   STRING                                  { $$ = [@1.first_line, "string", yytext]; }
    |   TRUE                                    { $$ = [@1.first_line, "true"]; }
    |   FALSE                                   { $$ = [@1.first_line, "false"]; }
    ;

expr_list
    :   expr_list expr SEMI                     { $$ = $1.concat([$2]); }
    |   expr SEMI                               { $$ = [$1]; }
    ;

actuals
    :   acutal_list                             { $$ = $1; }
    |   /* empty */                             { $$ = []; }
    ;

actual_list
    :   actual_list COMMA expr                  { $$ = $1.concat([$3]); }
    |   expr                                    { $$ = [$1]; }
    ;

type
    : TYPE                                      { $$ = [@1.first_line, yytext]; }
    ;
    
id
    : IDENTIFIER                                { $$ = [@1.first_line, yytext]; }
    ;

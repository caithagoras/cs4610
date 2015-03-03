
%token AT CASE CLASS COLON COMMA DIVIDE DOT ELSE EQUALS ESAC FALSE FI IDENTIFIER IF IN INHERITS INTEGER ISVOID LARROW LBRACE LE LET LOOP LPAREN LT MINUS NEW NOT OF PLUS POOL RARROW RBRACE RPAREN SEMI STRING THEN TILDE TIMES TRUE TYPE WHILE

%right LET
%right LARROW
%left NOT
%nonassoc LT LE EQUALS
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
    :   id LPAREN formals RPAREN COLON type LBRACE expr RBRACE
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
    :   id LARROW expr                          { $$ = [$1[0], "assign", $1, $3]; }
    |   expr AT type DOT id LPAREN actuals RPAREN
                                                { $$ = [$1[0], "static_dispatch", $1, $3, $5, $7 ] }
    |   expr DOT id LPAREN actuals RPAREN       { $$ = [$1[0], "dynamic_dispatch", $1, $3, $5] }
    |   id LPAREN actuals RPAREN                { $$ = [$1[0], "self_dispatch", $1, $3] }
    |   IF expr THEN expr ELSE expr FI          { $$ = [$1[0], "if", $2, $4, $6]; }
    |   WHILE expr LOOP expr POOL               { $$ = [$1[0], "while", $2, $4]; }
    |   LBRACE expr_list RBRACE                 { $$ = [$1[0], "block", $2 ]; }
    |   NEW type                                { $$ = [$1[0], "new", $2]; }
    |   ISVOID expr                             { $$ = [$1[0], "isvoid", $2];}
    |   expr PLUS expr                          { $$ = [$1[0], "plus", $1, $3];}
    |   expr MINUS expr                         { $$ = [$1[0], "minus", $1, $3]; }
    |   expr TIMES expr                         { $$ = [$1[0], "times", $1, $3]; }
    |   expr DIVIDE expr                        { $$ = [$1[0], "divide", $1, $3]; }
    |   TILDE expr                              { $$ = [$1[0], "negate", $2]; }
    |   expr LT expr                            { $$ = [$1[0], "lt", $1, $3]; }
    |   expr LE expr                            { $$ = [$1[0], "le", $1, $3]; }
    |   expr EQUALS expr                        { $$ = [$1[0], "eq", $1, $3]; }
    |   NOT expr                                { $$ = [$1[0], "not", $2]; }
    |   LPAREN expr RPAREN                      { $$ = $2; }    
    |   id                                      { $$ = [$1[0], "identifier", $1]; }
    |   INTEGER                                 { $$ = [$1[0], "integer", Number(yytext[1])]; }
    |   STRING                                  { $$ = [$1[0], "string", yytext[1]]; }
    |   TRUE                                    { $$ = [$1[0], "true"]; }
    |   FALSE                                   { $$ = [$1[0], "false"]; }
    |   LET let_binding_list IN expr            { $$ = [$1[0], "let", $2, $4]; }
    |   CASE expr OF case_element_list ESAC     { $$ = [$1[0], "case", $2, $4]; }
    ;

expr_list
    :   expr_list expr SEMI                     { $$ = $1.concat([$2]); }
    |   expr SEMI                               { $$ = [$1]; }
    ;

actuals
    :   acutal_list                             { $$ = $1; }
    |   /* empty */                             { $$ = []; }
    ;

acutal_list
    :   acutal_list COMMA expr                  { $$ = $1.concat([$3]); }
    |   expr                                    { $$ = [$1]; }
    ;

let_binding_list
    :   let_binding_list COMMA let_binding      { $$ = $1.concat([$3]); }
    |   let_binding                             { $$ = [$1]; }
    ;

let_binding
    :   id COLON type                           { $$ = ["let_binding_no_init", $1, $3]; }
    |   id COLON type LARROW expr               { $$ = ["let_binding_init", $1, $3, $5]; }
    ;
    

case_element_list
    :   case_element_list case_element          { $$ = $1.concat([$2]); }
    |   case_element                            { $$ = [$1]; }
    ;

case_element
    :   id COLON type RARROW expr SEMI          { $$ = [$1, $3, $5]; }
    ;

type
    : TYPE                                      { $$ = [yytext[0], yytext[1]]; }
    ;
    
id
    : IDENTIFIER                                { $$ = [yytext[0], yytext[1]]; }
    ;

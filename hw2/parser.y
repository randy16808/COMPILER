%{
#include <stdio.h>
#include <stdlib.h>

extern int linenum;             /* declared in lex.l */
extern FILE *yyin;              /* declared by lex */
extern char *yytext;            /* declared by lex */
extern char buf[256];           /* declared in lex.l */
%}

%nonassoc KW_BOOL KW_INT KW_FLOAT KW_DOUBLE KW_STRING KW_CONST KW_VOID
%nonassoc END
%nonassoc LESS_THAN_ELSE
%nonassoc KW_ELSE
%nonassoc '(' ')'


%left OR
%left AND
%left NOT
%nonassoc LT LE EQ GE GT NE
%left '+' '-'
%left '*' '/' '%'

%token KW_RETURN
%token KW_BREAK
%token KW_CONTINUE
%token KW_FOR
%token KW_WHILE
%token KW_DO
%token KW_IF
%token ';'		    
%token ID           
%token KW_BOOL         
%token KW_INT
%token KW_FLOAT
%token KW_DOUBLE
%token KW_STRING
%token '='
%token ','
%token KW_PRINT
%token KW_READ
%token BOOLEAN_CONSTANT
%token INTEGER_CONSTANT
%token FLOATING_CONSTANT
%token STRING_CONSTANT
%token KW_CONST
%token KW_VOID
%%

program : declaration_list funct_definition decl_and_def_list 
        | declaration_list proc_definition decl_and_def_list  
		    | proc_definition decl_and_def_list 
		    | funct_definition decl_and_def_list 
		    ;

decl_and_def_list: decl_and_def_list declaration_list  %prec END 
				         | decl_and_def_list definition_list  %prec END 
                 |
                 ;

declaration_list : declaration_list const_decl 
                 | declaration_list var_decl 
                 | declaration_list funct_decl 
                 | declaration_list proc_decl 
                 | const_decl 
                 | var_decl 
                 | funct_decl 
                 | proc_decl 
		 		         ;
 
definition_list : definition_list funct_definition 
                | definition_list proc_definition  
                | funct_definition
                | proc_definition 
                ;

 /* function */

funct_decl : type scalar_id '(' zero_or_more_args ')'  ';'
           ;

proc_decl : KW_VOID scalar_id '(' zero_or_more_args ')' ';'
          ;

funct_definition : type scalar_id '(' zero_or_more_args ')'  compound_stat 
                 ;
 
proc_definition : KW_VOID scalar_id '(' zero_or_more_args ')'  compound_stat  
                ;

 /* variable declaration*/

type : KW_INT 
	   | KW_BOOL
     | KW_DOUBLE  
     | KW_FLOAT 
     | KW_STRING  
     ;      

var_decl : type var_list ';' 
         ;  

var_list: var_entry ',' var_list
		    | var_entry
		    ;

var_entry: identifier
		     | scalar_id '=' expr
		     | array_id '=' '{' zero_or_more_expr_list '}'
		     ;

identifier: scalar_id 
		      | array_id
		      ;

scalar_id : ID
		      ;

array_id: scalar_id array_index
		    ;

array_index: array_singleindex array_index
		       | array_singleindex
		       ;

array_singleindex: '[' INTEGER_CONSTANT ']' 
		   		       ;

 /* constant declaration */
const_decl : KW_CONST type const_list ';'
		       ;

const_list: const_list ',' const_list_entry
		      | const_list_entry
		      ;

const_list_entry: scalar_id '=' literal_constant
			         	| scalar_id '=' '-' literal_constant %prec '*'
			         	;

literal_constant : INTEGER_CONSTANT
                 | FLOATING_CONSTANT
                 | BOOLEAN_CONSTANT
                 | STRING_CONSTANT 
                 ;


 /*argument*/

zero_or_more_args: arg_list
			         	 | 
				         ;

arg_list: arg_list_entry ',' arg_list
	     	| arg_list_entry
		    ;

arg_list_entry: type identifier
			        | KW_CONST type identifier
			        ;

 /*statement*/
  stat : compound_stat
       | simple_stat
       | if_stat
       | while_stat
       | for_stat
       | jump_stat
       | funct_call ';'
       ;
 
 /*compound statement*/

compound_stat: '{' zero_or_more_var_const_decl_list  zero_or_more_stat_list '}'  
			       ;

zero_or_more_var_const_decl_list: var_const_decl_list
								                |
								                ;

var_const_decl_list: var_const_decl_entry var_const_decl_list
				           | var_const_decl_entry
				           ;

var_const_decl_entry: var_decl
				          	| const_decl
				          	;

zero_or_more_stat_list: stat_list
                      | 
           			      ;

stat_list : stat_list stat
          | stat
          ;

 /*simple statement*/

 simple_stat: var_ref '=' expr ';'
 	      		| KW_PRINT  expr   ';' 
 		       	| KW_READ var_ref ';'
 		       	;

var_ref: scalar_id
	     | array_ref
	     ;

array_ref: scalar_id array_index_exp
		     ;

array_index_exp: array_singleindex_exp array_index_exp
			         | array_singleindex_exp
			         ;

array_singleindex_exp: '[' expr ']' 
		   		 	         ;


 /*expression*/

expr : '-' expr %prec '*'
	   | expr '*' expr
     | expr '/' expr
     | expr '%' expr
     | expr '+' expr
     | expr '-' expr
     | expr LT expr
     | expr LE expr
     | expr EQ expr
     | expr GE expr
     | expr GT expr
     | expr NE expr
     | NOT expr
     | expr AND expr
     | expr OR expr
     | literal_constant 
     | funct_call
     | var_ref
     | '(' expr ')'  
	   ;

bool_expr : expr
          ;

 /*function invocation*/
funct_call: scalar_id  '('  zero_or_more_expr_list ')'
          ; 

zero_or_more_expr_list:  expr_list 
                      | 
                      ;

expr_list: expr_list ',' expr
		     | expr
		     ;

 /*conditional statement*/

if_stat : if_only_stat
        | if_else_stat 
        ;

if_only_stat : KW_IF '(' bool_expr ')' compound_stat %prec LESS_THAN_ELSE
             ;

if_else_stat : KW_IF '(' bool_expr ')' compound_stat KW_ELSE compound_stat 
             ;

 /* while-statment */

while_stat : KW_WHILE '(' bool_expr ')' compound_stat 
           | KW_DO compound_stat KW_WHILE '(' bool_expr ')' ';'
           ;

 /* For-statment */
for_stat : KW_FOR '(' for_expr ';' for_expr ';' for_expr ')' '{' zero_or_more_stat_list '}' 
         ;

for_expr : expr
         | one_or_more_for_assignment 
         | 
         ;

one_or_more_for_assignment : var_ref '=' expr
                           | one_or_more_for_assignment ',' var_ref '=' expr
                           ;
 /*jump statement*/
 jump_stat : KW_RETURN expr ';'
           | KW_BREAK ';'
           | KW_CONTINUE ';'
           ;


%%

int yyerror( char *msg )
{
  fprintf( stderr, "\n|--------------------------------------------------------------------------\n" );
	fprintf( stderr, "| Error found in Line #%d: %s\n", linenum, buf );
	fprintf( stderr, "|\n" );
	fprintf( stderr, "| Unmatched token: %s\n", yytext );
  fprintf( stderr, "|--------------------------------------------------------------------------\n" );
  exit(-1);
}

int  main( int argc, char **argv )
{
	if( argc != 2 ) {
		fprintf(  stdout,  "Usage:  ./parser  [filename]\n"  );
		exit(0);
	}

	FILE *fp = fopen( argv[1], "r" );
	
	if( fp == NULL )  {
		fprintf( stdout, "Open  file  error\n" );
		exit(-1);
	}
	
	yyin = fp;
	yyparse();

	fprintf( stdout, "\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	fprintf( stdout, "|  There is no syntactic error!  |\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	exit(0);
}

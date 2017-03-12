%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "header.h"
#include "symtab.h"
#include "semcheck.h"

#define MAX_STACK 100
#define MAX_LOCALS 100
#define MAX_TMP 20
#define MAX_STRING_LENGTH 20
#define MAX_VarNum_size  MAX_LOCALS
#define MAX_initial_size 4096
#define MAX_STRING_SIZE 4096
#define TMP0 (MAX_LOCALS-10)
#define TMP1 (MAX_LOCALS-9)
#define TMP2 (MAX_LOCALS-8)
#define TMP3 (MAX_LOCALS-7)
extern int linenum;
extern FILE	*yyin;
extern char	*yytext;
extern char buf[256];
extern int Opt_Symbol;		/* declared in lex.l */
char initial[MAX_initial_size];

FILE * ByteCode;
int scope = 0;
int paramVarNum = 0;
char fileName[256];
struct SymTable *symbolTable;
__BOOLEAN paramError;
struct PType *funcReturn;
__BOOLEAN semError = __FALSE;
int inloop = 0;
extern const char* asm_type[];
const char* asm_type_s[] ={ "i", "f", "d", "i", "s", "v" };
const char* TYPE_NAME[] = {"int","float","double","bool","string","void"};
void asmFunc(char *name, int Return , int flag, struct param_sem *params);
void asmArith( struct expr_sem *op1, OPERATOR operator, struct expr_sem *op2);
void asmArith2( struct expr_sem *op1, OPERATOR operator, struct expr_sem *op2);
void asmOperator(OPERATOR operator, int k);
void asmRelationalOperator(OPERATOR operator, int k);
int search(struct expr_sem* expr);
void asmLiteral(struct ConstAttr * a);
void asmRead(struct expr_sem *expr);
int asmCoer(int type1,int type2);
int mapType(struct PType *type);
void asmFuncInvoke(char * name);
void asmFuncTail();
void asmFuncReturn(int k);
int tmptype=-1;
int nextNum=0;
int pure_var = 1;
int LabelBool=0;
int LabelIf=0;
int LabelLoop=0;
int LabelWhile=0;
int LabelFor=0;
int notLoad = 0;
int FuncDecl=0;
char ID_tmp[MAX_TMP][MAX_VarNum_size][30]; //scope varnum name
int ID_init[MAX_TMP][MAX_VarNum_size];
char Func_tmp[10][20];
char para_name[10][10];
int Func_tmp_size=0;

struct ID_t{
	int type;
	int scope;
	int VarNum;
	char* name[30];
	int integerVal;
	float floatVal;
	double doubleVal;
	__BOOLEAN booleanVal;
	char *stringVal;
};

struct ID_t now_ID;
%}

%union {
	int intVal;
	float floatVal;	
	char *lexeme;
	struct idNode_sem *id;
	struct ConstAttr *constVal;
	struct PType *ptype;
	struct param_sem *par;
	struct expr_sem *exprs;
	struct expr_sem_node *exprNode;
	struct constParam *constNode;
	struct varDeclParam* varDeclNode;
};

%token	LE_OP NE_OP GE_OP EQ_OP AND_OP OR_OP
%token	READ BOOLEAN WHILE DO IF ELSE TRUE FALSE FOR INT PRINT BOOL VOID FLOAT DOUBLE STRING CONTINUE BREAK RETURN CONST
%token	L_PAREN R_PAREN COMMA SEMICOLON ML_BRACE MR_BRACE L_BRACE R_BRACE ADD_OP SUB_OP MUL_OP DIV_OP MOD_OP ASSIGN_OP LT_OP GT_OP NOT_OP

%token <lexeme>ID
%token <intVal>INT_CONST 
%token <floatVal>FLOAT_CONST
%token <floatVal>SCIENTIFIC
%token <lexeme>STR_CONST

%type<ptype> scalar_type dim
%type<par> array_decl parameter_list
%type<constVal> literal_const
%type<constNode> const_list 
%type<exprs> variable_reference logical_expression logical_term logical_factor relation_expression arithmetic_expression term factor logical_expression_list literal_list initial_array
%type<intVal> relation_operator add_op mul_op dimension
%type<varDeclNode> identifier_list


%start program
%%

program :		decl_list 
			    funct_def
				decl_and_def_list 
				{
					if(Opt_Symbol == 1)
					printSymTable( symbolTable, scope );	
				}
		;

decl_list : decl_list var_decl
		  | decl_list const_decl
		  | decl_list funct_decl
		  |
		  ;


decl_and_def_list : decl_and_def_list var_decl
				  | decl_and_def_list const_decl
				  | decl_and_def_list funct_decl
				  | decl_and_def_list funct_def
				  | 
				  ;

		  
funct_def : scalar_type ID L_PAREN R_PAREN 
			{
				funcReturn = $1; 
				struct SymNode *node;
				node = findFuncDeclaration( symbolTable, $2 );
				
				if( node != 0 ){
					verifyFuncDeclaration( symbolTable, 0, $1, node );
				}
				else{
					insertFuncIntoSymTable( symbolTable, $2, 0, $1, scope, __TRUE );
				}
				if(strcmp($2,"main") == 0)  nextNum=1; 
				else  nextNum=0; 
				asmFunc($2, mapType($1) , 0, NULL);
				bzero(ID_tmp+scope,MAX_VarNum_size*30);
				int i=0;
				for(i=0 ; i<Func_tmp_size; i++) 
				{
					fprintf(ByteCode,"%s %d\n",Func_tmp[i],nextNum);
					strcpy(ID_tmp[scope][nextNum],para_name[i]);
					ID_init[scope][nextNum] = 1;
					nextNum++;
				}
				Func_tmp_size = 0;
			}
			compound_statement 
				{ funcReturn = 0; 
				  if(strcmp($2,"main") != 0) asmFuncReturn(mapType($1)); 
				  asmFuncTail();
				 }	
		  | scalar_type ID L_PAREN parameter_list R_PAREN  
			{				
				funcReturn = $1;
				paramError = checkFuncParam( $4 );
				if( paramError == __TRUE ){
					fprintf( stdout, "########## Error at Line#%d: param(s) with several fault!! ##########\n", linenum );
					semError = __TRUE;
				}
				// check and insert function into symbol table
				else{
					struct SymNode *node;
					node = findFuncDeclaration( symbolTable, $2 );

					if( node != 0 ){
						if(verifyFuncDeclaration( symbolTable, $4, $1, node ) == __TRUE){	
							insertParamIntoSymTable( symbolTable, $4, scope+1 );
						}				
					}
					else{
						insertParamIntoSymTable( symbolTable, $4, scope+1 );				
						insertFuncIntoSymTable( symbolTable, $2, $4, $1, scope, __TRUE );
					}
					if(strcmp($2,"main") == 0) nextNum=1; 
					else  nextNum=0;
					int i=0;
					for( ; i<MAX_VarNum_size; i++) ID_init[scope][i]=0;
					bzero(ID_tmp+scope,MAX_VarNum_size*30);

					asmFunc($2, mapType($1), 1, $4);
					for(i=0 ; i<Func_tmp_size; i++) 
					{
						fprintf(ByteCode,"%s %d\n",Func_tmp[i],nextNum);
						strcpy(ID_tmp[scope][nextNum],para_name[i]);
						ID_init[scope][nextNum] = 1;
						nextNum++;
					}
					Func_tmp_size = 0;
				}
			} 	
			compound_statement { funcReturn = 0; asmFuncReturn(mapType($1)); asmFuncTail(); }
		  | VOID ID L_PAREN R_PAREN 
			{
				funcReturn = createPType(VOID_t); 
				struct SymNode *node;
				node = findFuncDeclaration( symbolTable, $2 );

				if( node != 0 ){
					verifyFuncDeclaration( symbolTable, 0, createPType( VOID_t ), node );					
				}
				else{
					insertFuncIntoSymTable( symbolTable, $2, 0, createPType( VOID_t ), scope, __TRUE );	
				}
				if(strcmp($2,"main") == 0)  nextNum=1; 
				else  nextNum=0; 
				asmFunc($2, 5, 0, NULL);
				bzero(ID_tmp+scope,MAX_VarNum_size*30);
				int i=0;
				for(i=0 ; i<Func_tmp_size; i++) 
				{
					fprintf(ByteCode,"%s %d\n",Func_tmp[i],nextNum);
					strcpy(ID_tmp[scope][nextNum],para_name[i]);
					ID_init[scope][nextNum] = 1;
					nextNum++;
				}
				Func_tmp_size = 0;

			}
			compound_statement { funcReturn = 0; asmFuncTail();}	
		  | VOID ID L_PAREN parameter_list R_PAREN
			{									
				funcReturn = createPType(VOID_t);
				
				paramError = checkFuncParam( $4 );
				if( paramError == __TRUE ){
					fprintf( stdout, "########## Error at Line#%d: param(s) with several fault!! ##########\n", linenum );
					semError = __TRUE;
				}
				// check and insert function into symbol table
				else{
					struct SymNode *node;
					node = findFuncDeclaration( symbolTable, $2 );

					if( node != 0 ){
						if(verifyFuncDeclaration( symbolTable, $4, createPType( VOID_t ), node ) == __TRUE){	
							insertParamIntoSymTable( symbolTable, $4, scope+1 );				
						}
					}
					else{
						insertParamIntoSymTable( symbolTable, $4, scope+1 );				
						insertFuncIntoSymTable( symbolTable, $2, $4, createPType( VOID_t ), scope, __TRUE );
					}
					if(strcmp($2,"main") == 0)  nextNum=1; 
					else  nextNum=0; 
					asmFunc($2, 5, 1, $4);
					int i=0;
					for( ; i<MAX_VarNum_size; i++) ID_init[scope][i]=0;
					bzero(ID_tmp+scope,MAX_VarNum_size*30);
					for(i=0 ; i<Func_tmp_size; i++) 
					{
						fprintf(ByteCode,"%s %d\n",Func_tmp[i],nextNum);
						strcpy(ID_tmp[scope][nextNum],para_name[i]);
						ID_init[scope][nextNum] = 1;
						nextNum++;
					}
					Func_tmp_size = 0;
				}
			} 
			compound_statement { funcReturn = 0; asmFuncTail();}		  
		  ;

funct_decl : scalar_type ID L_PAREN R_PAREN SEMICOLON
			{
				insertFuncIntoSymTable( symbolTable, $2, 0, $1, scope, __FALSE );	
			}
		   | scalar_type ID L_PAREN parameter_list R_PAREN SEMICOLON
		    {
				paramError = checkFuncParam( $4 );
				if( paramError == __TRUE ){
					fprintf( stdout, "########## Error at Line#%d: param(s) with several fault!! ##########\n", linenum );
					semError = __TRUE;
				}
				else {
					insertFuncIntoSymTable( symbolTable, $2, $4, $1, scope, __FALSE );
				}
			}
		   | VOID ID L_PAREN R_PAREN SEMICOLON
			{				
				insertFuncIntoSymTable( symbolTable, $2, 0, createPType( VOID_t ), scope, __FALSE );
			}
		   | VOID ID L_PAREN parameter_list R_PAREN SEMICOLON
			{
				paramError = checkFuncParam( $4 );
				if( paramError == __TRUE ){
					fprintf( stdout, "########## Error at Line#%d: param(s) with several fault!! ##########\n", linenum );
					semError = __TRUE;	
				}
				else {
					insertFuncIntoSymTable( symbolTable, $2, $4, createPType( VOID_t ), scope, __FALSE );
				}
			}
		   ;

parameter_list : parameter_list COMMA scalar_type ID
			   {
				struct param_sem *ptr;
				ptr = createParam( createIdList( $4 ), $3 );
				param_sem_addParam( $1, ptr );
				$$ = $1;
				sprintf(Func_tmp[Func_tmp_size],"	%sload",asm_type_s[tmptype]);
				strcpy(para_name[Func_tmp_size],$4);
				Func_tmp_size++;
			   }
			   | parameter_list COMMA scalar_type array_decl
			   {
				$4->pType->type= $3->type;
				param_sem_addParam( $1, $4 );
				$$ = $1;
			   }
			   | scalar_type array_decl 
			   { 
				$2->pType->type = $1->type;  
				$$ = $2;
			   }
			   | scalar_type ID { $$ = createParam( createIdList( $2 ), $1 );
					sprintf(Func_tmp[Func_tmp_size],"	%sload",asm_type_s[tmptype]);
					strcpy(para_name[Func_tmp_size],$2);
					Func_tmp_size++;
				 }
			   ;

var_decl : scalar_type  identifier_list SEMICOLON
			{
				struct varDeclParam *ptr;
				struct SymNode *newNode;
				for( ptr=$2 ; ptr!=0 ; ptr=(ptr->next) ) {						
					if( verifyRedeclaration( symbolTable, ptr->para->idlist->value, scope ) == __FALSE ) { }
					else {
						if( verifyVarInitValue( $1, ptr, symbolTable, scope ) ==  __TRUE ){	
							newNode = createVarNode( ptr->para->idlist->value, scope, ptr->para->pType );
							insertTab( symbolTable, newNode );
						}
					}
				}
			}
			;

identifier_list : identifier_list COMMA ID
				{					
					struct param_sem *ptr;	
					struct varDeclParam *vptr;				
					ptr = createParam( createIdList( $3 ), createPType( VOID_t ) );
					vptr = createVarDeclParam( ptr, 0 );	
					addVarDeclParam( $1, vptr );
					$$ = $1; 
					if(scope == 0) fprintf(ByteCode,".field public static %s %s\n",$3, asm_type[tmptype] );
					strcpy(ID_tmp[scope][nextNum], $3);
					//printf("name: %s[%d][%d]\n",ID_tmp[scope][nextNum],scope,nextNum);
					nextNum++;						
				}
                | identifier_list COMMA ID ASSIGN_OP logical_expression
				{
					struct param_sem *ptr;	
					struct varDeclParam *vptr;				
					ptr = createParam( createIdList( $3 ), createPType( VOID_t ) );
					vptr = createVarDeclParam( ptr, $5 );
					vptr->isArray = __TRUE;
					vptr->isInit = __TRUE;	
					addVarDeclParam( $1, vptr );	
					$$ = $1;
					if(scope == 0){ 
						fprintf(ByteCode,".field public static %s %s\n",$3, asm_type[tmptype] );
					}
					else{
						asmCoer(mapType($5->pType), tmptype);
						fprintf(ByteCode,"	%sstore %d\n",asm_type_s[tmptype],nextNum);
					}
					strcpy(ID_tmp[scope][nextNum], $3);
					ID_init[scope][nextNum] = 1;
					//printf("name: %s[%d][%d]\n",ID_tmp[scope][nextNum],scope,nextNum);
					nextNum++;		
				}
                | identifier_list COMMA array_decl ASSIGN_OP initial_array
				{
					struct varDeclParam *ptr;
					ptr = createVarDeclParam( $3, $5 );
					ptr->isArray = __TRUE;
					ptr->isInit = __TRUE;
					addVarDeclParam( $1, ptr );
					$$ = $1;	
				}
                | identifier_list COMMA array_decl
				{
					struct varDeclParam *ptr;
					ptr = createVarDeclParam( $3, 0 );
					ptr->isArray = __TRUE;
					addVarDeclParam( $1, ptr );
					$$ = $1;
				}
                | array_decl ASSIGN_OP initial_array
				{	
					$$ = createVarDeclParam( $1 , $3 );
					$$->isArray = __TRUE;
					$$->isInit = __TRUE;	
				}
                | array_decl 
				{ 
					$$ = createVarDeclParam( $1 , 0 ); 
					$$->isArray = __TRUE;
				}
                | ID ASSIGN_OP {notLoad=0;} logical_expression
				{
					struct param_sem *ptr;					
					ptr = createParam( createIdList( $1 ), createPType( VOID_t ) );
					$$ = createVarDeclParam( ptr, $4 );		
					$$->isInit = __TRUE;
					if(scope == 0){ 
						fprintf(ByteCode,".field public static %s %s\n",$1, asm_type[tmptype] );
					}
					else{
						asmCoer(mapType($4->pType), tmptype);
						fprintf(ByteCode,"	%sstore %d\n",asm_type_s[tmptype],nextNum);
					}
					strcpy(ID_tmp[scope][nextNum], $1);
					ID_init[scope][nextNum] = 1;
					//printf("name: %s[%d][%d]\n",ID_tmp[scope][nextNum],scope,nextNum);
					nextNum++;	
				}
                | ID 
				{
					struct param_sem *ptr;					
					ptr = createParam( createIdList( $1 ), createPType( VOID_t ) );
					$$ = createVarDeclParam( ptr, 0 );
					if(scope == 0) fprintf(ByteCode,".field public static %s %s\n",$1, asm_type[tmptype] );
					strcpy(ID_tmp[scope][nextNum], $1);
					//printf("name: %s[%d][%d]\n",ID_tmp[scope][nextNum],scope,nextNum);
					nextNum++;			
				}
                ;
		 
initial_array : L_BRACE literal_list R_BRACE { $$ = $2; }
			  ;

literal_list : literal_list COMMA logical_expression
				{
					struct expr_sem *ptr;
					for( ptr=$1; (ptr->next)!=0; ptr=(ptr->next) );				
					ptr->next = $3;
					$$ = $1;
				}
             | logical_expression
				{
					$$ = $1;
				}
             |
             ;

const_decl 	: CONST scalar_type const_list SEMICOLON
			{
				struct SymNode *newNode;				
				struct constParam *ptr;
				for( ptr=$3; ptr!=0; ptr=(ptr->next) ){
					if( verifyRedeclaration( symbolTable, ptr->name, scope ) == __TRUE ){//no redeclare
						if( ptr->value->category != $2->type ){//type different
							if( !(($2->type==FLOAT_t || $2->type == DOUBLE_t ) && ptr->value->category==INTEGER_t) ) {
								if(!($2->type==DOUBLE_t && ptr->value->category==FLOAT_t)){	
									fprintf( stdout, "########## Error at Line#%d: const type different!! ##########\n", linenum );
									semError = __TRUE;	
								}
								else{
									newNode = createConstNode( ptr->name, scope, $2, ptr->value );
									insertTab( symbolTable, newNode );
								}
							}							
							else{
								newNode = createConstNode( ptr->name, scope, $2, ptr->value );
								insertTab( symbolTable, newNode );
							}
						}
						else{
							newNode = createConstNode( ptr->name, scope, $2, ptr->value );
							insertTab( symbolTable, newNode );
						}
					}
				}
			}
			;

const_list : const_list COMMA ID ASSIGN_OP literal_const
			{				
				addConstParam( $1, createConstParam( $5, $3 ) );
				$$ = $1;
				//asmLiteral($5);
			}
		   | ID ASSIGN_OP literal_const
			{
				$$ = createConstParam( $3, $1 );
				//asmLiteral($3);
			}
		   ;

array_decl : ID dim 
			{
				$$ = createParam( createIdList( $1 ), $2 );
			}
		   ;

dim : dim ML_BRACE INT_CONST MR_BRACE
		{
			if( $3 == 0 ){
				fprintf( stdout, "########## Error at Line#%d: array size error!! ##########\n", linenum );
				semError = __TRUE;
			}
			else
				increaseArrayDim( $1, 0, $3 );			
		}
	| ML_BRACE INT_CONST MR_BRACE	
		{
			if( $2 == 0 ){
				fprintf( stdout, "########## Error at Line#%d: array size error!! ##########\n", linenum );
				semError = __TRUE;
			}			
			else{		
				$$ = createPType( VOID_t ); 			
				increaseArrayDim( $$, 0, $2 );
			}		
		}
	;
	
compound_statement : {scope++;}L_BRACE var_const_stmt_list R_BRACE
					{ 
						// print contents of current scope
						if( Opt_Symbol == 1 )
							printSymTable( symbolTable, scope );
							
						deleteScope( symbolTable, scope );	// leave this scope, delete...
						scope--; 
					}
				   ;

var_const_stmt_list : var_const_stmt_list statement	
				    | var_const_stmt_list var_decl
					| var_const_stmt_list const_decl
				    |
				    ;

statement : compound_statement
		  | simple_statement
		  | conditional_statement
		  | while_statement
		  | for_statement
		  | function_invoke_statement
		  | jump_statement
		  ;		

simple_statement :  variable_reference 
					{
						search($1);
						if(now_ID.scope != 0 && ID_init[now_ID.scope][now_ID.VarNum] == 1) 
							fprintf(ByteCode,"	%sstore %d\n",asm_type_s[now_ID.type],now_ID.VarNum);
					} 
					ASSIGN_OP logical_expression SEMICOLON
					{
						// check if LHS exists
						__BOOLEAN flagLHS = verifyExistence( symbolTable, $1, scope, __TRUE );
						// id RHS is not dereferenced, check and deference
						__BOOLEAN flagRHS = __TRUE;
						if( $4->isDeref == __FALSE ) {
							flagRHS = verifyExistence( symbolTable, $4, scope, __FALSE );
						}
						// if both LHS and RHS are exists, verify their type
						if( flagLHS==__TRUE && flagRHS==__TRUE )
							verifyAssignmentTypeMatch( $1, $4 );
						
						if($4->pType->type == INTEGER_t){
							if(now_ID.type == 1){ //float
								fprintf(ByteCode,"    i2f\n");
							}else if(now_ID.type == 2){ //double
								fprintf(ByteCode,"    i2d\n");
							}
						}
						else if($4->pType->type == FLOAT_t)
						{
							if(now_ID.type == 2){ //double
								fprintf(ByteCode,"    f2d\n");
							}
						}
						search($1);
						ID_init[now_ID.scope][now_ID.VarNum]=1;
						if(now_ID.scope==0) fprintf(ByteCode,"putstatic %s/%s %s\n",fileName,now_ID.name,asm_type[now_ID.type]);
    					else fprintf(ByteCode,"    %sstore %d\n",asm_type_s[now_ID.type],now_ID.VarNum);
					}
				 | PRINT { fprintf(ByteCode,"    getstatic java/lang/System/out Ljava/io/PrintStream;\n");} 
					 logical_expression SEMICOLON  
					 { 
					   fprintf(ByteCode,"    invokevirtual java/io/PrintStream/print(%s)V\n",asm_type[now_ID.type]); 
						verifyScalarExpr( $3, "print" );
				 	}
				 | READ {notLoad = 1; }variable_reference SEMICOLON 
					{ 
					 	if( verifyExistence( symbolTable, $3, scope, __TRUE ) == __TRUE )						
							verifyScalarExpr( $3, "read" ); 
					 	asmRead($3);
					 	notLoad = 0; 
				 }
				 ;
IF_stat:  IF L_PAREN conditional_if  R_PAREN {fprintf(ByteCode,"    ifeq L_if_else_%d\n",LabelIf);}  
			compound_statement ELSE_stat
		;

ELSE_stat:  ELSE {
					fprintf(ByteCode,"    goto L_if_exit_%d\n",LabelIf); 
					fprintf(ByteCode,"L_if_else_%d:\n",LabelIf);
				} compound_statement {fprintf(ByteCode,"L_if_exit_%d:\n",LabelIf); LabelIf++; }
			| {	fprintf(ByteCode,"L_if_else_%d:\n",LabelIf); LabelIf++; }
		// | %prec LESS_THEN_ELSE {	fprintf(ByteCode,"L_if_else_%d:\n",LabelIf); LabelIf++; }
		 ;
conditional_statement: IF_stat
					 ;
/*
conditional_statement : IF L_PAREN conditional_if  R_PAREN {fprintf(ByteCode,"    ifeq Lelse_%d\n",LabelIf);} 
						compound_statement 
						{	fprintf(ByteCode,"Lelse_%d:\n",LabelIf);
							//fprintf(ByteCode,"    goto Lexit_%d\n",LabelIf); 
							LabelIf++;
						}
					  | IF L_PAREN conditional_if  R_PAREN {fprintf(ByteCode,"    ifeq Lelse_%d\n",LabelIf);} 
					    compound_statement
							{	
								printf("hello\n");
								fprintf(ByteCode,"    goto Lexit_%d\n",LabelIf); 
								fprintf(ByteCode,"Lelse_%d:\n",LabelIf);
							} 
						ELSE   {fprintf(ByteCode,"Lexit_%d:\n",LabelIf); LabelIf++;}
						compound_statement
					  ;
			*/		  
conditional_if : logical_expression { verifyBooleanExpr( $1, "if" ); };;					  

				
while_statement : WHILE {fprintf(ByteCode,"L_while_begin_%d:\n",LabelWhile); } 
				  L_PAREN logical_expression { verifyBooleanExpr( $4, "while" ); } R_PAREN 
				  { inloop++; fprintf(ByteCode,"	ifeq L_while_exit_%d\n",LabelWhile);}	
				  compound_statement 
				  { inloop--; fprintf(ByteCode,"	goto L_while_begin_%d\n",LabelWhile);
				  	fprintf(ByteCode,"L_while_exit_%d:\n",LabelWhile); LabelWhile++;
				  }
				| { inloop++; } DO compound_statement WHILE L_PAREN logical_expression R_PAREN SEMICOLON  
					{ 
						 verifyBooleanExpr( $6, "while" );
						 inloop--; 
						
					}
				;


				
for_statement : FOR L_PAREN initial_expression SEMICOLON {fprintf(ByteCode,"L_For_begin_%d:\n",LabelFor);}
				control_expression SEMICOLON {
					fprintf(ByteCode,"	ifne L_For_true_%d\n",LabelFor);
					fprintf(ByteCode,"	goto L_For_exit_%d\n",LabelFor);
					fprintf(ByteCode,"L_For_increase_%d:\n",LabelFor);     }
				increment_expression R_PAREN  { inloop++; 
					fprintf(ByteCode,"	goto L_For_begin_%d\n",LabelFor);
					fprintf(ByteCode,"L_For_true_%d:\n",LabelFor);}
				compound_statement  { inloop--; fprintf(ByteCode,"	goto L_For_increase_%d\n",LabelFor);
					fprintf(ByteCode,"L_For_exit_%d:\n",LabelFor); LabelFor++;}
			  ;

initial_expression : initial_expression COMMA statement_for		
				   | initial_expression COMMA logical_expression
				   | logical_expression	
				   | statement_for
				   |
				   ;

control_expression : control_expression COMMA statement_for
				   {
						fprintf( stdout, "########## Error at Line#%d: control_expression is not boolean type ##########\n", linenum );
						semError = __TRUE;	
				   }
				   | control_expression COMMA logical_expression
				   {
						if( $3->pType->type != BOOLEAN_t ){
							fprintf( stdout, "########## Error at Line#%d: control_expression is not boolean type ##########\n", linenum );
							semError = __TRUE;	
						}
				   }
				   | logical_expression 
					{ 
						if( $1->pType->type != BOOLEAN_t ){
							fprintf( stdout, "########## Error at Line#%d: control_expression is not boolean type ##########\n", linenum );
							semError = __TRUE;	
						}
					}
				   | statement_for
				   {
						fprintf( stdout, "########## Error at Line#%d: control_expression is not boolean type ##########\n", linenum );
						semError = __TRUE;	
				   }
				   |
				   ;

increment_expression : increment_expression COMMA statement_for
					 | increment_expression COMMA logical_expression
					 | logical_expression
					 | statement_for
					 |
					 ;

statement_for 	: variable_reference 
					{
						search($1);
						if(now_ID.scope != 0 && ID_init[now_ID.scope][now_ID.VarNum] == 1) 
							fprintf(ByteCode,"	%sstore %d\n",asm_type_s[now_ID.type],now_ID.VarNum);
					} 
				  ASSIGN_OP logical_expression
					{
						// check if LHS exists
						__BOOLEAN flagLHS = verifyExistence( symbolTable, $1, scope, __TRUE );
						// id RHS is not dereferenced, check and deference
						__BOOLEAN flagRHS = __TRUE;
						if( $4->isDeref == __FALSE ) {
							flagRHS = verifyExistence( symbolTable, $4, scope, __FALSE );
						}
						// if both LHS and RHS are exists, verify their type
						if( flagLHS==__TRUE && flagRHS==__TRUE )
							verifyAssignmentTypeMatch( $1, $4 );
						
						if($4->pType->type == INTEGER_t){
							if(now_ID.type == 1){ //float
								fprintf(ByteCode,"    i2f\n");
							}else if(now_ID.type == 2){ //double
								fprintf(ByteCode,"    i2d\n");
							}
						}
						else if($4->pType->type == FLOAT_t)
						{
							if(now_ID.type == 2){ //double
								fprintf(ByteCode,"    f2d\n");
							}
						}
						search($1);
						ID_init[now_ID.scope][now_ID.VarNum]=1;
						if(now_ID.scope==0) fprintf(ByteCode,"putstatic %s/%s %s\n",fileName,now_ID.name,asm_type[now_ID.type]);
    					else fprintf(ByteCode,"    %sstore %d\n",asm_type_s[now_ID.type],now_ID.VarNum);
					}
					;
					 
					 
function_invoke_statement : ID L_PAREN logical_expression_list R_PAREN SEMICOLON
							{
								verifyFuncInvoke( $1, $3, symbolTable, scope );
								asmFuncInvoke($1);
							}
						  | ID L_PAREN R_PAREN SEMICOLON
							{
								verifyFuncInvoke( $1, 0, symbolTable, scope );
								asmFuncInvoke($1);
							}
						  ;

jump_statement : CONTINUE SEMICOLON
				{
					if( inloop <= 0){
						fprintf( stdout, "########## Error at Line#%d: continue can't appear outside of loop ##########\n", linenum ); semError = __TRUE;
					}
				}
			   | BREAK SEMICOLON 
				{
					if( inloop <= 0){
						fprintf( stdout, "########## Error at Line#%d: break can't appear outside of loop ##########\n", linenum ); semError = __TRUE;
					}
				}
			   | RETURN logical_expression SEMICOLON
				{
					verifyReturnStatement( $2, funcReturn );
				}
			   ;

variable_reference : ID
					{
						$$ = createExprSem( $1 );
						search($$); 
						if(!notLoad && ID_init[now_ID.scope][now_ID.VarNum] == 1){
							if(now_ID.scope==0) 
								fprintf(ByteCode,"getstatic %s/%s %s\n",fileName,$1,asm_type[now_ID.type]);
							else 
								fprintf(ByteCode,"	%sload %d\n",asm_type_s[now_ID.type],now_ID.VarNum);
						}
					}
				   | variable_reference dimension
					{	
						increaseDim( $1, $2 );
						$$ = $1;
					}
				   ;

dimension : ML_BRACE arithmetic_expression MR_BRACE
			{
				$$ = verifyArrayIndex( $2 );
			}
		  ;
		  
logical_expression : logical_expression OR_OP logical_term
					{
						asmArith( $1, OR_t, $3 );
						verifyAndOrOp( $1, OR_t, $3 );
						$$ = $1;
					}
				   | logical_term { $$ = $1; }
				   ;

logical_term : logical_term AND_OP logical_factor
				{
					asmArith( $1, AND_t, $3 );
					verifyAndOrOp( $1, AND_t, $3 );
					$$ = $1;
				}
			 | logical_factor { $$ = $1; }
			 ;

logical_factor : NOT_OP logical_factor
				{
					fprintf(ByteCode,"    iconst_1\n");
					fprintf(ByteCode,"    %sxor\n",asm_type_s[now_ID.type]);
					verifyUnaryNOT( $2 );
					$$ = $2;
				}
			   | relation_expression { $$ = $1; }
			   ;

relation_expression : arithmetic_expression relation_operator arithmetic_expression
					{
						asmArith2(  $1, $2, $3 );
						verifyRelOp( $1, $2, $3 );
						$$ = $1;
					}
					| arithmetic_expression { $$ = $1; }
					;

relation_operator : LT_OP { $$ = LT_t; }
				  | LE_OP { $$ = LE_t; }
				  | EQ_OP { $$ = EQ_t; }
				  | GE_OP { $$ = GE_t; }
				  | GT_OP { $$ = GT_t; }
				  | NE_OP { $$ = NE_t; }
				  ;

arithmetic_expression : arithmetic_expression add_op term
			{
				asmArith( $1, $2, $3 );
				verifyArithmeticOp( $1, $2, $3 );
				$$ = $1;
			}
           | relation_expression { $$ = $1; }
		   | term { $$ = $1; }
		   ;

add_op	: ADD_OP { $$ = ADD_t; }
		| SUB_OP { $$ = SUB_t; }
		;
		   
term : term mul_op factor
		{
			asmArith( $1, $2, $3 );
			if( $2 == MOD_t ) {
				verifyModOp( $1, $3 );
			}
			else {
				verifyArithmeticOp( $1, $2, $3 );
			}
			$$ = $1;
		}
     | factor { $$ = $1; }
	 ;

mul_op 	: MUL_OP { $$ = MUL_t; }
		| DIV_OP { $$ = DIV_t; }
		| MOD_OP { $$ = MOD_t; }
		;
		
factor : variable_reference
		{
			verifyExistence( symbolTable, $1, scope, __FALSE );
			$$ = $1;
			$$->beginningOp = NONE_t;
			struct SymNode *ptr;
			ptr = lookupSymbol( symbolTable, $1->varRef->id, scope, __FALSE );
			if(ptr->category == CONSTANT_t)
			{
				switch( ptr->attribute->constVal->category ) {
				 case INTEGER_t:
					fprintf(ByteCode,"	ldc %d\n",ptr->attribute->constVal->value.integerVal);
					break;
				 case FLOAT_t:
				 	fprintf(ByteCode,"	ldc %f\n",ptr->attribute->constVal->value.floatVal);
					break;
				case DOUBLE_t:
				 	fprintf(ByteCode,"	ldc2_w %lf\n",ptr->attribute->constVal->value.doubleVal);
					break;
				 case BOOLEAN_t:
				 	if( ptr->attribute->constVal->value.booleanVal == __TRUE ) 
					{	fprintf(ByteCode,"	iconst_1\n");
						break;
					}
					else
					{
						fprintf(ByteCode,"	iconst_0\n");
						break;
					}
				 case STRING_t:
				 	fprintf(ByteCode,"	ldc \"%s\"\n",ptr->attribute->constVal->value.stringVal);
					break;
				}
			}
			//printf("hello\n");
		}
	   | SUB_OP variable_reference
		{
			if( verifyExistence( symbolTable, $2, scope, __FALSE ) == __TRUE )
			verifyUnaryMinus( $2 );
			$$ = $2;
			$$->beginningOp = SUB_t;
		}		
	   | L_PAREN logical_expression R_PAREN
		{
			$2->beginningOp = NONE_t;
			$$ = $2; 
		}
	   | SUB_OP L_PAREN logical_expression R_PAREN
		{
			verifyUnaryMinus( $3 );
			$$ = $3;
			$$->beginningOp = SUB_t;
		}
	   | ID L_PAREN logical_expression_list R_PAREN
		{
			$$ = verifyFuncInvoke( $1, $3, symbolTable, scope );
			$$->beginningOp = NONE_t;
			asmFuncInvoke($1);
		}
	   | SUB_OP ID L_PAREN logical_expression_list R_PAREN
	    {
			$$ = verifyFuncInvoke( $2, $4, symbolTable, scope );
			$$->beginningOp = SUB_t;
		}
	   | ID L_PAREN R_PAREN
		{
			$$ = verifyFuncInvoke( $1, 0, symbolTable, scope );
			$$->beginningOp = NONE_t;
			asmFuncInvoke($1);
		}
	   | SUB_OP ID L_PAREN R_PAREN
		{
			$$ = verifyFuncInvoke( $2, 0, symbolTable, scope );
			$$->beginningOp = SUB_OP;
		}
	   | literal_const
	    {
			  $$ = (struct expr_sem *)malloc(sizeof(struct expr_sem));
			  $$->isDeref = __TRUE;
			  $$->varRef = 0;
			  $$->pType = createPType( $1->category );
			  $$->next = 0;
			  if( $1->hasMinus == __TRUE ) {
			  	$$->beginningOp = SUB_t;
			  }
			  else {
				$$->beginningOp = NONE_t;
			  }
			  if(scope != 0)
			  {
				  switch($1->category){
				    case INTEGER_t:
				        fprintf(ByteCode,"	ldc %d\n",$1->value.integerVal);
				        break;
				    case BOOLEAN_t:
				        fprintf(ByteCode,"    iconst_%d\n",$1->value.booleanVal);
				        break;
				    case STRING_t:
				        fprintf(ByteCode,"	ldc \"%s\"\n",$1->value.stringVal);
				        break;
				    case FLOAT_t:
				        fprintf(ByteCode,"	ldc %f\n",$1->value.floatVal);
				        break;
				    case DOUBLE_t:
				        fprintf(ByteCode,"	ldc2_w %lf\n",$1->value.doubleVal);
				        break;
	    		  }
	    	  }
		}
	   ;

logical_expression_list : logical_expression_list COMMA logical_expression
						{
			  				struct expr_sem *exprPtr;
			  				for( exprPtr=$1 ; (exprPtr->next)!=0 ; exprPtr=(exprPtr->next) );
			  				exprPtr->next = $3;
			  				$$ = $1;
						}
						| logical_expression { $$ = $1; }
						;

		  


scalar_type : INT { $$ = createPType( INTEGER_t ); tmptype=0; }
			| DOUBLE { $$ = createPType( DOUBLE_t ); tmptype=2; }
			| STRING { $$ = createPType( STRING_t ); tmptype=4;}
			| BOOL { $$ = createPType( BOOLEAN_t ); tmptype=3; }
			| FLOAT { $$ = createPType( FLOAT_t );  tmptype=1;}
			;
 
literal_const : INT_CONST
				{
					int tmp = $1;
					$$ = createConstAttr( INTEGER_t, &tmp );
				}
			  | SUB_OP INT_CONST
				{
					int tmp = -$2;
					$$ = createConstAttr( INTEGER_t, &tmp );
				}
			  | FLOAT_CONST
				{
					float tmp = $1;
					$$ = createConstAttr( FLOAT_t, &tmp );
				}
			  | SUB_OP FLOAT_CONST
			    {
					float tmp = -$2;
					$$ = createConstAttr( FLOAT_t, &tmp );
				}
			  | SCIENTIFIC
				{
					double tmp = $1;
					$$ = createConstAttr( DOUBLE_t, &tmp );
				}
			  | SUB_OP SCIENTIFIC
				{
					double tmp = -$2;
					$$ = createConstAttr( DOUBLE_t, &tmp );
				}
			  | STR_CONST
				{
					$$ = createConstAttr( STRING_t, $1 );
				}
			  | TRUE
				{
					SEMTYPE tmp = __TRUE;
					$$ = createConstAttr( BOOLEAN_t, &tmp );
				}
			  | FALSE
				{
					SEMTYPE tmp = __FALSE;
					$$ = createConstAttr( BOOLEAN_t, &tmp );
				}
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


int search(struct expr_sem* expr){
	int i,j=scope;
	struct SymNode *node = 0;
	node = lookupSymbol( symbolTable, expr->varRef->id, j, __FALSE );
	if(node != 0){
		now_ID.scope = node->scope;
		now_ID.type = mapType(node->type);
		strcpy(now_ID.name, node->name);
		//printSymTable( symbolTable, now_ID.scope);
		//printf("%s(scope: %d)(type: %d)\n",node->name,now_ID.scope,now_ID.type);
	}
	if(node == 0) return -1;
	j = now_ID.scope;
	for(i=0; i<MAX_TMP; i++){
		//printf("ID_tmp[%d][%d]:%s and %s\n",j,i,ID_tmp[j][i], expr->varRef->id);
		if(strcmp(ID_tmp[j][i],expr->varRef->id) == 0)
		{
			now_ID.VarNum = i;
		//	printf("%s: %d\n",now_ID.name, now_ID.VarNum);
			return i;
		}
	}
	return -1;
}

int asmCoer(int type1,int type2)
{
	if(type1 == 0){
		if(type2 == 1){ //float
			fprintf(ByteCode,"    i2f\n");
			return 1;
		}else if(type2 == 2){ //double
			fprintf(ByteCode,"    i2d\n");
			return 2;
		}
	}
	else if(type1 == 1)
	{
		if(type2 == 2){ //double
			fprintf(ByteCode,"    f2d\n");
			return 2;
		}
	}
	else return -1;
}

void asmFunc(char *name, int Return , int flag, struct param_sem *params)
{
	if(strcmp(name,"main")==0)
	{
		fprintf(ByteCode,".method public static main([Ljava/lang/String;)V\n");
		fprintf(ByteCode,".limit stack %d\n",MAX_STACK);
	    fprintf(ByteCode,".limit locals %d\n",MAX_LOCALS);
	    fprintf(ByteCode,"    new java/util/Scanner\n");
	    fprintf(ByteCode,"    dup\n");
	    fprintf(ByteCode,"    getstatic java/lang/System/in Ljava/io/InputStream;\n");
	    fprintf(ByteCode,"    invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V\n");
	    fprintf(ByteCode,"    putstatic %s/_sc Ljava/util/Scanner;\n",fileName);
	}
	else if(flag)
	{
		fprintf(ByteCode,".method public static %s(", name);
		struct param_sem *parPtr;
		struct idNode_sem *idPtr;
		for( parPtr=params ; parPtr!=0 ; parPtr=(parPtr->next) ) {
			for( idPtr=(parPtr->idlist) ; idPtr!=0 ; idPtr=(idPtr->next) ) {
				fprintf(ByteCode,"%s",asm_type[mapType(parPtr->pType)]);
			}
		}
		fprintf(ByteCode,")%s\n",asm_type[Return]);
		fprintf(ByteCode,".limit stack %d\n",MAX_STACK);
	    fprintf(ByteCode,".limit locals %d\n",MAX_LOCALS);
	}
	else
	{
		fprintf(ByteCode,".method public static %s(", name);
		fprintf(ByteCode,")%s\n",asm_type[Return]);
		fprintf(ByteCode,".limit stack %d\n",MAX_STACK);
	    fprintf(ByteCode,".limit locals %d\n",MAX_LOCALS);
	}
}

void asmFuncTail(){
    fprintf(ByteCode,"    return\n");
    fprintf(ByteCode,".end method\n");
}

int mapType(struct PType *type)
{
	char buffer[50];
	memset( buffer, 0, sizeof(buffer) );
	struct PType *pType = type;

	switch( pType->type ) {
	 case INTEGER_t: return 0;
	 case FLOAT_t: return 1;
	 case DOUBLE_t: return 2;
	 case BOOLEAN_t: return 3;
	 case STRING_t: return 4;
	 case VOID_t: return 5;
	}
	return -1;
}


void asmRead(struct expr_sem *expr)
{
	fprintf(ByteCode,"    getstatic %s/_sc Ljava/util/Scanner;\n",fileName);
	int k = mapType(expr->pType);
    switch(expr->pType->type){
	    case INTEGER_t:
	    	fprintf(ByteCode,"    invokevirtual java/util/Scanner/nextInt()I\n");
	        break;
	    case FLOAT_t:
	    	fprintf(ByteCode,"    invokevirtual java/util/Scanner/nextFloat()F\n");
	        break;
	    case DOUBLE_t:
	    	fprintf(ByteCode,"    invokevirtual java/util/Scanner/nextDouble()D\n");
	        break;
	    case BOOLEAN_t:
	    	fprintf(ByteCode,"    invokevirtual java/util/Scanner/nextBoolean()Z\n");
	        break;
    } 
    //int kk =search(expr);
    if(now_ID.scope==0) fprintf(ByteCode,"putstatic %s/%s %s\n",fileName,now_ID.name,asm_type[now_ID.type]);
    else fprintf(ByteCode,"    %sstore %d\n",asm_type_s[k],now_ID.VarNum);
}

void asmArith( struct expr_sem *op1, OPERATOR operator, struct expr_sem *op2)
{
	int k1 = mapType(op1->pType), k2 =mapType(op2->pType);
	if(k1 != k2){ //need coersion
		if(k1 < k2){ //k1 need to turn type=>store k2 ,turn k1, push k2
			fprintf(ByteCode,"    %sstore %d\n",asm_type_s[k2],TMP1);
			asmCoer(k1,k2);
			fprintf(ByteCode,"    %sload %d\n",asm_type_s[k2],TMP1);
			k1 = k2;
		}
		else{ //k2 need to turn type
			asmCoer(k2,k1);
		}
	}
	asmOperator(operator, k1);
}

void asmOperator(OPERATOR operator, int k){
	switch(operator){
		case ADD_t:
			fprintf(ByteCode,"    %sadd\n",asm_type_s[k]);
			return;
		case SUB_t:
			fprintf(ByteCode,"    %ssub\n",asm_type_s[k]);
			return;
		case MUL_t:
			fprintf(ByteCode,"    %smul\n",asm_type_s[k]);
			return;
		case DIV_t:
			fprintf(ByteCode,"    %sdiv\n",asm_type_s[k]);
			return;
		case MOD_t:
			fprintf(ByteCode,"    %srem\n",asm_type_s[k]);
			return;
		case AND_t:
			fprintf(ByteCode,"    %sand\n",asm_type_s[k]);
			return;
		case OR_t:
			fprintf(ByteCode,"    %sor\n",asm_type_s[k]);
			return;
		case NOT_t:
			fprintf(ByteCode,"    %sxor\n",asm_type_s[k]);
			return;
	}
}

void asmArith2( struct expr_sem *op1, OPERATOR operator, struct expr_sem *op2)
{
	int k1 = mapType(op1->pType), k2 =mapType(op2->pType);
	if(k1 != k2){ //need coersion
		if(k1 < k2){ //k1 need to turn type=>store k2 ,turn k1, push k2
			fprintf(ByteCode,"    %sstore %d\n",asm_type_s[k2],TMP1);
			asmCoer(k1,k2);
			fprintf(ByteCode,"    %sload %d\n",asm_type_s[k2],TMP1);
			k1 = k2;
		}
		else{ //k2 need to turn type
			asmCoer(k2,k1);
		}
	}
	asmRelationalOperator(operator, k1);
}

void asmRelationalOperator(OPERATOR operator, int k){ //k:type
	if(k==0) fprintf(ByteCode,"    isub\n");
	else if(k==1) fprintf(ByteCode,"    %fcmpl\n");
	else fprintf(ByteCode,"    %dcmpl\n");
	switch(operator){
		case LT_t:
			fprintf(ByteCode,"    iflt L_Bool_TRUE_%d\n",LabelBool);
			break;
		case LE_t:
			fprintf(ByteCode,"    ifle L_Bool_TRUE%d\n",LabelBool);
			break;
		case EQ_t:
			fprintf(ByteCode,"    ifeq L_Bool_TRUE_%d\n",LabelBool);
			break;
		case GE_t:
			fprintf(ByteCode,"    ifge L_Bool_TRUE_%d\n",LabelBool);
			break;
		case GT_t:
			fprintf(ByteCode,"    ifgt L_Bool_TRUE_%d\n",LabelBool);
			break;
		case NE_t:
			fprintf(ByteCode,"    ifne L_Bool_TRUE_%d\n",LabelBool);
			break;
	}
	fprintf(ByteCode,"    iconst_0 ; false = 0\n");
	fprintf(ByteCode,"    goto L_Bool_FALSE_%d\n",LabelBool);
	fprintf(ByteCode,"L_Bool_TRUE_%d:\n",LabelBool);
	fprintf(ByteCode,"    iconst_1 ; true = 1\n");
	fprintf(ByteCode,"L_Bool_FALSE_%d:\n",LabelBool);
	LabelBool++;
}

void asmFuncReturn(int k)
{	
	//if(k==5)  fprintf(ByteCode,"	return\n");
	fprintf(ByteCode,"	%sreturn\n",asm_type_s[k]);
}

void asmFuncInvoke(char * name)
{
	int i,j=scope;
	struct SymNode *node = 0;
	node = lookupSymbol( symbolTable, name, 0, __FALSE );
	fprintf(ByteCode,"	invokestatic %s/%s(",fileName,name);
	struct PTypeList *listPtr;
	// compare each parameter
	for( listPtr=(node->attribute->formalParam->params); (listPtr!=0) ; listPtr=(listPtr->next)) {
		fprintf(ByteCode,"%s",asm_type[mapType(listPtr->value)]);
	}
	fprintf(ByteCode,")%s\n",asm_type[mapType(node->type)]);
}

void asmLiteral(struct ConstAttr * a)
{
	 if(scope != 0)
	  {
		  switch(a->category){
		    case INTEGER_t:
		        fprintf(ByteCode,"	ldc %d\n",a->value.integerVal);
		        break;
		    case BOOLEAN_t:
		        fprintf(ByteCode,"    iconst_%d\n",a->value.booleanVal);
		        break;
		    case STRING_t:
		        fprintf(ByteCode,"	ldc \"%s\"\n",a->value.stringVal);
		        break;
		    case FLOAT_t:
		        fprintf(ByteCode,"	ldc %f\n",a->value.floatVal);
		        break;
		    case DOUBLE_t:
		        fprintf(ByteCode,"	ldc2_w %lf\n",a->value.doubleVal);
		        break;
		  }
	  }
	  else
	  {
		  switch(a->category){
		    case INTEGER_t:
		        fprintf(ByteCode," = %d\n",a->value.integerVal);
		        break;
		    case BOOLEAN_t:
		        fprintf(ByteCode," = %d\n",a->value.booleanVal);
		        break;
		    case STRING_t:
		        fprintf(ByteCode," = \"%s\"\n",a->value.stringVal);
		        break;
		    case FLOAT_t:
		        fprintf(ByteCode," = %f\n",a->value.floatVal);
		        break;
		    case DOUBLE_t:
		        fprintf(ByteCode," = %lf\n",a->value.doubleVal);
		        break;
		  }
	  } 
}
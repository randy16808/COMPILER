#include <stdio.h>
#include <stdlib.h>
#include "header.h"
#include "symtab.h"
#include <string.h>

extern int yyparse();
extern FILE* yyin;

extern struct SymTable *symbolTable;
extern struct PType *funcReturn;
extern char fileName[256];
extern FILE * ByteCode;

extern __BOOLEAN semError; 

int  main( int argc, char **argv )
{
	if( argc == 1 )
	{
		yyin = stdin;
	}
	else if( argc == 2 )
	{
		FILE *fp = fopen( argv[1], "r" );
		if( fp == NULL ) {
				fprintf( stderr, "Open file error\n" );
				exit(-1);
		}
		yyin = fp;
	}
	else
	{
	  	fprintf( stderr, "Usage: ./parser [filename]\n" );
   		exit(0);
 	} 

 	char tmp1[50];
 	char tmp2[50];
 	//strcpy(tmp1,"ou");
 	strcpy(fileName ,"output");
 	sprintf(tmp2,"output.j");
 	ByteCode = fopen(tmp2,"w");
 	fprintf(ByteCode,".class public %s\n",fileName);    
    fprintf(ByteCode,".super java/lang/Object\n");
    fprintf(ByteCode,".field public static _sc Ljava/util/Scanner;\n");

	symbolTable = (struct SymTable *)malloc(sizeof(struct SymTable));
	initSymTab( symbolTable );

	// initial function return recoder

	yyparse();	/* primary procedure of parser */

	if(semError == __TRUE){	
		fprintf( stdout, "\n|--------------------------------|\n" );
		fprintf( stdout, "|  There is no syntactic error!  |\n" );
		fprintf( stdout, "|--------------------------------|\n" );
	}
	else{
		fprintf( stdout, "\n|-------------------------------------------|\n" );
		fprintf( stdout, "| There is no syntactic and semantic error! |\n" );
		fprintf( stdout, "|-------------------------------------------|\n" );
	}

	exit(0);
}


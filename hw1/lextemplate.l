%{
#define LIST             { strncat(buf,yytext, sizeof(buf)-strlen(buf)-1); }
#define token(t)         { LIST; if (Opt_Token) printf("<\"%s\">\n",t); }
#define tokenString(t,s) { LIST; if(Opt_Token) printf("<%s:%s>\n",t,s); }
#define MAX_LINE_LENGTH 257
#define MAX_ID_LENGTH 257
int Opt_Source = 1;
int Opt_Token = 1;
int Opt_Statistic = 1;
int linenum = 1;
char buf[MAX_LINE_LENGTH];
struct id{
    char name[256];
    int fre;
    struct id *next;
};
struct id * head = NULL;
void count(char*);
struct id * create(char*);
%}

delimeter (\(|\)|\[|\]|\{|\}|;|,)
operator  (\+|\-|\*|\/|%|=|<|<=|!=|>|>=|==|&&|(\|\|)|!)


keyword (while|do|if|else|true|false|for|int|print|const|read|boolean|bool|void|float|double|string|continue|break|return)
letter  [A-Za-z]
digit   [0-9]
id      {letter}({letter}|{digit})*
integer 0|[1-9][0-9]*
string  \"([^\n\\\"]*(\\\\)*(\\\")*)*\"
ws      [ \t]+


%Start COMMENT
%Start PRAGMA

%%

\n {LIST; if(Opt_Source) printf("%d:%s\n",linenum, buf);
    ++linenum; buf[0]='\0'; BEGIN INITIAL;}

<COMMENT>(.)*   {LIST;} 
<COMMENT>"*/"   {LIST; BEGIN INITIAL;}
<INITIAL>"/*"   {LIST; BEGIN COMMENT;}
<INITIAL>"//"   {LIST; BEGIN COMMENT;} 
<PRAGMA>"/*"   {LIST; BEGIN COMMENT;}
<PRAGMA>"//"   {LIST; BEGIN COMMENT;}

 /*whitespace*/
{ws}            {LIST;}

 /*Pragma Directives*/
"#pragma token on"      {LIST; Opt_Token=1; BEGIN PRAGMA;}
"#pragma token off"     {LIST; Opt_Token=0; BEGIN PRAGMA;}
"#pragma source on"     {LIST; Opt_Source=1; BEGIN PRAGMA;}
"#pragma source off"    {LIST; Opt_Source=0; BEGIN PRAGMA;}
"#pragma statistic on"  {LIST; Opt_Statistic=1; BEGIN PRAGMA;}
"#pragma statistic off" {LIST; Opt_Statistic=0; BEGIN PRAGMA;}

 /*delimeter*/
<INITIAL>{delimeter} {tokenString("delim",yytext);}

 /*operator*/
<INITIAL>{operator} {token(yytext);}
 /*
 operator \+|\-|\*|/|%|=|<|<=|!=|>=|>|==|&&|(\|\|)|!

"+"     {token("+");}
"-"     {token("-");}
"*"     {token("*");}
"/"     {token("/");}
"%"     {token("%");}
"="     {token("=");}
"<"     {token("<");}
"<="    {token("<=");}
"!="    {token("!=");}
">="    {token(">=");}
">"     {token(">");}
"=="    {token("==");}
"&&"    {token("&&");}
"||"    {token("||");}
"!"     {token("!");}
 */

 /*keyword*/
<INITIAL>{keyword} {tokenString("KW", yytext);}

 /*identifiers*/
<INITIAL>{id}   {tokenString("id",yytext); count(yytext);}

 /*integer*/
<INITIAL>{integer} {tokenString("integer",yytext);}

 /*floatinf point */
<INITIAL>{digit}+\.{digit}+ {tokenString("float",yytext);}

 /*scientific notation*/
<INITIAL>{digit}+(\.{digit}+)?([Ee]([+\-])?{digit}+)? {tokenString("scientific",yytext);}
 
 /*string*/
 
<INITIAL>{string}   {
                char str[256];
                strcpy(str,yytext+1);
                str[yyleng-2]='\0';
                for(int i=0 ; i<strlen(str); i++)
                {
                    if(str[i]=='\\' && (str[i+1]=='\\' || str[i+1]== '"')){ str[i] ='\n'; i++;}
                }
                char * pch ;
                char s[256] = "";
                pch = strtok(str, "\n");
                while(pch != NULL)
                {
                    strcat(s, pch);
                    pch = strtok(NULL, "\n");
                }
                tokenString("string",s);
           }
 
. {  //unrecognized character
            fprintf(stderr, "Error at line %d: %s\n", linenum,yytext);
            exit(1);
        }

%%

int main(int argc, char ** argv)
{
    if(argc >= 2)
    {
        yyin = fopen(argv[1], "r");
        if(NULL == yyin)
        {
            fprintf(stderr, "Cannot open: %s\n", argv[1]);
            exit(-1);
        }
    }    
    else yyin = stdin;

    yylex();
    if(Opt_Statistic) //print frequency table
    {
        printf("frequencies of identifiers:\n");
        struct id* tmp = head;
        while(tmp)
        {
            printf("%s: %d\n", tmp->name, tmp->fre);
            tmp = tmp->next;
        }
   }

    exit(0);
}
 
void count(char * yytext)
{
    if(head == NULL){ head = create(yytext); }
    else
    {
        struct id* now = head;
        struct id* pre = NULL;
        while(now)
        {
            if(strcmp(now->name, yytext) == 0)
            {
                now->fre++; break;
            }
            pre = now;
            now = now->next;
        }
        if(now == NULL)
        {
            now = create(yytext);
            pre->next = now;
        }
    }
}
 
struct id* create(char* yytext)
{
    struct id* tmp = (struct id*)malloc(sizeof(struct id));
    tmp->next = NULL;
    tmp->fre = 1;
    memset(tmp->name, 0, 256);
    strncpy(tmp->name, yytext, strlen(yytext));
    return tmp;
}
 
        

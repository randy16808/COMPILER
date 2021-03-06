%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "symtab.h"
    #include "header.h"
    const char* DUMP_FORMAT = "%-33s%-11s%-12s%-19s%-24s\n";
    const char* KIND_NAME[10];
    const char* TYPE_NAME[] = {"int","float","double","bool","string","void"};
    const char* OP_NAME[] = {"**ERROR**","arithmetic 'Add'","arithmetic 'Sub'","arithmetic 'Mul'","arithmetic 'Div'","arithmetic 'Mod'","relational"};
    const char* OP_NEED_TYPE[] = {"**ERROR**","numeric operand","numeric operand","numeric operand","numeric operand","integer operand","numeric operand"};
    extern int Opt_Source;
    extern int Opt_Token;
    extern int Opt_Statistic;
    extern int Opt_Symbol;
    extern int linenum;             /* declared in lex.l */
    extern FILE *yyin;              /* declared by lex */
    extern char *yytext;            /* declared by lex */
    extern char buf[256];           /* declared in lex.l */
    char tempBuf[MAX_STRING_SIZE];
    int funcTypeTmp;
    bool supress = false;
    bool coercion_check_only = false; // when doing coercion, DO NOT modify source typeStruct_t
    int scopeTypeTmp = SCOPE_ERROR;
    int lastStmtType = STMT_ERROR; // last stmt type, for 'return check'
    
    ErrorPool errorPool;
    funcOption_t funcOption;
    typeStruct_t typeTmp;
    string_pool str_pool;

    void add_errorPool(const char* msg);

    void init_typeStruct(typeStruct_t* pType){
        pType->v_type = T_ERROR;
        pType->dim = 0;
        pType->is_const = false;
    }

    void init_funcOption()
    {
        funcOption.isDeclare = false;
        funcOption.pTypeList = NULL;
        funcOption.retType = T_ERROR;
        funcOption.paramAddOK = false;
    }
 
    void tmp_type_init_scalar(int type)
    {
        init_typeStruct(&typeTmp);
        typeTmp.val = 0;
        typeTmp.v_type = type;
    }

    void tmp_type_add_dim(int dim)
    {
        typeTmp.dims[typeTmp.dim] = dim;
        ++typeTmp.dim;
    }

// type string generate
    void generate_type_string(char* buf,typeStruct_t* pType)
    {
        char indice[MAX_STRING_SIZE];
        bzero(buf,MAX_STRING_SIZE);
        strcat(buf,TYPE_NAME[pType->v_type]);
        for(int i=0;i<pType->dim;i++){
            snprintf(indice,MAX_STRING_SIZE,"[%d]",pType->dims[i]);
            strcat(buf,indice);
        }
    }

// init a symbol table
    void init_table(symbol_table* p_table,int level)
    {
        p_table->end = 0;
        p_table->level = level;
        if(scopeTypeTmp != SCOPE_ERROR){
            p_table->scopeType = scopeTypeTmp;
            scopeTypeTmp = SCOPE_ERROR;
        }
        else{
            p_table->scopeType = SCOPE_NORMAL;
        }
    }
   
    symbol_table* push_table(int mode);
    symbol_table* pop_table();
    void init_table_stack();
    bool check_typeList_consist(typeList_t* a,typeList_t* b,bool allow_coercion);

    void add_ptr(void* str)
    {
        int i = str_pool.end;
        str_pool.data[i] = str;
        str_pool.end +=1 ;
    }
    
// Add string
    void add_string(char* str)
    {
        add_ptr(str);
    }

// output error msg
    void print_error(const char* msg)
    {
        if(!supress){
            snprintf(tempBuf,MAX_STRING_SIZE,"##########Error at Line #%d: %s##########\n",linenum,msg);
            fprintf(ERR_FD,tempBuf);
            char* str = (char*)malloc(strlen(tempBuf)+1);
            strcpy(str,tempBuf);
            add_string(str);
            add_errorPool(str);
        }
    }
    // declare table
    table_stack stk;
    // get current table
    symbol_table* current_table();
  
// return true if not an array
    bool check_dimension(typeStruct_t* pType)
    {
        for(int i=0;i<pType->dim;i++){
            if(pType->dims[i] <= 0){
                print_error("Array index must greater than 0");
                return false;
            }
        }
        return true;
    }

// check variable entry exist within the same scope  
    bool check_name_used(const char* name,int kind){ 
       
        char errbuf[MAX_STRING_SIZE];
        symbol_table* table = current_table(); 
        for(int i=0;i<table->end;i++){
            s_table_entry* ent = &table->entries[i]; 
            if(strcmp(name,ent->name)==0){
                snprintf(errbuf,MAX_STRING_SIZE,"%s identifier '%s' has been declared in the same scope.",KIND_NAME[kind],name);
                print_error(errbuf);
                return true;
            }
        }
        return false;
    }

//check function declaration
    bool check_FuncDeclaration(const char* name, int kind, symbol_table * table)
    {
        char errbuf[MAX_STRING_SIZE];
        for(int i=0;i<table->end;i++)
        {
            s_table_entry* ent = &table->entries[i];
            if(strcmp(name,ent->name)==0)
            {
                // check if previous one is declare-function
                if(ent->kind==K_FUNCTION && ent->isDeclare){
                    // check return type
                    if(funcOption.retType != T_ERROR && ent->type.v_type != funcOption.retType){
                        print_error("Return type of function definition is not match with declaration");
                        return false;
                    }
                    // check param type
                    if(!check_typeList_consist(&ent->attr.param_list,funcOption.pTypeList,false)) {
                        print_error("Parameter types of function definition is not match with declaration");
                        return false;
                    }
                    funcOption.paramAddOK = true;
                    ent->isMatch = true;
                    return false;
                }
                else{
                    snprintf(errbuf,MAX_STRING_SIZE,"Function identifier '%s' has been used before.",name);
                    print_error(errbuf);
                    return false;
                }
            }
        }
        return true;
    }

// check addable
    bool check_entry_addable(const char* name,int kind){
        bool result = true;
        // type error check
        if(typeTmp.v_type < 0) result = false;
        
        // kind check
        if(kind == K_FUNCTION)
        {
            //check function addable
            if(funcOption.isDeclare  && check_name_used(name,kind)) result = false;
            else if(!check_FuncDeclaration(name,kind, current_table()))  result = false;
        }
        else
        {
            if(typeTmp.v_type == T_VOID)
            {
                print_error("'void' cannot be used to declare variable/constant/parameter");
                result = false;
            }
            if(check_name_used(name,kind)) result = false;
            if(!check_dimension(&typeTmp)) result = false;
        }

        return result;
    } 
    
// add table entry with name, option : extra augment -- CANCEL
    s_table_entry* create_basic_entry(const char* name,int kind)
    {  
        if(!check_entry_addable(name,kind)) return NULL;
        symbol_table* table = current_table();
        s_table_entry* ent =&table->entries[table->end];
        ent->level = table->level;
        table->end++;
        bzero(ent->name,sizeof(ent->name));
        strncpy(ent->name,name,32);
        ent->kind = kind;
      
        if(kind == K_FUNCTION)
        {
            init_typeStruct(&ent->type);
            ent->type.v_type = funcOption.retType;
            ent->isDeclare = funcOption.isDeclare;
        }
        else
        {
            memcpy(&ent->type,&typeTmp,sizeof(typeTmp));
            ent->isDeclare = false;
        }
        typeTmp.dim = 0;  // clear array
        ent->option = 0;
        ent->isMatch = false;
        return ent;
    }
    // make constant declaration  
    s_table_entry* constant_declare(const char* name,typeStruct_t type) 
    {
        s_table_entry* ent =  create_basic_entry(name,K_CONSTANT);
        if(ent){ // if that entry has successfully added
            switch(type.v_type){
                case T_INT: ent->attr.int_val = type.val; break;
                case T_FLOAT: ent->attr.float_val = type.fval; break;
                case T_DOUBLE: ent->attr.double_val = type.dval; break;
                case T_BOOL: ent->attr.bool_val = type.val; break;
                case T_STRING: ent->attr.string_val = type.sval; break;
            }
        }
        return ent;
    }
    bool check_dim_consist(typeStruct_t* a,typeStruct_t* b)
    {
        if(a->dim != b->dim) return false;

        for(int i=0;i<a->dim;i++)
            if(a->dims[i]!=b->dims[i])return false;

        return true;
    }
    // check and try to do coercion
    // it will change later to fit earlier
    bool do_type_coercion(typeStruct_t* pTarget,typeStruct_t* pSource)
    {
        bool result = false;
        int sourceType = pSource->v_type;
        int targetType = pTarget->v_type;
        if(targetType == T_ERROR || sourceType == T_ERROR){
            // no coercion when someone is T_ERROR
            return false;
        }
        else if(!check_dim_consist(pTarget,pSource)){
            // dim must be the same to do coercion
            print_error("Caonnot perform coercion on arrays with different size or dimension");
            return false;
        }
        else if(targetType == sourceType){
            // same type do not need coercion
            result =  true;
        }
        else{
            if(targetType == T_FLOAT)
            {
                if(sourceType==T_INT){
                    if(!coercion_check_only){
                        if(pSource->is_const){ // only constant can convert value
                            pSource->fval = (float)pSource->val;
                        }
                        pSource->v_type = T_FLOAT;
                    }
                    result = true;
                }
                else result = false;
            }
            else if(sourceType==T_DOUBLE)
            {
                if(!coercion_check_only) pSource->v_type = T_DOUBLE;
                if(sourceType == T_INT) //INT to DOUBLE
                {
                    if(pSource->is_const) pSource->dval = (double)pSource->val;
                    result = true;
                }
                else if(sourceType == T_FLOAT) //FLOAT to DOUBLE
                {
                    if(pSource->is_const) pSource->dval = (double)pSource->fval;
                    result =  true;
                }
                else result = false;
            }
        }
        if(!result){
            char msg[MAX_STRING_SIZE];
            snprintf(msg,MAX_STRING_SIZE,"'%s' cannot be coerced into '%s'",TYPE_NAME[sourceType],TYPE_NAME[targetType]);
            print_error(msg);
        }
        return result;
    }

// check if that assignment is ok, a cannot be const
    bool check_variable_assignable(typeStruct_t* a,typeStruct_t* b)
    {
        if(a->v_type == T_ERROR || b->v_type == T_ERROR) return false;
        if(a->dim > 0 || b->dim > 0)
        {
            print_error("Array cannot be involved in assignment");
            return false;
        }
        if(a->is_const){
            print_error("Cannot do assignment to constant variable");
            return false;
        }
        return do_type_coercion(a,b);
    }

// check if two type struct is all the same, but errors are distinct
    bool check_typeStruct_same(typeStruct_t* a,typeStruct_t* b)
    {
        if(a->v_type == T_ERROR || b->v_type == T_ERROR) return false;
        
        if((a->v_type != b->v_type) || (a->dim != b->dim) ) return false;
        for(int i=0;i<a->dim;i++)
            if(a->dims[i]!=b->dims[i]) return false;
        return true;
    }

//check typeList consist   
     bool check_typeList_consist(typeList_t* a,typeList_t* b,bool allow_coercion)
     {
        if(a->end != b->end) return false;
        for(int i=0 ; i<a->end ; i++)
        {
            if(allow_coercion)
            {
                if(!do_type_coercion(&a->data[i],&b->data[i])) return false;
            }
            else if(!check_typeStruct_same(&a->data[i],&b->data[i])) return false;
        }
        return true;
    } 

// check consistant
    int check_function_call_consist(const char* name,typeList_t* pTypeList)
    {
        int ret_type = T_ERROR; 
        int errType = ENTRY_NOT_FOUND; 
        bool old_supress = supress;
        supress = true;
        symbol_table* pTable =  &stk.tables[0];  //global table
        for(int i=0;i<pTable->end;i++){
            s_table_entry* ent = &pTable->entries[i];
            if(strcmp(name,ent->name)==0)
            {
                if(ent->kind != K_FUNCTION) errType = NOT_FUNCTION;
                else
                {
                    if(check_typeList_consist(&ent->attr.param_list,pTypeList,true))
                    {
                        ret_type = ent->type.v_type;
                    }
                    else   // parameter mismatch
                    {
                        ret_type = T_ERROR;
                        errType = ENTRY_MISMATCH;
                    }
                }
                break;
            }
        }
        supress = old_supress;
        char errmsg[MAX_STRING_SIZE];
        if(ret_type < 0)
        {
            switch(errType){
                case ENTRY_NOT_FOUND: snprintf(errmsg,MAX_STRING_SIZE,"Function '%s' isn't declared.",name); break;
                case ENTRY_MISMATCH: snprintf(errmsg,MAX_STRING_SIZE,"Function call '%s' error : parameter types are not consist with its declaration",name); break;
                case NOT_FUNCTION: snprintf(errmsg,MAX_STRING_SIZE,"symbol '%s' is not a function.",name); break;
                default: bzero(errmsg,MAX_STRING_SIZE); break;
            }
            print_error(errmsg);
        }
        return ret_type;
    }

// generate constant attribute
    void generate_constant_attr_string(char* buf,s_table_entry* ent)
    {
        size_t buf_sz = MAX_STRING_SIZE;
        switch(ent->type.v_type){
            case T_INT:  snprintf(buf,buf_sz,"%d",ent->attr.int_val); break;
            case T_FLOAT: snprintf(buf,buf_sz,"%f",ent->attr.float_val); break;
            case T_DOUBLE: snprintf(buf,buf_sz,"%lf",ent->attr.double_val); break;
            case T_BOOL: snprintf(buf,buf_sz,"%s",ent->attr.int_val == 1 ? "true" : "false"); break;
            case T_STRING: snprintf(buf,buf_sz,"%s",ent->attr.string_val); break;
        }
    }

// generate param list
    void generate_function_attr_atring(char* buf,s_table_entry* ent)
    {
        bzero(buf,MAX_STRING_SIZE);
        // for each type
        for(int i=0;i<ent->attr.param_list.end;i++)
        {    
            if(i!=0) strcat(buf,","); // comma
            typeStruct_t* pType = &ent->attr.param_list.data[i];
            // scalar type
            strcat(buf,TYPE_NAME[pType->v_type]);
            // array indice
            for(int j=0;j<pType->dim;j++){
                snprintf(tempBuf,MAX_STRING_SIZE,"[%d]",pType->dims[j]);
                strcat(buf,tempBuf);
            }
        }
    }

// attr string generate
    void generate_attr_string(char* buf,s_table_entry* ent)
    {
        bzero(buf,MAX_STRING_SIZE);
        switch(ent->kind)
        {
            case K_CONSTANT: generate_constant_attr_string(buf,ent); break;
            case K_FUNCTION: generate_function_attr_atring(buf,ent); break;
        }
    }

// table dump
    void dump_table(symbol_table* table){
        if(!Opt_Symbol){
            return;
        }
        char levelStr[MAX_STRING_SIZE];
        char typeStr[MAX_STRING_SIZE];
        char attrStr[MAX_STRING_SIZE];
        
        printf("=======================================================================================\n");
        printf(DUMP_FORMAT,"Name","Kind","Level","Type","Attribute");
        printf("---------------------------------------------------------------------------------------\n");
        for(int i=0;i<table->end;i++){
            s_table_entry* ent = &table->entries[i];
            snprintf(levelStr,MAX_STRING_SIZE,"%d(%s)",ent->level,ent->level > 0 ? "local" : "global" );
            generate_type_string(typeStr,&ent->type);
            generate_attr_string(attrStr,ent);
            printf(DUMP_FORMAT,ent->name,KIND_NAME[ent->kind],levelStr,typeStr,attrStr);
        }
        printf("=======================================================================================\n");
    }

    void run_function_declare_definition_test();
    void program_reduce_process()
    {
        dump_table(current_table());
        run_function_declare_definition_test();
    }
   
    bool operatorTypeChecker(typeStruct_t* pType, int op, bool left)
    {
        bool result = false;
        if(op == OP_MOD)
        {
            if(pType->v_type == T_INT) result = true;
        }
        else if (op==OP_ADD || op==OP_SUB || op==OP_MUL || op==OP_DIV || op==OP_RELATION)
        {
            if(pType->v_type == T_INT || pType->v_type == T_FLOAT || pType->v_type == T_DOUBLE) result = true;
            else result = false;
        }
        if(!result)
        {
            if(pType->v_type != T_ERROR)
            {
                char errmsg[MAX_STRING_SIZE];
                if(left)
                {
                    snprintf(errmsg,MAX_STRING_SIZE,"%s operator expect '%s' on left-hand-side, but '%s' provided.",OP_NAME[op],OP_NEED_TYPE[op],TYPE_NAME[pType->v_type]);
                }
                else
                {
                    snprintf(errmsg,MAX_STRING_SIZE,"%s operator expect '%s' on right-hand-side, but '%s' provided.",OP_NAME[op],OP_NEED_TYPE[op],TYPE_NAME[pType->v_type]);
                }
                print_error(errmsg);
            }   
        }
        return result;
    }

    int extractGreaterType(int a,int b)
    {
        if(a == T_DOUBLE || b == T_DOUBLE) return T_DOUBLE;
        else if(a == T_FLOAT || b == T_FLOAT) return T_FLOAT;
        else return T_INT;
    }

    bool noArrayCheck(typeStruct_t* pType)
    {
        if(pType->v_type != T_ERROR && pType->dim > 0){
            char errmsg[MAX_STRING_SIZE];
            snprintf(errmsg,MAX_STRING_SIZE,"%d dimension of %s array type is not allow here.",pType->dim,TYPE_NAME[pType->v_type]);
            print_error(errmsg);
            return false;
        }
        return true;
    }

// check whether given expr has correct type for binary numeric operator, return the final computation type
    int binary_numeric_expr_check(typeStruct_t* lhs,typeStruct_t* rhs,int op)
    {
        int ret_type = T_ERROR;
        if(!noArrayCheck(lhs)||!noArrayCheck(rhs)) { ret_type = T_ERROR; }  // check if has array type
        else if(!operatorTypeChecker(lhs,op,true)) { ret_type = T_ERROR; }  // LHS
        else if(!operatorTypeChecker(rhs,op,false)) { ret_type = T_ERROR; }  // RHS
        else { // all correct,  set return type
            if(op == OP_RELATION) ret_type = T_BOOL;
            else if(op == OP_MOD) ret_type = T_INT;
            else if(op==OP_ADD || op==OP_SUB || op==OP_MUL || op==OP_DIV) ret_type = extractGreaterType(lhs->v_type,rhs->v_type);
        }
        return ret_type;
    }

// find variable/parameter/constant type for that name
    void fill_type_for_name(typeStruct_t* pType,const char* name)
    {
        bool found = false;
        // search each table
        for(int i = stk.end-1; i>=0 && !found ;i-- )
        {
            symbol_table* pTable = &stk.tables[i];
            for(int j = pTable->end-1;j>=0&&!found;j--)
            {
                s_table_entry* ent = &pTable->entries[j];
                if(ent->kind == K_PARAMETER || ent->kind==K_VARIABLE || ent->kind == K_CONSTANT)
                {
                    if(strcmp(ent->name, name) == 0)
                    {
                        found = true;
                        pType->v_type = ent->type.v_type;
                        pType->dim = ent->type.dim;
                        memcpy(&pType->dims,&ent->type.dims,sizeof(pType->dims));
                        if(ent->kind == K_CONSTANT) pType->is_const = true;
                        else pType->is_const = false;
                    }
                }
            }
        }
        if(!found)
        {
            char errmsg[MAX_STRING_SIZE];
            snprintf(errmsg,MAX_STRING_SIZE,"variable/parameter/constant identifier '%s' not declared.",name);
            print_error(errmsg);
            pType->v_type = T_ERROR;
        }
        return ;
    }

    void perform_negation(typeStruct_t* pType)
    {
       
        if(pType->v_type == T_FLOAT) pType->fval *= -1;
        else if(pType->v_type == T_DOUBLE)  pType->dval *= -1;
        else if(pType->v_type == T_INT) pType->val *= -1;
    }

    // check if array initializer type and size are OK
    bool check_array_initializer(typeStruct_t* pIndice,typeList_t* pList){
        int total = 1;
        // run each dim and check the dim
        for(int i=0;i<pIndice->dim;i++){
            int dim = pIndice->dims[i];
            if(dim <= 0){
                print_error("Array initializer:Array index must greater than or equal to 0");
                return false;
            }
            total *= dim;
        }
        // check count exceeds?
        if(pList->end > total){
            print_error("Array initializer:The number of initializer exceeds the array size");
            return false;
        }
        // check expression match
        // valid struct
        typeStruct_t valid;
        init_typeStruct(&valid);
        valid.v_type = pIndice->v_type;
        // coercion only need check
        bool old_check = coercion_check_only;
        coercion_check_only = true;
        for(int i=0;i<pList->end;i++){
            typeStruct_t* pType = &pList->data[i];
            if(!do_type_coercion(&valid,pType)){
                // convert fail
                return false;
            }
        }
        coercion_check_only = old_check;
        // pass
        return true;
    }
    
    //
      bool checkFlag(int checked,int flag){
        return (flag & checked) > 0;
    }
    // check current table or outter table has that scope rule
    bool check_scope_match(int scopeRule){
        for(int i = stk.end-1;i>=0;i-- ){
            symbol_table* pTable = &stk.tables[i];
            if(checkFlag(pTable->scopeType,scopeRule)){
                return true;
            }
        }
        return false;
        
    }

// process loop check
    bool process_loop_check()
    {
        if(!check_scope_match(SCOPE_LOOP)){
            print_error("'break/continue' can only used inside loops");
        }
    }

// process return-able check
    bool process_return_function_check(typeStruct_t* pType)
    {
        // check-in function
        if(!check_scope_match(SCOPE_FUNCTION)){
            print_error("'return' can only used inside non-void function");
            return false;
        }
        // scalar check
        if(pType->dim > 0){
            print_error("return value should not be an array type");
            return false;
        }
        // check function type
        bool old_check = coercion_check_only;
        coercion_check_only = true;
        typeStruct_t target;
        init_typeStruct(&target);
        target.v_type = funcOption.retType;
        if(!do_type_coercion(&target,pType)){
            return false;
        }
        coercion_check_only = old_check;
        return true;
    }

//check if the type is scalar
    bool check_scalarExpr(typeStruct_t* pType)
    {
        if(pType->v_type==T_ERROR) return false;
        if(pType->dim > 0)
        {
            print_error("Cannot use 'print' on an array variable.");
            return false;
        }
        return true; 
    }
   
// check if expr is printable
    bool check_printable(typeStruct_t* pType)
    {
       
        if(pType->v_type==T_ERROR) return false;
        if(pType->dim > 0)
        {
            print_error("Cannot use 'print' on an array variable.");
            return false;
        }
        return true; 
    }

// check if var_ref is readable for KW_READ
    bool check_readable(typeStruct_t* pType)
    {
        if(pType->v_type==T_ERROR) return false;
        // constant can't read
        if(pType->is_const){
            print_error("Cannot use 'read' on a constant variable.");
            return false;
        }
        // cannot read array
        if(pType->dim > 0){
            print_error("Cannot use 'read' on an array variable.");
            return false;
        }
        return true;
    }
    
    void go_to_loop_scope(){
        scopeTypeTmp = SCOPE_LOOP;
    }
    
    %}

%union {
    int value;
    char* text;
    int type;
    typeStruct_t typeStruct;
    typeList_t typeList;
}
%nonassoc <type> INT BOOL FLOAT DOUBLE STRING CONST VOID
%nonassoc END
%nonassoc LESS_THAN_ELSE
%nonassoc KW_ELSE
%nonassoc ARITHMETIC_PARENTHESES
%nonassoc LOWER_THEN_ARITHMETIC_PARENTHESES
%nonassoc '(' ')'
%left OP_OR
%left OP_AND
%left OP_NOT
%nonassoc OP_LT OP_LE OP_EQ OP_GE OP_GT OP_NE
%left '+' '-'
%left '*' '/' '%'
%token KW_RETURN
%token KW_BREAK
%token KW_CONTINUE
%token KW_FOR
%token KW_WHILE
%token KW_DO
%token KW_IF
%token SEMICOLON    
%token <text> ID        
%token INT         
%token BOOL
%token FLOAT
%token DOUBLE
%token STRING
%token ASSIGN
%token COMMA
%token KW_PRINT
%token KW_READ
%token <typeStruct> INTEGER_CONSTANT
%token <typeStruct> FLOATING_CONSTANT
%token <typeStruct> BOOLEAN_CONSTANT
%token <typeStruct> STRING_CONSTANT
%token VOID
%token CONST
%type <text> scalar_id identifier
%type <type> var_decl type var_list var_entry
%type <typeStruct> literal_constant single_arg
%type <typeList> args arg_list
%type <typeStruct> expr funct_call var_ref bool_expr
%type <typeStruct> intexpr_array_indice array_indice
%type <typeStruct> intexpr_single_array_indice  single_array_indice
%type <typeList> expr_list zero_or_more_expr_list
%%
program : declaration_list funct_def decl_and_def_list  { program_reduce_process();}
        | funct_def decl_and_def_list { program_reduce_process();}
        ;

 /*
decl_and_def_list : decl_and_def_list var_decl
                    | decl_and_def_list const_decl
                    | decl_and_def_list funct_decl
                    | decl_and_def_list funct_def
                    | 
                    ;
*/
decl_and_def_list   : decl_and_def_list declaration_list %prec END  
                    | decl_and_def_list definition_list %prec END 
                    |
                    ;

declaration_list : declaration_list const_decl
                 | declaration_list var_decl
                 | declaration_list funct_decl
                 | const_decl 
                 | var_decl 
                 | funct_decl
                 ;

var_decl : type var_list  SEMICOLON
         ;

type : INT {tmp_type_init_scalar($1);$$=$1;}
     | BOOL {tmp_type_init_scalar($1);$$=$1;}
     | DOUBLE {tmp_type_init_scalar($1);$$=$1;}
     | FLOAT {tmp_type_init_scalar($1);$$=$1;}
     | STRING {tmp_type_init_scalar($1);$$=$1;}
     | VOID {tmp_type_init_scalar($1);$$=$1;}
     ;

/* scalar id or arrray id */
identifier : scalar_id
           | scalar_id array_indice
           ;

/* A list of variable entry, one or more */
var_list : var_entry
         | var_entry COMMA var_list
         ;

/* Variable Entry, either "identifier" or "identifier = decl_expr" */
var_entry : identifier { create_basic_entry($1,K_VARIABLE);}
          | scalar_id ASSIGN expr { if($3.v_type >= 0 && do_type_coercion(&typeTmp,&$3)) create_basic_entry($1,K_VARIABLE); }
          | scalar_id array_indice ASSIGN '{' zero_or_more_expr_list '}' {
                if(check_array_initializer(&typeTmp,&$5)) create_basic_entry($1,K_VARIABLE);
            }
          ;

/* Pure ID */
scalar_id : ID { $$ = $1; }
          ;

array_indice : single_array_indice array_indice
             | single_array_indice
             ;

single_array_indice : '[' INTEGER_CONSTANT ']' { tmp_type_add_dim($2.val);}
                    ;

/* constant variable */
const_decl : CONST type const_list SEMICOLON
           ;

const_list : single_const
           | single_const COMMA const_list
           ;

single_const : scalar_id ASSIGN literal_constant {
                typeStruct_t type;
                memcpy(&type,&$3,sizeof(type));
                if(do_type_coercion(&typeTmp,&type)) constant_declare($1,type);
             }
             | scalar_id ASSIGN '-' literal_constant %prec '*'{
                typeStruct_t type;
                memcpy(&type,&$4,sizeof(type));
                if(do_type_coercion(&typeTmp,&type))
                { 
                    bool success = true;
                    switch(typeTmp.v_type)
                    {
                        case T_INT: type.val *= -1; break;
                        case T_FLOAT: type.fval *= -1; break;
                        case T_DOUBLE: type.dval *= -1; break;
                        default:
                            print_error("Only numeric constant can use negation '-'");
                            success = false; break;
                    }
                    if(success)  constant_declare($1,type);
                }
            }
            ;

literal_constant : INTEGER_CONSTANT
                 | FLOATING_CONSTANT
                 | BOOLEAN_CONSTANT
                 | STRING_CONSTANT
                 ;

/* function declaration */
funct_decl : type scalar_id '(' args ')'  SEMICOLON {
                init_funcOption();
                funcOption.isDeclare = true;
                funcOption.retType = $1;
                s_table_entry* ent = create_basic_entry($2,K_FUNCTION);
                if(ent)
                {
                    ent->type.v_type = $1;
                    ent->attr.param_list.end = 0;
                    for(int i=0;i<$4.end;i++)
                    {
                        typeStruct_t* pTarget = &ent->attr.param_list.data[ent->attr.param_list.end];
                        ++ent->attr.param_list.end;
                        pTarget->v_type = $4.data[i].v_type;
                        pTarget->dim = $4.data[i].dim;
                        memcpy(pTarget->dims,$4.data[i].dims,sizeof(pTarget->dims));
                    }
                }
            }
            ;

/* definition list */
definition_list : definition_list funct_def
                | funct_def
                ;

/* function definition */
funct_def : type scalar_id '(' args ')'  {
            init_funcOption();
            funcOption.isDeclare = false;
            funcOption.pTypeList = &$4;
            funcOption.retType = $1;
            s_table_entry* ent = create_basic_entry($2,K_FUNCTION);
            int effective_type;
            if(!ent) effective_type = $1;
            else effective_type = ent->type.v_type;
            scopeTypeTmp = effective_type == T_VOID ? SCOPE_NORMAL : SCOPE_FUNCTION;
            funcOption.retType = effective_type;
            push_table(TABLE_PRETEND);
            if(ent || funcOption.paramAddOK)
            {
                if(ent) ent->attr.param_list.end = 0;
                for(int i=0;i<$4.end;i++)
                {
                    if($4.data[i].v_type == T_ERROR){ // Error arg
                        continue;
                    }
                    if(!check_dimension(&$4.data[i])){ // dimension check
                        continue;
                    }
                    if(ent){
                        typeStruct_t* pTarget = &ent->attr.param_list.data[ent->attr.param_list.end];
                        ++ent->attr.param_list.end;
                        pTarget->v_type = $4.data[i].v_type;
                        pTarget->dim = $4.data[i].dim;
                        memcpy(pTarget->dims,$4.data[i].dims,sizeof(pTarget->dims));
                    }
                    s_table_entry* param = create_basic_entry($4.data[i].sval,K_PARAMETER);
                    if (param)
                    {
                        param->type.v_type =  $4.data[i].v_type;
                        param->type.dim = $4.data[i].dim;
                        memcpy(param->type.dims,$4.data[i].dims,sizeof(param->type.dims));
                    }
                }
            }
            
        } compound_stat
        {
            if(lastStmtType != STMT_COMPOUND_RETURN && $1!=T_VOID){
                print_error("Last statment of non-void function must be a 'return' statment");
            }
        }
        ;

/* Arguments, zero or more */
args :  arg_list
     |  /* empty list */ {
         $$.end = 0;
     }
     ;

/* Single argument */
single_arg :  type identifier {
                $$.v_type = typeTmp.v_type;
                $$.dim = typeTmp.dim;
                memcpy(&$$.dims,&typeTmp.dims,sizeof($$.dims));
                typeTmp.dim = 0; 
                $$.is_const = FALSE;
                $$.sval = $2;
            }
            |  CONST type identifier {
                $$.v_type = typeTmp.v_type;
                $$.dim = typeTmp.dim;
                memcpy(&$$.dims,&typeTmp.dims,sizeof($$.dims));
                typeTmp.dim = 0; 
                $$.is_const = TRUE;
                $$.sval = $3;
            }
            ;

/* A list of args */
arg_list : arg_list COMMA single_arg {
            memcpy(&$$,&$1,sizeof($$)); 
            $$.data[$$.end].is_const = $3.is_const;
            $$.data[$$.end].v_type = $3.v_type;
            $$.data[$$.end].dim = $3.dim;
            memcpy(&$$.data[$$.end].dims,&$3.dims,sizeof($$.data[$$.end].dims));
            $$.data[$$.end].sval = $3.sval;
            $$.end += 1;
        }
        | single_arg     {

            $$.end = 1;
            $$.data[0].is_const = $1.is_const;
            $$.data[0].v_type = $1.v_type;
            $$.data[0].dim = $1.dim;
            memcpy(&$$.data[0].dims,&$1.dims,sizeof($$.data[0].dims));
            $$.data[0].sval = $1.sval;
         
        }
        ;

/* compound statment */
compound_stat : '{' {push_table(TABLE_NORMAL);} var_const_stmt_list '}' {
                 pop_table();
                 if(lastStmtType == STMT_RETURN) lastStmtType = STMT_COMPOUND_RETURN;
                 else lastStmtType = STMT_NORMAL;
              }
              ;

var_const_stmt_list : var_const_stmt_list stat
                    | var_const_stmt_list var_decl
                    | var_const_stmt_list const_decl
                    |
                    ;
   /*                               
var_const_stmt_list: var_const_stmt_list local_decl_list
                   | var_const_stmt_list zero_or_more_stat_list
                   |
                   ;
  
local_decl_list : var_decl_list
                |
                ;

var_decl_list : var_decl_entry  var_decl_list
              | var_decl_entry
              ;

var_decl_entry : var_decl
               | const_decl
               ;
  
zero_or_more_stat_list : stat_list
                       |
                       ;
stat_list : stat_list stat
          | stat
          ;
  */
/* statement */
stat : compound_stat 
     | simple_stat  {lastStmtType = STMT_NORMAL;}
     | if_stat  {lastStmtType = STMT_NORMAL;}
     | while_stat  {lastStmtType = STMT_NORMAL;}
     | for_stat  {lastStmtType = STMT_NORMAL;}
     | jump_stat 
     | funct_call SEMICOLON
     ;

/* if-statment */
if_stat : if_only_stat
        | if_else_stat
        ;
if_only_stat : KW_IF '(' bool_expr ')' compound_stat %prec LESS_THAN_ELSE
             ;
if_else_stat : KW_IF '(' bool_expr ')' compound_stat KW_ELSE compound_stat
             ;

/* while-statment */
while_stat : KW_WHILE {go_to_loop_scope();} '(' bool_expr ')' compound_stat
           | KW_DO {go_to_loop_scope();} compound_stat KW_WHILE '(' bool_expr ')' SEMICOLON
           ;

/* For-statment */
for_stat : KW_FOR   {go_to_loop_scope();} '(' for_expr SEMICOLON for_bool_expr SEMICOLON for_expr ')' compound_stat
         ;
for_bool_expr : bool_expr
              |
              ;
for_expr : expr
         | one_or_more_for_assignment
         |
         ;

single_assginment : var_ref ASSIGN expr {  check_variable_assignable(&$1,&$3);}
                  ;
one_or_more_for_assignment : single_assginment
                           | one_or_more_for_assignment COMMA single_assginment
                           ;

/* Jump statment */
jump_stat : KW_RETURN expr SEMICOLON {process_return_function_check(&$2);lastStmtType = STMT_RETURN;}
          | KW_BREAK SEMICOLON {process_loop_check(); lastStmtType = STMT_NORMAL;}
          | KW_CONTINUE SEMICOLON {process_loop_check();lastStmtType = STMT_NORMAL;}
          ;

/* Simple statment */
simple_stat : single_assginment SEMICOLON
            | KW_PRINT expr SEMICOLON {check_printable(&$2);} 
            | KW_READ var_ref SEMICOLON { check_readable(&$2); }
            ;

var_ref : scalar_id {
            $$.sval = $1; // name
            fill_type_for_name((typeStruct_t*)&$$,$1);
        }
        | scalar_id intexpr_array_indice
        {
            $$.sval = $1; // name
            fill_type_for_name((typeStruct_t*)&$$,$1);
            if($$.v_type != T_ERROR){
                int dim = $$.dim;
                int expand = 0;
                for(int i=0;i<$2.dim;i++)
                {
                    if(dim > 0){ // can expand
                        dim--;
                        expand++;
                    }
                    else{ // cannot expand furthur
                        char errmsg[MAX_STRING_SIZE];
                        snprintf(errmsg,MAX_STRING_SIZE," '%s' is %d dimension, but reference is %d dimension",$1,$$.dim,$2.dim);
                        print_error(errmsg);
                        $$.v_type = T_ERROR;
                        break;
                    }
                }
                // shift array if no error occur
                if($$.v_type != T_ERROR)
                {
                    for(int i=0;i<dim;i++) $$.dims[i] = $$.dims[i+expand];
                    $$.dim = dim;
                }
            }
            $$.is_const = false;
        }
        ;

intexpr_array_indice : intexpr_single_array_indice intexpr_array_indice
                    {
                        $$ = $2;
                        $$.dims[$$.dim] = $1.dims[0];
                        $$.dim++;
                    }
                    | intexpr_single_array_indice
                    ;

intexpr_single_array_indice : '[' expr ']'
                            {
                                $$.dim = 1;
                                // check if expr is integer and not an array
                                if($2.v_type != T_INT)
                                {
                                    $$.v_type = T_ERROR;
                                    if($2.v_type != T_ERROR)
                                    {
                                        char errmsg[MAX_STRING_SIZE];
                                        snprintf(errmsg,MAX_STRING_SIZE,"Array index must be scalar integer type");
                                        print_error(errmsg);
                                    }
                                }
                                else{
                                    // furthur check if constant and the number < 0
                                    if($2.is_const && $2.val < 0)
                                    {
                                        $$.v_type = T_ERROR;
                                        print_error("Array index must greater than or equal to 0");    
                                    }
                                    else $$.v_type = T_INT;
                                }
                            }
                            ;

bool_expr : expr
          {
              $$ = $1;
              if($$.v_type != T_BOOL)
              {
                  $$.v_type = T_ERROR;
                  if($1.v_type != T_ERROR)
                  {
                    char errmsg[MAX_STRING_SIZE];
                    snprintf(errmsg,MAX_STRING_SIZE,"Expect a boolean expression here, but '%s' provided",TYPE_NAME[$1.v_type]);
                    print_error(errmsg);
                  }
              }
          }
          ;

relation_op : OP_LT
            | OP_LE
            | OP_GT
            | OP_GE
            | OP_NE
            | OP_EQ
            ;

binary_logic_op : OP_AND
                | OP_OR
                ;

expr : expr '+' expr
     {
         $$.v_type = binary_numeric_expr_check((typeStruct_t*)&$1,(typeStruct_t*)&$3,OP_ADD);
         $$.is_const = false;
     }
    | expr '-' expr
    {
        $$.v_type = binary_numeric_expr_check((typeStruct_t*)&$1,(typeStruct_t*)&$3,OP_SUB);
        $$.is_const = false;
    }
    | expr '*' expr
    {
        $$.v_type = binary_numeric_expr_check((typeStruct_t*)&$1,(typeStruct_t*)&$3,OP_MUL);
        $$.is_const = false;
    }
    | expr '/' expr
    {
        $$.v_type = binary_numeric_expr_check((typeStruct_t*)&$1,(typeStruct_t*)&$3,OP_DIV);
        $$.is_const = false;
    }
    | expr '%' expr
    {
        $$.v_type = binary_numeric_expr_check((typeStruct_t*)&$1,(typeStruct_t*)&$3,OP_MOD);
        $$.is_const = false;
    }
    | '-' expr %prec '*' {
        $$ = $2;
        // check type is numeric or not
        if($2.v_type==T_INT || $2.v_type==T_FLOAT || $2.v_type==T_DOUBLE )
        {
            if($$.is_const) perform_negation((typeStruct_t*)&$$);
        }
        else
        {
            $$.v_type = T_ERROR;
            if($2.v_type != T_ERROR){
                char errmsg[MAX_STRING_SIZE];
                snprintf(errmsg,MAX_STRING_SIZE,"Only numeric expression can use negation '-', but '%s' provided.",TYPE_NAME[$2.v_type]);
                print_error(errmsg);
            }
        }
    }
    | '(' expr ')' { $$ = $2; }
    | expr relation_op expr %prec OP_EQ{
        $$.v_type = binary_numeric_expr_check((typeStruct_t*)&$1,(typeStruct_t*)&$3,OP_RELATION);
        $$.is_const = false;
    }
    | expr binary_logic_op expr %prec OP_AND {  // both side need to be boolean type
        if($1.v_type != T_BOOL)
        {
            $$.v_type = T_ERROR;
            if($1.v_type != T_ERROR){
                char errmsg[MAX_STRING_SIZE];
                snprintf(errmsg,MAX_STRING_SIZE,"Binary logical operator expect a boolean operand on left-hand-side, but '%s' provided.",TYPE_NAME[$1.v_type]);
                print_error(errmsg);
            }
        }
        else if($3.v_type != T_BOOL)
        {
            $$.v_type = T_ERROR;
            if($3.v_type != T_ERROR){
                char errmsg[MAX_STRING_SIZE];
                snprintf(errmsg,MAX_STRING_SIZE,"Binary logical operator expect a boolean operand on right-hand-side, but '%s' provided.",TYPE_NAME[$3.v_type]);
                print_error(errmsg);
            }
        }
        else $$ = $1;
        $$.is_const = false;
    }
    | OP_NOT expr {
        $$ = $2;
        if($$.v_type != T_BOOL)
        {
            $$.v_type = T_ERROR;
            if($$.v_type != T_ERROR){
                char errmsg[MAX_STRING_SIZE];
                snprintf(errmsg,MAX_STRING_SIZE,"Logical operator NOT:'!' expect a boolean operand, but '%s' provided.",TYPE_NAME[$$.v_type]);
                print_error(errmsg);
            }
        }
        $$.is_const = false;
    }
    | literal_constant 
    | funct_call 
    | var_ref 
    ;

funct_call : scalar_id '(' zero_or_more_expr_list ')' {
                $$.sval = $1;
                $$.v_type = check_function_call_consist($1,&$3);
                $$.is_const = false;
            }
            ;

zero_or_more_expr_list :  expr_list  // keep
                       |   {  $$.end = 0; }   // empty list
                       ;

/* one or more expr */
expr_list : expr //single expr
          {
              $$.end = 1;
              $$.data[0] = $1;
          }
          | expr_list COMMA expr
          {
              $$ = $1;
              $$.data[$$.end] = $3;
              $$.end++;
          }
          ;

%%
int clear(){
    // clear pool
    for(int i=0;i<str_pool.end;i++){
        free(str_pool.data[i]);
    }
}

void add_errorPool(const char* msg){
    if(errorPool.end<MAX_ERROR_MSG){
        errorPool.pool[errorPool.end] = msg;
        ++errorPool.end;
    }
    else{
        fprintf( stderr, "*** WARNING!!! Cannot add more error messages into pool !! ***\n" );
    }
}
// show all error, return if there is at least an error
bool show_errors(){
    if(errorPool.end==0){
        return false;
    }
    #ifdef SHOW_ALL_ERROR
    printf("-------Dump all errors-------\n");
    for(int i=0;i<errorPool.end;i++){
        printf(errorPool.pool[i]);
    }
    #endif
    return true;
}

// get current_table
    symbol_table* current_table(){  return &stk.tables[stk.end-1]; }

// push new table and return it
    symbol_table* push_table(int mode){
        table_stack* p_table_stk = &stk;
        if(mode==TABLE_PRETEND){
            ++p_table_stk->pretend;
        }
        else if(p_table_stk->pretend > 0){
            --p_table_stk->pretend;
            return &p_table_stk->tables[p_table_stk->end-1];
        }
        symbol_table* p_new = &p_table_stk->tables[p_table_stk->end];
        init_table(p_new,p_table_stk->level);
        p_table_stk->end += 1;
        p_table_stk->level += 1;
        return p_new;
    }

// pop table and return it
    symbol_table* pop_table()
    {
        dump_table(current_table());
        table_stack* p_table_stk = &stk;
        symbol_table* old_table = &p_table_stk->tables[p_table_stk->end-1];
        p_table_stk->end -= 1;
        p_table_stk->level -= 1;
        return old_table;
    }

// init a table stack
    void init_table_stack()
    {
        stk.end = 0;
        stk.level = 0;
        stk.pretend = 0;
        scopeTypeTmp = SCOPE_NORMAL;
        push_table(TABLE_NORMAL);
    }

// function declare-definition test
    void run_function_declare_definition_test()
    {
        func_check_t check[MAX_TABLE_ENTRY];
        int end = 0;
        // scan current table
        symbol_table* table = current_table();
        for(int i=0;i<table->end;i++){
            s_table_entry* ent = &table->entries[i];
            if(ent->kind==K_FUNCTION && ent->isDeclare)  // single declare found, record it
            { 
                check[end].name = ent->name;
                check[end].declare = true;
                check[end].define = ent->isMatch;
                end++;
            }
        }
        char errmsg[MAX_STRING_SIZE];
        // check if there exist some function
        for(int i=0;i<end;i++)
        {
            if(!check[i].define){ // someone haven't defined yet
                snprintf(errmsg,MAX_STRING_SIZE,"Function %s has been declared but not be defined",check[i].name);
                print_error(errmsg);
            }
        }
    }

int yyerror( char *msg )
{
    fprintf( stderr, "\n|--------------------------------------------------------------------------\n" );
    fprintf( stderr, "| Error found in Line #%d: %s\n", linenum, buf );
    fprintf( stderr, "|\n" );
    fprintf( stderr, "| Unmatched token: %s\n", yytext );
    fprintf( stderr, "|--------------------------------------------------------------------------\n" );
    clear();
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

    // init
    KIND_NAME[1] = "function";
    KIND_NAME[2] = "parameter";
    KIND_NAME[4] = "variable";
    KIND_NAME[8] = "constant";
    init_id_table();
    str_pool.end = 0;
    init_table_stack(&stk);
    errorPool.end = 0;
    yyin = fp;
    yyparse();
    
    if (Opt_Statistic)
    {
         show_freq_table();
    }
    dispose_id_table();
    
    if(!show_errors()){
        fprintf( stdout, "\n" );
        fprintf( stdout, "|---------------------------------------------|\n" );
        fprintf( stdout, "|  There is no syntactic and semantic error!  |\n" );
        fprintf( stdout, "|---------------------------------------------|\n" );
    }
    else
    {
        fprintf( stdout, "\n|--------------------------------|\n" );
        fprintf( stdout, "|  There is no syntactic error!  |\n" );
        fprintf( stdout, "|--------------------------------|\n" );
    }
    clear();
    exit(0);
}

	typedef struct {
        int end;
        const char* pool[MAX_STRING_POOL];
	}  ErrorPool;

	typedef struct {
	    bool isDeclare;
	    typeList_t* pTypeList;
	    int retType;
	    bool paramAddOK;
	} funcOption_t;

// symbol type
	typedef struct{
	    bool is_const;
	    int type;
	    int dim;
	    int dims[MAX_DIM+1]; // n-th dim. store in dims[n], scalar has dim = 0
	} param_type;

 // symbol attribute
    typedef union{
        int int_val;
        float float_val;
        double double_val;
        int bool_val;
        char* string_val;
        typeList_t param_list;
    } sym_attr;
// symbol table entry
    typedef struct{
        char name[33];
        int kind;
        int level;
        typeStruct_t type;
        sym_attr attr;
        int option;
        bool isDeclare;
        bool isMatch;
    } s_table_entry;
// symbol table
    typedef struct{
        s_table_entry entries[MAX_TABLE_ENTRY];
        int end;
        int level;
        int scopeType;
        
    } symbol_table;
// table stack
    typedef struct{
        symbol_table tables[MAX_TABLES];
        int end;
        int level;
        int pretend;
    } table_stack;

// string pointer pool
    typedef struct{
        void* data[MAX_STRING_POOL];
        int end;
    } string_pool;

// record struct
	typedef struct{
	    bool declare;
	    bool define;
	    const char* name;
	} func_check_t;
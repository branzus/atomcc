#ifndef SYM_H_
#define SYM_H_



typedef struct _Symbol symbol_t;
typedef struct _Symbols symbols_t;

typedef enum {TB_INT, TB_DOUBLE, TB_CHAR, TB_STRUCT, TB_VOID} typebase_t;



typedef struct {
  typebase_t typeBase; //TB_*
  symbol_t *s; //struct definition for TB_STRUCT
  int nElements; //>0 array of given size, 0=aray without size, <0 no array
} type_t;

typedef enum {CLS_VAR, CLS_FUNC, CLS_EXTFUNC, CLS_STRUCT} cls_t;

typedef enum {MEM_GLOBAL, MEM_ARG, MEM_LOCAL} mem_t;



struct _Symbols{
  symbol_t **begin;
  symbol_t **end;
  symbol_t **after;

};

struct _Symbol {
  const char* name; //a reference to the name stored in a token
  cls_t cls; //CLS_*
  mem_t mem; //MEM_*
  type_t type;
  int depth; //0-global 1-in fuction, 2..-nested blocks in function
  union {
    symbols_t args;
    symbols_t members;
  };
  union {
    void *addr; //vm: the memory address for global symbols
    int offset; //vm: the stack offset for local symbols
  };
};

typedef union {
  long int i;
  double d;
  const char *str;
} CtVal;

typedef struct {
  type_t type;
  int isLVal;
  int isCtVal;
  CtVal ctVal;
} retval_t;


void initSymbols(symbols_t *symbols);
symbol_t *addSymbol(symbols_t *symbols, const char *name, cls_t cls);
symbol_t *findSymbol(symbols_t *symbols, const char *name);
void printSymbols(symbols_t *symbols, int level);
void printSymbol(symbol_t *symbols, int level);
void deleteSymbolsAfter(symbols_t *symbols, symbol_t *crtFunc);
symbol_t *addExtFunc(symbols_t *symbols,const char *name, type_t type, void *addr);
symbol_t *addFuncArg(symbol_t *func, const char *name, type_t type);
//type_t getArithType(type_t *s1, type_t *s2);
type_t createType(typebase_t type, int nElements);
symbol_t *requireSymbol(symbols_t *symbols, const char* name);
//void cast(type_t * t1, type_t *t2);
#endif

#include "sym.h"
#include "lex.h"
#include<stdio.h>
#include<stdlib.h>
#include "utils.h"
#include<string.h>
extern token_t crtTk;
int crtDepth = 0;
//symbols_t symbols;
//symbol_t crtFunc;
//symbol_t crtStruct;


static char *typebase[] = {"TB_INT", "TB_DOUBLE", "TB_CHAR", "TB_STRUCT", "TB_VOID"};
static char *cls[] = {"CLS_VAR", "CLS_FUNC", "CLS_EXTFUNC", "CLS_STRUCT"};
static char *mem[] = {"MEM_GLOBAL", "MEM_ARG", "MEM_LOCAL"};

void initSymbols(symbols_t *symbols)
{
  symbols->begin = NULL;
  symbols->end = NULL;
  symbols->after = NULL;
}

symbol_t *addSymbol(symbols_t *symbols, const char *name, cls_t cls)
{
  symbol_t *s;
  if(symbols->end == symbols->after)
    {
      int count = symbols->after - symbols->begin;
      int n = count * 2;
      if (n == 0)
	n = 1;
      symbols->begin = realloc(symbols->begin, n * sizeof(symbol_t *));
      if(symbols->begin == NULL) my_err("not enough memory reallocating symbols");
      symbols->end = symbols->begin + count;
      symbols->after = symbols->begin + n;
    }
  SAFEALLOC(s, symbol_t, 1);
  *symbols->end++ = s;
  s->name = name;
  s->cls = cls;
  s->depth = crtDepth;
  s->mem = 0;
  //  printSymbol(s, crtDepth*2);
  return s;
}


void printminus(int nr)
{
  int i = 0;
  while(i++!= nr)
    putchar('-');
}

void printSymbol(symbol_t *s, int level)
{
  printminus(level);
  printf("%s : %s : %s : %d -- %s[%d]\n", mem[s->mem], cls[s->cls], typebase[s->type.typeBase], s->depth, s->name, s->type.nElements);
  if (s->cls == CLS_STRUCT)
    printSymbols(&s->members, level + 2);
  else if(s->cls == CLS_FUNC)
    printSymbols(&s->args, level + 2);
}


void printSymbols(symbols_t *symbols, int level)
{
  symbol_t **s;
  for(s = symbols->begin; s != symbols->end; s++)
    printSymbol(*s,level);
}

symbol_t *findSymbol(symbols_t *symbols, const char *name)
{
  symbol_t **s = NULL;
  if(symbols->end == NULL)
    return NULL;
  for(s = symbols->end - 1; s != symbols->begin && s; s--)
    if(!strcmp(name, (*s)->name))
      return *s;
  if(s)
    if(!strcmp(name, (*s)->name))
      return *s;
    
  
  return NULL;
}


symbol_t *requireSymbol(symbols_t *symbols, const char* name)
{
  symbol_t *ret = findSymbol(symbols, name);
  if(ret == NULL)
    my_err("required symbol %s not found in symbol table", name);
  return ret;

}

void deleteSymbolsAfter(symbols_t *symbols, symbol_t *sym)
{
  if(symbols->end == NULL)
    my_err("no symbols to delete from");
  symbol_t **s = NULL;
  for(s = symbols->end - 1; s != symbols->begin && s; s--)
    if (*s == sym)
      {
	symbol_t **i = NULL;
	for(i = s + 1; i != symbols->end; i++)
	  {
	    printSymbol(*i, (*i)->depth * 2);
	    free(*i);
	  }
	symbols->end = s + 1;
	return;
      }
  if(s)
    if (*s == sym)
      {
	symbol_t **i = NULL;
	for(i = s + 1; i != symbols->end; i++)
	  {
	    printSymbol(*i, (*i)->depth * 2);
	    free(*i);
	  }
	
	symbols->end = s + 1;
	return;
      }

  my_err("symbol to delete after not found");
}


type_t createType(typebase_t type, int nElements)
{
  //printf("Creating type: %s\n", typebase[type]);
  type_t t;
  t.typeBase = type;
  t.nElements = nElements;
  return t;
}

symbol_t *addExtFunc(symbols_t *symbols,const char *name, type_t type, void *addr)
{
  symbol_t *s = addSymbol(symbols, name, CLS_EXTFUNC);
  s->type = type;
  s->addr = addr;
  initSymbols(&s->args);
  return s;
}

symbol_t *addFuncArg(symbol_t *func, const char *name, type_t type)
{
  symbol_t *a = addSymbol(&func->args, name, CLS_VAR);
  a->type = type;
  return a;
}




/*
int main(void)
{
  symbols_t symbols;
  initSymbols(&symbols);
  symbol_t *s;
  addSymbol(&symbols, "petrica", CLS_VAR);
  addSymbol(&symbols, "ionica", CLS_FUNC);
  s = addSymbol(&symbols, "structica", CLS_STRUCT);
  addSymbol(&symbols, "exica", CLS_EXTFUNC);
  addSymbol(&symbols, "funcics", CLS_FUNC);
  printSymbols(&symbols);

  s = findSymbol(&symbols, "s");
  if(s) printf("%s\n", s->name);
  puts("deleting");
  deleteSymbolsAfter(&symbols, s);
  printSymbols(&symbols);  




  return 0;
}
*/

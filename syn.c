#include "lex.h"
#include "utils.h"
#include "sym.h"
#include "vm.h"
#include<stdio.h>
#include<stdlib.h>

extern token_t tokens;
extern char *pBegCh;
extern char *pCrtCh;
token_t consumedTk;
token_t crtTk;

extern int crtDepth;
extern instr_t lastInstruction;
symbols_t symbols;
symbol_t *crtFunc;
symbol_t *crtStruct;
int offset = 0;
int sizeArgs = 0;
instr_t crtLoopEnd;

int expr(retval_t *rv);

int typeFullSize(type_t *type);


int typeBaseSize(type_t *type)
{
  int size = 0;
  symbol_t **s;

  switch(type->typeBase)
    {
    case TB_INT: size = sizeof(int); break;
    case TB_DOUBLE: size = sizeof(double); break;
    case TB_CHAR: size = sizeof(char); break;
    case TB_STRUCT:
      for(s = type->s->members.begin; s != type->s->members.end; s++)
	size += typeFullSize(&(*s)->type);
      break;
    case TB_VOID: size = 0; break;
    default: my_err("invalid type base: %d", type->typeBase);
    }
  return size;
}

int typeFullSize(type_t *type)
{
  return typeBaseSize(type) * (type->nElements > 0 ? type->nElements : 1);
}

int typeArgSize(type_t *type)
{
  if (type->nElements >= 0) return sizeof(void *);
  return typeBaseSize(type);
}


instr_t getRVal(retval_t *rv)
{

  if(rv->isLVal)
    {
      switch(rv->type.typeBase)
	{
	case TB_INT:
	case TB_DOUBLE:
	case TB_CHAR:
	case TB_STRUCT:
	  addInstrI(O_LOAD, typeArgSize(&rv->type));
	  break;
	default:
	  tk_err(crtTk, "unhandled typed: %d", rv->type.typeBase);
	}
    }
  return lastInstruction;
}



instr_t createCondJmp(retval_t *rv)
{
  if(rv->type.nElements >= 0)
    return addInstr(O_JF_A);
  else
    {
      getRVal(rv);
      switch(rv->type.typeBase)
	{
	case TB_CHAR: return addInstr(O_JF_C);
	case TB_DOUBLE: return addInstr(O_JF_D);
	case TB_INT: return addInstr(O_JF_I);
	default: return NULL;
	}
    }
}
void addCastInstr(instr_t after, type_t *actualType, type_t *neededType)
{
  if(actualType->nElements >= 0 || neededType->nElements >= 0)
    return;
  switch(actualType->typeBase)
    {
    case TB_CHAR:
      switch(neededType->typeBase)
	{
	case TB_CHAR: break;
	case TB_INT: addInstrAfter(after, O_CAST_C_I); break;
	case TB_DOUBLE: addInstrAfter(after, O_CAST_C_D); break;
	default: my_err("can't cast from to");
	}
      break;
    case TB_INT:
      switch(neededType->typeBase)
	{
	case TB_CHAR: addInstrAfter(after, O_CAST_I_C); break;
	case TB_INT: break;
	case TB_DOUBLE: addInstrAfter(after, O_CAST_I_D); break;
	default: my_err("can't cast from to");
	}
      break;
    case TB_DOUBLE:
      switch(neededType->typeBase)
	{
	case TB_CHAR: addInstrAfter(after, O_CAST_D_C); break;
	case TB_INT: addInstrAfter(after, O_CAST_D_I); break;
	case TB_DOUBLE:  break;
	default: my_err("can't cast from to");
	}
      break;
    default: my_err("can't cast");
    }
}










type_t getArithType(type_t *s1, type_t *s2)
{
  if(s1->nElements != -1 || s2->nElements != -1)
    tk_err(crtTk, "can't operate on arrays yet");
  switch(s1->typeBase)
    {
    case TB_STRUCT:
    case TB_VOID:
      tk_err(crtTk, "struct or void fmm");
    case TB_DOUBLE:
      return createType(TB_DOUBLE, -1);
    case TB_INT:
      if(s2->typeBase != TB_DOUBLE)
	return createType(TB_INT, -1);
      else
	return createType(TB_DOUBLE, -1);
    case TB_CHAR:
      return createType(s2->typeBase, -1);
    }
  tk_err(crtTk, "incompatible types");
  return createType(TB_VOID, -100);
}


void cast(type_t *dst, type_t *src)
{
  //  printf("Casting from %s:%d to %s:%d\n", typebase[src->typeBase], src->nElements, typebase[dst->typeBase], dst->nElements);
  if(src->nElements > -1)
    {
      if(dst->nElements > -1)
	{
	  if (src->typeBase != dst->typeBase)
	    tk_err(crtTk, "an array cannot be converted to an array of another type");
	}
      else
	tk_err(crtTk, "an array cannot be converted to a non-array");
    }
  else if (dst->nElements > -1)
    tk_err(crtTk, "a non-array cannot be converted to an array");

  switch(src->typeBase)
    {
    case TB_CHAR:
    case TB_INT:
    case TB_DOUBLE:
      switch(dst->typeBase)
	{
	case TB_CHAR://TODOODODODOODODODODODO when code generation happens
	case TB_INT:
	case TB_DOUBLE:
	  return;
	case TB_VOID:
	case TB_STRUCT:
	  tk_err(crtTk, "incompatible types1");
	}
    case TB_STRUCT:
      if(dst->typeBase == TB_STRUCT)
	{
	  if(src->s != dst->s)
	    tk_err(crtTk, "a structure cannot be converted to another structure");
	  return;
	}
    case TB_VOID:
      ;
    }
  tk_err(crtTk, "incompatible types");
}

void addVar(token_t tkName, type_t *t)
{
  symbol_t *s;
  if(crtStruct)
    {
      if(findSymbol(&crtStruct->members, tkName->text))
	tk_err(tkName, "symbol redefinition %s", tkName->text);
      s = addSymbol(&crtStruct->members, tkName->text, CLS_VAR);
    }
  else if(crtFunc)
    {
      s = findSymbol(&symbols, tkName->text);
      if(s && s->depth == crtDepth)
	tk_err(tkName, "symbol redefinition %s", tkName->text);
      s = addSymbol(&symbols, tkName->text, CLS_VAR);
      s->mem = MEM_LOCAL;
    }
  else
    {
      if(findSymbol(&symbols, tkName->text))
	tk_err(tkName, "symbol redefinition %s", tkName->text);
      s = addSymbol(&symbols, tkName->text, CLS_VAR);
      s->mem = MEM_GLOBAL;
    }
  s->type = *t;
  if (crtStruct || crtFunc)
    s->offset = offset;
  else
    s->addr = allocGlobal(typeFullSize(&s->type));
  offset += typeFullSize(&s->type);
}


int consume(code_t code)
{
  if (crtTk->code == code)
    {
      consumedTk = crtTk;
      crtTk = crtTk->next;
      return 1;
    }
  return 0;
}

int typeBase(type_t *ret)
{
  if(consume(INT))
    {
      ret->typeBase = TB_INT;
      return 1;
    }
  if(consume(DOUBLE))
    {
      ret->typeBase = TB_DOUBLE;
      return 1;
    }
  if(consume(CHAR))
    {
      ret->typeBase = TB_CHAR;
      return 1;
    }
  if(consume(STRUCT))
    {
      if(consume(ID))
	{
	  token_t tkName = consumedTk;
	  symbol_t *s = findSymbol(&symbols, tkName->text);
	  if(!s) tk_err(tkName, "undefined symbol: %s", tkName->text);
	  if(s->cls != CLS_STRUCT) tk_err(tkName, "%s is not a struct", tkName->text);
	  ret->typeBase = TB_STRUCT;
	  ret->s = s;
	  // printf("%s: %d\n",tkName->text, typeBaseSize(ret));
	  return 1;
	}
      tk_err(crtTk, "expected identifier after struct keyword");
    }
  return 0;
}

int arrayDecl(type_t *ret)
{
  if (!consume(LBRACKET))
    return 0;
  retval_t rv;
  instr_t instrBeforeExpr = lastInstruction;
  if(expr(&rv))
    {
      if(!rv.isCtVal) tk_err(crtTk, "the array size is not a constant");
      if(rv.type.typeBase != TB_INT) tk_err(crtTk, "the array size is not an integer");
      ret->nElements = rv.ctVal.i;
        deleteInstructionsAfter(instrBeforeExpr);
    }
  else
    ret->nElements=0;

  if (!consume(RBRACKET)) tk_err(crtTk, "expected ']' in array declaration");
  return 1;

}


int declVar()
{
  token_t start = crtTk;
  instr_t starti = lastInstruction;
  type_t t;
  if(typeBase(&t) && consume(ID))
    {

      token_t tkName = consumedTk;
      if(!arrayDecl(&t))
	t.nElements = -1;
      addVar(tkName, &t);
      while(consume(COMMA))
	{
	  if(!consume(ID)) tk_err(crtTk, "expected identifier after ',' in variable declaration");
	  tkName = consumedTk;
	  if(!arrayDecl(&t))
	    {
	      t.nElements = -1;
	    }
	  addVar(tkName, &t);
	}
      if(!consume(SEMICOLON)) tk_err(crtTk, "expected ';' at variable declaration end");
      return 1;
    }
  crtTk = start;
  deleteInstructionsAfter(starti);
  return 0;
}



int declStruct()
{
  token_t start =crtTk;
  instr_t starti = lastInstruction;
  if(!consume(STRUCT))
    return 0;
  if(!consume(ID)) tk_err(crtTk, "expected identifier after struct keyword");

  token_t tkName = consumedTk; //save name 
  if(!consume(LACC))
    {
      crtTk =start;
      deleteInstructionsAfter(starti);
      return 0;
    }
  offset = 0;

  if (findSymbol(&symbols, tkName->text))
    tk_err(tkName, "symbol redefinition %s", tkName->text);

  crtStruct = addSymbol(&symbols, tkName->text, CLS_STRUCT); //signal we are in struct
  initSymbols(&crtStruct->members);
  
  while(declVar());
  if(!consume(RACC)) tk_err(crtTk, "expected '}' after attribute declaration in struct");
  if(!consume(SEMICOLON)) tk_err(crtTk, "expected ';' after struct declaration");
  
  crtStruct = NULL;
  return 1;
}





int typeName(type_t *ret)
{
  if(!typeBase(ret))
    return 0;
  if(!arrayDecl(ret))
    ret->nElements = -1;
  return 1;
}

int stm();

int stmCompound()
{
  symbol_t *start = symbols.end[-1];
  
  if(!consume(LACC))
    return 0;
  crtDepth++;
  
  while(declVar() || stm());

  if(!consume(RACC))
    tk_err(crtTk, "expected '}' at end of block");
  crtDepth--;
  
  deleteSymbolsAfter(&symbols, start);
  return 1;
}

int funcArg()
{
  type_t t;
  if(!typeBase(&t)) return 0;
  if(!consume(ID)) tk_err(crtTk, "expected identifiers");
  token_t tkName = consumedTk;
  if(!arrayDecl(&t))
    t.nElements = -1;

  symbol_t *s = addSymbol(&symbols, tkName->text, CLS_VAR);
  s->mem = MEM_ARG;
  s->type = t;
  s->offset = offset;
  
  s = addSymbol(&crtFunc->args, tkName->text, CLS_VAR);
  s->mem = MEM_ARG;
  s->type = t;
  s->offset = offset;

  offset += typeArgSize(&s->type);
  return 1;
}

int declFunc()
{
  token_t start = crtTk;
  instr_t starti = lastInstruction;
  type_t t;
  if(typeBase(&t))
    {
    if(consume(MUL))
      t.nElements = 0;
    else
      t.nElements = -1;
    }
  else
    {
      if(!consume(VOID))
	return 0;
      t.typeBase = TB_VOID;
      t.nElements = -1;
    }
  token_t tkName;
  if (consume(ID) && (tkName = consumedTk)  && consume(LPAR))
    {
      sizeArgs = offset = 0;
      if(findSymbol(&symbols, tkName->text))
	tk_err(tkName, "symbol redefinition: %s", tkName->text);
      crtFunc = addSymbol(&symbols, tkName->text, CLS_FUNC);
      //crtFunc->mem = MEM_GLOBAL;
      initSymbols(&crtFunc->args);
      crtFunc->type = t;
      crtDepth++;
      
      if(funcArg())
	{
	  while(consume(COMMA))
	    if(!funcArg()) tk_err(crtTk, "expected next function argument after ','");
	}
      if(!consume(RPAR)) tk_err(crtTk, "expected ')' at end of function declaration");
      crtDepth--;

      crtFunc->addr = addInstr(O_ENTER);
      sizeArgs = offset; //update args for correct FP indexing
      symbol_t **ps;
      for(ps = symbols.begin; ps != symbols.end; ps++)
	if((*ps)->mem == MEM_ARG)
	  (*ps)->offset -= sizeArgs + 2 * sizeof(void *);
      offset = 0;
      
      if(!stmCompound()) tk_err(crtTk, "expected function body");

      ((instr_t) crtFunc->addr)->args[0].i = offset; //setup enter argument;
      if(crtFunc->type.typeBase == TB_VOID)
	addInstrII(O_RET, sizeArgs, 0);

      deleteSymbolsAfter(&symbols, crtFunc);
      crtFunc = NULL;
      return 1;
    }
  deleteInstructionsAfter(starti);
  crtTk = start;
  return 0;

}

int ruleIf()
{
  instr_t i1,i2;
  if(!consume(IF))
    return 0;
  if(!consume(LPAR)) tk_err(crtTk, "expected '(' after if");
  retval_t rv;
  if(!expr(&rv)) tk_err(crtTk, "expected expression between paranthesis after if");

  if (rv.type.typeBase == TB_STRUCT)
    tk_err(crtTk, "a structure cannot be logically tested");

  if(!consume(RPAR)) tk_err(crtTk, "expected ')' after expression in if statement");
  i1 = createCondJmp(&rv);
  if(!stm()) tk_err(crtTk, "expected if body");

  if(!consume(ELSE)) {
    i1->args[0].addr = addInstr(O_NOP);
    return 1;
  }

  i2 = addInstr(O_JMP);

  if(!stm()) tk_err(crtTk, "expected else body");

  i1->args[0].addr = i2->flink;

  i1 = i2;
  i1->args[0].addr = addInstr(O_NOP);
  
  return 1;
}

int ruleWhile()
{
  
  if(!consume(WHILE))
    return 0;

  instr_t oldLoopEnd = crtLoopEnd;
  crtLoopEnd = createInstr(O_NOP);
  instr_t i1 = lastInstruction;
    
  if(!consume(LPAR)) tk_err(crtTk, "expected '(' after while");
  retval_t rv;
  if(!expr(&rv)) tk_err(crtTk, "expected expression between paranthesis after while");

  if (rv.type.typeBase == TB_STRUCT)
    tk_err(crtTk, "a structure cannot be logically tested");
  
  if(!consume(RPAR)) tk_err(crtTk, "expected ')' after expression in while statement");
  instr_t i2 = createCondJmp(&rv);
  
  if(!stm()) tk_err(crtTk, "expected while body");

  addInstrA(O_JMP, i1->flink);
  appendInstr(crtLoopEnd);
  i2->args[0].addr = crtLoopEnd;
  crtLoopEnd = oldLoopEnd;
  return 1;
}

int ruleFor()
{
  if(!consume(FOR))
    return 0;
  instr_t oldLoopEnd = crtLoopEnd;
  crtLoopEnd = createInstr(O_NOP);
  
  if(!consume(LPAR)) tk_err(crtTk, "expected '(' after for");
  
  retval_t rv1, rv2, rv3;
  if(expr(&rv1))
    {
      if(typeArgSize(&rv1.type))
	addInstrI(O_DROP, typeArgSize(&rv1.type));
    }
  if(!consume(SEMICOLON)) tk_err(crtTk, "expected ';' after first for expression");

  instr_t i2 = lastInstruction;
  instr_t i4 = NULL;
  if(expr(&rv2))
    {
      if (rv2.type.typeBase == TB_STRUCT)
	tk_err(crtTk, "a structure cannot be logically tested");
      i4 = createCondJmp(&rv2);
    }
  
  if(!consume(SEMICOLON)) tk_err(crtTk, "expected ';' after second for expression");
  instr_t ib3 = lastInstruction;
  
  if(expr(&rv3))
    {
      if(typeArgSize(&rv3.type))
	addInstrI(O_DROP, typeArgSize(&rv3.type));
    }

  if(!consume(RPAR)) tk_err(crtTk, "expected ')' after third for expression");

  instr_t ibs = lastInstruction;
  
  if(!stm()) tk_err(crtTk, "expected for body");

  if(ib3 != ibs)
    {
      instr_t i3 = ib3->flink;
      instr_t is = ibs->flink;
      ib3->flink = is;
      is->blink = ib3;
      lastInstruction->flink = i3;
      i3->blink = lastInstruction;
      ibs->flink = NULL;
      lastInstruction = ibs;
    }
  addInstrA(O_JMP, i2->flink);
  appendInstr(crtLoopEnd);
  if (i4)
    i4->args[0].addr = crtLoopEnd;
  crtLoopEnd = oldLoopEnd;
  return 1;
}




int stm()
{
  //instr_t i,i1,i2,i3,i4,is,ib3,ibs;
  
  if(stmCompound())
    return 1;
  if(ruleIf() || ruleWhile() || ruleFor())
    return 1;
  
  if(consume(BREAK))
    {
      if(!consume(SEMICOLON)) tk_err(crtTk, "expected ';' after break");
      if (!crtLoopEnd)
	tk_err(crtTk, "break without for or while");
      addInstrA(O_JMP, crtLoopEnd);
      return 1;
    }

  retval_t rv;
  if(consume(RETURN))
    {
      if(expr(&rv))
	{
	  if(crtFunc->type.typeBase == TB_VOID)
	    tk_err(crtTk, "void function should not return a value");
	  cast(&crtFunc->type, &rv.type);
	  instr_t i = getRVal(&rv);
	  addCastInstr(i, &rv.type, &crtFunc->type);
	}
      if(!consume(SEMICOLON)) tk_err(crtTk, "expected ';' after return");
      if(crtFunc->type.typeBase == TB_VOID)
	addInstrII(O_RET, sizeArgs, 0);
      else
	addInstrII(O_RET, sizeArgs, typeArgSize(&crtFunc->type));
      return 1;
    }

  token_t start = crtTk;
  instr_t starti = lastInstruction;
  if(expr(&rv))
    if(typeArgSize(&rv.type))
      addInstrI(O_DROP, typeArgSize(&rv.type));
  if(consume(SEMICOLON))
    return 1;

  crtTk = start;
  deleteInstructionsAfter(starti);
  return 0;
}



int exprUnary(retval_t *rv);



int exprCast(retval_t *rv)
{
  token_t start =crtTk;
  instr_t starti = lastInstruction;
  type_t t;
  retval_t rve;
  if(consume(LPAR) && typeName(&t) && consume(RPAR) && exprCast(&rve))
    {
      cast(&t, &rve.type);
      if(rv->type.nElements < 0 && rv->type.typeBase != TB_STRUCT)
	{
	  switch(rve.type.typeBase)
	    {
	    case TB_CHAR:
	      switch(t.typeBase)
		{
		case TB_INT: addInstr(O_CAST_C_I); break;
		case TB_DOUBLE:addInstr(O_CAST_C_D);break;
		default: tk_err(crtTk, "should not cas here");
		}
	      break;
	    case TB_DOUBLE:
	      switch(t.typeBase){
	      case TB_CHAR:addInstr(O_CAST_D_C);break;
	      case TB_INT:addInstr(O_CAST_D_I);break;
	      default: tk_err(crtTk, "should not cas here");
	      }
	      break;
	    case TB_INT:
	      switch(t.typeBase){
	      case TB_CHAR:addInstr(O_CAST_I_C);break;
	      case TB_DOUBLE:addInstr(O_CAST_I_D);break;
	      default: tk_err(crtTk, "should not cas here");
	      }
	      break;
	    default: tk_err(crtTk, "should not cas here");
	    }
	}
      
      rv->type = t;
      rv->isCtVal = rv->isLVal = 0;
      return 1;
    }
  deleteInstructionsAfter(starti);
  crtTk = start;
  return exprUnary(rv);
}

int exprMul(retval_t * rv)
{
  retval_t rve;
  instr_t i1,i2;
  type_t t1, t2;
  
  if(exprCast(rv))
    {
      while((consume(MUL) || consume(DIV)))
	{
	  token_t tk = consumedTk;

	  i1 = getRVal(rv);
	  t1 = rv->type;

	  if(!exprCast(&rve))
	    tk_err(crtTk,"expected expression after mul/div");
	  else
	    {
	      if (rv->type.nElements>-1 || rve.type.nElements > -1)
		tk_err(crtTk, "an array cannot be multiplied or divided");
	      if(rv->type.typeBase == TB_STRUCT || rve.type.typeBase == TB_STRUCT)
		tk_err(crtTk, "a structure cannot be multiplied or divided");
	      rv->type = getArithType(&rv->type, &rve.type);
	      if(rv->isCtVal && rve.isCtVal)
		{
		  rv->isCtVal = 1;
		  if (tk->code == MUL)
		    rv->ctVal.i *= rve.ctVal.i;
		  else
		    rv->ctVal.i /= rve.ctVal.i;
		  /*
		  addInstrI(O_PUSHCT_I, rv->ctVal.i);
		  continue;
		  rv->isLVal = 0;
		  */
		}
	      else
		rv->isCtVal = 0;
	      rv->isLVal = 0;
	      
	      
	      i2 = getRVal(&rve);
	      t2 = rve.type;
	      addCastInstr(i1, &t1, &rv->type);
	      addCastInstr(i2, &t2, &rv->type);

	      if(tk->code == MUL)
		{
		  switch(rv->type.typeBase) {
		  case TB_INT: addInstr(O_MUL_I); break;
		  case TB_DOUBLE: addInstr(O_MUL_D); break;
		  case TB_CHAR: addInstr(O_MUL_C); break;
		  default: tk_err(crtTk,"can't add structs");
		  }
		}
	      else
		{
		  switch(rv->type.typeBase) {
		  case TB_INT: addInstr(O_DIV_I); break;
		  case TB_DOUBLE: addInstr(O_DIV_D); break;
		  case TB_CHAR: addInstr(O_DIV_C); break;
		  default: tk_err(crtTk, "can't add structs");
		  }
		}

	      
	    }
	}
      return 1;
    }
  return 0;
  
}


int exprAdd(retval_t *rv)
{
  retval_t rve;
  
  instr_t i1,i2;
  type_t t1, t2;
  
  if(exprMul(rv))
    {
      while((consume(ADD) || consume(SUB)))
	{
	  token_t tk = consumedTk;

	  i1 = getRVal(rv);
	  t1 = rv->type;
	  
	  if(!exprMul(&rve))
	    tk_err(crtTk,"expected expression after add/sub");
	  else
	    {
	      if (rv->type.nElements>-1 || rve.type.nElements > -1)
		tk_err(crtTk, "an array cannot be added or subtracted");
	      if(rv->type.typeBase == TB_STRUCT || rve.type.typeBase == TB_STRUCT)
		tk_err(crtTk, "a structure cannot be added or subtracted");
	      rv->type = getArithType(&rv->type, &rve.type);

		
	      if(rv->isCtVal && rve.isCtVal)
		{
		  rv->isCtVal = 1;
		  if(tk->code == ADD)
		    rv->ctVal.i += rve.ctVal.i;
		  else
		    rv->ctVal.i -= rve.ctVal.i;

		  /*
		  addInstrI(O_PUSHCT_I, rv->ctVal.i);
		  continue;
		  rv->isLVal = 0;
		  */
		}
	      else
		rv->isCtVal = 0;
	      rv->isLVal = 0;

	      i2 = getRVal(&rve);
	      t2 = rve.type;
	      addCastInstr(i1, &t1, &rv->type);
	      addCastInstr(i2, &t2, &rv->type);

	      if(tk->code == ADD)
		{
		  switch(rv->type.typeBase) {
		  case TB_INT: addInstr(O_ADD_I); break;
		  case TB_DOUBLE: addInstr(O_ADD_D); break;
		  case TB_CHAR: addInstr(O_ADD_C); break;
		  default: tk_err(crtTk,"can't add structs");
		  }
		}
	      else
		{
		  switch(rv->type.typeBase) {
		  case TB_INT: addInstr(O_SUB_I); break;
		  case TB_DOUBLE: addInstr(O_SUB_D); break;
		  case TB_CHAR: addInstr(O_SUB_C); break;
		  default: tk_err(crtTk, "can't add structs");
		  }
		}
	    }
	}
      return 1;
      
    }
  return 0; 
}
  

int exprRel(retval_t *rv)
{
  
  retval_t rve;
  instr_t i1,i2;
  type_t t,t1,t2;
  
  if(exprAdd(rv))
    {

      while((consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ)))
	{

	  i1 = getRVal(rv);
	  t1 = rv->type;

	  token_t tkop = consumedTk;
	  if(!exprAdd(&rve))
	    tk_err(crtTk,"expected expression after relative operator");
	  else
	    {
	      
	      if(rv->type.nElements > -1 || rve.type.nElements > -1)
		tk_err(crtTk,"an array cannot be compared");
	      if(rv->type.typeBase==TB_STRUCT||rve.type.typeBase==TB_STRUCT)
		tk_err(crtTk,"a structure cannot be compared");
	      
	      i2 = getRVal(&rve); t2 = rve.type;
	      t = getArithType(&t1, &t2);
	      addCastInstr(i1, &t1, &t);
	      addCastInstr(i2, &t2, &t);

	      switch(tkop->code)
		{
		case LESS:
		  switch(t.typeBase){
		  case TB_INT:addInstr(O_LESS_I);break;
		  case TB_DOUBLE:addInstr(O_LESS_D);break;
		  case TB_CHAR:addInstr(O_LESS_C);break;
		  default: tk_err(crtTk,"illegal compare");
		  }
		  break;
                case LESSEQ:
		  switch(t.typeBase){
		  case TB_INT:addInstr(O_LESSEQ_I);break;
		  case TB_DOUBLE:addInstr(O_LESSEQ_D);break;
		  case TB_CHAR:addInstr(O_LESSEQ_C);break;
		  default: tk_err(crtTk,"illegal compare");
		  }
		  break;
                case GREATER:
		  switch(t.typeBase){
		  case TB_INT:addInstr(O_GREATER_I);break;
		  case TB_DOUBLE:addInstr(O_GREATER_D);break;
		  case TB_CHAR:addInstr(O_GREATER_C);break;
		  default: tk_err(crtTk,"illegal compare");
		  }
		  break;
                case GREATEREQ:
		  switch(t.typeBase){
		  case TB_INT:addInstr(O_GREATEREQ_I);break;
		  case TB_DOUBLE:addInstr(O_GREATEREQ_D);break;
		  case TB_CHAR:addInstr(O_GREATEREQ_C);break;
		  default: tk_err(crtTk,"illegal compare");
		  }
		  break;
		default: tk_err(crtTk,"illegal compare");
		}
	    
	      rv->type=createType(TB_INT,-1);
	      rv->isCtVal=rv->isLVal=0;
	    }
	}
      return 1;
    }
  return 0;
  
}

int exprEq(retval_t *rv)
{
  retval_t rve;
  instr_t i1,i2;
  type_t t,t1,t2;
  //  instr_t starti = lastInstruction;
  if(exprRel(rv))
    {

      //deleteInstructionsAfter(starti);
	      
      while((consume(EQUAL) || consume(NOTEQ)))
	{
	  i1 = rv->type.nElements < 0 ? getRVal(rv) : lastInstruction;
	  t1 = rv->type;
	  token_t tkop = consumedTk;
	  if(!exprRel(&rve))
	    tk_err(crtTk,"expected expression after equality test");
	  else
	    {
	      
	      if(rv->type.typeBase==TB_STRUCT||rve.type.typeBase==TB_STRUCT)
		tk_err(crtTk,"a structure cannot be compared");
	      
	      if(rv->type.nElements >= 0)
		addInstr(tkop->code == EQUAL? O_EQ_A : O_NOTEQ_A);
	      else
		{
		  i2 = getRVal(&rve);
		  t2 = rve.type;
		  t = getArithType(&t1, &t2);
		  addCastInstr(i1, &t1, &t);
		  addCastInstr(i2, &t2, &t);
		  if(tkop->code == EQUAL)
		    {
		      switch(t.typeBase)
			{
			case TB_INT: addInstr(O_EQ_I); break;
			case TB_DOUBLE: addInstr(O_EQ_D); break;
			case TB_CHAR: addInstr(O_EQ_C);
			default: my_err("should not get here from eq");
			}
		    }
		  else
		    {
		      switch(t.typeBase)
			{
			case TB_INT: addInstr(O_NOTEQ_I); break;
			case TB_DOUBLE: addInstr(O_NOTEQ_D); break;
			case TB_CHAR: addInstr(O_NOTEQ_C);
			default: my_err("should not get here from noteq");
			}

		      
		    }
		    
		}
	      
	      rv->type=createType(TB_INT,-1);
	      rv->isCtVal=rv->isLVal=0;
	    }
	}
      return 1;
    }
  return 0;
}

int exprAnd(retval_t *rv)
{

  instr_t i1,i2;
  type_t t, t1, t2;
  retval_t rve;
  if (exprEq(rv))
    {

      while(consume(AND))
	{
	  i1 = rv->type.nElements < 0 ? getRVal(rv) : lastInstruction;
	  t1 = rv->type;
	  if(!exprEq(&rve))
	    tk_err(crtTk,"expected expression after and");
	  else
	    {
	      
	      
	      if(rv->type.typeBase==TB_STRUCT||rve.type.typeBase==TB_STRUCT)
		tk_err(crtTk,"a structure cannot be logically tested");
	      
	      if(rv->type.nElements >= 0)
		addInstr(O_AND_A);
	      else
		{
		  i2 = getRVal(&rve);
		  t2 = rve.type;
		  t = getArithType(&t1, &t2);
		  addCastInstr(i1, &t1, &t);
		  addCastInstr(i2, &t2, &t);
		  switch(t.typeBase)
		    {
		    case TB_INT: addInstr(O_AND_I); break;
		    case TB_DOUBLE: addInstr(O_AND_D); break;
		    case TB_CHAR: addInstr(O_AND_C);
		    default: my_err("should not get here from and");
		    }
		}
	      
	      rv->type=createType(TB_INT,-1);
	      rv->isCtVal=rv->isLVal=0;
	    }
	}
      return 1;
    }
  return 0;
}

int exprOr(retval_t *rv)
{

  instr_t i1, i2;
  type_t t,t1,t2;
  
  retval_t rve;
  if(exprAnd(rv))
    {

      while(consume(OR))
	{
	  i1 = rv->type.nElements < 0 ? getRVal(rv) : lastInstruction;
	  t1 = rv->type;
	  if(!exprAnd(&rve))
	    tk_err(crtTk,"expected expression after or");
	  else
	    {
 
	      
	      if(rv->type.typeBase==TB_STRUCT||rve.type.typeBase==TB_STRUCT)
		tk_err(crtTk,"a structure cannot be logically tested");
	      
	      
	      if(rv->type.nElements >= 0)
		addInstr(O_OR_A);
	      else
		{
		  i2 = getRVal(&rve);
		  t2 = rve.type;
		  t = getArithType(&t1, &t2);
		  addCastInstr(i1, &t1, &t);
		  addCastInstr(i2, &t2, &t);
		  switch(t.typeBase)
		    {
		    case TB_INT: addInstr(O_OR_I); break;
		    case TB_DOUBLE: addInstr(O_OR_D); break;
		    case TB_CHAR: addInstr(O_OR_C);
		    default: my_err("should not get here from or");
		    }
		}
	      
	      rv->type=createType(TB_INT,-1);
	      rv->isCtVal=rv->isLVal=0;
	    }
	}
      return 1;
    }
  
  return 0;
}

int exprAssign(retval_t *rv)
{
  token_t start = crtTk;
  instr_t starti = lastInstruction;
  retval_t rve;
  if(exprUnary(rv) && consume(ASSIGN) && exprAssign(&rve))
    {
      if(!rv->isLVal) tk_err(crtTk, "cannot assign to a non-lval");
      if(rv->type.nElements > -1 || rve.type.nElements > -1)
	tk_err(crtTk, "the arrays cannot be assigned");
      cast(&rv->type, &rve.type);

      instr_t i = getRVal(&rve);
      addCastInstr(i, &rve.type, &rv->type);
      addInstrII(O_INSERT, sizeof(void *) + typeArgSize(&rv->type), typeArgSize(&rv->type));
      addInstrI(O_STORE, typeArgSize(&rv->type));
      
      rv->isCtVal = rv->isLVal = 0;
      return 1;
    }
  crtTk = start;
  deleteInstructionsAfter(starti);
  if(exprOr(rv))
    return 1;
  
  return 0;
}

int expr(retval_t *rv)
{
  if(exprAssign(rv)) return 1;
  return 0;
}

int exprPrimary(retval_t *rv);
int exprPostfix(retval_t *rv)
{
  //  token_t start = crtTk;
  retval_t rve;
  if(exprPrimary(rv))
    {
      while(1)
	{
	  token_t bif = crtTk;
	  instr_t bifi = lastInstruction;
	  if(consume(DOT))
	    {
	      if(!consume(ID))
		tk_err(crtTk,"expected identifier after dot");
	      
	      token_t tkName = consumedTk;
	      symbol_t *sStruct = rv->type.s;
	      symbol_t *sMember = findSymbol(&sStruct->members, tkName->text);
	      if(!sMember)
		tk_err(tkName, "struct %s does not have member %s", sStruct->name, tkName->text);
	      rv->type = sMember->type;
	      rv->isLVal = 1;
	      rv->isCtVal = 0;
	      if(sMember->offset)
		{
		  addInstrI(O_PUSHCT_I, typeBaseSize(&rv->type));
		  addInstr(O_OFFSET);
		}
	      continue;
	    }
	  crtTk = bif;
	  deleteInstructionsAfter(bifi);
	  if(consume(LBRACKET))
	    {
	      if(!expr(&rve)) tk_err(crtTk,"expected expression after '['");
	      if(rv->type.nElements < 0)
		tk_err(crtTk, "trying to index something that is not an array");
	      type_t typeInt = createType(TB_INT, -1);
	      cast(&typeInt, &rve.type);
	      rv->type = rv->type;
	      rv->type.nElements = -1;
	      rv->isLVal = 1;
	      rv->isCtVal = 0;
	      if(!consume(RBRACKET)) tk_err(crtTk,"expected ']'");

	      addCastInstr(lastInstruction, &rve.type, &typeInt);
	      getRVal(&rve);
	      if(typeBaseSize(&rv->type) != 1)
		{
		  addInstrI(O_PUSHCT_I, typeBaseSize(&rv->type));
		  addInstr(O_MUL_I);
		}
	      addInstr(O_OFFSET);
	      
	      continue;
	    }
	  crtTk = bif;
	  deleteInstructionsAfter(bifi);
	  return 1;
	}
    }
  return 0;
}

int exprPrimary(retval_t *rv)
{
  if(consume(CT_INT))
    {
      //printf("found int: %d\n", consumedTk->i);
      rv->type = createType(TB_INT, -1);
      rv->ctVal.i = consumedTk->i;
      rv->isCtVal = 1;
      rv->isLVal = 0;


      addInstrI(O_PUSHCT_I, consumedTk->i);

      return 1;
    }

  if(consume(CT_REAL))
    {
      //printf("found double: %f\n", consumedTk->r);
      rv->type = createType(TB_DOUBLE, -1);
      rv->ctVal.d = consumedTk->r;
      rv->isCtVal = 1;
      rv->isLVal = 0;
      
      instr_t i = addInstr(O_PUSHCT_D);
      i->args[0].d = consumedTk->r;
      
      return 1;
    }
    
  if(consume(CT_CHAR))
    {
      rv->type = createType(TB_CHAR, -1);
      rv->ctVal.i = consumedTk->i;
      rv->isCtVal = 1;
      rv->isLVal = 0;

      addInstrI(O_PUSHCT_C, consumedTk->i);
      
      return 1;
    }
  if(consume(CT_STRING))
    {
      rv->type = createType(TB_CHAR, 0);
      rv->ctVal.str = consumedTk->text;
      rv->isCtVal = 1;
      rv->isLVal = 0;

      addInstrA(O_PUSHCT_A, consumedTk->text);
      
      return 1; 
    }
  
  if(consume(LPAR))
    {
      if(!expr(rv)) tk_err(crtTk,"expected expression after '('");
      if(!consume(RPAR)) tk_err(crtTk,"expected ')'");
      return 1;
    }

  if (consume(ID))
    {
      instr_t i =NULL;
      
      token_t tkName = consumedTk;
      symbol_t *s = findSymbol(&symbols, tkName->text);
      if(!s)
	tk_err(tkName, "undefined symbol %s", tkName->text);
      rv->type = s->type;
      rv->isCtVal = 0;
      rv->isLVal = 1;
      if(consume(LPAR))
	{
	  if(s->cls != CLS_FUNC && s->cls != CLS_EXTFUNC)
	    tk_err(crtTk, "call of the non-function %s", tkName->text);
	  symbol_t **crtDefArg = s->args.begin;
	  retval_t arg;
	  if(expr(&arg))
	    {
	      if(crtDefArg == s->args.end) tk_err(crtTk, "too many arguments in call");
	      cast(&(*crtDefArg)->type, &arg.type);

	      if((*crtDefArg)->type.nElements <0)
		i = getRVal(&arg);
	      else
		i = lastInstruction;
	      addCastInstr(i, &arg.type, &(*crtDefArg)->type);
	      
	      crtDefArg++;
	      
	      while(consume(COMMA))
		if(!expr(&arg)) tk_err(consumedTk, "expected expression after commma");
		else
		  {
		    if(crtDefArg == s->args.end) tk_err(crtTk, "too many arguments in call");
		    cast(&(*crtDefArg)->type, &arg.type);
		    
		    if((*crtDefArg)->type.nElements <0)
		      i = getRVal(&arg);
		    else
		      i = lastInstruction;
		    addCastInstr(i, &arg.type, &(*crtDefArg)->type);
		    
		    crtDefArg++;
		  }
	    }
	  if(consume(RPAR))
	    {
	      if(crtDefArg != s->args.end) tk_err(crtTk, "too few arguments in call");
	      rv->type = s->type;
	      rv->isCtVal = rv->isLVal = 0;

	      i = addInstr(s->cls == CLS_FUNC ? O_CALL : O_CALLEXT);
	      i->args[0].addr = s->addr;
	      
	      return 1;
	    }
	    
	  else tk_err(crtTk, "expected ')'");
	}
      else if(s->cls == CLS_FUNC || s->cls == CLS_EXTFUNC)
	tk_err(crtTk,"missing call for function %s", tkName->text);

      if(s->depth)
	addInstrI(O_PUSHFPADDR, s->offset);
      else
	addInstrA(O_PUSHCT_A, s->addr);
      
      return 1;
    }
  
  return 0;
}

int exprUnary(retval_t *rv)
{
  if(exprPostfix(rv))
    return 1;

  if(consume(SUB) || consume(NOT))
    {
      token_t tkop = consumedTk;
      if(exprUnary(rv))
	{
	  if (tkop->code == SUB)
	    {
	      if (rv->type.nElements >= 0) tk_err(crtTk, "unary '-' cannot be applied to array");
	      if (rv->type.typeBase == TB_STRUCT) tk_err(crtTk, "unary '-' cannot be applied to structures");
	      getRVal(rv);
	      switch(rv->type.typeBase){
	      case TB_CHAR:addInstr(O_NEG_C);break;
	      case TB_INT:addInstr(O_NEG_I);break;
	      case TB_DOUBLE:addInstr(O_NEG_D);break;
	      default: tk_err(crtTk, "should not cast '-'");
	      }
	    }
	  else  {
	    if (rv->type.typeBase == TB_STRUCT) tk_err(crtTk, "'!' cannot be applied to structures");
	    if(rv->type.nElements<0)
	      {
		getRVal(rv);
		switch(rv->type.typeBase){
		case TB_CHAR:addInstr(O_NOT_C);break;
		case TB_INT:addInstr(O_NOT_I);break;
		case TB_DOUBLE:addInstr(O_NOT_D);break;
		default: tk_err(crtTk, "should not cast '!'");
	      }
	      }
	    else
	      {
		addInstr(O_NOT_A);      
	      }
	    rv->type = createType(TB_INT, -1);
	  }
	  rv->isCtVal = rv->isLVal = 0;
	  return 1;
	}
      else tk_err(crtTk, "expected unary expression after -/!");
    }
  return 0;
}


int unit()
{
  instr_t labelMain = addInstr(O_CALL);
  addInstr(O_HALT);
  while(declStruct() || declFunc() || declVar());
  if(!consume(END)) tk_err(crtTk, "expected end");
  labelMain->args[0].addr = requireSymbol(&symbols,"main")->addr;
  
  return 1;
}



int main(int argc, char *argv[])
{
  
  FILE *f = fopen(argv[1], "r");
  if(!f)
    my_err("Error opening file.");

  fseek(f, 0, SEEK_END);
  long fsize = ftell(f);
  fseek(f, 0, SEEK_SET);

  SAFEALLOC(pBegCh, char, fsize+1);
  if(fread(pBegCh, fsize, 1, f) != 1)
    my_err("Error reading file.");
  pBegCh[fsize] = '\0';
  pCrtCh = pBegCh;

  while(getNextToken() != END);
    //    print_token(lastToken);
  //print_token(lastToken);
  token_t t;
  for(t = tokens; t != NULL; t = t->next)
    print_token(t);
  crtTk = tokens;
  initSymbols(&symbols);
  addDefaultSymbols(&symbols);

  /*
  s = addSymbol(&symbols, "a", CLS_VAR);
  type_t typ = {TB_INT, NULL, -1};
  s->type = typ;
  retval_t rv;
  */
  //  type_t type;
  printf("%d\n", unit());
  //printf("%d[%d]", type.typeBase, type.nElements);
  //  printf("%d\n", declVar());
  print_token(consumedTk);
  printSymbols(&symbols,0);
  startProgram();
  //puts(pBegCh);

    //print_token(tokens);
  //  print_token(lastToken);
  

  return 0;
}

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "utils.h"
#include "vm.h"
#include "sym.h"


#define STACK_SIZE (32 * 1024)
#define GLOBAL_SIZE (32 * 1024)

char stack[STACK_SIZE];
char *SP;
char *stackAfter;

char globals[GLOBAL_SIZE];
int nGlobals;


//symbols_t symbols;


instr_t lastInstruction, instructions;


void pushd(double d)
{
  if(SP + sizeof(double) > stackAfter) my_err("out of stack space");
  *(double *)SP = d;
  SP += sizeof(double);
}

double popd()
{
  SP -= sizeof(double);
  if(SP < stack) my_err("negative stack value");
  return *(double *)SP;
}

void pushi(int d)
{
  if(SP + sizeof(int) > stackAfter) my_err("out of stack space");
  *(int *)SP = d;
  SP += sizeof(int);
}

int popi()
{
  SP -= sizeof(int);
  if(SP < stack) my_err("negative stack value");
  return *(int *)SP;
}

void pushc(char d)
{

  if(SP + sizeof(char) > stackAfter) my_err("out of stack space");
  *(char *)SP = d;
  SP += sizeof(char);
}

char popc()
{
  SP -= sizeof(char);
  if(SP < stack) my_err("negative stack value");
  return *(char *)SP;
}

void pusha(void *a)
{
  if(SP + sizeof(void *) > stackAfter) my_err("out of stack space");
  *(void **)SP = a;
  SP += sizeof(void *);
}

void *popa()
{
  SP -= sizeof(void *);
  if(SP < stack) my_err("negative stack value");
  return *(void **)SP;
}

instr_t createInstr(int opcode)
{
  instr_t i;
  SAFEALLOC(i, Instr, 1);
  i->opcode = opcode;
  return i;
}


void insertInstrAfter(instr_t after, instr_t i)
{
  i->flink = after->flink;
  i->blink = after;
  after->flink = i;
  if(i->flink == NULL) lastInstruction = i;
}


instr_t addInstrAfter(instr_t after, opcode_t op)
{
  instr_t i = createInstr(op);
  insertInstrAfter(after, i);
  return i;
}


instr_t appendInstr(instr_t i)
{
  if (lastInstruction)
    lastInstruction->flink = i;
  else
    instructions = i;
  lastInstruction = i;
  i->flink = NULL;
  return i;
}

instr_t addInstr(int opcode)
{
  instr_t i = createInstr(opcode);
  i->flink = NULL;
  i->blink = lastInstruction;
  if (lastInstruction)
    lastInstruction->flink = i;
  else
    instructions = i;
  lastInstruction = i;
  return i;
}


instr_t addInstrA(int opcode, void *addr)
{
  instr_t i = addInstr(opcode);
  i->args[0].addr = addr;
  return i;
}

instr_t addInstrI(int opcode, int val)
{
  instr_t i = addInstr(opcode);
  i->args[0].i = val;
  return i;
}

instr_t addInstrII(int opcode, int val1, int val2)
{
  instr_t i = addInstr(opcode);
  i->args[0].i = val1;
  i->args[1].i = val2;
  return i;
}

void deleteInstructionsAfter(instr_t start)
{
  instr_t i;
  instr_t next;
  if(! (start->flink)) //already last instruction
    return;
  for(i = start->flink, next = start->flink->flink; next; next = next->flink)
    {
      free(i);
      i = next;
    }
  free(i);
  lastInstruction = start;
  start->flink = NULL;

}


void startProgram()
{
  run(instructions);
}


void run(instr_t IP)
{
  int iVal1, iVal2;
  double dVal1, dVal2;
  char cVal1, cVal2;
  char *aVal1, *aVal2;
  char *FP = 0, *oldSP;
  SP = stack;
  stackAfter = stack + STACK_SIZE;
  while(1)
    {
      printf("%p/%d\t", IP, (int)(SP - stack));
      switch(IP->opcode) {
      case O_CALL:
	aVal1 = IP->args[0].addr;
	printf("CALL\t %p\n", aVal1);
	pusha(IP->flink);
	IP = (instr_t)aVal1;
	break;
      case O_CALLEXT:
	printf("CALLEXT\t%p\n", IP->args[0].addr);
	(*(void (*)())IP->args[0].addr)();
	IP = IP->flink;
	break;
      case O_CAST_I_D:
	iVal1 = popi();
	dVal1 = (double)iVal1;
	printf("CAST_I_D\t(%d -> %g)\n", iVal1, dVal1);
	pushd(dVal1);
	IP = IP->flink;
	break;
      case O_CAST_I_C:
	iVal1 = popi();
	cVal1 = (char)iVal1;
	printf("CAST_I_C\t(%d -> %d)\n", iVal1, cVal1);
	pushc(cVal1);
	IP = IP->flink;
	break;
      case O_CAST_D_I:
	dVal1 = popd();
	iVal1 = (int)dVal1;
	printf("CAST_D_I\t(%g -> %d)\n", dVal1, iVal1);
	pushi(iVal1);
	IP = IP->flink;
	break;
      case O_CAST_D_C:
	dVal1 = popd();
	cVal1 = (char)dVal1;
	printf("CAST_D_C\t(%g -> %d)\n", dVal1, cVal1);
	pushc(cVal1);
	IP = IP->flink;
	break;
      case O_CAST_C_I:
	cVal1 = popc();
	iVal1 = (int)cVal1;
	printf("CAST_C_I\t(%d -> %d)\n", cVal1, iVal1);
	pushi(iVal1);
	IP = IP->flink;
	break;
      case O_CAST_C_D:
	cVal1 = popc();
	dVal1 = (double)cVal1;
	printf("CAST_C_D\t(%d -> %g)\n", cVal1, dVal1);
	pushd(dVal1);
	IP = IP->flink;
	break;
      	
      case O_DROP:
	iVal1 = IP->args[0].i;
	printf("DROP\t%d\n", iVal1);
	if(SP - iVal1 < stack) my_err("not enough stack bytes");
	SP -= iVal1;
	IP = IP->flink;
	break;
      case O_ENTER:
	iVal1 = IP->args[0].i;
	printf("ENTER\t%d\n", iVal1);
	pusha(FP);
	FP = SP;
	SP += iVal1;
	IP = IP->flink;
	break;
	
      case O_EQ_D:
	dVal1 = popd();
	dVal2 = popd();
	printf("EQ_D\t(%g == %g -> %d)\n", dVal2, dVal1, dVal2 == dVal1);
	pushi(dVal2 == dVal1);
	IP = IP->flink;
	break;	
      case O_EQ_I:
	iVal1 = popi();
	iVal2 = popi();
	printf("EQ_I\t(%d == %d -> %d)\n", iVal2, iVal1, iVal2 == iVal1);
	pushi(iVal2 == iVal1);
	IP = IP->flink;
	break;
      case O_EQ_C:
	cVal1 = popc();
	cVal2 = popc();
	printf("EQ_C\t(%d == %d -> %d)\n", cVal2, cVal1, cVal2 == cVal1);
	pushi(cVal2 == cVal1);
	IP = IP->flink;
	break;
      case O_EQ_A:
	aVal1 = popa();
	aVal2 = popa();
	printf("EQ_A\t(%p == %p -> %d)\n", aVal2, aVal1, aVal2 == aVal1);
	pushi(aVal2 == aVal1);
	IP = IP->flink;
	break;

      case O_NOTEQ_D:
	dVal1 = popd();
	dVal2 = popd();
	printf("NOTEQ_D\t(%g != %g -> %d)\n", dVal2, dVal1, dVal2 != dVal1);
	pushi(dVal2 != dVal1);
	IP = IP->flink;
	break;	
      case O_NOTEQ_I:
	iVal1 = popi();
	iVal2 = popi();
	printf("NOTEQ_I\t(%d != %d -> %d)\n", iVal2, iVal1, iVal2 != iVal1);
	pushi(iVal2 != iVal1);
	IP = IP->flink;
	break;
      case O_NOTEQ_C:
	cVal1 = popc();
	cVal2 = popc();
	printf("NOTEQ_C\t(%d != %d -> %d)\n", cVal2, cVal1, cVal2 != cVal1);
	pushi(cVal2 != cVal1);
	IP = IP->flink;
	break;
      case O_NOTEQ_A:
	aVal1 = popa();
	aVal2 = popa();
	printf("NOTEQ_A\t(%p != %p -> %d)\n", aVal2, aVal1, aVal2 != aVal1);
	pushi(aVal2 != aVal1);
	IP = IP->flink;
	break;

	
      case O_GREATER_D:
	dVal1 = popd();
	dVal2 = popd();
	printf("GREATER_D\t(%g == %g -> %d)\n", dVal2, dVal1, dVal2 > dVal1);
	pushi(dVal2 > dVal1);
	IP = IP->flink;
	break;	
      case O_GREATER_I:
	iVal1 = popi();
	iVal2 = popi();
	printf("GREATER_I\t(%d == %d -> %d)\n", iVal2, iVal1, iVal2 > iVal1);
	pushi(iVal2 > iVal1);
	IP = IP->flink;
	break;
      case O_GREATER_C:
	cVal1 = popc();
	cVal2 = popc();
	printf("GREATER_C\t(%d == %d -> %d)\n", cVal2, cVal1, cVal2 > cVal1);
	pushi(cVal2 > cVal1);
	IP = IP->flink;
	break;

      case O_GREATEREQ_D:
	dVal1 = popd();
	dVal2 = popd();
	printf("GREATEREQ_D\t(%g == %g -> %d)\n", dVal2, dVal1, dVal2 >= dVal1);
	pushi(dVal2 >= dVal1);
	IP = IP->flink;
	break;	
      case O_GREATEREQ_I:
	iVal1 = popi();
	iVal2 = popi();
	printf("GREATEREQ_I\t(%d == %d -> %d)\n", iVal2, iVal1, iVal2 >= iVal1);
	pushi(iVal2 >= iVal1);
	IP = IP->flink;
	break;
      case O_GREATEREQ_C:
	cVal1 = popc();
	cVal2 = popc();
	printf("GREATEREQ_C\t(%d == %d -> %d)\n", cVal2, cVal1, cVal2 >= cVal1);
	pushi(cVal2 >= cVal1);
	IP = IP->flink;
	break;

      case O_LESS_D:
	dVal1 = popd();
	dVal2 = popd();
	printf("LESS_D\t(%g == %g -> %d)\n", dVal2, dVal1, dVal2 < dVal1);
	pushi(dVal2 < dVal1);
	IP = IP->flink;
	break;	
      case O_LESS_I:
	iVal1 = popi();
	iVal2 = popi();
	printf("LESS_I\t(%d == %d -> %d)\n", iVal2, iVal1, iVal2 < iVal1);
	pushi(iVal2 < iVal1);
	IP = IP->flink;
	break;
      case O_LESS_C:
	cVal1 = popc();
	cVal2 = popc();
	printf("LESS_C\t(%d == %d -> %d)\n", cVal2, cVal1, cVal2 < cVal1);
	pushi(cVal2 < cVal1);
	IP = IP->flink;
	break;
	
      case O_LESSEQ_D:
	dVal1 = popd();
	dVal2 = popd();
	printf("LESSEQ_D\t(%g == %g -> %d)\n", dVal2, dVal1, dVal2 <= dVal1);
	pushi(dVal2 <= dVal1);
	IP = IP->flink;
	break;	
      case O_LESSEQ_I:
	iVal1 = popi();
	iVal2 = popi();
	printf("LESSEQ_I\t(%d == %d -> %d)\n", iVal2, iVal1, iVal2 <= iVal1);
	pushi(iVal2 <= iVal1);
	IP = IP->flink;
	break;
      case O_LESSEQ_C:
	cVal1 = popc();
	cVal2 = popc();
	printf("LESS_C\t(%d == %d -> %d)\n", cVal2, cVal1, cVal2 <= cVal1);
	pushi(cVal2 <= cVal1);
	IP = IP->flink;
	break;

      case O_HALT:
	printf("HALT\n");
	return;
      case O_INSERT:
	iVal1 = IP->args[0].i;
	iVal2 = IP->args[1].i;
	printf("INSERT\t%d,%d\n", iVal1, iVal2);
	if (SP + iVal2 > stackAfter) my_err("out of stack");
	memmove(SP - iVal1 + iVal2, SP - iVal1, iVal1);
	memmove(SP - iVal1, SP + iVal2, iVal2);
	SP += iVal2;
	IP = IP->flink;
	break;
	
      case O_JT_I:
	iVal1 = popi();
	printf("JT_I\t%p\t(%d)\n", IP->args[0].addr, iVal1);
	IP = iVal1 ? IP->args[0].addr : IP->flink;
	break;
      case O_JT_A:
	aVal1 = popa();
	printf("JT_A\t%p\t(%p)\n", IP->args[0].addr, aVal1);
	IP = aVal1 ? IP->args[0].addr : IP->flink;
	break;
      case O_JT_D:
	dVal1 = popd();
	printf("JT_D\t%p\t(%g)\n", IP->args[0].addr, dVal1);
	IP = dVal1 ? IP->args[0].addr : IP->flink;
	break;
      case O_JT_C:
	cVal1 = popc();
	printf("JT_C\t%p\t(%d)\n", IP->args[0].addr, cVal1);
	IP = cVal1 ? IP->args[0].addr : IP->flink;
	break;

      case O_JF_I:
	iVal1 = popi();
	printf("JF_I\t%p\t(%d)\n", IP->args[0].addr, iVal1);
	IP = (!iVal1) ? IP->args[0].addr : IP->flink;
	break;
      case O_JF_A:
	aVal1 = popa();
	printf("JF_A\t%p\t(%p)\n", IP->args[0].addr, aVal1);
	IP = (!aVal1) ? IP->args[0].addr : IP->flink;
	break;
      case O_JF_D:
	dVal1 = popd();
	printf("JF_D\t%p\t(%g)\n", IP->args[0].addr, dVal1);
	IP = (!dVal1) ? IP->args[0].addr : IP->flink;
	break;
      case O_JF_C:
	cVal1 = popc();
	printf("JF_C\t%p\t(%d)\n", IP->args[0].addr, cVal1);
	IP = (!cVal1) ? IP->args[0].addr : IP->flink;
	break;

      case O_JMP:
	printf("JMP\t%p\n", IP->args[0].addr);
	IP = IP->args[0].addr;
	break;
	
      case O_LOAD:
	iVal1 = IP->args[0].i;
	aVal1 = popa();
	printf("LOAD\t%d\t(%p)\n", iVal1, aVal1);
	if(SP + iVal1 > stackAfter) my_err("out of stack");
	memcpy(SP, aVal1, iVal1);
	SP += iVal1;
	IP = IP->flink;
	break;
      case O_OFFSET:
	iVal1 = popi();
	aVal1 = popa();
	printf("OFFSET\t(%p + %d -> %p)\n", aVal1, iVal1, aVal1 + iVal1);
	pusha(aVal1 + iVal1);
	IP = IP->flink;
	break;
      case O_PUSHFPADDR:
	iVal1 = IP->args[0].i;
	printf("PUSHFPADDR\t%d\t(%p)\n", iVal1, FP + iVal1);
	pusha(FP + iVal1);
	IP = IP->flink;
	break;
	
      case O_PUSHCT_A:
	aVal1 = IP->args[0].addr;
	printf("PUSHCT_A\t%p\n", aVal1);
	pusha(aVal1);
	IP = IP->flink;
	break;
      case O_PUSHCT_I:
	iVal1 = IP->args[0].i;
	printf("PUSHCT_I\t%d\n", iVal1);
	pushi(iVal1);
	IP = IP->flink;
	break;
      case O_PUSHCT_D:
	dVal1 = IP->args[0].d;
	printf("PUSHCT_D\t%g\n", dVal1);
	pushd(dVal1);
	IP = IP->flink;
	break;
      case O_PUSHCT_C:
	cVal1 = IP->args[0].i;
	printf("PUSHCT_C\t%d\n", cVal1);
	pushc(cVal1);
	IP = IP->flink;
	break;
      
      case O_RET:
	iVal1 = IP->args[0].i;
	iVal2 = IP->args[1].i;
	printf("RET\t%d,%d\n", iVal1, iVal2);
	oldSP = SP;
	SP = FP;
	FP = popa();
	IP = popa();
	if (SP - iVal1 < stack) my_err("not enough stack bytes");
	SP -= iVal1;
	memmove(SP, oldSP - iVal2, iVal2);
	SP += iVal2;
	break;
	
      case O_STORE:
	iVal1 = IP->args[0].i;
	if (SP - (sizeof(void *) + iVal1) < stack) my_err("not enough stack bytes for SET");
	aVal1 = *(void **)(SP -(sizeof(void *) + iVal1));
	printf("STORE\t%d\t(%p)\n", iVal1, aVal1);
	memcpy(aVal1, SP - iVal1, iVal1);
	SP -= sizeof(void *) + iVal1;
	IP = IP->flink;
	break;
      case O_SUB_D:
	dVal1 = popd();
	dVal2 = popd();
	printf("SUB_D\t(%g-%g -> %g)\n", dVal2, dVal1, dVal2 - dVal1);
	pushd(dVal2 - dVal1);
	IP = IP->flink;
	break;
      case O_SUB_I:
	iVal1 = popi();
	iVal2 = popi();
	printf("SUB_I\t(%d - %d -> %d)\n", iVal2, iVal1, iVal2 - iVal1);
	pushi(iVal2 - iVal1);
	IP = IP->flink;
	break;
      case O_SUB_C:
	cVal1 = popc();
	cVal2 = popc();
	printf("SUB_C\t(%d - %d -> %dd)\n", cVal2, cVal1, cVal2 - cVal1);
	pushc(cVal2 - cVal1);
	IP = IP->flink;
	break;
      case O_ADD_D:
	dVal1 = popd();
	dVal2 = popd();
	printf("ADD_D\t(%g+%g -> %g)\n", dVal2, dVal1, dVal2 + dVal1);
	pushd(dVal2 + dVal1);
	IP = IP->flink;
	break;
      case O_ADD_I:
	iVal1 = popi();
	iVal2 = popi();
	printf("ADD_I\t(%d + %d -> %d)\n", iVal2, iVal1, iVal2 + iVal1);
	pushi(iVal2 + iVal1);
	IP = IP->flink;
	break;
      case O_ADD_C:
	cVal1 = popc();
	cVal2 = popc();
	printf("ADD_C\t(%d + %d -> %d)\n", cVal2, cVal1, cVal2 + cVal1);
	pushc(cVal2 + cVal1);
	IP = IP->flink;
	break;
	
      case O_AND_A:
	aVal1 = popa();
	aVal2 = popa();
	printf("AND_A\t(%p && %p -> %d)\n", aVal2, aVal1, aVal2 && aVal1);
	pushi(aVal2 && aVal1);
	IP = IP->flink;
	break;
      case O_AND_C:
	cVal1 = popc();
	cVal2 = popc();
	printf("AND_C\t(%d && %d -> %d)\n", cVal2, cVal1, cVal2 && cVal1);
	pushi(cVal2 && cVal1);
	IP = IP->flink;
	break;
      case O_AND_I:
	iVal1 = popi();
	iVal2 = popi();
	printf("AND_I\t(%d && %d -> %d)\n", iVal2, iVal1, iVal2 && iVal1);
	pushi(iVal2 && iVal1);
	IP = IP->flink;
	break;
      case O_AND_D:
	dVal1 = popd();
	dVal2 = popd();
	printf("AND_D\t(%g / %g -> %d)\n", dVal2, dVal1, dVal2 && dVal1);
	pushi(dVal2 && dVal1);
	IP = IP->flink;
	break;

      case O_OR_A:
	aVal1 = popa();
	aVal2 = popa();
	printf("OR_A\t(%p || %p -> %d)\n", aVal2, aVal1, aVal2 || aVal1);
	pushi(aVal2 || aVal1);
	IP = IP->flink;
	break;
      case O_OR_C:
	cVal1 = popc();
	cVal2 = popc();
	printf("OR_C\t(%d && %d -> %d)\n", cVal2, cVal1, cVal2 || cVal1);
	pushi(cVal2 || cVal1);
	IP = IP->flink;
	break;
      case O_OR_I:
	iVal1 = popi();
	iVal2 = popi();
	printf("OR_I\t(%d && %d -> %d)\n", iVal2, iVal1, iVal2 || iVal1);
	pushi(iVal2 || iVal1);
	IP = IP->flink;
	break;
      case O_OR_D:
	dVal1 = popd();
	dVal2 = popd();
	printf("OR_D\t(%g / %g -> %d)\n", dVal2, dVal1, dVal2 || dVal1);
	pushi(dVal2 || dVal1);
	IP = IP->flink;
	break;
      
      case O_DIV_C:
	cVal1 = popc();
	cVal2 = popc();
	printf("DIV_C\t(%d / %d -> %d)\n", cVal2, cVal1, cVal2 / cVal1);
	pushc(cVal2 / cVal1);
	IP = IP->flink;
	break;
      case O_DIV_I:
	iVal1 = popi();
	iVal2 = popi();
	printf("DIV_I\t(%d / %d -> %d)\n", iVal2, iVal1, iVal2 / iVal1);
	pushi(iVal2 / iVal1);
	IP = IP->flink;
	break;
      case O_DIV_D:
	dVal1 = popd();
	dVal2 = popd();
	printf("DIV_D\t(%g / %g -> %g)\n", dVal2, dVal1, dVal2 / dVal1);
	pushd(dVal2 / dVal1);
	IP = IP->flink;
	break;

      case O_MUL_C:
	cVal1 = popc();
	cVal2 = popc();
	printf("MUL_C\t(%d * %d -> %d)\n", cVal2, cVal1, cVal2 * cVal1);
	pushc(cVal2 * cVal1);
	IP = IP->flink;
	break;
      case O_MUL_I:
	iVal1 = popi();
	iVal2 = popi();
	printf("MUL_I\t(%d * %d -> %d)\n", iVal2, iVal1, iVal2 * iVal1);
	pushi(iVal2 * iVal1);
	IP = IP->flink;
	break;
      case O_MUL_D:
	dVal1 = popd();
	dVal2 = popd();
	printf("MUL_D\t(%g * %g -> %g)\n", dVal2, dVal1, dVal2 * dVal1);
	pushd(dVal2 * dVal1);
	IP = IP->flink;
	break;

      case O_NEG_C:
	cVal1 = popc();
	printf("NEG_C\t(%d -> %d)\n", cVal1, -cVal1);
	pushc(-cVal1);
	IP = IP->flink;
	break;
      case O_NEG_I:
	iVal1 = popi();
	printf("NEG_I\t(%d -> %d)\n", iVal1, -iVal1);
	pushi(-iVal1);
	IP = IP->flink;
	break;
      case O_NEG_D:
	dVal1 = popd();
	printf("NEG_D\t(%g -> %g)\n", dVal1, -dVal1);
	pushd(-dVal1);
	IP = IP->flink;
	break;

      case O_NOT_C:
	cVal1 = popc();
	printf("NOT_C\t(%d -> %d)\n", cVal1, !cVal1);
	pushi(!cVal1);
	IP = IP->flink;
	break;
      case O_NOT_I:
	iVal1 = popi();
	printf("NOT_I\t(%d -> %d)\n", iVal1, !iVal1);
	pushi(!iVal1);
	IP = IP->flink;
	break;
      case O_NOT_D:
	dVal1 = popd();
	printf("NOT_D\t(%g -> %d)\n", dVal1, !dVal1);
	pushi(!dVal1);
	IP = IP->flink;
	break;
      case O_NOT_A:
	aVal1 = popa();
	printf("NOT_A\t(%p -> %d)\n", aVal1, !aVal1);
	pushi(!aVal1);
	IP = IP->flink;
	break;

      case O_NOP:
	printf("NOP\n");
	IP = IP->flink;
	break;


	
      default:
	my_err("invalid opcode: %d", IP->opcode);
      }
    }
}


void *allocGlobal(int size)
{
  void *p = globals + nGlobals;
  if(nGlobals + size > GLOBAL_SIZE) my_err("insufficient space for globals");
  nGlobals += size;
  return p;
}

char *hai = "haida haida";
void mvTest(symbols_t *symbols)
{
  instr_t l;
  instr_t test;
  int *v = allocGlobal(sizeof(int));
  addInstrA(O_PUSHCT_A, v);
  addInstrI(O_PUSHCT_I, 3);
  addInstrI(O_STORE, sizeof(int));
  l = addInstrA(O_PUSHCT_A, v);
  addInstrI(O_LOAD, sizeof(int));
  addInstrA(O_CALLEXT, requireSymbol(symbols, "put_i")->addr);
  addInstrA(O_PUSHCT_A, hai);

  test = addInstrA(O_CALLEXT, requireSymbol(symbols, "put_s")->addr);
  addInstrA(O_CALLEXT, requireSymbol(symbols, "get_i")->addr);
  addInstrA(O_CALLEXT, requireSymbol(symbols, "put_i")->addr);

  deleteInstructionsAfter(test);

  addInstrA(O_PUSHCT_A, v);
  addInstrA(O_PUSHCT_A, v);
  addInstrI(O_LOAD, sizeof(int));
  addInstrI(O_PUSHCT_I, 1);
  addInstr(O_SUB_I);
  addInstrI(O_STORE, sizeof(int));
  addInstrA(O_PUSHCT_A, v);
  addInstrI(O_LOAD, sizeof(int));
  addInstrA(O_JT_I, l);
  addInstr(O_HALT);
}

void put_i()
{
  printf("#%d\n", popi());
}

void put_d()
{
  printf("#%g\n", popd());
}

void put_c()
{
  printf("#%c\n", popc());
}

void put_s()
{
  printf("#%s\n", (char *)popa());
}

void get_i()
{
  int a;
  putchar('#');
  scanf("%d", &a);
  pushi(a);
}

void get_d()
{
  double d;
  putchar('#');
  scanf("%lf", &d);
  pushd(d);
}

void get_c()
{
  char a;
  putchar('#');
  scanf("%c", &a);
  pushc(a);
}

void get_s()
{
  char *ret;
  SAFEALLOC(ret, char, 64);
  putchar('#');
  scanf("%63s", ret);
  pusha(ret);
}




void addDefaultSymbols(symbols_t *symbols)
{
  symbol_t *s = addExtFunc(symbols,"put_s", createType(TB_VOID, -1), put_s);
  addFuncArg(s, "s", createType(TB_CHAR, 0));
  
  s = addExtFunc(symbols,"get_s", createType(TB_CHAR, 0), get_s);
  
  s = addExtFunc(symbols,"put_i", createType(TB_VOID, -1), put_i);
  addFuncArg(s, "i", createType(TB_INT, -1));
  
  
  s = addExtFunc(symbols,"get_i", createType(TB_INT, -1), get_i);
  
  s = addExtFunc(symbols,"put_d", createType(TB_VOID, -1), put_d);
  addFuncArg(s, "d", createType(TB_DOUBLE, -1));
  
  s = addExtFunc(symbols,"get_d", createType(TB_DOUBLE, -1), get_d);

  s = addExtFunc(symbols,"put_c", createType(TB_VOID, -1), put_c);
  addFuncArg(s, "c", createType(TB_CHAR, -1));
  
  s = addExtFunc(symbols,"get_c", createType(TB_CHAR, -1), get_c);
  
  //  s = addExtFunc(&symbols,"seconds", createType(TB_DOUBLE, -1), seconds());
  
}



/*
int main(void)
{

  initSymbols(&symbols);
  addDefaultSymbols();
  mvTest();
  run(instructions);
  return 0;
}
*/

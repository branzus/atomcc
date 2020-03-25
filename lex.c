#include<stdio.h>
#include<stdlib.h>
#include<ctype.h>
#include<string.h>
#include<stdarg.h>
#include "lex.h"
#include "utils.h"

token_t tokens = NULL;
token_t lastToken = NULL;
char *pBegCh;
char *pCrtCh;
int line = 1;

static char* codestrings[] = {"ID", "END", "CT_INT", "CT_STRING", "CT_CHAR", "CT_REAL","ASSIGN", "SEMICOLON",
			      "BREAK", "CHAR", "EQUAL", "DOUBLE", "ELSE", "FOR", "IF", "INT", "RETURN",
			      "STRUCT", "VOID", "WHILE", "COMMA", "LPAR", "RPAR", "LBRACKET",
			      "RBRACKET", "LACC", "RACC", "ADD", "SUB", "MUL", "DIV", "DOT", "AND", "OR",
			      "NOT", "NOTEQ", "LESS", "LESSEQ", "GREATER", "GREATEREQ", "COMMENT", "LINECOMMENT"};

void tk_err(token_t tk, const char *fmt, ...)
{
  va_list va;
  va_start(va,fmt);
  fprintf(stderr, "error in line %d: ", tk->line);
  vfprintf(stderr, fmt, va);
  fputc('\n', stderr);
  va_end(va);
  exit(-1);

}


void print_token(token_t t)
{
  if(!t)
    {
      puts("No token to print");
      return;
    }
  
  if (t->code == CT_INT)
    printf("%d:%s: %d\n",(t)->line, codestrings[t->code], (t)->i);
  else if (t->code == CT_CHAR)
    printf("%d:%s: %c\n",(t)->line, codestrings[t->code], (t)->i);
  else if (t->code == CT_STRING || t->code == ID)
    printf("%d:%s: \"%s\"\n", t->line, codestrings[t->code], t->text);
  else if (t->code == CT_REAL)
    printf("%d:%s: %f\n", t->line, codestrings[t->code], t->r);
  else printf("%d:%s\n", t->line, codestrings[t->code]);
}





//adds a token to the token list and returns a pointer to the token
token_t add_token(code_t code) {
  
  token_t tk;
  SAFEALLOC(tk, token, 1);

  tk->code = code;
  tk->line = line;
  tk->next = NULL;
  
  if(lastToken)
    lastToken->next = tk;
  else
    tokens = tk;

  lastToken = tk;
  return tk;
}


char *create_string(const char *pStart, int len)
{
  char *new;
  SAFEALLOC(new, char, len + 1);
  memcpy(new, pStart, len);
  new[len] = '\0';
  return new;

}

char getescapechar(char ch)
{
  //  if (ch != 'a' && ch != 'b' && ch != 'f' && ch != 'n' && ch != 'r' && ch != 't' && ch != 'v' &&
  //  ch != '\'' && ch != '?' && ch != '"' && ch != '\\' && ch != '0')

  
  switch(ch)
    {
    case 'a':
      return '\a';
    case 'b':
      return '\b';
    case 'f':
      return '\f';
    case 'n':
      return '\n';
    case 'r':
      return '\r';
    case 't':
      return '\t';
    case 'v':
      return '\v';
    case '\'':
      return '\'';
    case '?':
      return '\?';
    case '"':
      return '\"';
    case '\\':
      return '\\';
    case '0':
      return '\0';
    }
  my_err("invalid escaped character");
  return 0;
}

char *get_string_value(const char* pStart, int size) //expects to receive pStart -> "a" with size 3
{
  if (size < 2 || pStart[0] != '"' || pStart[size - 1] != '"')
    my_err("get_string_value called with inappropriate arguments");
  char *ret;
  SAFEALLOC(ret, char, size - 1);
  int i;
  int j = 0;
  for(i = 1; i < size - 1; i++, j++)
    if (pStart[i] != '\\')
      ret[j] = pStart[i];
    else
      {
	i++;
	ret[j] = getescapechar(pStart[i]);
      }
  ret[j] = '\0';
  return ret;
}


int getNextToken()
{
  int state = 0, nCh;
  char ch;
  const char *pStartCh;
  token_t tk;
  
  //  puts(pCrtCh);
  while(1)
    {
      ch = *pCrtCh;
      //putchar(ch);
      switch(state)
	{
	case 0: //beginning
	  if(isalpha(ch) || ch == '_') //identifier start
	    {
	      pStartCh = pCrtCh;
	      pCrtCh++;
	      state = 1;//identifier continue
	    }
	  else if (ch == '=') //assignment or eq test
	    {
	      pCrtCh++;
	      state = 3; //
	    }
	  else if (ch == ' ' || ch == '\r' || ch == '\t')
	    pCrtCh++; //consume leading spaces
	  else if (ch == '\n') //increment line number
	    {
	      line++;
	      pCrtCh++;
	    }
	  else if (ch == '0')
	    {
	      pStartCh = pCrtCh; //mark start of number starting with 0
	      pCrtCh++;
	      state = 4;

	    }
	  else if (isdigit(ch))
	    {
	      pStartCh = pCrtCh; //mark start of number starting with != 0
	      pCrtCh++;
	      state = 5;
	    }
	  else if (ch == '\'')
	    {
	      pStartCh = pCrtCh;
	      pCrtCh++;
	      state = 13; //character value
	    }
	  else if (ch == '"')
	    {
	      pStartCh = pCrtCh;
	      pCrtCh++;
	      state = 16;//string
	      
	    }
	  else if (ch == ',')
	    {
	      pCrtCh++;
	      add_token(COMMA);
	      return COMMA;
	    }
	  else if (ch == ';')
	    {
	      pCrtCh++;
	      add_token(SEMICOLON);
	      return SEMICOLON;
	    }
	  else if (ch == '(')
	    {
	      pCrtCh++;
	      add_token(LPAR);
	      return LPAR;
	    }
	  else if (ch == ')')
	    {
	      pCrtCh++;
	      add_token(RPAR);
	      return RPAR;
	    }
	  else if (ch == '[')
	    {
	      pCrtCh++;
	      add_token(LBRACKET);
	      return LBRACKET;
	    }
	  else if (ch == ']')
	    {
	      pCrtCh++;
	      add_token(RBRACKET);
	      return RBRACKET;
	    }
	  else if (ch == '{')
	    {
	      pCrtCh++;
	      add_token(LACC);
	      return LACC;
	    }
	  else if (ch == '}')
	    {
	      pCrtCh++;
	      add_token(RACC);
	      return RACC;
	    }
	  else if (ch == '+')
	    {
	      pCrtCh++;
	      add_token(ADD);
	      return ADD;
	    }
	  else if (ch == '-')
	    {
	      pCrtCh++;
	      add_token(SUB);
	      return SUB;
	    }
	  else if (ch == '*')
	    {
	      pCrtCh++;
	      add_token(MUL);
	      return MUL;
	    }
	  else if (ch == '/')
	    {
	      pCrtCh++;
	      state = 21;
	    }
	  else if (ch == '.')
	    {
	      pCrtCh++;
	      add_token(DOT);
	      return DOT;
	    }
	  else if (ch == '!')
	    {
	      pCrtCh++;
	      state = 18; //might be ! or !=
	    }
	  else if (ch == '&')
	    {
	      pCrtCh++;
	      if (*pCrtCh != '&')
		tk_err(add_token(AND), "invalid character after &");
	      pCrtCh++;
	      add_token(AND);
	      return AND;
	    }
	  else if (ch == '|')
	    {
	      pCrtCh++;
	      if (*pCrtCh != '|')
		tk_err(add_token(AND), "invalid character after |");
	      pCrtCh++;
	      add_token(OR);
	      return OR;
	    }
	  else if (ch == '<')
	    {
	      pCrtCh++;
	      state = 19; //might be < or <=
	    }
	  else if (ch == '>')
	    {
	      pCrtCh++;
	      state = 20; //might be > or >=
	    }

	    
	  else if(ch == '\0') //end yay
	    {
	      add_token(END);
	      return END;
	    }
	  else tk_err(add_token(END), "invalid character");
	  break;
	case 1: //we are reading an identifier
	  if(isalnum(ch) || ch == '_')
	    pCrtCh++;
	  else
	    state = 2;
	  break;
	case 2: //an identifier was just read
	  nCh = pCrtCh - pStartCh;
	  //keywords tests
	  if(nCh == 5 && !memcmp(pStartCh, "break", 5))
	    tk = add_token(BREAK);
	  else if(nCh == 4 && !memcmp(pStartCh, "char", 4))
	    tk = add_token(CHAR);
	  else if(nCh == 6 && !memcmp(pStartCh, "double", 6))
	    tk = add_token(DOUBLE);
	  else if(nCh == 4 && !memcmp(pStartCh, "else", 4))
	    tk = add_token(ELSE);
	  else if(nCh == 3 && !memcmp(pStartCh, "for", 3))
	    tk = add_token(FOR);
	  else if(nCh == 2 && !memcmp(pStartCh, "if", 2))
	    tk = add_token(IF);
	  else if(nCh == 3 && !memcmp(pStartCh, "int", 3))
	    tk = add_token(INT);
	  else if(nCh == 6 && !memcmp(pStartCh, "return", 6))
	    tk = add_token(RETURN);
	  else if(nCh == 6 && !memcmp(pStartCh, "struct", 6))
	    tk = add_token(STRUCT);
	  else if(nCh == 4 && !memcmp(pStartCh, "void", 4))
	    tk = add_token(VOID);
	  else if(nCh == 5 && !memcmp(pStartCh, "while", 5))
	    tk = add_token(WHILE);
	  
	  //TODO: Add rest of keywords
	  else //if not a keyword then it's an ID
	    {
	      tk = add_token(ID);
	      tk->text = create_string(pStartCh, nCh);
	    }
	  return tk->code;

	case 3: //an '=' was read
	  if(ch == '=') //another == means equal test
	    {
	      pCrtCh++;
	      add_token(EQUAL);
	      return EQUAL;
	    }
	  else
	    { //only one '=' => assignment
	      add_token(ASSIGN); 
	      return ASSIGN;
	    }
	case 4: //read a 0
	  if(ch == 'x')
	    {
	      pCrtCh++;
	      state = 6; //mark hex digits
	    }
	  else
	    state = 7; //not hex ==>octal
	  break;
	case 5: //read first digit of number != 0
	  if(isdigit(ch))
	    pCrtCh++;
	  else if(ch == '.')
	    {
	      pCrtCh++;
	      //state = 6 TODO value for float
	      state = 8; //mark just read dot from float
	    }
	  else if (ch == 'e' || ch =='E')
	    {
	      pCrtCh++;
	      state = 10; //no '.', straight to exponent
	    }
	  else
	    {
	      //printf("got 10 base int\n");
	      tk = add_token(CT_INT);
	      tk->i = strtol(pStartCh, NULL, 10);
	      return CT_INT;
	    }
	  break;
	case 6: //hex digit, just read x
	  if (isdigit(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F'))
	    pCrtCh++;
	  else
	    {
	      //printf("got 16 base int\n");
	      tk = add_token(CT_INT);
	      tk->i = strtol(pStartCh, NULL, 16);
	      return CT_INT;
	    }
	  break;
	case 7: //read 0 followed by something != x, probably octal, might still be float
	  if (ch >= '0' && ch <= '7')
	    pCrtCh++;
	  else if (ch == '.')
	    {
	      pCrtCh++;
	      state = 8; //actually float
	    }
	  else if (ch == 'e' || ch == 'E')
	    {
	      pCrtCh++;
	      state = 10;
	    }
	  else if (isdigit(ch))
	    {
	      pCrtCh++;
	      state = 9; //state going over digits, until '.'
	    }
	  else
	    {
	      //puts("got 8 base int");
	      tk = add_token(CT_INT);
	      tk->i = strtol(pStartCh, NULL, 8);
	      return CT_INT;
	    }
	  break;
	case 8: //just read . of float
	  if(isdigit(ch))
	    pCrtCh++;
	  else if (ch == 'e' || ch == 'E')
	    {
	      if (pCrtCh[-1] == '.')
		tk_err(add_token(END), "no . right before exponent allowed");
	      pCrtCh++;
	      state = 10;
	    }
	  else
	    {
	      tk = add_token(CT_REAL);
	      tk->r = strtod(pStartCh, NULL);
	      return CT_REAL;
	    }

	  break;

	case 9: //state going over digits until '.' or exponent because of a number starting with 0 which can't be octal
	  if(ch == '.')
	    {
	      pCrtCh++;
	      state = 8;
	    }
	  else if (ch == 'e' || ch =='E')
	    {
	      pCrtCh++;
	      state = 10;
	    }
	  else if(isdigit(ch))
	    pCrtCh++;
	  else tk_err(add_token(END), "invalid number: expected digit or '.'; got %c", ch);
	  break;

	case 10: //just read e/E of exponent
	  if(ch == '-' || ch == '+')
	    {
	      pCrtCh++;
	      state = 11;
	    }
	  else
	    state = 11;
	  break;
	case 11: //read e and sign; must have a digit
	  if(isdigit(ch))
	    {
	      pCrtCh++;
	      state = 12;
	    }
	  else tk_err(add_token(END), "expected digit got %c", ch);
	  break;
	case 12: //read first digit after exponent, must read the rest now
	  if(isdigit(ch))
	    pCrtCh++;
	  else
	    {
	      tk = add_token(CT_REAL);
	      tk->r = strtod(pStartCh, NULL);
	      return CT_REAL;
	    }
	  break;
	case 13: //just read an apostrophe ==> read character
	  if(ch == '\\')
	    {
	      pCrtCh++;
	      state = 15;
	    }
	  else if(ch != '\0')
	    {
	      pCrtCh++;
	      state = 14;
	    }
	  else tk_err(add_token(END), "unexpected end, expecte literal character");
	  break;

	case 14: //expect to find end of character
	  if (ch == '\'')
	    {
	      pCrtCh++;
	      nCh = pCrtCh - pStartCh;
	      tk = add_token(CT_CHAR);
	      if (nCh == 3)
		tk->i = pCrtCh[-2];
	      else if (nCh == 4)
		tk->i = getescapechar(pCrtCh[-2]);
	      else
		tk_err(add_token(END), "invalid size of character literal");
	      return CT_CHAR;
	    }
	  else tk_err(add_token(END), "expected end of character literal");
	  break;
	case 15://just read \ in CT_CHAR, expect a legal escaped char
	  if (ch == '\0')
	    tk_err(add_token(END), "unexpected EOF while parsing character literal");
	  pCrtCh++;
	  state = 14;
	  break;

	case 16: //parsing string until closing quote
	  if (ch == '"') //found closing quote
	    {
	      pCrtCh++;
	      nCh = pCrtCh - pStartCh;
	      tk = add_token(CT_STRING);
	      tk->text = get_string_value(pStartCh, nCh);
	      return CT_STRING;
	    }
	  else if (ch == '\\')
	    {
	      pCrtCh++;
	      state = 17;
	    }
	  else if (ch == '\0')
	    tk_err(add_token(END), "unexpected EOF while parsing string");
	  else
	    pCrtCh++;
	  break;

	case 17: //backslash in string
	  if(ch == '\0')
	    tk_err(add_token(END), "unexpected EOF while parsing string");
	  pCrtCh++;
	  state = 16;
	  break;

	case 18: //read an !
	  if(ch == '=')
	    {
	      pCrtCh++;
	      add_token(NOTEQ);
	      return NOTEQ;
	    }
	  else
	    {
	      add_token(NOT);
	      return NOT;
	    }
	  break;
	case 19: //read an <
	  if(ch == '=')
	    {
	      pCrtCh++;
	      add_token(LESSEQ);
	      return LESSEQ;
	    }
	  else
	    {
	      add_token(LESS);
	      return LESS;
	    }
	  break;
	case 20: //read an >
	  if(ch == '=')
	    {
	      pCrtCh++;
	      add_token(GREATEREQ);
	      return GREATEREQ;
	    }
	  else
	    {
	      add_token(GREATER);
	      return GREATER;
	    }
	  break;
	case 21: //got an /
	  if (ch == '/') //line comment?
	    {
	      pCrtCh++;
	      while(*pCrtCh != '\n' && *pCrtCh != '\0') //go to end of line comment
		pCrtCh++;
	      return LINECOMMENT;
	    }
	  else if (ch == '*')
	    {
	      pCrtCh++;
	      state = 22;
	    }
	  else
	    {
	      add_token(DIV);
	      return DIV;
	    }
	  break;
	case 22: //in comment
	  if (ch == '*')
	    {
	      pCrtCh++;
	      state = 23;
	    }
	  else if (ch == '\0')
	    tk_err(add_token(END), "unexpected end of file in comment");
	  else if (ch == '\n')
	    {
	      pCrtCh++;
	      line++;
	    }
	  else
	    pCrtCh++;
	  break;
	case 23: //found a * in commnet might be end of comment
	  if (ch == '/')
	    {
	      pCrtCh++;
	      return COMMENT;
	    }
	  else if (ch == '\0')
	    tk_err(add_token(END), "unexpected end of file in comment");
	  else
	    {
	      state = 22;
	    }
	  break;
	  
	  
	}
    }
}

/*
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
  line = 1;

  while(getNextToken() != END);
    //    print_token(lastToken);
  //print_token(lastToken);
  token_t t;
  for(t = tokens; t != NULL; t = t->next)
    print_token(t);
  
  //puts(pBegCh);

    //print_token(tokens);
  //  print_token(lastToken);
  

  return 0;
}
*/

#ifndef LEX_H_
#define LEX_H_



typedef enum {ID, END, CT_INT, CT_STRING, CT_CHAR, CT_REAL, ASSIGN, SEMICOLON, BREAK, CHAR, EQUAL,
	      DOUBLE, ELSE, FOR, IF, INT, RETURN, STRUCT, VOID, WHILE, COMMA, LPAR, RPAR,
	      LBRACKET, RBRACKET, LACC, RACC, ADD, SUB, MUL, DIV, DOT, AND, OR, NOT, NOTEQ, LESS, LESSEQ,
	      GREATER, GREATEREQ, COMMENT, LINECOMMENT} code_t;


typedef struct Token *token_t;

typedef struct Token {
  code_t code;
  union {
    char *text;
    int i;
    double r;
  };
  int line;
  token_t next;
} token;


void tk_err(token_t tk, const char *fmt, ...);
void print_token(token_t t);
int getNextToken();

#endif

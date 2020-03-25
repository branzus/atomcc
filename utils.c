#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>


void my_err(const char *fmt,...)
{
  va_list va;
  va_start(va, fmt);
  fprintf(stderr, "error: ");
  vfprintf(stderr, fmt, va);
  fputc('\n', stderr);
  va_end(va);
  exit(-1);
}

/*
void my_perr(char *err)
{
  perror
}
*/

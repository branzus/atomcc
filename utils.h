#ifndef UTI_H_
#define UTI_H_

#define SAFEALLOC(var,type,i) if((var = malloc(sizeof(type) * (i))) == NULL) my_err("Not enough memory.")



void my_err(const char *fmt,...);



#endif

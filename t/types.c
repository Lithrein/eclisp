void ((*x))(char (*[3]));
char (*(*x));
char (*(x)[3]);
void (x)(int ((*))[3]);
int ((*x))[13];
int (*(x)[13]);
void ((*x))(void);
void (x)(void ((*((*))[]))(char));
void ((*volatile (x)[3]))(char ((*const ))[5]);
char (*(((*x))(void))[3]);
int ((*(x)(void)))[3];
void ((*x))(void);
void ((*((*(x)(void)))[]))(char);
char ((*const ((*volatile (x)[3]))(void)))[5];
/* A function with slightly less parenthesis in the types.  */
int (main)(int argc, int ((*(*argv)))[3])
{
  return 1;
}

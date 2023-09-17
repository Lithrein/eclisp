#if defined(__WIN32__)
#define NB_THREADS 32
#include <threads.h>
#elif defined(__LINUX__)
#include <pthreads.h>
#endif

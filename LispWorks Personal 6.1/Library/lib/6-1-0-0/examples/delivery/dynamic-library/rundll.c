/*
** $Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/delivery:dynamic-library:rundll.c,v 1.1.1.1 2011/08/24 13:26:18 davef Exp $
**
** Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
**
** This sample file can be used to create a test program for
** loading LispWorks shared libraries.
**
** Compile this by running make
** Create the LispWorks shared library using deliver.lisp
** Test by: ./rundll ./lispworks-shared-library.so testfun1
**
*/

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

typedef int(*run_function)(int, char**);

int main(int argc, char **argv)
{
  char *filename;
  char *symbolname;
  void *handle;
  void *address;

  filename = argv[1];
  symbolname = argv[2];

  handle = dlopen(filename, RTLD_LAZY);
  if (!handle) {
    fprintf(stderr, "Failed to load shared library:\n %s\n",
            dlerror());
    exit(1);
  }

  address = dlsym(handle, symbolname);
  if (!address) {
    fprintf(stderr, "Failed to find symbol:\n %s\n",
            dlerror());
    exit(1);
  }

   run_function func = (run_function)address;
   return func(argc, argv);
}


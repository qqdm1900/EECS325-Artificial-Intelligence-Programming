;; -*- Mode: Text; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/delivery:dynamic-library:README.txt,v 1.2.1.1 2011/08/24 13:26:18 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.


Dynamic Library
---------------

A minimal example of creating and testing a dynamic library on Unix
platforms.

Use deliver.lisp as the delivery script (see ../README.txt). This
creates the dynamic library in this directory.

To test it, first create rundll by executing make in this directory, then 
run 

  rundll <dynamic-library> testfun1       
  rundll <dynamic-library> testfun2

  rundll <dynamic-library> junk

where <dynamic-library> is the file that was created by the delivery
script.

The first two lines should "work", i.e. print a message of the form
"This is test function 2.". 
The last line should give an error because of failure to find the symbol. 

Notes: 

On machines that can run both 64-bit and 32-bit programs, you need to
match the architecture of the dynamic library, i.e. the Lisp image, to
the architecture of rundll. If they do not match then rundll gives an
error. In this case either use the other architecture in LispWorks, or
edit the appropriate makefile (Makefile-<uname>) to get the
architecture right.  If you use 32-bit LispWorks, you typically need to
add -m32 to cc.

This example should work on all supported x86 Unix architectures.  At
the time of writing that includes Linux, SunOS, FreeBSD and Darwin.

Files:

   example.lisp
     The example application code. Here contains just two foriegn
     callables that just print a message. In real applications these
     callables will actually do the work.
 
   deliver.lisp 
     The delivery script. 

   rundll.c 
     Source of the rundll program, which takes as argument a dynamic
     library and a function name, load the library and calls the function. 

   Makefile
     The main makefile that just call Makefile-<uname>. 

   Makefile-Linux
   Makefile-FreeBSD
   Makefile-SunOS
   Makefile-Darwin
      OS-specific makefiles. 

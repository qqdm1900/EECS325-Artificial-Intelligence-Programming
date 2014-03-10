;; -*- Mode: Text; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/delivery:ntservice:README.txt,v 1.3.1.1 2011/08/24 13:26:18 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.


LispWorks for Windows service example
-------------------------------------

  files:
    deliver.lisp
      The delivery script.

    ntservice.lisp
      The NTSERVICE implementation modified for LispWorks.

    service-control-handler-64.dll
      A DLL that ntservice.lisp needs on 64-bit LispWorks.

    service-control-handler-32.dll  
      A DLL that ntservice.lisp needs on 32-bit LispWorks.

    service-control-handler.c
      Source of the two Dlls above, not needed for building. 

    build-config.lisp
      Configuration parameters of the application.

    define-service.lisp
      Code that uses ntservice interface.
 
    testapp-lw.lisp       The example application code.

    testapp-lw-test.lisp 
      Code to "configure" the service by communicating with it using a
      named pipe.

This directory contains an example of creating a Windows service. In
particular, it shows how the service can communicate with other
applications using named pipes. Such communications are intended to
allow users to interact with the service, via a GUI application that
connects to the service, because the service itself cannot use the
GUI.

It is modified to work on Windows 7, as well as previous Windows
versions. It requires LispWorks 6.0.1 or later. If you use LispWorks 
6.0.1, you need the patches "pipes" and "impersonating-user". 

The example service does nothing, except writing to a file, and also
opens a named pipe and waits for input from it. In response to the
input it can change the access of the named pipe, execute processes
while impersonating another user, or just update its "configuration"
(which is just what it prints to the output file). The file
testapp-lw-test.lisp contains example code and various tests to
execute in the LispWorks IDE to test the service.


1. Building
 
To build the service just use deliver.lisp as the build script, either
in the Application Builder or on the command line.

The delivery script uses *my-test-service-executable*, which is
defined in build-config.lisp, to decide where to save the service
executable, You may want to change this. The name of the output file
that it writes to is *output-file-name*, which is also defined in
build-config.lisp.

2. Installing

Once you built successfully, you need to install it. You do that by
executing on the command line:

  <executable-path> /install


where <executable-path> is the path of the executable that was
delivered (or a copy of it).

The installation must be done with administrator permissions. On
Windows XP that means that you have to be logged on as an
administrator. On Windows 7 and Vista that is not sufficient: you also
need to run explicitly with administrator privileges. The easiest way
to do this is to start the Task Manager, click (or select)
 "Show processes from all users"
at the bottom, then menu:
 "File" -> "New Task (Run...)"
Check
 "Create this task with adminstrative privileges"
and then enter the above command.

Uninstalling is done by running
   
    <executable-path> /remove

3. Starting and stopping

By default, the code makes the service :MANUAL (see in
define-service.lisp), which means it has to be explicitly started. The
easiest way to do that is to use the Services Control Panel:

      Control Panel -> Administration Tools -> Services 

      Find the service called "My test service" (value of
     *service-display-name* in build-config.lisp). 

Alternatively you can do from the command line:

      net start my-test-service
      net stop my-test-service

   (my-test-service is the value of *service-name* in build-config.lisp). 


4. Testing it

The service writes to the output file (value of *output-file-name* in
build-config.lisp). After starting, it just writes iteration counts
each 100 seconds. It also keeps a named pipe open (name is
"testapp-lw-configuration", the value of *configuration-pipe-name* in
build-config.lisp) that can be used to "configure" it. The file
testapp-lw-test.lisp can be used to test it. Load this file into the
LispWorks IDE and then go through the tests, execute them and check
the result.

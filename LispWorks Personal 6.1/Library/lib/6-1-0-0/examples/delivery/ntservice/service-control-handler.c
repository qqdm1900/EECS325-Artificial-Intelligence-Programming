/*


  This is compiled

 ----- for 64 by:

 call "%ProgramFiles(x86)%\Microsoft Visual Studio 8\VC\vcvarsall.bat" x64
  cl /LD /Gz service-control-handler.c /link /export:setup_service_control_handler /out:service-control-handler-64.dll

 ---  for 32bit by:

 call "%ProgramFiles(x86)%\Microsoft Visual Studio 8\VC\vcvarsall.bat" x86
  cl /LD /Gz service-control-handler.c /link /export:setup_service_control_handler /out:service-control-handler-32.dll

This assumes that you Microsoft Visual Studio is installed in %ProgramFiles(x86)%. You may need
to edit it if you have a different installation. 

*/

#include <windows.h>
#include <stdio.h>
#include <process.h>

static void (*service_control_handler_func)(DWORD) ; 

static void outer_service_control_handler (DWORD fdwControl)
{

  (*service_control_handler_func) (fdwControl) ; 
}

void *setup_service_control_handler (void(*func)(DWORD))
{
   service_control_handler_func = func ;
   return    (void*) &outer_service_control_handler ; 
}

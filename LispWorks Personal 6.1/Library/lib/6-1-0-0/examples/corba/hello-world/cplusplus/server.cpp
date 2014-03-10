/*
** $Header: /hope/lwhope1-cam/hope.0/compound/56/LISPcorba-doc-hello-world/RCS/cplusplus:server.cpp,v 1.1.12.1 2011/08/24 13:26:00 davef Exp $
**
** Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
**
** Sample C++ implementation of the HelloWorld demo.  Tested with Orbacus 4.0.5.
*/


#include <orb.h>

#include <hello-world_impl.h>

#include <stdlib.h>
#include <errno.h>

#include <fstream.h>

int
main(int argc, char* argv[], char*[])
{
  CORBA::ORB_var orb;

  try {
    orb = CORBA::ORB_init(argc, argv);
    CORBA::Object_var poaObj = orb -> resolve_initial_references("RootPOA");
    PortableServer::POA_var rootPOA = PortableServer::POA::_narrow(poaObj);
    PortableServer::POAManager_var manager = rootPOA -> the_POAManager();
    HelloWorld::world_impl world(rootPOA);
    HelloWorld::world_var hello = world . _this();

    CORBA::String_var s = orb -> object_to_string(hello);
    const char *iorpath = IORPATH;
    ofstream out(iorpath);
    if(out.fail()) {
      cerr << argv[0] << ": can't open `" << iorpath << "': "
	   << strerror(errno) << endl;
      return EXIT_FAILURE;
    }
    out << '"' << s << '"' << endl;
    out.close();
    
    manager -> activate();
    orb -> run();

  }
  catch(const CORBA::Exception& ex) {
    cerr << ex << endl;
  }

  if(!CORBA::is_nil(orb)) {
    try {
      orb -> destroy();
    }
    catch(const CORBA::Exception& ex) {
      cerr << ex << endl;
    }
  }

  return 0;
}

/*
** $Header: /hope/lwhope1-cam/hope.0/compound/56/LISPcorba-doc-hello-world/RCS/cplusplus:client.cpp,v 1.1.12.1 2011/08/24 13:26:00 davef Exp $
**
** Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
**
** Sample C++ client for the HelloWorld demo.  Tested with Orbacus 4.0.5.
*/


#include <orb.h>

#include <hello-world.h>

#include <stdlib.h>
#include <errno.h>

#include <fstream.h>

int
main(int argc, char* argv[], char*[])
{
  CORBA::ORB_var orb;

  try {
    orb = CORBA::ORB_init(argc, argv);

    const char *iorpath = IORPATH;
    ifstream in(iorpath);
    if(in.fail()) {
      cerr << argv[0] << ": can't open `" << iorpath << "': "
	   << strerror(errno) << endl;
      return EXIT_FAILURE;
    }
    char buf[10000];
    in >> buf;
    in.close();

    char *ior = buf+1;
    ior[strlen(ior)-1] = '\0';

    CORBA::Object_var obj = orb -> string_to_object(ior);
    HelloWorld::world_var hello = HelloWorld::world::_narrow(obj);
    assert(!CORBA::is_nil(hello));

    CORBA::String_var res = hello -> hello();

    cout << "Server says `" << res << "'" << endl;
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

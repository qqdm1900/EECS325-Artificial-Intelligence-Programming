/*
** $Header: /hope/lwhope1-cam/hope.0/compound/56/LISPcorba-doc-hello-world/RCS/cplusplus:hello-world_impl.cpp,v 1.1.12.1 2011/08/24 13:26:00 davef Exp $
**
** Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
*/


#include <orb.h>
#include <hello-world_impl.h>

//
// IDL:HelloWorld:1.0
//

//
// IDL:HelloWorld/world:1.0
//
HelloWorld::world_impl::world_impl(PortableServer::POA_ptr poa)
    : poa_(PortableServer::POA::_duplicate(poa))
{
}

HelloWorld::world_impl::~world_impl()
{
}

PortableServer::POA_ptr
HelloWorld::world_impl::_default_POA()
{
    return PortableServer::POA::_duplicate(poa_);
}

//
// IDL:HelloWorld/world/hello:1.0
//
char*
HelloWorld::world_impl::hello()
    throw(CORBA::SystemException)
{
    char* _r = CORBA::string_dup("Hello World!");
    return _r;
}

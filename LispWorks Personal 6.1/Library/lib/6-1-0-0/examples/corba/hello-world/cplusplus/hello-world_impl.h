/*
** $Header: /hope/lwhope1-cam/hope.0/compound/56/LISPcorba-doc-hello-world/RCS/cplusplus:hello-world_impl.h,v 1.1.12.1 2011/08/24 13:26:00 davef Exp $
**
** Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
*/


#ifndef ___helloworld_impl_h__
#define ___helloworld_impl_h__

#include <hello-world_skel.h>

//
// IDL:HelloWorld:1.0
//
namespace HelloWorld
{

//
// IDL:HelloWorld/world:1.0
//
class world_impl : virtual public POA_HelloWorld::world,
                   virtual public PortableServer::RefCountServantBase
{
    world_impl(const world_impl&);
    void operator=(const world_impl&);

    PortableServer::POA_var poa_;

public:

    world_impl(PortableServer::POA_ptr);
    ~world_impl();

    virtual PortableServer::POA_ptr _default_POA();

    //
    // IDL:HelloWorld/world/hello:1.0
    //
    virtual char* hello()
        throw(CORBA::SystemException);
};

} // End of namespace HelloWorld

#endif

#
# $Header: /hope/lwhope1-cam/hope.0/compound/56/LISPcorba-doc-hello-world/RCS/cplusplus:Makefile,v 1.1.12.1 2011/08/24 13:26:00 davef Exp $
#
# Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
#
# Sample C++ version of the HelloWorld demo.  Tested with Orbacus 4.0.5.
#


OBROOT=/usr/local
IORPATH=/tmp/hello.ior

CPPFLAGS=-g -I${OBROOT}/include -I. -DIORPATH='"${IORPATH}"'
LDFLAGS=-L${OBROOT}/lib -lOB

SERVER_OBJS=server.o hello-world_impl.o hello-world_skel.o
CLIENT_OBJS=client.o
COMMON_OBJS=hello-world.o

all: server client

server: ${SERVER_OBJS} ${COMMON_OBJS}

client: ${CLIENT_OBJS} ${COMMON_OBJS}

server.o: hello-world_impl.h

client.o: hello-world.h

hello-world_impl.h: hello-world_skel.h

hello-world_skel.h: hello-world.h

hello-world.cpp hello-world.h hello-world_skel.cpp hello-world_skel.h: ../hello-world.idl
	idl ../hello-world.idl


clean:
	rm -f hello-world.cpp hello-world.h hello-world_skel.cpp hello-world_skel.h
	rm -f server ${SERVER_OBJS}
	rm -f client ${CLIENT_OBJS}
	rm -f ${COMMON_OBJS}

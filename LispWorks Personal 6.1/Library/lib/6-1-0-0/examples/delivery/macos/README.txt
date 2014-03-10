;; -*- Mode: Text; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/delivery:macos:README.txt,v 1.2.1.1 2011/08/24 13:26:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

MacOS
-----

Examples of creating Cocoa applications. 

There are two examples in this directory: the first builds an
applications whcih creates only one window at a time; the second
builds an application that can open multiple windows.

Most importantly, both examples demonstrate how to create a Cocoa
application bundle by using templates.

Files:
  single-window-application.lisp
    The delivery script for the single window application 
    (see ../README.txt how to use delivery script).

  multiple-window-application.lisp
    The delivery script for the multiple windows application.

  templates/
    A sub-directory containing the templates used to create the bundles
    (refers to by the delivery scripts). 

  files/
    A sub-directory containing files for testing. 

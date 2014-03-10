;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/delivery:dynamic-library:deliver.lisp,v 1.1.1.1 2011/08/24 13:26:18 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;; Delivery script for use with rundll.c

(in-package "CL-USER")

(load-all-patches)

(compile-file (current-pathname "example.lisp")
              :output-file :temp
              :load t)

(deliver nil
         (current-pathname "lispworks-shared-library" nil)
         0
         :dll-exports '("testfun1" "testfun2"))


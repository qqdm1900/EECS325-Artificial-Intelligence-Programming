;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/delivery:ntservice:deliver.lisp,v 1.3.1.1 2011/08/24 13:26:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.


(in-package :cl-user)

(load-all-patches)

(compile-file-if-needed (current-pathname "ntservice") :load t)

(compile-file-if-needed (current-pathname "build-config") :load t)
(compile-file-if-needed (current-pathname "define-service") :load t)
(compile-file-if-needed (current-pathname "testapp-lw") :load t)

(deliver 'ntservice::standard-main *my-test-service-executable* 0
         :multiprocessing t)

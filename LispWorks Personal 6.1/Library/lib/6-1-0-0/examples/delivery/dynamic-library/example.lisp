;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/delivery:dynamic-library:example.lisp,v 1.1.1.1 2011/08/24 13:26:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;; Example foreign callable functions for use with rundll.c

(in-package "CL-USER")

(fli:define-foreign-callable (testfun1 :result-type :int)
    ((argc :int)
     (argv (:pointer (:pointer :char))))
  (declare (ignore argc argv))
  (write-line "This is test function 1.")
  (force-output)
  0)

(fli:define-foreign-callable (testfun2 :result-type :int)
    ((argc :int)
     (argv (:pointer (:pointer :char))))
  (declare (ignore argc argv))
  (write-line "This is test function 2.")
  (force-output)
  0)


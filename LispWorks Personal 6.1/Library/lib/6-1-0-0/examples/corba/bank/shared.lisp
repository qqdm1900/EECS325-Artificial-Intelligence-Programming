;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/43/LISPcorba-doc-bank/RCS/shared.lisp,v 1.2.12.1 2011/08/24 13:25:59 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(defparameter *bank-ior-file*
  #+win32 "c:/temp/bank-demo.ior"
  #-win32 "/tmp/bank-demo.ior")


(defun object-to-file (orb object)
  (with-open-file (st *bank-ior-file* :direction :output :if-exists :supersede)
    (prin1 (op:object_to_string orb object) st)))

(defun file-to-object (orb)
  (with-open-file (st *bank-ior-file*)
    (op:string_to_object orb (read st))))



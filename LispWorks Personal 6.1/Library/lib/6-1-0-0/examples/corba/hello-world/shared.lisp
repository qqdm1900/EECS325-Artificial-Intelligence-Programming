;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/56/LISPcorba-doc-hello-world/RCS/shared.lisp,v 1.2.12.1 2011/08/24 13:26:00 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(defparameter *hello-world-ior-file* 
  #+win32 "c:/temp/hello.ior"
  #-win32 "/tmp/hello.ior")

(defun object-to-file (orb object)
  (with-open-file (st *hello-world-ior-file* :direction :output :if-exists :supersede)
    (prin1 (op:object_to_string orb object) st)))

(defun file-to-object (orb)
  (with-open-file (st *hello-world-ior-file*)
    (op:string_to_object orb (read st))))


;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/editor:syntax-coloring:defsys.lisp,v 1.1.9.1 2011/08/24 13:26:18 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(defsystem syntax-coloring (:documentation "A simple implementation of syntax coloring.")
  :members 
  (("pkg" :source-only t)
   ("syntax-coloring"))
  :rules
  ((:in-order-to :compile ("syntax-coloring") 
    (:requires (:load "pkg")))))


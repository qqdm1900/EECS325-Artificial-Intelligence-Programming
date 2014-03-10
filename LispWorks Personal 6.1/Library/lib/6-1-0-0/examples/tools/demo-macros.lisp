;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/tools:demo-macros.lisp,v 1.2.1.1 2011/08/24 13:26:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;; simple code to demonstrate some of the tools.
;; See the LispWorks IDE User Guide.

(in-package "CL-USER")

(defmacro my-macro (func x &rest args)
  (rebinding (x args)
    `(apply ,func (apply ,func ,x ,args) ,args)))

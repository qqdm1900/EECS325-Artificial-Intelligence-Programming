;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/truck:defsystem.lisp,v 1.1.13.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

; -*-mode : lisp ; package : kw-user -*-

(in-package kw-user)

(defsystem truck-prog (:system-type :kb-system)
  :members
   ("truck-rules"))

(defsystem truck-data (:system-type :kb-init-system)
  :members
  ("truck-objs"))

(defsystem truck ()
  :members
   (("truck-prog" :type :kb-system)
    ("truck-data" :type :kb-init-system)))

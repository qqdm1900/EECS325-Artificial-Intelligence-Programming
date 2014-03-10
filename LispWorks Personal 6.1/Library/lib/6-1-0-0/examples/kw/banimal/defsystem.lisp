;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/banimal:defsystem.lisp,v 1.1.13.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

; -*-mode : lisp ; package : kw-user -*-

(in-package kw-user)

(defsystem banimal-prog (:system-type :kb-system)
  :members
   ("banimal-rules"))

(defsystem banimal-data (:system-type :kb-init-system)
  :members
  ("banimal-objs"))

(defsystem banimal ()
  :members
   (("banimal-prog" :type :kb-system)
    ("banimal-data" :type :kb-init-system)))


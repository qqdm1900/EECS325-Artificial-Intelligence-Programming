;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/train:defsystem.lisp,v 1.1.13.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

; -*-mode : lisp ; package : kw-user -*-

(in-package kw-user)

(defsystem train-prog (:system-type :kb-system)
  :members
   ("train-rules"))

(defsystem train-data (:system-type :kb-init-system)
  :members
  ("train-objs"))

(defsystem train ()
  :members
   (("train-prog" :type :kb-system)
    ("train-data" :type :kb-init-system)))

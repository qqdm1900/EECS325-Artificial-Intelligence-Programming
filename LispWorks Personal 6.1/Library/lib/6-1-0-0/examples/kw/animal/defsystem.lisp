;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/animal:defsystem.lisp,v 1.1.13.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package kw-user)


(defsystem animal-prog (:system-type :kb-system)
  :members
   ("animal-rules"))

(defsystem animal-data (:system-type :kb-init-system)
  :members
  ("animal-objs"))

(defsystem animal ()
  :members
   (("animal-prog" :type :kb-system)
    ("animal-data" :type :kb-init-system)))


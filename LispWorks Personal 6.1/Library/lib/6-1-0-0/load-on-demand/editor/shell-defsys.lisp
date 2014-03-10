;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/63/LISPeditor/RCS/shell-defsys.lisp,v 1.1.16.1 2011/08/24 13:26:13 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;; nick 19Mar96

#+:harlequin-common-lisp
(in-package "SYSTEM")
#+:lucid
(in-package "LUCID")

(defsystem shell ()
  :members (
	    ("shell-pkg" :source-only t)
	    "shell-buffer"
	    )
  :rules
  ((:in-order-to :compile :all (:requires (:load :previous)))))

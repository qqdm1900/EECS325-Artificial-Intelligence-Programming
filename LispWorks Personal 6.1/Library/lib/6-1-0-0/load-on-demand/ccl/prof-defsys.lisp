;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/42/LISPccl/RCS/prof-defsys.lisp,v 1.7.2.1 2011/08/24 13:25:54 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "SYSTEM")

(defsystem profile
  (:package system)
  :members ("prof-vars"
	    ("profile-stacks" :features (not :no-function-register))
            ("x86-profstak" :features :lispworks-x86)
	    ("prof-windows" :features :mswindows)
	    "profsetup"
	    "profrun"
            ("prof-unix-rm" :features :unix) ;; empty in non-rm
	    "prfclue")
  :rules ((:in-order-to :compile :all
			(:requires (:load "prof-vars")))
	  (:in-order-to :load :all
			(:requires (:load :serial)))))



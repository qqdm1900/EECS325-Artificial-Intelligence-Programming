;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/63/LISPparsergen/RCS/defsys.lisp,v 1.9.14.1 2011/08/24 13:27:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

#+:harlequin-common-lisp
(in-package "SYSTEM")
#+:lucid
(in-package "LUCID")

(defsystem parser-runtime ()
  :members ((pkg :source-only t)
            variables
	    messages
	    lexer
	    errorrecovery
	    switch-states
	    despatch
	    utilities
	    )
  :rules ((:in-order-to :compile :all
			(:requires (:load :serial)))))


(defsystem parser-generate (:optimize ((speed 3)
                                          (space 3)
                                          (safety 0)
                                          #+harlequin-common-lisp (fixnum-safety 0)
                                          ))
  :members ((require-parser-runtime :features :harlequin-pc-lisp
                                    :source-only t)
            newitems
	    grammar
	    items
	    pre-process-gram
	    newkerns
	    lr1close
	    propagate
	    newnewderiv
	    close
	    generate-actions
	    parseractions
	    newbuild)
  :rules ((:in-order-to :compile :all
			(:requires (:load :serial)))))

(defsystem parsergen (:optimize ((speed 3)
                                 (space 3)
                                 (safety 0)
                                 #+harlequin-common-lisp (fixnum-safety 0)
                                 ))
  :members ((parser-runtime :type :system)
            (parser-generate :type :system))
  :rules ((:in-order-to :compile :all
			(:requires (:load :serial)))))

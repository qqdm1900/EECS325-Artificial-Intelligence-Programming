;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/5/LISPffi-tools-parser/RCS/defsys.lisp,v 1.11.11.1 2011/08/24 13:26:29 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "SYSTEM")


;;----------------------------------------------------------------------
;; process-foreign-file stuff
;;----------------------------------------------------------------------

(defsystem c++-parser (:default-type :lisp-file)
  :members (("../../parsergen/pkg.lisp"  :source-only t)
	    lexer
            src-parser
            parser-top )
  :rules ((:in-order-to :load :all (:requires (:load :serial)))
          (:in-order-to :compile :all (:requires (:load :serial)))))


(defsystem foreign-preprocessor (:default-type :lisp-file)
  :members ((pkg :source-only t)
	    preproc
            )
  :rules ((:in-order-to :load :all (:requires (:load :serial)))
          (:in-order-to :compile :all (:requires (:load :serial)))))

(defsystem foreign-parser (:default-type :lisp-file)
  :members (load-preproc
            variables
            io
            ("c++-parser" :type :system)
            lex-hack
            c-parser
            cplusplus-defns ;; must follow c-parser
            unparse-c
            f-types  ;;; PJG 17June95
            top
            )
  :rules ((:in-order-to :load :all (:requires (:load :serial)))
          (:in-order-to :compile :all (:requires (:load :serial)))))

(defsystem foreign-parser-all (:default-type :lisp-file)
  :members ((foreign-preprocessor :type :system)
            (foreign-parser :type :system)
            )
  :rules ((:in-order-to :load :all (:requires (:load :serial)))
          (:in-order-to :compile :all (:requires (:load :serial)))))

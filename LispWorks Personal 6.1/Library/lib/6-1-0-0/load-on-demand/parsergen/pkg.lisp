;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/63/LISPparsergen/RCS/pkg.lisp,v 1.4.15.1 2011/08/24 13:27:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

#+:harlequin-common-lisp
(in-package "SYSTEM")
#+:lucid
(in-package "LUCID")


;; PJG 8Aug95
;; 1. removed references to useless indent code
;; 2. removed the exports of
;;         *lexer*                - internal usage only
;;         *symbol-to-string*     - internal usage only
;;         *token-pos-function*   - never used

(defpackage "PARSERGEN"
  (:nicknames)
  (:add-use-defaults)
  (:export "CURRENT-SYMBOL"
           "CURRENT-SYMBOL-VALUE"
           "DEFPARSER"
           "DISCARD-INPUT"
           "OUTPUT-ERROR"
           "OUTPUT-MESSAGE"
           "OUTPUT-WARNING"
           "PEEK-NEXT-LEXEME"
           "PUSH-AND-RESTART"
           "PUSH-TOKEN"
           "READ-NEXT-LEXEME"
           "RUN-PARSER"))

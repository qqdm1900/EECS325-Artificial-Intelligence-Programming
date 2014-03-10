;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/63/LISPeditor/RCS/indent-defs.lisp,v 1.35.1.1 2011/08/24 13:26:11 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "EDITOR")

;; This file contains the indentations with which LispWorks was sent out.  This
;; file is NOT read in each time LispWorks runs, so any modified indentations
;; should be put in an initialisation file or saved inside a new image.  The
;; purpose of providing this file is to give the user some examples of how 
;; indentations can be specified.

;; (SETUP-INDENT FNAME ARGS &OPTIONAL ORDINARY-INDENTATION SPECIAL-INDENTATION)
;; defines how FNAME forms are indented.  ARGS is the number of "special"
;; arguments at the beginning of a FNAME form.  SPECIAL-INDENTATION
;; specifies the indentation for these special arguments (default 4), and
;; ORDINARY-INDENTATION is the indentation for the rest (default 2).


;;----------------------------------------------------------------------------
;; Common Lisp definitions
;;----------------------------------------------------------------------------

(setup-indent "assert" 2)
(setup-indent "block" 1)
(setup-indent "case" 1)
(setup-indent "catch" 1)
(setup-indent "ccase" 1)			   
(setup-indent "ctypecase" 1)
(setup-indent "defconstant" 1)
(setup-indent "define-compiler-macro" 2 2 4)
(setup-indent "define-condition" 2)
(setup-indent "define-method-combination" 2 2 4)
(setup-indent "define-modify-macro" 2 2 4)
(setup-indent "define-setf-expander" 2)
(setup-indent "define-symbol-macro" 1)
(setup-indent "defmacro" 2 2 10)
(setup-indent "defpackage" 1)
(setup-indent "defparameter" 1)
(setup-indent "defglobal-parameter" 1)
(setup-indent "defsetf" 3 2 9)
(setup-indent "defstruct" 1)
(setup-indent "deftype" 2 2 9)
(setup-indent "defun" 2 2 7)
(setup-indent "defclass" 2 2 10)
(setup-indent "defmethod" 2 2 11)
(setup-indent "defgeneric" 2 2 4)
(setup-indent "defvar" 1)
(setup-indent "defglobal-variable" 1)
(setup-indent "destructuring-bind" 2 2 4)
(setup-indent "do" 2 2 4)
(setup-indent "do*" 2 2 5)
(setup-indent "do-all-symbols" 1)
(setup-indent "do-external-symbols" 1)
(setup-indent "do-symbols" 1)
(setup-indent "dolist" 1)
(setup-indent "dolists" 1)
(setup-indent "dotimes" 1)
(setup-indent "ecase" 1)
(setup-indent "etypecase" 1)
(setup-indent "eval-when" 1)
(setup-indent "flet" 1 nil nil 'flet)
(setup-indent "flet-with-debugger-env" 1 nil nil 'flet)
(setup-indent "handler-bind" 1)
(setup-indent "handler-case" 1 nil nil 'handler-case)
(setup-indent "ignore-errors" 0)
(setup-indent "if" 2 2 4)
(setup-indent "labels" 1 nil nil 'flet)
(setup-indent "lambda" 1 2 8)
(setup-indent "let" 1 2)
(setup-indent "let*" 1 2)
(setup-indent "locally" 0)
(setup-indent "macrolet" 1 nil nil 'flet)
(setup-indent "multiple-value-bind" 2 2 4)
(setup-indent "multiple-value-call" 1)
(setup-indent "multiple-value-prog1" 1)
(setup-indent "multiple-value-prog2" 1)
(setup-indent "multiple-value-setq" 2 2 4)
(setup-indent "multiple-value-setf" 2 2 4)
(setup-indent "print-unreadable-object" 1)
(setup-indent "prog" 1)
(setup-indent "prog*" 1)
(setup-indent "prog1" 1)
(setup-indent "prog2" 2)
(setup-indent "progv" 2)
(setup-indent "progn" 0 2)
(setup-indent "restart-bind" 1 2 4)
(setup-indent "restart-case" 1 2 4 'handler-case)
(setup-indent "return-from" 1 2)
(setup-indent "symbol-macrolet" 1)
(setup-indent "typecase" 1)
(setup-indent "unless" 1)
(setup-indent "unwind-protect" 1)
(setup-indent "unwind-protect-blocking-interrupts" 1)
(setup-indent "unwind-protect-blocking-interrupts-in-cleanups" 1)
(setup-indent "when" 1)
(setup-indent "with-accessors" 2 2 4)
(setup-indent "with-condition-restarts" 2 2 4)
(setup-indent "with-input-from-string" 1)
(setup-indent "with-open-file" 1)
(setup-indent "with-open-stream" 1)
(setup-indent "with-output-to-string" 1)
(setup-indent "with-package-iterator" 1)
(setup-indent "with-simple-restart" 1)
(setup-indent "with-standard-io-syntax" 0)
(setup-indent "with-slots" 2 2 4)


;;; Some CL macros would need definitions, were it not for the fact
;;; that the indenter looks for &BODY in the argument list, and
;;; automatically indents body forms by two spaces.


;;----------------------------------------------------------------------------
;; Some LW externals definitions
;;----------------------------------------------------------------------------

(setup-indent "compiler-let" 1)
(setup-indent "defadvice" 2 2 4)
(setup-indent "define-setf-method" 2)
(setup-indent "rebinding" 1)
(setup-indent "stack-let" 1 2)
(setup-indent "top-level-form" 1 2 4)
(setup-indent "when-let" 1)
(setup-indent "if-let" 2 2 4)
(setup-indent "with-unique-names" 1)

(setup-indent "defun-with-raw" 2 2 7)
(setup-indent "defasm" 1)
(setup-indent "with-file-stats" 2 2 4)

(setup-indent "defresource" 2 2 4)
(setup-indent "allocate-resource" 1 2 4)
(setup-indent "deallocate-resource" 2 2 4)
(setup-indent "deallocate-whole-resource" 1 2 4)
(setup-indent "clear-resource" 1 2 4)
(setup-indent "map-resource" 2 2 4)

	   


;;----------------------------------------------------------------------------
;; Various GUI definitions
;;----------------------------------------------------------------------------

(setup-indent "define-environment" 2 2 5)
(setup-indent "with-environment" 1 2 4)

(setup-indent "with-lock" 1 2 4)


;;----------------------------------------------------------------------------
;; Editor definitions
;;----------------------------------------------------------------------------

(setup-indent "with-point" 1 2 3)
(setup-indent "with-random-typeout" 1 2 3)
(setup-indent "define-editor-variable" 0 3 0)
(setup-indent "define-editor-mode-variable" 1 3 0)

(setup-indent "defcommand" 4 2 5)
(setup-indent "command-case" 0)
(setup-indent "with-input-from-region" 1)
(setup-indent "with-output-to-point" 1)
(setup-indent "do-strings" 1)
(setup-indent "do-alpha-chars" 1)

(setup-indent "defmode" 1)
(setup-indent "recording-for-undo" 2 2 4)
(setup-indent "collect-undo" 1 2 4)

(setup-indent "new-modifying-buffer" 1)
(setup-indent "modifying-buffer-pre-p" 2)
(setup-indent "use-buffer" 1)
(setup-indent "use-buffer-and-window" 2 2 4)
(setup-indent "with-buffer-point-and-mark" 1 2 4)
(setup-indent "with-compilation-environment-at-point" 1 2 4)
(setup-indent "with-echo-area" 1 2 4)
(setup-indent "with-output-to-help-stream" 1 2 4)
(setup-indent "with-output-to-help-window" 1 2 4)
(setup-indent "with-package-for-point" 1 2 4)
(setup-indent "with-defvar-action" 1 2 4)
(setup-indent "with-random-typeout-to-window" 1 2 4)


;;----------------------------------------------------------------------------
;; defsystem indentations
;;----------------------------------------------------------------------------

(setup-indent "defsystem" 1 2 4)
(setup-indent "with-scm-target-machine" 1 2 4)


;;----------------------------------------------------------------------------
;; debugger indentations
;;----------------------------------------------------------------------------

(setup-indent "for-next-frames" 2 2 4)
(setup-indent "for-prev-frames" 2 2 4)

(setup-indent "debug-message" 1 2 4)
(setup-indent "when-debugging" 1 2 4)


;;----------------------------------------------------------------------------
;; Graphics Ports indentations
;;----------------------------------------------------------------------------

(setup-indent "with-allocated-object" 1 2 4)
(setup-indent "with-object-locked" 1 2 4)
(setup-indent "defsubst" 1 2)
(setup-indent "define-state-saver" 1 2 5)
(setup-indent "transform-bind" 2 2 4)
(setup-indent "define-graphics-function" 2 2 4)
(setup-indent "with-graphics-state" 1 2 4)
(setup-indent "with-inverse-graphics" 1 2 4)
(setup-indent "with-graphics-transform" 1 2 4)
(setup-indent "with-graphics-scale" 1 2 4)
(setup-indent "with-graphics-translation" 1 2 4)
(setup-indent "with-graphics-rotation" 1 2 4)
(setup-indent "with-transformed-points" 1 2 4)
(setup-indent "with-transformed-rect" 1 2 4)
(setup-indent "with-transformed-area" 1 2 4)
(setup-indent "with-transformed-point" 1 2 4)
(setup-indent "with-pixmap-graphics-port" 1 2 4)
(setup-indent "without-relative-drawing" 1 2 4)
(setup-indent "rectangle-bind" 2 2 4)
(setup-indent "unless-empty-rect-bind" 2 2 4)
(setup-indent "rect-bind" 2 2 4)


;;----------------------------------------------------------------------------
;; Raw indentations
;;----------------------------------------------------------------------------

(setup-indent "raw-case" 1 2 4)


;;; ----------------------------------------------------------------------
;;; Foreign Language Interface
;;; PJG 14Aug'96 - removed the common FFI definitions and replaced them
;;;                with the foreign language interface definitions.
;;; ----------------------------------------------------------------------

(setup-indent "define-c-enum" 1 2 4)
(setup-indent "define-c-union" 1 2 4)
(setup-indent "define-c-struct" 1 2 4)
(setup-indent "define-c-typedef" 1 2 4)
(setup-indent "define-foreign-funcallable" 2 2 4)
(setup-indent "define-foreign-function" 2 2 4)
(setup-indent "define-foreign-variable" 1 2 4)
(setup-indent "define-foreign-type" 2 2 4)
(setup-indent "define-foreign-callable" 2 2 4)
(setup-indent "define-foreign-converter" 3 2 4)
(setup-indent "define-foreign-pointer" 2)


;;----------------------------------------------------------------------------
;; SQL indentations
;;----------------------------------------------------------------------------

(setup-indent "def-view-class" 2 2 10)


;;----------------------------------------------------------------------------
;; KnowledegeWorks indentations
;;----------------------------------------------------------------------------

(setup-indent "def-kb-class" 2 2 10)
(setup-indent "def-named-kb-class" 2 2 10)
(setup-indent "def-kb-struct" 1)
(setup-indent "defcontext" 1)


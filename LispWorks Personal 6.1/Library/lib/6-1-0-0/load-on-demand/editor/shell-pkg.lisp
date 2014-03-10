;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/63/LISPeditor/RCS/shell-pkg.lisp,v 1.10.2.1 2011/08/24 13:26:12 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;; nick 19Mar96 - extract pkg definition from shell-buffer.lisp

(in-package "EDITOR")

;; The shell interface is a mess, reputedly
(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc #'(lambda (name) (intern name "EDITOR"))
        '("EXIT-SHELL-WINDOW"
	  "INIT-SHELL-WINDOW"
	  "KILL-SHELL-WINDOW"
          "SHELL-TOP-LEVEL"
	  "SHELL-WINDOW-SUBPROCESS-PID")))

(defpackage "SHELL"
  (:add-use-defaults)
  (:use
   "EDITOR" "MP"
   )
  (:import-from "EDITOR"
		"%SET-BUFFER-PATHNAME" "*GET-RID-OF-LAST-DOT*" 
		"BUFFER"
		"DO-CD-COMMAND" "EXIT-SHELL-WINDOW"
		"FLAG-WINDOW-BUFFER"
		"INIT-SHELL-WINDOW" "KILL-SHELL-WINDOW"
		"MAKE-COMMAND-RUN-TOOL" "MAKE-SHELL-TOOL" "PROCESS-EVENTS-UNTIL"
		"SHELL-TOP-LEVEL" "SHELL-WINDOW-SUBPROCESS-PID"
		"UPDATE-HISTORY-LIST")
  #-:Lucid
  (:import-from "SYSTEM"
		"OPEN-PIPE"
		"WHITESPACE-CHAR-P" "LISP-MEMORY-COPY-BYTES")
  #+:Lucid
  (:import-from "LUCID"
		"LISP-MEMORY-COPY-BYTES")
  #+:Lucid
  (:import-from "SYSTEM"
		"WHITESPACE-CHAR-P")
  
  (:export "*SHELL-INTR-CHARACTER*" "*SHELL-PROMPT-REGEXP-STRING*"
	   "CD-COMMAND" "INTERRUPT-SHELL-SUBJOB-COMMAND" "SHELL-SEND-EOF-COMMAND"
	   "STOP-SHELL-SUBJOB-COMMAND"
	   ))

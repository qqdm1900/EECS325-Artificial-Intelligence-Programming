;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/editor:commands:delete-deletes-selection.lisp,v 1.1.1.1 2011/08/24 13:26:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;----------------------------------------------------------------------------
;;
;; examples/editor/commands/delete-deletes-selection.lisp
;;
;; Example of how to make the Delete key delete highlight text.
;;
;; To use, evaluate code below and press Delete key.

;; Define new command 
(editor:defcommand "Delete Highlighted Region or Next Character" (p)
     "Delete the highlighted region, or the next character if there is no highlighted region."
     "Delete highlighted region, otherwise next character."
  (let ((buffer (editor:current-buffer)))
    (if (editor::region-highlighted-p buffer)
        (editor:delete-region-command p)
      (editor:delete-next-character-command p))))

;;; Make the Delete key invoke the command
(editor:bind-key "Delete Highlighted Region or Next Character" "Delete")

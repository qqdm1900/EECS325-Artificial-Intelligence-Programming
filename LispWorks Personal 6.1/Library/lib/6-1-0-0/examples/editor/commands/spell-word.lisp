;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/editor:commands:spell-word.lisp,v 1.5.1.1 2011/08/24 13:26:18 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;; ----------------------------------------------------------------------
;;; Spell Word - Currently only works on *nix
;;; Place the cursor on a word and run the command "Spell Word".  
;;; This will call the unix command (default - "spell") to verify the
;;; spelling of the current word. The result of the check will be
;;; displayed in the echo area.
;;; ----------------------------------------------------------------------

(editor:defcommand "Spell Word" (p)
     "Call Unix spell command on the current word, and display a message."
     "Call Unix spell command."
  (spell-word  p))

(defvar *spell-command* "spell"
  "The name of the unix command used by \"Spell Word\"")

(defun spell-word (p)
  (when-let (word
             (editor:with-current-point-locked
                 (cp :for-modification nil :errorp nil)
               
               (editor:backward-word-command p)
               (let* ((end-point (editor:copy-point cp)))
                 (editor:forward-word-command p)
                 (editor:points-to-string end-point cp))))
    (editor:message "Spelling word ~S" word)
    (multiple-value-bind (result output)
        (sys:call-system-showing-output
         (format nil "echo ~a | ~A~%" word *spell-command*)
         :show-cmd nil
         :output-stream nil)
      (cond
       ((/= 0 result) (editor:message "Call to ~S failed!" *spell-command*))
       ((string= output "") (editor:message "Spelling correct!"))
       (t (editor:message "Incorrect spelling : ~S" word))))))



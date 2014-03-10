;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/banimal:banimal-rules.lisp,v 1.2.11.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; an entirely backward chaining version of animal guessing game
;;; type (play-game) into kw listener to start

(in-package kw-user)

;;; -------------------------- OBJECT DEFINITIONS -----------------------

(def-kb-class node ()
   ((animal :initform nil :accessor node-animal :initarg :animal)
    (question :initform nil :accessor node-question :initarg :question)
    (yes-node :initform nil :accessor node-yes-node :initarg :yes-node)
    (no-node :initform nil :accessor node-no-node :initarg :no-node)))

(def-kb-class root ()
   ((node :initform nil :accessor root-node :initarg :node)))

(def-kb-struct current-node node)
(def-kb-struct game-over node animal answer)

(defrule play-game :backward
  ((play-game)
   <--
   ((capi:display-message "  ANIMAL GUESSING GAME - ~
          think of an animal to continue"))
   (root ? node ?node)
   (ask-question ?node)))

(defrule ask-question :backward
  ((ask-question ?node)
   <--
   (node ?node animal nil question ?q yes-node ?y-n no-node ?n-n)
   (cut)
   ((capi:confirm-yes-or-no ?q) ?answer)
   ((find-new-node ?answer ?y-n ?n-n) ?new-node)
   (ask-question ?new-node))
  ((ask-question ?node)
   <--
   (node ?node animal ?animal question nil)
   ((capi:confirm-yes-or-no (format nil "Is it a ~a?" ?animal)) ?answer)
   (game-done ?answer ?animal ?node)))

(defun find-new-node (answer yes-node no-node)
  (if answer yes-node no-node))

(defrule game-done :backward
  ((game-done t ? ?)
   <--)
  ((game-done nil ?animal ?node)
   <--
   (fetch-new-animal ?new-animal)
   ((capi:prompt-for-string
     (format nil "Tell me a question for which the answer is yes for a ~
~a and no for a ~a" ?new-animal ?animal)) ?question)
   (assert (node ?yes-node question nil animal ?new-animal))
   (assert (node ?no-node question nil animal ?animal))
   (assert (node ?node animal nil yes-node ?yes-node no-node ?no-node
		 question ?question))))

(defrule fetch-new-animal :backward
   ((fetch-new-animal ?new-animal)
    <--
;    (repeat)
    ((string-upcase (capi:prompt-for-string "What was your animal?"))
     ?new-animal)
    (not (= ?new-animal "NIL"))      ;; check if abort was pressed
    (or
     (doesnt-exist-already ?new-animal)
     (and ((capi:display-message "Animal exists already"))
          (fail)))))

(defrule doesnt-exist-already :backward
  ((doesnt-exist-already ?animal)
   <--
   (node ? animal ?animal)
   (cut)
   (fail))
  ((doesnt-exist-already ?animal)
   <-- ))

;;; -------------------------- SAVING THE ANIMAL BASE -----------------------

(defun save-animals (filename)
  (let* ((start-node (any '?node '(root ? node ?node)))
         (code `(make-instance 'root :node ,(node-code start-node)))
         (*print-pretty* t))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (write '(in-package kw-user) :stream stream)
      (write-char #\Newline stream)
      (write code :stream stream))
    nil))

(defun node-code (node)
  (when node
    `(make-instance 'node :question ,(node-question node)
		    :animal ',(node-animal node)
                    :yes-node ,(node-code (node-yes-node node))
                    :no-node ,(node-code (node-no-node node)))))

;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/whist:defsys.lisp,v 1.2.1.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; The code in this directory demonstrates incorporating
;;; knowledgeworks code inside normal LISP code. The lisp code
;;; does the display, and the knowledgeworks part only does 
;;; some of the computation to decide what to play. 

;;; 1. Load or eval this file.
;;; 2. compile and load the system WHIST.
;;; 3. (whist::run-whist)
;;;   "Game" -> "Restart"
;;; This deals you a "hand" of thirteen cards and start playing.
;;; You choose a card to play by clicking on it.

 
;;; The knowledgeworks code is in rules. lisp 
;;; There are two contexts:
;;;    assess   - this assess the hand of the computer (palyer
;;;               with kind automatic) and sets values 
;;;               in the cards to be used by the lead context.
;;;               Used inside RESET-COMMAND in tool.lisp when
;;;               restarting a game.

;;;    lead     - used to decide what to lead, based on values
;;;               set by assess. It sets the score of each card,
;;;               and the LISP code in AUTOMATIC-GET-LEAD in main.lisp
;;;               Use the score to decide what to lead. 


(require "kw")    



(defsystem whist-rules (:system-type :kb-system)
  :members
   ("rules"))

(defsystem whist ()
  :members
   (("defpkg" :source-only t)
     "main"
     "tool"
    ("whist-rules" :type :kb-system))
  :rules ((:in-order-to :compile :all (:requires (:load :serial)))))


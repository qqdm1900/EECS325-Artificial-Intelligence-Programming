;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/explanation:mab.lisp,v 1.1.13.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; -------------- MONKEY AND BANANA EXAMPLE WITH EXPLANATIONS ---------------
;;; to run (run-mab)
;;; for textual explanation (explain), or (explain-an-action) for window-menu

(in-package kw-user)

(def-named-kb-class monkey ()
  ((at :initarg :at :initform nil)
   (on :initarg :on :initform nil)
   (holds :initarg :holds :initform nil)))

(def-named-kb-class object ()
  ((at :initarg :at :initform nil)
   (weight :initarg :weight :initform nil)
   (on :initarg :on :initform nil)))

(def-kb-struct goal status type object to)
(def-kb-struct start)

;;; the strategy (mea lex specificity) simulates the OPS5 strategy MEA
;;; (these rules were copied directly from an OPS5 program)

(defcontext mab :strategy (mea lex specificity)
            :meta ((explain-context)))

(defrule mb1 :forward
  :context mab
  (goal ?g status active type holds object ?w)
  (object ?o kb-name ?w at ?p on ceiling)
  -->
  (assert (goal ? status active type move object ladder to ?p)))

(defrule mb2 :forward
  :context mab
  (goal ?g status active type holds object ?w)
  (object ?o1 kb-name ?w at ?p on ceiling)
  (object ?o2 kb-name ladder at ?p)
  -->
  (assert (goal ? status active type on object ladder)))

(defrule mb3 :forward
  :context mab
  (goal ?g status active type holds object ?w)
  (object ?o1 kb-name ?w at ?p on ceiling)
  (object ?o2 kb-name ladder at ?p)
  (monkey ?m on ladder)
  -->
  (assert (goal ? status active type holds object nil)))

(defrule mb4 :forward
  :context mab
  (goal ?g status active type holds object ?w)
  (object ?o1 kb-name ?w at ?p on ceiling)
  (object ?o2 kb-name ladder at ?p)
  (monkey ?m on ladder holds nil)
  -->
  ((format t "~%Grab ~s" ?w))
  (assert (monkey ?m holds ?w))
  (assert (goal ?g status satisfied)))

(defrule mb5 :forward
  :context mab
  (goal ?g status active type holds object ?w)
  (object ?o1 kb-name ?w at ?p on floor)
  -->
  (assert (goal ? status active type walk-to object ?p)))

(defrule mb6 :forward
  :context mab
  (goal ?g status active type holds object ?w)
  (object ?o1 kb-name ?w at ?p on floor)
  (monkey ?m at ?p)
  -->
  (assert (goal ? status active type holds object nil)))

(defrule mb7 :forward
  :context mab
  (goal ?g status active type holds object ?w)
  (object ?o1 kb-name ?w at ?p on floor)
  (monkey ?m at ?p holds nil)
  -->
  ((format t "~%Grab ~s" ?w))
  (assert (monkey ?m holds ?w))
  (assert (goal ?g status satisfied)))

(defexplain mb7
  :why ("Monkey is at the ~s which is on the floor" ?w)
  :what ("Monkey grabs the ~s" ?w)
  :because ("Monkey needs the ~s somewhere else" ?w))

(defrule mb8 :forward
  :context mab
  (goal ?g status active type move object ?o to ?p)
  (object ?o1 kb-name ?o weight light at ?q)
  (test (not (eq ?q ?p)))
  -->
  (assert (goal ? status active type holds object ?o)))

(defrule mb9 :forward
  :context mab
  (goal ?g status active type move object ?o to ?p)
  (object ?o1 kb-name ?o weight light at ?q)
  (test (not (eq ?q ?p)))
  (monkey ?m holds ?o)
  -->
  (assert (goal ? status active type walk-to object ?p)))

(defrule mb10 :forward
  :context mab
  (goal ?g status active type move object ?o to ?p)
  (object ?o1 kb-name ?o weight light at ?p)
  -->
  (assert (goal ?g status satisfied)))

(defrule mb11 :forward
  :context mab
  (goal ?g status active type walk-to object ?p)
  -->
  (assert (goal ? status active type on object floor)))

(defrule mb12 :forward
  :context mab
  (goal ?g status active type walk-to object ?p)
  (monkey ?m on floor at ?c holds nil)
  (test (not (eq ?c ?p)))
  -->
  ((format t "~%Walk to ~s" ?p))
  (assert (monkey ?m at ?p))
  (assert (goal ?g status satisfied)))

(defexplain mb12
  :why ("Monkey is on the floor holding nothing")
  :what ("Monkey walks to ~s" ?p)
  :because ("Monkey needs to do something with an object at ~s" ?p))

(defrule mb13 :forward
  :context mab
  (goal ?g status active type walk-to object ?p)
  (monkey ?m on floor at ?c holds ?w)
  (test (and ?w (not (eq ?c ?p))))
  (object ?o1 kb-name ?w)
  -->
  ((format t "~%Walk to ~s" ?p))
  (assert (monkey ?m at ?p))
  (assert (object ?o1 at ?p))
  (assert (goal ?g status satisfied)))

(defexplain mb13
  :why ("Monkey is on the floor and is holding the ~s" ?w)
  :what ("Monkey walks to ~s with the ~s" ?p ?w)
  :because ("Monkey wants the ~s to be at ~s" ?w ?p))

(defrule mb14 :forward
  :context mab
  (goal ?g status active type on object floor)
  (monkey ?m on ?x)
  (test (not (eq ?x 'floor)))
  -->
  ((format t "~%Jump onto the floor"))
  (assert (monkey ?m on floor))
  (assert (goal ?g status satisfied)))

(defexplain mb14
  :why ("Monkey is on ~s" ?x)
  :what ("Monkey jumps onto the floor")
  :because ("Monkey needs to go somewhere"))

(defrule mb15 :forward
  :context mab
  (goal ?g status active type on object ?o)
  (object ?o1 kb-name ?o at ?p)
  -->
  (assert (goal ? status active type walk-to object ?p)))

(defrule mb16 :forward
  :context mab
  (goal ?g status active type on object ?o)
  (object ?o1 kb-name ?o at ?p)
  (monkey ?m at ?p)
  -->
  (assert (goal ? status active type holds object nil)))

(defrule mb17 :forward
  :context mab
  (goal ?g status active type on object ?o)
  (object ?o1 kb-name ?o at ?p)
  (monkey ?m at ?p holds nil)
  -->
  ((format t "~%Climb onto ~s" ?o))
  (assert (monkey ?m on ?o))
  (assert (goal ?g status satisfied)))

(defexplain mb17
  :why ("Monkey is at the location of the ~s" ?o)
  :what ("Monkey climbs onto the ~s" ?o)
  :because ("Monkey wants to be on top of the ~s" ?o))

(defrule mb18 :forward
  :context mab
  (goal ?g status active type holds object nil)
  (monkey ?m holds ?x)
  (test ?x)
  -->
  ((format t "~%Drop ~s" ?x))
  (assert (monkey ?m holds nil))
  (assert (goal ?g status satisfied)))

(defexplain mb18
  :why ("Monkey is holding the ~s" ?x)
  :what ("Monkey drops the ~s" ?x)
  :because ("Monkey wants to do something for which he can't hold anything"))

(defrule mb19 :forward
  :context mab
  (goal ?g status active)
  -->
  (assert (goal ?g status not-processed)))

(defrule t1 :forward
  :context mab
  (start ?s)
  -->
  (assert (monkey ? at 5-7 on couch))
  (assert (object ? kb-name couch at 5-7 weight heavy))
  (assert (object ? kb-name bananas on ceiling at 2-2))
  (assert (object ? kb-name  ladder on floor at 9-5 weight light))
  (assert (goal ? status active type holds object bananas)))

(defun run-mab ()
  (reset)
  (make-instance 'start)
  (infer :contexts '(mab)))

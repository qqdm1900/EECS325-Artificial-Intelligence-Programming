;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/uncertainty:uncertain.lisp,v 1.1.13.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; ----------- A SIMPLE VERSION OF REASONING WITH UNCERTAINTY FACTORS --------

(in-package kw-user)

(defvar *c-factor* 1)               ;;; default certainty factor
(defvar *implication-strength* 1)   ;;; implication strength of a rule

(defun default-c-factor ()
  "if the forward chainer is not running, certainty factor is just 1"
  (if *in-interpreter* (* *implication-strength* *c-factor*) 1))

;;; uncertain objects need a slot to store their 'probability'
;;; this slot defaults to the value returned by default-c-factor

(def-kb-class uncertain-kb-object ()
  ((c-factor :initform (default-c-factor) :initarg :c-factor)))

(defun object-c-factor (obj)
  "if an object has no uncertainty slot, return 1 (i.e. certain)"
  (if (slot-exists-p obj 'c-factor) (slot-value obj 'c-factor) 1))

(defun inst-c-factor (inst)
  "the certainty factor of an instantiation"
  (token-c-factor (inst-token inst)))

(defun token-c-factor (token)
  "the certainty factor of an ANDed list of objects (just multiply them)"
  (reduce '* (mapcar 'object-c-factor token)))

(defun implication-strength (val)
  "for a rule to set the implication strength"
  (setq *implication-strength* val))

;;; this function increases the certainty of the object which is the
;;; first argument by an amount dependent on the combined certainty of
;;; the remaining arguments

(defun add-evidence (obj &rest token)
  "increments the certainty of obj based on the certainty of token"
  (let ((c-f (slot-value obj 'c-factor)))
    (setf (slot-value obj 'c-factor)
          (+ c-f
             (* (- 1 c-f) *implication-strength* (token-c-factor token))))))

;;; this tactic is dynamic as the certainty factor slot gets changed by
;;; calling add-evidence

(deftactic certainty :dynamic (i1 i2)
  "a conflict resolution tactic to prefer more certain instantiations"
  (> (inst-c-factor i1) (inst-c-factor i2)))

;;; Before firing a rule this meta-interpreter just sets the value of
;;; *c-factor* to the certainty of the instantiation so that any new
;;; uncertain objects made get this (times *implication-strength*) as
;;; their certainty. Also sets *implication-strength* to 1 as a default
;;; in case the rule doesn't set it.

(defrule uncertain-context :backward
  ((uncertain-context)
   <--
   (start-cycle)
   (instantiation ?inst)
   ((progn (setq *c-factor* (inst-c-factor ?inst))
      (setq *implication-strength* 1)))
   (fire-rule ?inst)
   (cut)
   (uncertain-context)))

;;; ------------------------- SOME EXAMPLE RULES ------------------------
;;; to run: (run-diagnose)

(def-kb-struct start)
(def-kb-class symptom (uncertain-kb-object) ((type :initarg :type)))
(def-kb-class fault (uncertain-kb-object) ((type :initarg :type)))
(def-kb-class remedy (uncertain-kb-object) ((type :initarg :type)))

;;; this context sets up the initial hypotheses and gathers evidence
;;; this doesn't need the meta-interpreter as that's only necesssary
;;; for transparent assignment of certainty factors to new objects

(defcontext diagnose :strategy ())

(defrule start-rule :forward
  :context diagnose
  (start ?s)
  -->
  (assert (symptom ? type over-heat c-factor 1))
  (assert (symptom ? type power-loss c-factor 1))
  (assert (fault ? type lack-of-oil c-factor 0.5))  ; an initial suspicion
  (assert (fault ? type lack-of-water c-factor 0))
  (assert (fault ? type battery c-factor 0))
  (assert (fault ? type unknown c-factor 0))
  (context (cure)))                             ; next context onto agenda

(defrule diagnose1 :forward
  :context diagnose
  (symptom ?s type over-heat)
  (fault ?f type lack-of-water)
  -->
  ((implication-strength 0.9))
  ((add-evidence ?f ?s)))

(defrule diagnose2 :forward
  :context diagnose
  (symptom ?s type overheat)
  (fault ?f type unknown)
  -->
  ((implication-strength 0.1))
  ((add-evidence ?f ?s)))

(defrule diagnose3 :forward
  :context diagnose
  (symptom ?s type wont-start)
  (fault ?f type battery)
  -->
  ((implication-strength 0.9))
  ((add-evidence ?f ?s)))

(defrule diagnose4 :forward
  :context diagnose
  (symptom ?s type wont-start)
  (fault ?f type unknown)
  -->
  ((implication-strength 0.1))
  ((add-evidence ?f ?s)))

(defrule diagnose5 :forward
  :context diagnose
  (symptom ?s type power-loss)
  (fault ?f type lack-of-oil)
  -->
  ((implication-strength 0.9))
  ((add-evidence ?f ?s)))

(defrule diagnose6 :forward
  :context diagnose
  (symptom ?s type power-loss)
  (fault ?f type unknown)
  -->
  ((implication-strength 0.1))
  ((add-evidence ?f ?s)))

;;; any two distinct symptoms strengthens the hypothesis that there's
;;; something more serious going wrong

(defrule diagnose7 :forward
  :context diagnose
  (symptom ?s1 type ?t1)
  (symptom ?s2 type ?t2)
  (test (not (eq ?t1 ?t2)))
  (fault ?f type unknown)
  -->
  ((implication-strength 1))
  ((add-evidence ?f ?s1 ?s2)))

;;; here we need the meta-interpreter to assign the right certainty factors
;;; to the remedy objects
;;; also use certainty as a conflict resolution tactic to print the suggested
;;; remedies out in order

(defcontext cure :strategy (priority certainty)
            :meta ((uncertain-context)))

(defrule cure1 :forward
  :context cure
  (fault ?f type unknown)
  -->
  ((implication-strength 0.1))
  (assert (remedy ? type cross-fingers))
  ((implication-strength 0.9))
  (assert (remedy ? type go-to-garage)))

(defrule cure2 :forward
  :context cure
  (fault ?f type lack-of-oil)
  -->
  ((implication-strength 1))
  (assert (remedy ? type add-oil)))

(defrule cure3 :forward
  :context cure
  (fault ?f type lack-of-water)
  -->
  ((implication-strength 1))
  (assert (remedy ? type add-water)))

(defrule cure4 :forward
  :context cure
  (fault ?f type battery)
  -->
  ((implication-strength 1))
  (assert (remedy ? type new-battery)))

(defrule print-cures :forward
  :context cure
  :priority 5
  (remedy ?r type ?t)
  -->
  ((format t "~%Suggest remedy ~a with certainty-factor ~a"
           ?t (slot-value ?r 'c-factor))))

(defun run-diagnose ()
  (reset)
  (make-instance 'start)
  (infer :contexts '(diagnose)))


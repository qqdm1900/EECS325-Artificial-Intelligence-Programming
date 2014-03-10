;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/spill:spill.lisp,v 1.1.13.1 2011/08/24 13:25:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package kw-user)

#| Problem Description:
There are five buildings connected by outflow pipes to a stream. The buildings
produce chemicals which, if something goes wrong, may leak into the stream.
The aim is to have a system which helps the plant overseeer decide what to do
if contamination is detected in one of the outflow pipes.

The source(s) of the contamination must be found and dealt with, and it is also
important to try to stop the contamination getting into the stream.

Contamination may be continuously flowing in which case it can be traced back
to its source, or it may occur for a discrete period of time in which case
analysis of the chemicals in the contamination can help determine where the
source(s) might have been.

This is only a very simple demonstration system.

To run the demo, type (run-spill)
|#

;;; The real physical objects in the problem are represented using CLOS as they
;;; might be needed elsewhere in a serious system (or they might have been
;;; inherited from elsewhere).

(def-named-kb-class chemical () ((type :initarg :type :reader chem-type)))
(def-named-kb-class building () ((waste :initarg :waste)))
(def-named-kb-class outflow () ((from :initarg :from)))

;;; Facts internal to the rule interpreter are represented using structures.
;;; This is more efficient.

(def-kb-struct sample location constituents contaminated)
(def-kb-struct source building certain)
(def-kb-struct chemical-analysis sample chemicals)
(def-kb-struct containment location type certain (urgency 0))

(defvar *bg-output*)   ;; nice to have a background trace of what's going on

;;; In this case we have five buildings numbered 1 to 5. Each produces a
;;; cocktail of one or more of the chemicals X, Y and Z. There are 4 outflow
;;; pipes (numbered 1 to 4). Building 1 is connected via outflow 1 straight
;;; to the stream. Buildings 2 and 3 drain through common outflow 2 into
;;; outflow 4, and buildings 4 and 5 drain through common outflow 3 also into
;;; outflow 4.

(defun run-spill ()
  (reset)
  (make-instance 'chemical :kb-name 'x :type 'dangerous)
  (make-instance 'chemical :kb-name 'y :type 'dangerous)
  (make-instance 'chemical :kb-name 'z :type 'harmless)
  (make-instance 'building :kb-name 'bldng-1
                 :waste (list 'x 'y 'z))
  (make-instance 'building :kb-name 'bldng-2
                 :waste (list 'y 'z))
  (make-instance 'building :kb-name 'bldng-3
                 :waste (list 'z))
  (make-instance 'building :kb-name 'bldng-4
                 :waste (list 'x 'z))
  (make-instance 'building :kb-name 'bldng-5
                 :waste (list 'x))
  (make-instance 'outflow :kb-name 'outflow-1
                 :from (names-to-objects 'bldng-1))
  (make-instance 'outflow :kb-name 'outflow-2
                 :from (names-to-objects 'bldng-2 'bldng-3))
  (make-instance 'outflow :kb-name 'outflow-3
                 :from (names-to-objects 'bldng-4 'bldng-5))
  (make-instance 'outflow :kb-name 'outflow-4
                 :from (names-to-objects 'outflow-2 'outflow-3))
  (let ((*bg-output* *standard-output*))
    (infer :contexts '(find-source prepare-recommendations recommendations))))

(defun names-to-objects (&rest args) (mapcar #'get-kb-object args))

(defun bg-output (string &rest args)
  (apply 'format *bg-output* string args))

;;; ---------------------------------------------------------------------------
;;; Find any possible candidate buildings which might be a source of the
;;; contamination. We can do this by backtracking the contamination upstream,
;;; or failing that by analysing the chemicals in the contamination and seeing
;;; which of the buildings upstream might be responsible.
;;; We ought really check that all the located sources really can account for
;;; the chemicals found, but we don't.

(defcontext find-source :strategy (priority))

;;; Prompt for the first noticed location of the contamination.

(defun prompt-for-location (title)
  (capi:prompt-with-list (findall '?loc '(or (outflow ?loc) (building ?loc)))
                         title
                         :print-function #'(lambda (loc)
                                             (prin1-to-string (kb-name loc)))))

;;; First thing is to find the location where the contamination is
;;; reported and infer that a sample from there is contaminated.

(defrule start-rule :forward
  :context find-source
  -->
  ((prompt-for-location "Contamination location?") ?loc)
  (test ?loc)              ;; check the user didn't click off the menu
  ((bg-output "~%Initial location of contamination ~S" (kb-name ?loc)))
  (assert (sample ? location ?loc contaminated t)))

;;; If we have a contaminated sample from a building then we can be certain
;;; that this building is a source of the contamination.

(defrule source-found :forward
  :context find-source
  (sample ?sample location ?loc contaminated t)
  (building ?loc)
  -->
  ((bg-output "~%Found source ~S" (kb-name ?loc)))
  (assert (source ? building ?loc certain t)))

;;; If we have a contaminated sample from some outflow, our first way to find
;;; the source is to take samples from further upstream hoping to trace it
;;; all the way back.

(defrule trace-upstream :forward
  :context find-source
  (sample ? location ?loc contaminated t)
  (outflow ?loc from ?upstream-locs)
  -->
  ((bg-output "~%Checking upstream of ~S" (kb-name ?loc)))
  (check-upstream-locs ?upstream-locs))

;;; Here we take samples from the given locations to check for contamination.

(defrule check-upstream-locs :backward
  ((check-upstream-locs ())
   <--)
  ((check-upstream-locs (?loc . ?locs))
   <--
   (take-sample ?loc)
   (check-upstream-locs ?locs)))

;;; Take a sample - just ask the user. In reality, this would do more!

(defrule take-sample :backward
  ((take-sample ?loc)
   <--
   ((capi:display-message "Take a sample from ~S" (kb-name ?loc)))
   ((capi:confirm-yes-or-no "Is this sample contaminated?") ?contaminated)
   (assert (sample ? location ?loc contaminated ?contaminated))))

;;; A check-list box to prompt for the chemicals in the sample.

(defun prompt-for-chemicals (title)
  (let* ((chems (findall '?chem '(chemical ?chem)))
         (items (mapcar 'kb-name chems)))
    (capi:prompt-with-list items title
                           :print-function 'symbol-name
                           :interaction :multiple-selection)))

;;; If we have a contaminated sample from some outflow, but none of the
;;; immediate upstream locations shows any contamination then we have to do
;;; a chemical analysis of the sample to determine which of the buildings
;;; upstream might be responsible for producing these chemicals.

(defrule need-analysis :forward
  :context find-source
  :priority 5
  (sample ?sample location ?outflow contaminated t)
  (outflow ?outflow from ?upstream-locs)
  (not (sample ? location ?loc contaminated t)
       (test (member ?loc ?upstream-locs)))
  -->
  ((bg-output "~%Sample ~S needs chemical analysis" (kb-name ?outflow)))
  ((prompt-for-chemicals
    (format nil "Chemicals in sample ~S?" (kb-name ?outflow)))
   ?sample-chemicals)
  (assert (chemical-analysis ? sample ?sample chemicals ?sample-chemicals)))

;;; Find all the buildings of which ?loc is downstream.

(defrule is-downstream :backward
  ((is-downstream ?loc ?loc)
   <--
   (building ?loc))
  ((is-downstream ?loc ?building)
   <--
   (outflow ?loc from ?locs)
   (member ?loc1 ?locs)
   (is-downstream ?loc1 ?building)))

;;; Do the required chemical analysis. All buildings upstream of the most
;;; upstream contaminated sample are possible candidates for the source.
;;; unless the building produces chemicals not found in the sample, in which
;;; case the building can be excluded. Buildings marked as possible sources
;;; in this way are not certain.

(defrule do-chemical-analysis :forward
  :context find-source
  (chemical-analysis ? sample ?sample chemicals ?chemicals)
  (sample ?sample location ?location)
  -->
  ((bg-output "~%Checking possible sources for sample ~S" (kb-name ?location)))
  (is-downstream ?location ?building)
  (building ?building waste ?waste)
  (test (every #'(lambda (chem) (member chem ?chemicals)) ?waste))
  (assert (source ? building ?building certain nil))
  ((bg-output "~%Chemical analysis does not exclude ~S" (kb-name ?building)))
  (fail))

;;; ---------------------------------------------------------------------------
;;; All the definite and possible sources of contamination have been
;;; identified. Now we decide what to do about them.

(defcontext prepare-recommendations :strategy (priority))

;;; If we have a contaminated sample but have not taken any samples downstream
;;; of it, we must do so as the most downstream contaminated place is where
;;; we must try and block the pollution.

(defrule check-downstream :forward
  :context prepare-recommendations
  :priority 25
  (sample ? location ?location contaminated t)
  (outflow ?outflow from ?upstream-locs)
  (test (member ?location ?upstream-locs))
  (not (sample ? location ?outflow))
  -->
  (take-sample ?outflow))

;;; If we have a contaminated sample and there is no further downstream point
;;; then we try to block the contamination here (shutting the stable door
;;; after the horse has bolted, really).

(defrule downstream-block :forward
  :context prepare-recommendations
  :priority 20
  (sample ?sample location ?location contaminated t)
  (not (sample ?sample location ?location)
       (outflow ?outflow from ?upstream-locs)
       (test (member ?location ?upstream-locs)))
  -->
  ((bg-output "~%Block contamination at ~S" (kb-name ?location)))
  (assert (containment ? location ?location type block urgency 10)))

;;; If we have a contaminated sample from some location and the sample from
;;; immediately downstream is not contaminated then we should try to block
;;; here.

(defrule containment-points :forward
  :context prepare-recommendations
  :priority 15
  (sample ? location ?location contaminated t)
  (outflow ?outflow from ?upstream-locs)
  (test (member ?location ?upstream-locs))
  (not (sample ? location ?outflow contaminated t))
  -->
  ((bg-output "~%Block contamination at ~S" (kb-name ?location)))
  (assert (containment ? location ?location type block urgency 10)))

;;; Is the chemical named chem-name dangerous?

(defun dangerous-p (chem-name)
  (eq (chem-type (get-kb-object chem-name)) 'dangerous))

;;; If we definitely know a building to be a source we send an engineer. If
;;; any of its wastes are dangerous we order its immediate shut-down.

(defrule check-building-certain :forward
  :context prepare-recommendations
  (building ?building waste ?waste)
  (source ? building ?building certain t)
  -->
  ((bg-output "~%Send engineer to ~S" (kb-name ?building)))
  (assert (containment ? location ?building type engineer urgency 5))
  (test (some 'dangerous-p ?waste))
  ((bg-output "~%Immediate shut down ~S" (kb-name ?building)))
  (assert (containment ? location ?building type shut-down urgency 8)))

;;; A building which we suspect of being a source need further investigation;
;;; this is more urgent if any of its wastes are dangerous.

(defrule check-building-uncertain :forward
  :context prepare-recommendations
  (source ? building ?building certain nil)
  (building ?building waste ?waste)
  -->
  ((bg-output "~%Investigate ~S" (kb-name ?building)))
  ((if (some 'dangerous-p ?waste) 6 4) ?urgency)
  (assert (containment ? location ?building type check urgency ?urgency)))

;;; ---------------------------------------------------------------------------
;;; Now we output the recommendations made. We could use a conflict resolution
;;; tactic based on the urgency of the required containment action; for now
;;; we'll just fudge this with priority.

(defcontext recommendations :strategy (priority))

;;; Notify requirement of outlet blockage.

(defrule report-blocks :forward
  :context recommendations
  :priority 15
  (containment ? location ?location type block)
  -->
  ((capi:display-message "Outlet blockage required at ~S" (kb-name ?location))))

;;; Notify immediate shut-down for some building.

(defrule report-shut-downs :forward
  :context recommendations
  :priority 10
  (containment ? location ?location type shut-down)
  -->
  ((capi:display-message "Immediate shut down for ~S" (kb-name ?location))))

;;; Notify need for checking other buildings. If there is indeed a problem
;;; we go to the prepare-recommendations context to make final recommendations
;;; for the building in question.

(defrule report-checks :forward
  :context recommendations
  :priority 5
  (containment ? location ?building type check)
  (source ?source building ?building)
  -->
  ((capi:display-message "Please check ~S" (kb-name ?building)))
  ((capi:confirm-yes-or-no "Is there a problem?") t)
  (assert (source ?source certain t))
  (context (prepare-recommendations recommendations))
  (return))

;;; Notify need for an engineer.

(defrule report-engineer :forward
  :context recommendations
  (containment ? location ?location type engineer)
  -->
  ((capi:display-message "Send engineer to ~S" (kb-name ?location))))

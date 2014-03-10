;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/truck:truck-rules.lisp,v 1.2.12.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; ==================== TRUCKS ====================

;;; type (make-instance 'start), then (infer) to run
;;; then type (infer :contexts '(execute)) for route-finding

(in-package kw-user)

;;; -------------------- CLASSES --------------------

(def-named-kb-class town ()
  ((x :initarg :x)
   (y :initarg :y)))

(def-kb-class link ()
  ((town1 :initarg :town1)
   (town2 :initarg :town2)
   (distance :initarg :distance)
   (road :initarg :road)))

(def-named-kb-class truck ()
  ((capacity :initarg :capacity)
   (model :initarg :model)
   (location :initarg :location)
   (allocated-driver :initarg :allocated-driver :initform nil)
   (allocated-load :initarg :allocated-load :initform nil)))

(def-named-kb-class driver ()
  ((qualified-for :initarg :qualified-for)
   (location :initarg :location)
   (allocated-truck :initarg :allocated-truck :initform nil)
   (allocated-load :initarg :allocated-load :initform nil)))

(def-named-kb-class load ()
  ((start-location :initarg :start-location)
   (destination :initarg :destination)
   (size :initarg :size)
   (allocated-truck :initarg :allocated-truck :initform nil)
   (allocated-driver :initarg :allocated-driver :initform nil)))

;;; -------------------- KB STRUCTURES --------------------

(def-kb-struct start)
(def-kb-struct redo)
(def-kb-struct possible-trucks-for-load load trucks)
(def-kb-struct best-route-for-load load route)

;;; -------------------- DEFAULT CONTEXT --------------------

;;; just kick the interpreter off with the correct contexts on the agenda

(defrule start-rule :forward
  (start ?s)
  -->
  (erase ?s)
  (context (allocate-trucks-for-loads       ; place contexts on agenda
	    allocate-drivers-for-trucks
	    connect-loads-and-drivers
	    summarise))
  (return))                                 ; go to the next context

(defrule redo-rule :forward
  (redo ?r)
  -->
  (erase ?r)
  (context (reschedule summarise))          ; place contexts on agenda
  (return))                                 ; go to the next context


;;; -------------------- ALLOCATE-TRUCKS-FOR-LOADS --------------------

;;; figures out which trucks can carry which loads (modulo any user-defined
;;; pre-allocations) and allocates a truck to each load

(defcontext allocate-trucks-for-loads
  :auto-return t
  :strategy (priority recency order))

;;; first make a list of all trucks big enough to carry each load

(defrule initiate-possible-trucks :forward
  :context allocate-trucks-for-loads
  :priority 8
  (load ?load allocated-truck nil)
  -->
  (assert (possible-trucks-for-load ?p load ?load trucks ())))

;;; if the load has been pre-allocated a driver, the truck must be big
;;; enough for the load, and the driver qualified for the truck

(defrule gather-unallocated-trucks1 :forward
  :context allocate-trucks-for-loads
  :priority 7
  (load ?load allocated-truck nil allocated-driver ?driver1
	start-location ?start size ?size)
  (truck ?truck allocated-load nil allocated-driver ?driver2
	 capacity ?capacity location ?start model ?model)
  (test (and ?driver1 (>= ?capacity ?size)
	     (or (not ?driver2) (eq ?driver1 ?driver2))))
  (driver ?driver1 qualified-for ?models)
  (test (member ?model ?models))
  (possible-trucks-for-load ?p load ?load trucks ?trucks)
  (test (not (member ?truck ?trucks)))
  -->
  (assert (possible-trucks-for-load ?p load ?load trucks (?truck . ?trucks))))

;;; if the load has not been pre-allocated a driver just check the truck
;;; is big enough for the load

(defrule gather-unallocated-trucks2 :forward
  :context allocate-trucks-for-loads
  :priority 6
  (load ?load allocated-truck nil allocated-driver nil
        start-location ?start size ?size)
  (truck ?truck allocated-load nil capacity ?capacity location ?start)
  (test (>= ?capacity ?size))
  (possible-trucks-for-load ?p load ?load trucks ?trucks)
  (test (not (member ?truck ?trucks)))
  -->
  (assert (possible-trucks-for-load ?p load ?load trucks (?truck . ?trucks))))

;;; an allocated truck is not big enough for the load

(defrule bad-truck-for-load :forward
  :context allocate-trucks-for-loads
  :priority 6
  (load ?load allocated-truck ?truck start-location ?start size ?size)
  (truck ?truck allocated-load ?load capacity ?capacity location ?start)
  (test (< ?capacity ?size))
  -->
  ((error "Pre-allocated truck ~A too small for load ~A." ?truck ?load)))

;;; allocate the load to the smallest truck that can carry it

(defrule allocate-truck-for-load :forward
  :context allocate-trucks-for-loads
  :priority 3
  (possible-trucks-for-load ?p load ?load trucks ?trucks)
  (load ?load allocated-truck nil)
  -->
  (erase ?p)
  ((best-truck ?trucks) ?best-truck)
  (test ?best-truck)
  (assert (truck ?best-truck allocated-load ?load))
  (assert (load ?load allocated-truck ?best-truck)))

(defun best-truck (trucks)
  "Smallest truck with unallocated load."
  (loop with best-truck = nil
        for truck in trucks do
        (unless (slot-value truck 'allocated-load)
          (when (or (not best-truck)
                    (< (slot-value truck 'capacity)
                       (slot-value best-truck 'capacity)))
	    (setq best-truck truck)))
        finally (return best-truck)))


;;; -------------------- ALLOCATE-DRIVERS-FOR-TRUCKS --------------------

;;; allocates a driver to each truck

(defcontext allocate-drivers-for-trucks
  :auto-return t
  :strategy (priority recency order))

;;; allocate a driver to a truck if qualified to drive it

(defrule connect-driver-and-truck :forward
  :context allocate-drivers-for-trucks
  :priority 15
  (truck ?truck allocated-driver nil allocated-load ?load model ?model)
  (driver ?driver allocated-truck nil allocated-load ?load
          qualified-for ?models)
  (test (member ?model ?models))
  -->
  (assert (truck ?truck allocated-driver ?driver))
  (assert (driver ?driver allocated-truck ?truck))) 


;;; allocates driver to truck as above but checks for preallocations

(defrule allocate-driver-for-truck :forward
  :context allocate-drivers-for-trucks
  :priority 10
  (truck ?truck allocated-driver nil
         allocated-load ?load1 model ?model location ?start)
  (driver ?driver allocated-truck nil
	  allocated-load ?load2 qualified-for ?models location ?start)
  (test (or (eq ?load1 ?load2) (not ?load1) (not ?load2)))
  (test (member ?model ?models))
  -->
  (assert (truck ?truck allocated-driver ?driver))
  (assert (driver ?driver allocated-truck ?truck)))

;;; a pre-allocated driver not qualified for the truck

(defrule bad-driver-for-truck :forward
  :context allocate-drivers-for-trucks
  :priority 9
  (truck ?truck allocated-driver ?driver model ?model location ?start)
  (driver ?driver allocated-truck ?truck qualified-for ?models location ?start)
  (test (not (member ?model ?models)))
  -->
  ((error "Pre-allocated driver ~A unqualified for truck ~A." ?driver ?truck)))

;;; -------------------- CONNECT-LOADS-AND-DRIVERS --------------------

;;; we've connected trucks to loads, and drivers to trucks... complete the
;;; triangle by connecting loads and drivers

(defcontext connect-loads-and-drivers
  :auto-return t
  :strategy ())

(defrule connect-load-and-driver :forward
  :context connect-loads-and-drivers
  :priority 10
  (load ?load allocated-driver nil allocated-truck ?truck)
  (driver ?driver allocated-load nil allocated-truck ?truck)
  -->
  (assert (load ?load allocated-driver ?driver))
  (assert (driver ?driver allocated-load ?load)))

;;; -------------------- SUMMARISE --------------------

;;; print out the result

(defcontext summarise
  :auto-return t
  :strategy (priority))

;;; loads that can be carried ok

(defrule summarise-allocated-loads :forward
  :context summarise
  :priority 10
  (load ?load allocated-truck ?t allocated-driver
	?d start-location ?start destination ?dest)
  (test (and ?t ?d))
  -->
  ((format t "~%Load ~A will be carried from ~A to ~A by truck ~
~A driven by driver ~A"  
           (kb-name ?load) (kb-name ?start) (kb-name ?dest) (kb-name ?t) (kb-name ?d))))

;;; loads which couldn't be allocated a driver

(defrule summarise-missing-drivers :forward
  :context summarise
  :priority 8
  (load ?load allocated-driver nil start-location ?start  destination ?dest)
  -->
  ((format t "~%Load ~A to go from ~A to ~A does not have a driver."
	   (kb-name ?load) (kb-name  ?start) (kb-name  ?dest))))

;;; loads which couldn't be allocated a truck

(defrule summarise-missing-trucks :forward
  :context summarise
  :priority 8
  (load ?load allocated-truck nil start-location ?start destination ?dest)
  -->
  ((format t "~%Load ~A to go from ~A to ~A does not have a truck."
	   (kb-name ?load) (kb-name  ?start) (kb-name  ?dest))))

(defmethod allocate ((load load) (truck truck) (driver driver))

  (with-slots (allocated-truck allocated-driver) load
	      (setq allocated-truck  truck
		    allocated-driver driver))

  (with-slots (allocated-driver allocated-load) truck
	      (setq allocated-driver driver
		    allocated-load   load))

  (with-slots (allocated-truck allocated-load) driver
	      (setq allocated-truck driver
		    allocated-load  load)))

;;; -------------------- RESCHEDULE --------------------
;;; We can try breaking/swapping some of the allocations made in order to
;;; try and join them up again better
;;;
;;; We now reschedule via two procedures:
;;;
;;;                  (1)  looking for orphan (completely unallocated) objects;
;;;                       stealing some allocatable objects;
;;;                       fixing the old pointers; and
;;;                       re-invoking the main routines to patch up the holes.
;;;
;;;                  (2)  looking for still incomplete allocation patterns; and
;;;                       smashing them up for the first procedure.

(defcontext reschedule
  :auto-return t
  :strategy (priority))

(defrule fix-load :forward
  :context reschedule
  :priority 10
  (load ?load allocated-driver nil allocated-truck nil
	start-location ?start size ?size)
  (truck ?truck allocated-load ?aload allocated-driver ?adriver
	 location ?start capacity ?capacity)
  (test (>= ?capacity ?size))
  -->
  (test (and (or (not ?aload)
		 (user-says-okay
		  "Fixing Load ~A: Re-allocate Load ~A and Truck ~A"
		  ?load ?aload ?truck))
	     (or (not ?adriver)
		 (user-says-okay
		  "Fixing Load ~A: Re-allocate Driver ~A and Truck ~A"
		  ?load ?adriver ?truck))))
  (assert (load ?load allocated-truck ?truck))
  (assert (truck ?truck allocated-load ?load))
  ((when ?aload (setf (slot-value ?aload 'allocated-truck) nil)))
  ((when ?adriver (setf (slot-value ?adriver 'allocated-truck) nil)))
  (context (allocate-trucks-for-loads allocate-drivers-for-trucks
				      connect-loads-and-drivers))
  (return))

(proclaim '(special kw-tools::*trucking-window*))

(defun user-says-okay (title &rest args)
  (setq title (apply #'format nil title args))
  (y-or-n-p "~A" title))

(defrule fix-driver :forward
  :context reschedule
  :priority 8
  (driver ?driver allocated-load nil allocated-truck nil
          location ?start qualified-for ?models)
  (truck ?truck allocated-load ?aload allocated-driver ?adriver
	 location ?start model ?model)
  (test (member ?model ?models))
  -->
  (test (and (or (not ?aload)
		 (user-says-okay
		  "Fixing Driver ~A: Re-allocate Truck ~A and Load ~A"
		  ?driver ?truck ?aload))
	     (or (not ?adriver)
		 (user-says-okay
		  "Fixing Driver ~A: Re-allocate Truck ~A and Driver ~A"
		  ?driver ?truck ?adriver))))
  (assert (driver ?driver allocated-truck ?truck))
  (assert (truck ?truck allocated-driver ?driver))
  ((when ?aload (setf (slot-value ?aload 'allocated-truck) nil)))
  ((when ?adriver (setf (slot-value ?adriver 'allocated-truck) nil)))
  (context (allocate-trucks-for-loads allocate-drivers-for-trucks
				      connect-loads-and-drivers))
  (return))

(defrule fix-truck :forward
  :context reschedule
  :priority 6
  (truck ?truck allocated-load nil allocated-driver nil
	 location ?start capacity ?capacity model ?model)
  (load ?load allocated-driver ?adriver allocated-truck ?atruck
	start-location ?start size ?size)
  (test (>= ?capacity ?size))
  (driver ?driver allocated-load ?aload allocated-truck ?btruck
	  qualified-for ?models)
  (test (member ?model ?models))
  -->
  (test (and (or (not ?aload)
		 (user-says-okay
		  "Fixing Truck ~A: Re-allocate Driver ~A and Load ~A"
		  ?truck ?driver ?aload))
	     (or (not ?btruck)
		 (user-says-okay
		  "Fixing Truck ~A: Re-allocate Driver ~A and Truck ~A"
		  ?truck ?driver ?btruck))
	     (or (not ?adriver)
		 (user-says-okay
		  "Fixing Truck ~A: Re-allocate Load ~A and Driver ~A"
		  ?truck ?load ?adriver))
	     (or (not ?atruck)
		 (user-says-okay
		  "Fixing Truck ~A: Re-allocate Load ~A and Truck ~A"
		  ?truck ?load ?atruck))))
  (assert (truck ?truck allocated-load ?load allocated-driver ?driver))
  (assert (load ?load allocated-truck ?truck allocated-driver ?driver))
  (assert (driver ?driver allocated-load ?load allocated-truck ?truck))
  ((when ?adriver (setf (slot-value ?adriver 'allocated-load) nil)))
  ((when ?atruck (setf (slot-value ?atruck 'allocated-load) nil)))
  ((when ?aload (setf (slot-value ?aload 'allocated-driver) nil)))
  ((when ?btruck (setf (slot-value ?btruck 'allocated-driver) nil)))
  (context (allocate-trucks-for-loads allocate-drivers-for-trucks
				      connect-loads-and-drivers))
  (return))

(defrule break-load-truck :forward
  :context reschedule
  :priority 4
  (load ?load allocated-driver nil allocated-truck ?truck)
  (test ?truck)
  -->
  (test (user-says-okay "Breaking: Re-allocate Load ~A and Truck ~A" ?load ?truck))
  (assert (load ?load allocated-truck nil))
  (assert (truck ?truck allocated-load nil)))

(defrule break-load-driver :forward
  :context reschedule
  :priority 2
  (load ?load allocated-driver ?driver allocated-truck nil)
  (test ?driver)
  -->
  (test (user-says-okay "Breaking: Re-allocate Load ~A and Driver ~A" ?load ?driver))
  (assert (load ?load allocated-driver nil))
  (assert (driver ?driver allocated-load nil)))

(defrule break-truck-driver :forward
  :context reschedule
  :priority 0
  (truck ?truck allocated-driver ?driver allocated-load nil)
  (test ?driver)
  -->
  (test (user-says-okay "Breaking: Re-allocate Driver ~A and Truck ~A" ?driver ?truck))
  (assert (driver ?driver allocated-truck nil))
    (assert (truck ?truck allocated-driver nil))) 

;;; -------------------- ROUTE FINDING --------------------

(defstruct route dist roads towns)

(defun choose-best-route (route-list)
  "return the shortest route"
  (let ((best-route (car route-list)))
    (dolist (route route-list best-route)
      (if (or (not (route-p best-route))
              (< (route-dist route) (route-dist best-route)))
          (setq best-route route)))))

(defcontext execute
  :auto-return t
  :strategy (priority))

;;; finds the shortest route

(defrule find-best-route :forward
  :context execute
  (load ?l start-location ?s destination ?d allocated-driver ?driver
	allocated-truck ?truck)
  (test (and ?truck ?driver))
  -->
  (findall ?r
	   (and (route ?s ?d ?dist ?roads ?towns)
		((make-route :dist ?dist :roads ?roads :towns ?towns) ?r))
	   ?all-routes)
  ((choose-best-route ?all-routes) ?route)
  (assert (best-route-for-load ?b load ?l route ?route)))

;;; moves the trucks to destination

(defrule move-load :forward
  :context execute
  (load ?l allocated-driver ?driver
	allocated-truck ?truck start-location ?start destination ?dest)
  (best-route-for-load ?b load ?l route ?route)
  -->
  (test (route-p ?route))
  ((format t "~%~s has been carried from ~s to ~s via ~s along roads ~s by driver ~s in truck ~s: total distance ~a miles"
	   (kb-name ?l) (kb-name ?start) (kb-name  ?dest)
           (mapcar 'kb-name (cdr (route-towns ?route)))
	   (route-roads ?route) (kb-name ?driver) (kb-name  ?truck)
	   (route-dist ?route))))


;;; the backward rules for route-finding...

(defvar *i-f* 0) ;;improvement factor

(defrule route :backward
  ( (route ?t1 ?t2 ?d ?r ?ints)
   <--
    (route1 ?t1 ?t2 (?t1) ?d ?r ?ints) ))

;;; a route between two towns exists if...

(defrule route1 :backward

  ;; ...there's a direct link between them         

  ( (route1 ?t1 ?t2 ?ints ?d (?r) ?ints1)
   <--
    (link-exists ?t1 ?t2 ?d ?r)
    ((reverse ?ints) ?ints1)
    (cut) )

  ;; ...or there's a direct link to an intermediate town, an a route
  ;; from this intermediate town to the destination

  ( (route1 ?t1 ?t2 ?ints ?new-dist (?r . ?roads) ?towns)
   <--
    (link-exists ?t1 ?int-town ?d ?r)
    (test (not (member ?int-town ?ints)))
    (test (>= (improvement-factor ?t1 ?t2 ?int-town) *i-f*))
    (route1 ?int-town ?t2 (?int-town . ?ints) ?total-distance ?roads ?towns)
    ((+ ?d ?total-distance) ?new-dist)))

;;; look for a direct link

(defrule link-exists :backward
  ( (link-exists ?t1 ?t2 ?d ?r)
   <--
    (link ? town1 ?t1 town2 ?t2 distance ?d road ?r) )
  ( (link-exists ?t1 ?t2 ?d ?r)
   <--
    (link ? town1 ?t2 town2 ?t1 distance ?d road ?r) ))

(defun get-displacement (town1 town2)
  (let ((x1 (slot-value town1 'x))
	(y1 (slot-value town1 'y))
	(x2 (slot-value town2 'x))
	(y2 (slot-value town2 'y)))
    (sqrt (+ (* (- x2 x1) (- x2 x1)) 
	     (* (- y2 y1) (- y2 y1))))))

;;; When trying to get from town1 to town2, the improvement-factor of
;;;town3 as an  intermediary is given by the amount by which it is
;;;closer to town2  than town1, as the crow flies, compared to the
;;;distance between  town3 and town1 
(defun improvement-factor (town1 town2 town3) 
  (let ((a (get-displacement town1 town2))
	(b (get-displacement town1 town3))
	(c (get-displacement town3 town2)))
    (if (<= c b)
	(when (> b 0) (/ (- a c) b))
      (when (> c 0) (/ (- a b) c)))))

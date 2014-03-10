;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/whist:rules.lisp,v 1.1.13.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "WHIST")

;;; ==================== ASSESS ====================

(defcontext assess
  :auto-return t
  :strategy (priority)
)

;; -------------------- ASSESS-SHORT-SUIT --------------------

(defrule assess-short-suit :forward
  :context assess
  :priority 19
    (suit ?s trumps nil)
    (suit ?t trumps t)
    (player ?p kind automatic lengths ?l)
    (test (<= (lookup ?s ?l) 2))
    (test (>= (lookup ?t ?l) 2))
  -->
    ((player-short-suit ?p) ?ss)
    (assert (player ?p short-suit (?s . ?ss)))
) 

;; -------------------- ASSESS-TRUMPS --------------------

(defrule assess-long-trumps :forward
  :context assess
  :priority 9
    (player ?p kind automatic lengths ?l)
    (suit ?t trumps t)
    (test (>= (lookup ?t ?l) 5))
  -->
    ((player-trumps ?p) ?k)
    (assert (player ?p trumps (:long . ?k)))
)

(defrule assess-strong-trumps :forward
  :context assess
  :priority 9
    (player ?p kind automatic cards ?c)
    (suit ?t trumps t)
    (test (>= (reduce #'+ (lookup ?t ?c) :key #'card-number) 44))
  -->
    ((player-trumps ?p) ?k)
    (assert (player ?p trumps (:strong . ?k)))
)

(defrule assess-short-trumps :forward
  :context assess
  :priority 9
    (player ?p kind automatic lengths ?l)
    (suit ?t trumps t)
    (test (<= (lookup ?t ?l) 2))
  -->
    ((player-trumps ?p) ?k)
    (assert (player ?p trumps (:short . ?k)))
)

(defrule assess-weak-trumps :forward
  :context assess
  :priority 9
    (player ?p kind automatic cards ?c)
    (suit ?t trumps t)
    (test (<= (reduce #'+ (lookup ?t ?c) :key #'card-number) 22))
  -->
    ((player-trumps ?p) ?k)
    (assert (player ?p trumps (:weak . ?k)))
) 


;;; ==================== LEAD ====================

(defcontext lead
  :auto-return t
  :strategy (priority)
) 
             
;; -------------------- SHORT-SUIT --------------------
;;
;; Play-out short suit to trump later.

(defrule short-suit :forward
  :context lead
  :priority 20
    (state ? player ?p)
    (player ?p short-suit ?s)
    (card ?c suit ?cs player ?p played nil)
  -->
    (member ?cs ?s)
    ((incf (card-score ?c) 10))
)

;; -------------------- TRUMPS --------------------
;;
;; If trumps are good then attack, if bad then avoid

(defrule trumps :forward
  :context lead
  :priority 10
    (state ? player ?p)
    (player ?p trumps ?k)
    (suit ?t trumps t)
    (card ?c suit ?t player ?p played nil)
  -->
    ((incf (card-score ?c)
           (reduce #'+ ?k
                   :key #'(lambda (x) (case x ((:long :strong) 10) ((:short :weak) -10))))))
)

;; -------------------- AVOID-BEING-TRUMPED --------------------

;; don't play something that's being trumped

(defrule avoid-being-trumpded :forward
  :context lead
  :priority 5
    (state ? player ?p)
    (player ?p cards ?cs)
    (suit ?s trumped t)
    (card ?c suit ?s player ?p played nil)
  -->
    ((decf (card-score ?c) 5))
) 

;; -------------------- EARLY-CAUTION --------------------
;;
;; Early caution for high cards

(defrule early-caution :forward
  :context lead
  :priority 2
    (state ? player ?p round ?r)
    (test (<= ?r 7))
    (card ?c number ?n player ?p played nil)
    (test (>= ?n 10))
  -->
    ((decf (card-score ?c) 5))
) 

;; -------------------- AVOID-HIGHER-CARD --------------------
;;
;; Avoid other higher unplayed cards for high cards

(defrule avoid-higher-card :forward
  :context lead
  :priority 1
    (state ? player ?p)
    (card ?c suit ?s number ?n player ?p played nil)
    (test (>= ?n 10))
    (card ?c2 suit ?s number ?n2 player ?p2 played nil)
    (test (not (eq ?p ?p2)))
    (test (> ?n2 ?n))
  -->
    ((decf (card-score ?c) 5))
)

;; -------------------- HIGHER CARD --------------------
;;
;; In general prefer higher cards

(defrule higher-card :forward
  :context lead
  :priority 0
    (state ? player ?p)
    (card ?c number ?n player ?p played nil)
  -->
    ((incf (card-score ?c) ?n))
)


;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/arches:arches.lisp,v 1.1.13.1 2011/08/24 13:25:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; -------------------- A SIMPLE LEARNING PROGRAM -----------------------

;;; to run: (and (findall ?e (example ?e) ?examples)
;;;              (learn ?examples ?arch-desc))

;;; Representation:
;;; Object = (object <list-of-parts> <list-of-relations>)
;;; Concept = (concept <list-of-parts> <must-relations> <relations> <must-not-relations>
;;; Positive example = (+ <object>)
;;; Negative example = (- <object>)
;;; Parts in an object denoted by variables, parts in a concept description are
;;; denoted by constants

(in-package kw-user)

;;; (learn <list-of-examples> <concept-description>) induces a concept
;;; description from a list of examples

(defrule learn :backward
  ((learn (?first-example . ?examples) ?concept-desc)
   <--
   (initialize ?first-example ?initial-hypothesis)
   (process-examples ?initial-hypothesis ?examples ?concept-desc)))

(defrule process-examples :backward
  ((process-examples ?concept-desc () ?concept-desc)
   <--)
  ((process-examples ?cur-desc (?example . ?examples) ?fin-desc)
   <--
   (object-type ?example ?object ?type)
   (match ?object ?cur-desc ?difference)
   (update ?type ?difference ?cur-desc ?new-desc)
   (process-examples ?new-desc ?examples ?fin-desc)))

;;; turn variables in the object into constants, assume 6 parts at most

(defrule initialize :backward
  ((initialize (+ (object ?parts ?rels)) (concept ?parts () ?rels ()))
   <--
   (namevars ?parts (part1 part2 part3 part4 part5 part6))))

(defrule update :backward

  ;; the forbidden relation rule: missing relations in a negative example
  ;; must be required in a concept description         

  ((update negative (+ ? (?extra-relation))
           (concept ?parts ?musts ?rels ?must-nots)
           (concept ?parts ?musts ?rels (?extra-relation . ?must-nots)))
   <--)

  ;; one missing and one extra relation in a negative example can also
  ;; be handled by both forbid extra and require missing

  ((update negative (+ ?missing ?)
           (concept ?parts ?musts ?rels ?must-nots)
           (concept ?parts ?new-musts ?new-rels ?must-nots))
   <--
   (= ?missing (? . ?))                          ; ?missing non-empty
   (conc ?missing ?musts ?new-musts)             ; add ?missing to ?musts
   (list-diff ?rels ?missing (+ ? ?new-rels)))   ; remove ?missing from ?rels

  ;; one missing and one extra relation in a genative example can also be
  ;; handled by both forbid extra and require missing
  ((update negative (+ (?miss-r) (?extra-r)) ?cur-desc ?new-desc)
   <--
   (update negative (+ () (?extra-r)) ?cur-desc ?inter-desc)  ; forbid
   (update negative (+ ?miss-r ()) ?inter-desc ?new-desc))    ; and require

  ;; the climb-taxonomy rule: generalise an isa relation by climbing the
  ;; a-kind=of taxonomy

  ((update positive (+ ((isa ?object ?class1)) ((isa ?object ?class2)))
           (concept ?parts ?musts ?rels ?must-nots)
           (concept ?parts ?musts ?new-rels ?must-nots))
   <--
   (climb ?class1 ?class)
   (climb ?class2 ?class)
   (cut)
   (replace (isa ?object ?class1) ?rels (isa ?object ?class) ?new-rels)))

;;; (match <object> <concept-description> <difference>)

(defrule match :backward
  ((match (object ?o-parts ?o-rels)
          (concept ?c-parts ?musts ?rels ?must-nots)
          (+ ?missing ?extras))
   <--
   (list-diff ?o-rels ?musts (+ () ?rest-rels))       ; match musts
   (short-lists (+ ?missing ?extras))                 ; generate short lists
   (list-diff ?o-parts ?c-parts (+ () ()))            ; match parts
   (list-diff ?rest-rels ?rels (+ ?missing ?extras))  ; match other relations
   (list-diff ?extras ?must-nots (+ ?must-nots ?))))  ; all ?must-nots missing

;;; (list-diff ?list1 ?list2 ?list2-less-list1 ?list1-less-list2)

(defrule list-diff :backward
  ((list-diff ?list1 () (+ () ?list1))
   <--)
  ((list-diff ?list1 (?x . ?list2) (+ ?miss ?extras))
   <--
   (delete ?list1 ?list11 ?x ?miss11 ?miss)
   (list-diff ?list11 ?list2 (+ ?miss11 ?extras))))

;;; (delete ?list ?list-possibly-less-x ?x ?del-list ?del-list-possibly-plus-x)
;;; if ?x is deleted from ?list then ?del-list-possibly-plus-x = ?del-list
;;; if not then ?list-possibly-less-x = ?list and ?del-list-possibly-plus-x
;;; = (?x . ?del-list)   (if ?x is not deleted then it is missing in ?list)

(defrule delete :backward
  ((delete () () ?x ?dels (?x . ?dels))
   <--)
  ((delete (?y . ?l) ?l ?x ?dels ?dels)
   <--
   (or (and (== ?x ?y) (cut))      ; literally equal: necessarily delete
       (= ?x ?y)))                 ; ?x ?y match: possible delete
  ((delete (?y . ?l) (?y . ?l1) ?x ?dels ?dels1)
   <--
   (delete ?l ?l1 ?x ?dels ?dels1)))

(defrule object-type :backward
  ((object-type (+ ?object) ?object positive)   ; positive example
   <--)
  ((object-type (- ?object) ?object negative)   ; negative example
   <--))

;;; remove ?item from ?list and add ?new-item producing ?new-list

(defrule replace :backward
  ((replace ?item ?list ?new-item (?new-item . ?list1))
   <--
   (delete ?list ?list1 ?item ? ?)))

;;; climb a-kind-of relation from class to super-class

(defrule climb :backward
  ((climb ?class ?class)
   <--)
  ((climb ?class ?super-class)
   <--
   (ako ?class1 ?class)
   (climb ?class1 ?super-class)))

;;; instantiates variables in a list of variables to names in ?name-list

(defrule namevars :backward
  ((namevars ?list ?name-list)
   <--
   (conc ?list ? ?name-list)))

(defrule conc :backward
  ((conc () ?l ?l)
   <--)
  ((conc (?x . ?l1) ?l2 (?x . ?l3))
   <--
   (conc ?l1 ?l2 ?l3)))

;;; generator of difference templates (+ ?list1 ?list2): short lists are
;;; generated first to force finding good match
;;; order of generation: (+ () ()), (+ () ?), (+ ? ()), (+ () (? . ?)) etc.

(defrule short-lists :backward
  ((short-lists (+ ?l1 ?l2))
   <--
   (conc ?l ? (? ? ?))
   (conc ?l1 ?l2 ?l)))

;;; the a-kind-of taxonomy

(defrule ako :backward
  ((ako figure polygon) <--)
  ((ako figure circle) <--)
  ((ako polygon convex-poly) <--)
  ((ako polygon concave-poly) <--)
  ((ako convex-poly stable-poly) <--)
  ((ako convex-poly unstable-poly) <--)
  ((ako stable-poly triangle) <--)
  ((ako stable-poly rectangle) <--)
  ((ako stable-poly trapezium) <--)
  ((ako unstable-poly unstable-triangle) <--)
  ((ako unstable-poly hexagon) <--))

;;; some examples for learning about an arch

(defrule example :backward
  ((example (+ (object (?a ?b ?c)
                       ((support ?a ?c) (support ?b ?c)
                        (isa ?a rectangle) (isa ?b rectangle) (isa ?c rectangle))))) <--)
  ((example (- (object (?a ?b ?c)
                       ((support ?a ?c) (support ?b ?c) (touch ?a ?b)
                        (isa ?a rectangle) (isa ?b rectangle) (isa ?c rectangle))))) <--)
  ((example (- (object (?a ?b ?c)
                       ((isa ?a rectangle) (isa ?b rectangle) (isa ?c rectangle))))) <--)
  ((example (+ (object (?a ?b ?c)
                       ((support ?a ?c) (support ?b ?c)
                        (isa ?a rectangle) (isa ?b rectangle) (isa ?c triangle))))) <--))


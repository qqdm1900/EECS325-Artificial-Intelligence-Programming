;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/hanoi:hanoi-rules.lisp,v 1.1.13.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; use: type e.g. (move-tower 3 1 2) into the KnowledgeWorks listener

(in-package kw-user)

(defrule move-tower :backward
  ((move-tower ?height ?from-peg ?to-peg)
   <--
   ((- 6 (+ ?from-peg ?to-peg)) ?spare-peg)
   (move-tower-internal ?height ?from-peg ?to-peg ?spare-peg)))

(defrule move-disc :backward
  ((move-disc ?size ?from-peg ?to-peg)
   <--
   ((format t "~%Move disc ~s from peg ~s to peg ~s"
            ?size ?from-peg ?to-peg))))

(defrule move-tower-internal :backward
  ((move-tower-internal 1 ?from-peg ?to-peg ?spare-peg)
   <--
   (move-disc 1 ?from-peg ?to-peg)
   (cut))
  ((move-tower-internal ?height ?from-peg ?to-peg ?spare-peg)
   <--
   ((1- ?height) ?height-1)
   (move-tower-internal ?height-1 ?from-peg ?spare-peg ?to-peg)
   (move-disc ?height ?from-peg ?to-peg)
   (move-tower-internal ?height-1 ?spare-peg ?to-peg ?from-peg)))

;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/animal:animal-objs.lisp,v 1.1.13.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(IN-PACKAGE KW-USER)
(MAKE-INSTANCE 'ROOT :NODE
               (MAKE-INSTANCE 'NODE :QUESTION "Does it have stripes?"
                              :ANIMAL (QUOTE NIL) :YES-NODE
                              (MAKE-INSTANCE 'NODE :QUESTION
                                             "Does it eat meat?"
                                             :ANIMAL (QUOTE NIL)
                                             :YES-NODE
                                             (MAKE-INSTANCE 'NODE
                                                            :QUESTION
                                                            NIL :ANIMAL
                                                            '"TIGER"
                                                            :YES-NODE
                                                            NIL
                                                            :NO-NODE
                                                            NIL)
                                             :NO-NODE
                                             (MAKE-INSTANCE 'NODE
                                                            :QUESTION
                                                            NIL :ANIMAL
                                                            '"ZEBRA"
                                                            :YES-NODE
                                                            NIL
                                                            :NO-NODE
                                                            NIL))
                              :NO-NODE
                              (MAKE-INSTANCE 'NODE :QUESTION
                                             "Does it lay eggs?"
                                             :ANIMAL (QUOTE NIL)
                                             :YES-NODE
                                             (MAKE-INSTANCE 'NODE
                                                            :QUESTION
                                                            NIL :ANIMAL
                                                            '"CHICKEN"
                                                            :YES-NODE
                                                            NIL
                                                            :NO-NODE
                                                            NIL)
                                             :NO-NODE
                                             (MAKE-INSTANCE 'NODE
                                                            :QUESTION
                                                            NIL :ANIMAL
                                                            '"SHEEP"
                                                            :YES-NODE
                                                            NIL
                                                            :NO-NODE
                                                            NIL))))

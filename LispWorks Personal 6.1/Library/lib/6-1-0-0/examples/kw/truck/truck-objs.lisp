;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/truck:truck-objs.lisp,v 1.1.13.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package kw-user)

(make-instance 'town :kb-name 'london :x 20 :y 0)
(make-instance 'town :kb-name 'baldock :x 0 :y 60)
(make-instance 'town :kb-name 'royston :x 20 :y 70)
(make-instance 'town :kb-name 'four-wentways :x 50 :y 70)
(make-instance 'town :kb-name 'cambridge :x 30 :y 90)
(make-instance 'town :kb-name 'newmarket :x 100 :y 90)

(make-instance 'link
               :town1 (get-kb-object 'london)
               :town2 (get-kb-object 'baldock)
               :distance 35 
	       :road 'A1)
(make-instance 'link
               :town1 (get-kb-object 'royston)
               :town2 (get-kb-object 'baldock)
               :distance 8 
	       :road 'A505)
(make-instance 'link
               :town1 (get-kb-object 'cambridge)
               :town2 (get-kb-object 'royston)
               :distance 12 
	       :road 'A10)
(make-instance 'link
               :town1 (get-kb-object 'london)
               :town2 (get-kb-object 'royston)
               :distance 45 
	       :road 'A10)
(make-instance 'link
               :town1 (get-kb-object 'royston)
               :town2 (get-kb-object 'four-wentways) 
	       :distance 15
               :road 'A505)
(make-instance 'link
               :town1 (get-kb-object 'london)
               :town2 (get-kb-object 'four-wentways) 
	       :distance 45
               :road 'A11)
(make-instance 'link
               :town1 (get-kb-object 'cambridge)
               :town2 (get-kb-object 'four-wentways) 
	       :distance 10
               :road 'A1303)
(make-instance 'link
               :town1 (get-kb-object 'cambridge)
               :town2 (get-kb-object 'newmarket)
	       :distance 20
               :road 'A45)
(make-instance 'link
               :town1 (get-kb-object 'newmarket)
               :town2 (get-kb-object 'four-wentways)
	       :distance 15
               :road 'A11)

(make-instance 'load
               :allocated-truck nil
               :allocated-driver nil
               :start-location (get-kb-object 'Cambridge)
               :destination (get-kb-object 'London)
	       :size 30
               :kb-name 'load1)
(make-instance 'load
               :allocated-truck nil
               :allocated-driver nil
               :start-location (get-kb-object 'Cambridge)
               :destination (get-kb-object 'baldock)
	       :size 40
               :kb-name 'load2)
(make-instance 'load
               :allocated-truck nil
               :allocated-driver nil
               :start-location (get-kb-object 'Cambridge)
               :destination (get-kb-object 'newmarket)
	       :size 35
               :kb-name 'load3)
(make-instance 'load
               :allocated-truck nil
               :allocated-driver nil
               :start-location (get-kb-object 'Cambridge)
               :destination (get-kb-object 'four-wentways)
	       :size 30
               :kb-name 'load4)
(make-instance 'load
               :allocated-truck nil
               :allocated-driver nil
               :start-location (get-kb-object 'Cambridge)
               :destination (get-kb-object 'royston)
	       :size 35
               :kb-name 'load5)
(make-instance 'load
               :allocated-truck nil
               :allocated-driver nil
               :start-location (get-kb-object 'Cambridge)
               :destination (get-kb-object 'London)
	       :size 30
               :kb-name 'load6)

(make-instance 'truck
               :allocated-driver nil
               :allocated-load nil
               :capacity 30
               :model 'Ford
	       :location (get-kb-object 'Cambridge)
               :kb-name 'ford1)
(make-instance 'truck
               :allocated-driver nil
               :allocated-load nil
               :capacity 35
               :model 'scania 
	       :location (get-kb-object 'Cambridge)
               :kb-name 'scania1)
(make-instance 'truck
               :allocated-driver nil
               :allocated-load nil
               :capacity 44
               :model 'leyland
	       :location (get-kb-object 'Cambridge)
               :kb-name 'leyland1)
(make-instance 'truck
               :allocated-driver nil
               :allocated-load nil
               :capacity 40
               :model 'volvo
	       :location (get-kb-object 'Cambridge)
               :kb-name 'volvo1)
(make-instance 'truck
               :allocated-driver nil
               :allocated-load nil
               :capacity 30
               :model 'daf
	       :location (get-kb-object 'Cambridge)
               :kb-name 'daf1)
(make-instance 'truck
               :allocated-driver nil
               :allocated-load nil
               :capacity 35
               :model 'Ford
	       :location (get-kb-object 'Cambridge)
               :kb-name 'ford2)

(make-instance 'driver
               :allocated-load nil
               :allocated-truck nil
               :qualified-for '(ford scania ) 
	       :location (get-kb-object 'cambridge)
	       :kb-name 'fred)
(make-instance 'driver
               :allocated-load nil
               :allocated-truck nil
               :qualified-for '(scania leyland)
	       :location (get-kb-object 'cambridge)
	       :kb-name 'bert)
(make-instance 'driver
               :allocated-load nil
               :allocated-truck nil
               :qualified-for '(ford leyland)
	       :location (get-kb-object 'cambridge)
	       :kb-name 'joe)
(make-instance 'driver
               :allocated-load nil
               :allocated-truck nil
               :qualified-for '( volvo daf)
	       :location (get-kb-object 'cambridge)
	       :kb-name 'harry)
(make-instance 'driver
               :allocated-load nil
               :allocated-truck nil
               :qualified-for '( volvo daf)
	       :location (get-kb-object 'cambridge)
	       :kb-name 'tom)
(make-instance 'driver
               :allocated-load nil
               :allocated-truck nil
               :qualified-for '(ford scania volvo)
	       :location (get-kb-object 'cambridge)
	       :kb-name 'frank)


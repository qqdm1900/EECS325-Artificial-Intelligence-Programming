;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/objc:manual.lisp,v 1.1.10.1 2011/08/24 13:26:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;; Various example definitions from the Objective-C and Cocoa User
;; Guide and Reference Manual.


(in-package "CL-USER")

(defpackage "OBJC-USER"
  (:use "OBJC")
  (:add-use-defaults t))

(in-package "OBJC-USER")


(define-objc-class my-object ()
  ((slot1 :initarg :slot1 :initform nil))
  (:objc-class-name "MyObject"))


#|
@implementation MyObject
- (unsigned int)areaOfWidth:(unsigned int)width
                height:(unsigned int)height
{
  return width*height;
}
@end
|#

(define-objc-method ("areaOfWidth:height:" (:unsigned :int))
    ((self my-object)
     (width (:unsigned :int))
     (height (:unsigned :int)))
  (* width height))


(define-objc-class my-special-object (my-object)
  ()
  (:objc-class-name "MySpecialObject"))

(define-objc-class my-other-object ()
  ()
  (:objc-class-name "MyOtherObject")
  (:objc-superclass-name "MyObject"))


#|
@implementation MySpecialObject
- (unsigned int)areaOfWidth:(unsigned int)width
                height:(unsigned int)height
{
  return 4*[super areaOfWidth:width height:height];
}
@end
|#

(define-objc-method ("areaOfWidth:height:" (:unsigned :int))
    ((self my-special-object)
     (width (:unsigned :int))
     (height (:unsigned :int)))
  (* 4 (invoke (current-super) "areaOfWidth:height:"
                               width height)))




(define-objc-struct (pair
                     (:foreign-name "_Pair"))
  (:first :float)
  (:second :float))

(define-objc-method ("pair" (:struct pair) result-pair)
    ((this my-object))
  (setf (fli:foreign-slot-value result-pair :first) 1f0
        (fli:foreign-slot-value result-pair :second) 2f0))



(define-objc-class my-size-mixin ()
  ())

(define-objc-method ("size" (:unsigned :int))
    ((self my-size-mixin))
  42)

(define-objc-class my-data (my-size-mixin)
  ()
  (:objc-class-name "MyData"))

(define-objc-class my-other-data (my-size-mixin)
  ()
  (:objc-class-name "MyOtherData"))


; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/misc:invoke-foreign-block.lisp,v 1.1.1.1 2011/09/12 17:20:32 martin Exp $" -*-


;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;; A simple example of invoking FOREIGN-BLOCKS. 


;;;     Works only on MacOS 10.6 or later

;;; An example of using fli:DEFINE-FOREIGN-BLOCK-INVOKER
;;; It definesan invoker that invokes blocks that take
;;; two :INT arguments and retrun an :INT. The invoker itself is 
;;; defined to take the arguments in keywords, and with default 
;;; values 3 and 5. 

;;; Test the integers invoker by 

;;;     (TEST-INVOKER)

;;; This create a block that take two arguments A and B , first prints
;;; what arguments it received, and then returns (+ (* 1000 A) B).
;;;  It then invokes this block using the invoker with different arguments. 

;;; In the output, first the testing function prints what arguments the invoker
;;; receives. Then the block itself prints what it received, and then the testing
;;; function prints the result. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;           Code                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   Defining an invoker ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Specific invoker for INTs. Eliminate the need for pointers.
;;; Notes:  
;;;;   *   We define passing args from LISP to foreign, so we use 
;;;       :REFERENCE-PASS (like fli:DEFINE-FOREIGN-FUNCTION would do)
;;;    *  We use :LAMBDA-LIST, so theinvoker takes the argument as
;;;       as keywords. That is done just to show that you can do it
;;;       It doesn't affect whatthe blocks, which is always defined 
;;;       by the ARGS lis (second argument of fli:define-foreign-block-invoker).


(fli:define-foreign-block-invoker invoke-foreign-block-int-two-INTS
                                  ((arg1 :int)
                                   (arg2 :int))
                                  :result-type :int
                                  :lambda-list (&key (arg1 3) (arg2 5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   using an invoker ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This gets a block and arguments and invokes
;;; the block on the arguments using invoke-foreign-block-int-two-INTS
;;; which was defined above. In this example the block was made inside
;;; lisp, but the same will work with any block that takes two :INT
;;; arguments and returns an :INT.
;;; Note that the arguments that are passed are keywords and value. The
;;; invoker interpret these correctly because it is defined to 
;;; keywords by the :LAMBDA-LIST keyword in the deinition above. 
;;; The block itself just get two arguments. 



(defun invoke-foreign-block-with-INTS (block  args)
  (apply 'invoke-foreign-block-int-two-INTS block args))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   Testing invocation of blocks   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; A wrapper for the invoker above, printing the arguments before
;;; and the result afterwards. 

(defun call-invoke-foreign-block-with-INTS (block &rest args)
  (format t "Invoking with arguments ~s~%"  args)
  (let ((res (invoke-foreign-block-with-INTS block args)))
    (format t "Result:  ~s~2%" res)))

;;; Deifnitions of the blocks. 

;;; We need these because we make the  blocks to invoke. In actual
;;; application the blocks may come from somewhere else (normally
;;; foreign code, but may be other LISP code), and in this
;;; case these definitions are not needed


;;; The functions for the blocks below. Print the
;;; actual arguments and returns a value based on both of them. 

(defun lisp-function-for-foreign-block-INTS (arg1 arg2)
  (format t "~6t>> Block received ~d ~d~%" arg1 arg2)
  (+ (* arg1 1000) arg2))


;;; Define FOREIGN-BLOCK-CALLABLE type.

;;; Blocks that are generated with this type will pass the two INTs to the function,
;;; and expect an :int as a return value. 

(fli:define-foreign-block-callable-type foreign-block-int-two-ints :int
                                         (:int :int))


;;; This creates a block using the function and callable type above, and
;;; then invoke it with various arguments. 

(defun test-invoker ()
  (fli:with-foreign-block
   (foreign-block-INTS  'foreign-block-int-two-ints
                         'lisp-function-for-foreign-block-ints)

    (call-invoke-foreign-block-with-INTS foreign-block-INTS
                                    :arg1 9)
    (call-invoke-foreign-block-with-INTS foreign-block-INTS)
    (call-invoke-foreign-block-with-INTS foreign-block-INTS
                                    :arg1 70 :arg2 80)))

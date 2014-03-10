; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/misc:foreign-blocks.lisp,v 1.3.1.3 2011/09/12 17:19:51 martin Exp $" -*-


;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;; Example of the basics of using FOREIGN-BLOCKS. 

;;;   works only on MacOS 10.6 or later

;;; The code here uses qsort_b to sort a vector, because that 
;;; is what Apple use in their examples of using blocks. It shows
;;; various ways of doing the same thing, which is just creating
;;; a block and calling qsort_b with it.

;;; Test it all by calling (test-all)

;;; This runs all the tests, and for each one prints the array
;;; with random numbers, and then the array aftar sorting it using
;;; q_sort. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;           Code                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;; The foreign function we are going to call. 
;;; The C signature is:
;;; qsort_b(void *base, 
;;;         size_t nel, 
;;;         size_t width, 
;;;         int (^compar)(const void *, const void *));
;;; The ^ in front of compar makes it a block. Note that the
;;; block receive pointers to the elements to compare, not
;;; the element themselves. 



(fli:define-foreign-function qsort_b ((arg :pointer)
                                      (count :size-t)
                                      (size :size-t)
                                      (code-block fli:foreign-block-pointer)))


;;; Define foreign-block-callable types.

;;; First a general type. Blocks generated with this will
;;; pass to the function two pointers, so the function itself
;;; will have to decide what to do with them. 

(fli:define-foreign-block-callable-type qsort_b-foreign-block-type-general :int
                                         (:pointer :pointer))

;;; A type for sorting INTs. Blocks that are generated with this type
;;; will pass the two INTs to the function. That actually saves creating
;;; the pointers, so it is much more efficient. 
;;; Note that we are passing the arguments from foreign code to LISP, so
;;; we use :REFERENCE-RETURN (as fli:DEINE-FOREIGN-CALLABLE would do).

(fli:define-foreign-block-callable-type qsort_b-foreign-block-type-ints :int
                                         ((:reference-return :int)
                                          (:reference-return :int)))

;;; The actual comparison. 

(defun compare-ints (x y)
  (cond ((> x y) 1)
        ((= x y) 0)
        (t -1)))

;;; helper function to reduce the noise in the callers. 

(defun call-qsort_b-for-ints  (foreign-vector len foreign-block)
  (qsort_b foreign-vector len (fli:size-of :int) foreign-block))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various ways of generating the block for calling q_sort ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The first two version are the proper way of doing. The other 3
;;; are just to show other functions. 

;;; 1. Do it with the general type, which means we get pointers
;;;    and need to dereference them before doing the comparison.

(defun qsort_b-proper-way-general (foreign-vector len)
  (fli:with-foreign-block 
      (foreign-block 'qsort_b-foreign-block-type-general 
                     #'(lambda (poi1 poi2)
                         (compare-ints (fli:dereference poi1 :type :int)
                                       (fli:dereference poi2 :type :int))))
    (call-qsort_b-for-ints foreign-vector len foreign-block)))

;;; 2. Do it with the INTs specific type, which already dereference
;;;    the arguments. That is much more efficient. 

(defun qsort_b-proper-way-specific (foreign-vector len)
  (fli:with-foreign-block 
      (foreign-block 'qsort_b-foreign-block-type-ints 'compare-ints)
    (call-qsort_b-for-ints foreign-vector len foreign-block)))

;;; 3. Create and free the foreign-block by explicit calls
;;;   to fli:ALLOCATE-FOREIGN-BLOCK and fli:FREE-FOREIGN-BLOCK

(defun qsort_b-explicit-creation (foreign-vector len)
  (let ((foreign-block (fli:allocate-foreign-block 'qsort_b-foreign-block-type-ints 
                                                   'compare-ints)))
    (prog1 (call-qsort_b-for-ints foreign-vector len foreign-block)
      (fli:free-foreign-block foreign-block))))

;;; 4. Use fli:WITH-LOCAL-FOREIGN-BLOCK. Note: Really we don't have a guarantee
;;;    that qsort_b is not going to invoke the block on another thread, so
;;;    this is not actually correct. 

(defun qsort_b-on-stack (foreign-vector len)
  (fli:with-local-foreign-block 
      (foreign-block 'qsort_b-foreign-block-type-ints 'compare-ints)
    (call-qsort_b-for-ints foreign-vector len foreign-block)))

;;; 5. Demonstrate that we can create a copy of a block, and use it after
;;;    the original block was freed. Here it is just dumb code, but copying
;;;    and using it later is quite commonly done in foreign code, e.g. 
;;;    dispatch_async. 

;;; Create a foreign-block using fli:FOREIGN-BLOCK-COPY for use
;;; in qsort_b-with-a-copy below.  Doesn't do anything more useful 
;;; than fli:ALLOCATE-FOREIGN-BLOCK, just shows using fli:FOREIGN-BLOCK-COPY.
;;; Note that fli:WITH-FOREIGN-BLOCK frees the one that it makes, but
;;; the copy continue to be a valid block. 

(defun create-foreign-block-by-copy (type comparator)
  (fli:with-foreign-block 
      (foreign-block type comparator)
    (fli:foreign-block-copy foreign-block)))


(defun qsort_b-with-a-copy (foreign-vector len)
  (let ((foreign-block (create-foreign-block-by-copy 'qsort_b-foreign-block-type-ints 
                                                     'compare-ints)))
    (prog1 (call-qsort_b-for-ints foreign-vector len foreign-block)
      ;; Since the block is a result of fli:foreign-block-copy
      ;; we need to free it using fli:foreign-block-release
      (fli:foreign-block-release foreign-block))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   Testing calls to q-sort    ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Generate an array with random elements so we can sort it. 

(defun generate-vector-of-ints ()
  (let ((len (+ 15 (random 10))))
    (let ((array (make-array len)))
      (dotimes (x len)
        (setf (aref array x)
              (- (* (random 30) (random 10))
                 (random 100))))
      array)))

;;; a.  Create a random vector of ints.
;;; b.  Print it.
;;; c.  Create a foreign vector of the same length.
;;; d.  Transfer the contents to the foreign vector
;;; e.  Sort the foreign vector using the argument function
;;; f.  Transfer the contents back to the vector
;;; g.  Print it again. 

(defun test-the-function (function)
  (let ((vector (generate-vector-of-ints)))  ; [a]
    (format t "~&Using ~a~%  Before: ~a~%" function vector)  ; [b]

    (let ((len (length vector)))   ; [c]
      (fli:with-dynamic-foreign-objects()
        (let ((array (fli:allocate-dynamic-foreign-object  
                      :type :int :nelems len)))
          
          (dotimes (x len) ; [d]
            (setf (fli:dereference array :index x :type :int) 
                  (svref vector x)))
                    
          (funcall function array len) ; [e]

          (dotimes (x len)             ;[f]
            (setf (svref vector x)
                  (fli:dereference array :index x :type :int))))))

    (format t "   After: ~a~%" vector) ; [f]
    ))


(defun test-all ()
  (test-the-function 'qsort_b-with-a-copy)
  (test-the-function 'qsort_b-on-stack)
  (test-the-function 'qsort_b-explicit-creation)
  (test-the-function 'qsort_b-proper-way-specific)
  (test-the-function 'qsort_b-proper-way-general))


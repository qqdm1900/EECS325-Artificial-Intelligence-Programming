; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/misc:grand-central-dispatch.lisp,v 1.3.1.2 2011/11/04 20:03:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;; A simple example showing how to use the Grand Central Dispatch (GCD)
;;; functionality in Mac OS X. 
;;; Demonstrates parallel execution using dispatch. 
;;; Uses the C functions:
;;;     dispatch_release
;;;     dispatch_get_global_queue
;;;     dispatch_async
;;;     dispatch_group_create
;;;     dispatch_group_async
;;;     dispatch_group_wait

;;; Works only on Mac OS X 10.6 or later

;;; To actually run the example execute either of these forms:
;;;
;;;   (CL-USER::RUN-EXAMPLE-GROUP-GCD-SLEEP)
;;;
;;;   (CL-USER::RUN-EXAMPLE-GROUP-GCD-WASTE-CPU)


;;; Each of these calls dispatches a "work-function" on each of a list
;;; of 50 random numbers between 0 and 5, and then waits for all of
;;; them to finish. While it is waiting, each second it prints the
;;; state of execution: How many dispatched functions started, how
;;; many finished, how many running (the difference between start and
;;; finish), and the number of LISP processs. The last number contains
;;; all LISP processes, including those that are automatically made to
;;; run the dispatched functions.

;;; In the RUN-EXAMPLE-GROUP-GCD-SLEEP call the "work-function" is
;;; SLEEP, so it doesn't use CPU. In this case the GCD should start
;;; more processes, because it sees that the CPUs are not busy. In the
;;; RUN-EXAMPLE-GROUP-GCD-WASTE-CPU the "work-function" (WASTE-CPU)
;;; uses CPU, so the GCD should start only small number of processes,
;;; similar to the number of CPUs that the machine has.

;;; The test functions takes as optional argument the priority to use
;;; (default :DEFAULT).  You can try to call them with other priority
;;; (:HIGH or :LOW), but without other users of the queues it
;;; shouldn't make any difference.

;;; Another interesting test is to compare the behavior when the
;;; machine is busy and when it isn't.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; code ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The argument type of the dispatch_* functions that take
;;; a block. 

(fli:define-c-typedef dispatch-block-t fli:foreign-block-pointer)

;;; The callable type for blocks by dispatch_* functions. 
(fli:define-foreign-block-callable-type dispatch-block-callable
                                        :void ())

;;; A dummy C structure. Pointers to this structure can be passed to
;;; DISPATCH-RELEASE and DISPATCH-RETAIN. We define all the dispatch
;;; object types as pointers to it. 

(fli:define-c-struct (dispatch-object-dummy-structure (:forward-reference-p t)))

(fli:define-foreign-function dispatch-retain ((dop (:pointer dispatch-object-dummy-structure))))
(fli:define-foreign-function dispatch-release ((dop (:pointer dispatch-object-dummy-structure))))

;;; The timeout type
(fli:define-c-typedef dispatch-time-t :uint64)
(defconstant DISPATCH_TIME_NOW (fli:cast-integer  0 :uint64))
(defconstant DISPATCH_TIME_FOREVER (fli:cast-integer (lognot 0) :uint64))

  
;;; The dispatch object types that we are going to use. 
(fli:define-c-typedef dispatch-queue-t (:pointer dispatch-object-dummy-structure)) 
(fli:define-c-typedef dispatch-group-t (:pointer dispatch-object-dummy-structure)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting a global queue ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant DISPATCH_QUEUE_PRIORITY_HIGH 2)
(defconstant DISPATCH_QUEUE_PRIORITY_DEFAULT 0)
(defconstant DISPATCH_QUEUE_PRIORITY_LOW -2)

(fli:define-foreign-function dispatch-get-global-queue
    ((priority :long) ;DISPATCH_QUEUE_PRIORITY_*
     (flags (:unsigned :long))) ;;; reserved, currently 0
  :result-type dispatch-queue-t)

(defun call-dispatch-get-global-queue (&optional priority)
  (let ((dp (case priority
             (:high DISPATCH_QUEUE_PRIORITY_HIGH)
             ((:default nil) DISPATCH_QUEUE_PRIORITY_DEFAULT)
             (:low DISPATCH_QUEUE_PRIORITY_LOW)
             (t (error "CALL-DISPATCH-GET-GLOBAL-QUEUE: invaid priority : ~s" priority)))))
    (dispatch-get-global-queue dp 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dispatch functions that we are going to use ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(fli:define-foreign-function dispatch-async
    ((queue dispatch-queue-t)
     (block dispatch-block-t)))


(fli:define-foreign-function dispatch-group-create
    ()
  :result-type dispatch-group-t)

(fli:define-foreign-function dispatch-group-async 
    ((group dispatch-group-t)
     (queue dispatch-queue-t)
     (block dispatch-block-t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Waiting for a group to finish ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The documentation of the timeout in dispatch-group-wait
;;; is not clear. By stepping through it we found that it uses
;;; mach absolute time (on Mac OS X).
;;; Since we want the timeout to be relative in seconds
;;; like all other waiting functions, we need to do
;;; some computations. 

(fli:define-c-struct mach-timebase-info-data-t
  (numer (:unsigned :int))
  (denom (:unsigned :int)))

(fli:define-foreign-function mach-timebase-info ((p :pointer)))

;;; This is fixed on any given machine, but can vary in principle if
;;; the image is saved and restarted on another machine. If you use
;;; this code in an application, make sure it is NIL on restart.

(defparameter *seconds-to-mach-absolute-time-ratio* nil)

;;; The info is how to convert absolute to nanos, we
;;; go from seconds to absolute so invert the denum/numer
;;; and multiply by billion. 
(defun seconds-to-mach-absolute-time-ratio()
  (or *seconds-to-mach-absolute-time-ratio*
      (setq *seconds-to-mach-absolute-time-ratio* 
            (* (expt 10 9)
               (fli:with-dynamic-foreign-objects
                   ((s mach-timebase-info-data-t))
                 (mach-timebase-info s)
                 (/ (fli:foreign-slot-value s 'denom)
                    (fli:foreign-slot-value s 'numer)))))))
  

(fli:define-foreign-function mach-absolute-time ()
  :result-type :uint64)

(fli:define-foreign-function dispatch-group-wait 
    ((group dispatch-group-t)
     (time dispatch-time-t)) 
  :result-type :long ; 0 on success, non-zero timeout 
  )

;;; This is the "proper" interface.
(defun call-dispatch-group-wait (dg timeout)
  (let ((ti (case timeout
              (0 DISPATCH_TIME_NOW)
              ((nil) DISPATCH_TIME_FOREVER)
              (t (+ (truncate (* timeout (seconds-to-mach-absolute-time-ratio)))
                    (mach-absolute-time))))))
    (zerop (dispatch-group-wait dg ti))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  LISP side interface ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Actually doing the dispatch of FUNC and ARGS, using the queue
;;; specified by the priority, which must be one of
;;; :HIGH,:DEFAULT,:LOW or NIL. If the group is non-nil, it must be a
;;; dispatch-group-t object, and we dispatch with it.

(defun apply-with-gcd-and-group (priority group func &rest args)
  (unless (mp:get-current-process)
    (error "Trying to use GCD without multiprocessing"))
  (let ((queue (call-dispatch-get-global-queue priority))
        (block (apply 'fli:allocate-foreign-block 'dispatch-block-callable func args)))
    (prog1
        (if group
            (dispatch-group-async group queue block)
          (dispatch-async queue block))
      (fli:free-foreign-block block))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      "Users"       ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Just print the process in which the lambda executes.
;;; Not used in the test.

(defun print-lisp-process-in-gcd ()
  (let ((output-stream *standard-output*))
    (apply-with-gcd-and-group
     :high
     nil
     #'(lambda (stream)
         (format stream "~% --- Process is ~a~%" (mp:get-current-process)))
     output-stream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;     Simple group test ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; diagnostics-arg, used to monitor the execution
;;; of the dispatched functions. 
;;; Here it is very simple, but in a real application it may be more
;;; complex, and may be also used to communicate "instruction" to the
;;; workers.

(defstruct diagnostics-args 
  (start-count 0)
  (finished-count 0))

;;; These can happen concurrently, so need to be atomic
(defun diagnostics-args-flag-started (flag)
  (sys:atomic-fixnum-incf (diagnostics-args-start-count flag)))

(defun diagnostics-args-flag-finished (flag)
  (sys:atomic-fixnum-incf (diagnostics-args-finished-count flag)))


;;;; Like SLEEP, this does nothing useful, but it wastes CPU.

(defun waste-cpu (time)
  (let ((end-time (+ (get-internal-real-time) (* time internal-time-units-per-second))))
      (loop (when (> (get-internal-real-time) end-time) (return 7)))))

;;; That is the function that actually goes into the GCD. 
;;; Uses diagnostics-args to communicate when it starts
;;; and finishes, and in the middle just invokes the work-function. 

(defun call-worker-function-with-diagnostics (diagnostics-args work-function arg)
  (diagnostics-args-flag-started diagnostics-args)
  (funcall work-function arg)
  (diagnostics-args-flag-finished diagnostics-args))


;;; This dispatches the work-function on each element
;;; in the ARGS list, and then waits for all of them
;;; to finish using dispatch-group-wait. While it is
;;; waiting it prints the state of the execution each
;;; second.

(defun run-example-GDC-group (priority work-function args)
  (let ((count (length args))
        (diagnostics-args (make-diagnostics-args))
        (group (dispatch-group-create)))
    (unwind-protect 
        (progn
          ;; dispatch the work function on each arg
          (dolist (arg args)
            (apply-with-gcd-and-group priority group 
                                      'call-worker-function-with-diagnostics 
                                      diagnostics-args work-function arg))
          ; wait 
          (loop until (call-dispatch-group-wait group 1)
                do (let ((started (diagnostics-args-start-count diagnostics-args))
                         (finished (diagnostics-args-finished-count diagnostics-args)))
                     (format t "  Start ~2d finished ~2d (running ~d) LISP process count ~d~%"
                             started finished (- started finished)
                             (mp:processes-count))))
          ; check
          (unless (= (diagnostics-args-start-count diagnostics-args) 
                     (diagnostics-args-finished-count diagnostics-args)
                     count)
            (warn "Something wrong: start ~d finished ~d count ~d~%"
                  (diagnostics-args-start-count diagnostics-args) 
                  (diagnostics-args-finished-count diagnostics-args)
                  count)))
      (dispatch-release group))))

(defvar *fifty-random-five-list*
  (loop for x below 50 collect (random 5)))

;;; Call the test with a list of fifty random numbers up to 5. 
;;; Also TIME it.
(defun run-example-group-GCD-with-fifty-random-5 (priority work-function)
  (time (run-example-GDC-group priority work-function *fifty-random-five-list*)))

;;; The actual run functions. 

(defun run-example-group-gcd-waste-cpu (&optional (priority :default))
  (run-example-group-GCD-with-fifty-random-5 priority 'waste-cpu))
(defun run-example-group-gcd-sleep (&optional (priority :default))
  (run-example-group-GCD-with-fifty-random-5 priority 'sleep))

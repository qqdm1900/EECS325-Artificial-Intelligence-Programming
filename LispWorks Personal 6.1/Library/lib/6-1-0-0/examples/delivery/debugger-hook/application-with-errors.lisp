;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/delivery:debugger-hook:application-with-errors.lisp,v 1.3.1.1 2011/08/24 13:26:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.


;;; An example of catching errors in a delivered application. 

;;; Use deliver.lisp to create a delivered application (see
;;; deliver.lisp for details).  Once delivered, run the application
;;; and click the left or right mouse buttons on the blue circle to
;;; see what it does when it gets an error.


;;; The "application" below just produces calls to ERROR.  These
;;; errors are caught and handled because the application sets
;;; *debugger-hook* to MY-DEBUGGER-HOOK (See the documentation for
;;; *DEBUGGER-HOOK*).

;;; Notes:

;;; After writing the log, MY-DEBUGGER-HOOK informs the user that an
;;; error can occurred using a simple call to CAPI:DISPLAY-MESSAGE.
;;; In a real application, you probably need something more
;;; sophisticated.

;;; MY-DEBUGGER-HOOK uses DBG:LOG-BUG-FORM because it is simple to
;;; use.  However, the output is Lisp debugging information, so you
;;; need to be a Lisp programmer to figure out what it means.  In your
;;; real application, you may want to write something that gives the
;;; user an indication of what they should do.

;;; DBG:LOG-BUG-FORM appends the output to the log, so it will grow
;;; forever.  You need to either clear it periodically or pass it a
;;; different pathname each time.

;;; The debugger hook is called after an error has occurred, so you
;;; cannot be sure of the state of the program.  You should try to
;;; avoid doing too much inside the hook, because that may produce
;;; recursive errors.  You could try to guard against recursive errors
;;; by rebinding *DEBUGGER-HOOK* inside MY-DEBUGGER-HOOK to a function
;;; that simply quits.


(in-package "CL-USER")

;;; A simple interface with a single pane that shows a blue circle,
;;; which gives errors when you click on it.

(capi:define-interface application-with-errors ()
  ()
  (:panes 
   (pane-1
    capi:output-pane
    :visible-min-width 100
    :visible-min-height 100
    :display-callback #'(lambda(pane x y width height)
                          (declare (ignore x y width height))
                          (gp:draw-circle pane 50 50 30 :foreground :blue :filled t))
    :input-model '(((:button-1 :press) error-here)
                   ((:button-3 :press) error-on-another-process))))
  (:layouts
   (main
    capi:row-layout
    '(pane-1))))

;;; This is called on a left button click because it is in the
;;; :INPUT-MODEL above (see the documentation for capi:output-pane).
;;; It just gives an error and returns nil (to avoid tail call).

(defun error-here (&rest x)
  (error "Inside input-model callback: ~a" x)
  nil)

;;; This is called on a right button click. 
;;; It runs another process which gives an error. 
 
(defun error-on-another-process (&rest x)
  (mp:process-run-function "Erroring process" () 
                           #'(lambda (x)
                               (error "On another process: ~a" x) 
                               nil)
                           x))


;;; This is the main function, passed to DELIVER (in deliver.lisp).

(defun main-function ()
  (setq *debugger-hook* 'my-debugger-hook)
  (capi:display (make-instance 'application-with-errors)))



;;; The debugger hook, which is set into *DEBUGGER-HOOK* in the
;;; main function above.  See the comment at the top of the file.

(defun my-debugger-hook (condition old-debugger-hook)
  (declare (ignore old-debugger-hook))
  (let ((path 
         (dbg:log-bug-form (format nil "an error occured : ~a" condition)
                           :message-stream nil)))
    (capi:display-message "Error log written to ~a :~% ~a"
                          path condition)
    (abort)))


;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/objc:movie-view.lisp,v 1.1.10.1 2011/08/24 13:26:19 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/objc/movie-view.lisp
;;
;; This example demonstrates the use of CAPI:COCOA-VIEW-PANE, containing an
;; NSMovieView and allowing a movie file to be opened and played.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-MOVIE-VIEW)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


(capi:define-interface movie-view-test ()
  ()
  (:panes
   (movie-pane
    capi:cocoa-view-pane
    :view-class "NSMovieView")
   (info-pane
    capi:title-pane)
   (controls
    capi:push-button-panel
    :items '("Open..." "Play")
    :callbacks '(movie-view-test-open
                 movie-view-test-play)
    :callback-type :interface))
  (:layouts
   (main-layout
    capi:column-layout
    '(movie-pane info-pane controls)))
  (:default-initargs
   :layout 'main-layout
   :title "Movie View Test"
   :best-width 700
   :best-height 550))


;;----------------------------------------------------------------------------
;; Callbacks
;;----------------------------------------------------------------------------

(defun movie-view-test-open (self)
  (with-slots (movie-pane info-pane) self
    (when-let (path (capi:prompt-for-file "Movie file"
                                          :filters '("Movie files" "*.mov")))
      (setf (capi:title-pane-text info-pane)
            (format nil "Loaded movie ~A" path))
      (let ((url (objc:invoke "NSURL"
                              "fileURLWithPath:"
                              (namestring path))))
        (objc:invoke (capi:cocoa-view-pane-view movie-pane)
                     "setMovie:"
                     (objc:invoke (objc:invoke "NSMovie" "alloc")
                                  "initWithURL:byReference:"
                                  url
                                  nil))))))

(defun movie-view-test-play (self) 
  (let* ((movie-pane (slot-value self 'movie-pane))
         (view (capi:cocoa-view-pane-view movie-pane)))
    (objc:invoke view "gotoBeginning:" view)
    (objc:invoke view "start:" view)))

;;----------------------------------------------------------------------------
;; The test function
;;----------------------------------------------------------------------------

(defun test-movie-view ()
  (capi:display (make-instance 'movie-view-test)))


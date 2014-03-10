;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/layouts:automatic-resize.lisp,v 1.6.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; A simple example of using objects with :AUTOMATIC-RESIZE inside
;;;  a  STATIC-LAYOUT or a subclass, PINBOARD-LAYOUT in this example. 

;;; The example creates a pinboard-layout with ellipse objects in opposite
;;; corners, placed such that they exactly touch each other. When
;;; the pane is resized, the ellipses change their shape accordingly. 

;;; To test, compile the buffer and then execute:

;;;   (CREATE-PANE-WITH-ELLIPSES :GREEN :BLUE NIL)
;;;   (CREATE-PANE-WITH-ELLIPSES :RED :YELLOW T)

;;; With third argument NIL, the ellipses are Top-Left and Bottom-Right,
;;; with T they are Top-Right and Bottom-Left. 

;;; During the call, it prints the argument for :AUTOMATIC-RESIZE
;;; to the standard output, so you can see what is passed in. 

;;; See documentation fo :AUTOMATIC-RESIZE in entries for CAPI:SIMPLE-PANE
;;; and CAPI:PINBOARD-OBJECT, and entry for CAPI:SET-OBJECT-AUTOMATIC-RESIZE.




;;;--------------------------------------------------------------

(in-package "CL-USER")

;;; We don't actually need to compute it each time, but in real code 
;;; you may need to. 

(defun compute-diagonal-ellipses-automatic-resize-specs (flipped)
  (let* ((diameter-to-dimension-ratio (/ 1 (+ 1 (sqrt 0.5))))
         (other-side-position-ratio (- 1 diameter-to-dimension-ratio))
         (size-spec `(:width-ratio ,diameter-to-dimension-ratio 
                      :height-ratio ,diameter-to-dimension-ratio))
         (top-spec (list* :x-align :left :y-align :top  size-spec))
         (bottom-spec (append top-spec `(:y-ratio ,other-side-position-ratio)))
         (right-spec-added `(:x-ratio ,other-side-position-ratio)))

    (if flipped
        (setq top-spec (append top-spec right-spec-added))
      (setq bottom-spec (append bottom-spec right-spec-added)))
    
    (values top-spec bottom-spec)))

;;; Making the ellipses 
(defun create-diagonal-ellipses (top-color bottom-color flipped)
  (multiple-value-bind (top-spec bottom-spec)
      (compute-diagonal-ellipses-automatic-resize-specs flipped)
    (format t "~%Top-spec:~%~8t~s~%Bottom-spec:~%~8t~s~%" top-spec bottom-spec)

    (let ((top-ellipse (make-instance 'capi:ellipse 
                                      :graphics-args `(:foreground ,top-color)
                                      :filled t 
                                      :automatic-resize top-spec))
          (bottom-ellipse (make-instance 'capi:ellipse :graphics-args `(:foreground ,bottom-color)
                                         :filled t
                                         :automatic-resize bottom-spec)))
      (list top-ellipse bottom-ellipse))))

(defun internal-create-pane-with-ellipses (top-color bottom-color flipped drawing-mode)
  (let ((pl (capi:contain (make-instance 'capi:pinboard-layout
                                         :draw-with-buffer t ; On windows helps prevent flicker. 
                                         :drawing-mode drawing-mode
                                         )
                          :title (if flipped "Top-right to bottom-left" "Top-left to bottom-right")))
        (ellipses (create-diagonal-ellipses top-color bottom-color flipped)))
    (capi:apply-in-pane-process
     pl #'(lambda ()
            (capi:manipulate-pinboard pl ellipses :add-many)))))

;;; We can also put the ellipses in the description of the pinboad-layout,
;;; but this way also shows using capi:MANIPULATE-PINBOARD
;;; DRAWING-MODE can also be :COMPATIBLE

(defun create-pane-with-ellipses (top-color bottom-color flipped &key (drawing-mode :quality))
  (internal-create-pane-with-ellipses top-color bottom-color flipped drawing-mode))




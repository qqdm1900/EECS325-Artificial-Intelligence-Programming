;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:paths.lisp,v 1.3.1.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/paths.lisp
;;
;; This example shows various uses of path drawing in Graphics Ports.
;; To see the example, compile and load this file and then execute:
;;
;; (CL-USER::PATHS-EXAMPLE)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

(defparameter *paths-example-items* nil)

(capi:define-interface paths-example ()
  ()
  (:panes
   (chooser capi:option-pane
            :items *paths-example-items*
            :selection-callback 'select-paths-example
            :callback-type :interface
            :print-function 'car
            :title "Choose example:")
   (drawing capi:output-pane
            :display-callback 'draw-paths-example
            :initial-constraints '(:min-width 850 :min-height 600)))
  (:layouts
   (main-layout capi:column-layout
                '(chooser drawing)))
  (:default-initargs
   :layout 'main-layout
   :title "Paths Example"))

(defun select-paths-example (self)
  (gp:invalidate-rectangle (slot-value self 'drawing)))

(defun draw-paths-example (pane x y width height)
  (declare (ignore x y width height))
  (let ((chooser (slot-value (capi:element-interface pane) 'chooser)))
    (funcall (second (capi:choice-selected-item chooser)) pane)))

(defmacro define-paths-example (name (pane) &body body)
  `(dspec:def (define-paths-example ,name)
     (install-define-paths-example ',name)
     (defun ,name (,pane) ,@body)))

(defun install-define-paths-example (name)
  (unless (find name *paths-example-items* :key 'second)
    (let* ((title-components (split-sequence "-" (string name)))
           (title (format nil "~:(~A~)~{ ~(~A~)~}"
                          (car title-components)
                          (cdr title-components))))
      (setq *paths-example-items*
            (append *paths-example-items* (list (list title name)))))))

(define-paths-example simple-shape (pane)
  (gp:draw-path pane `((:line 50 0)
                       (:line 50 50)
                       (:arc 0 -50 100 100 ,(/ pi -2) ,(/ pi -2)))
                20 20))

(define-paths-example simple-shape-repeated-and-scaled (pane)
  (gp:draw-path pane `((:line 50 0)
                       (:line 50 50)
                       (:arc 0 -50 100 100 ,(/ pi -2) ,(/ pi -2))
                       (:scale 2 2
                        ((:line 50 0)
                         (:line 50 50)
                         (:arc 0 -50 100 100 ,(/ pi -2) ,(/ pi -2)))))
                20 20))

(define-paths-example simple-shape-repeated-with-transforms (pane)
  (gp:draw-path pane #'(lambda (fn)
                         (funcall fn :line 50 0)
                         (funcall fn :line 50 50)
                         (funcall fn :arc 0 -50 100 100 (/ pi -2) (/ pi -2))
                         (funcall fn :scale 2 2
                                  `((:line 50 0)
                                    (:line 50 50)
                                    (:arc 0 -50 100 100 ,(/ pi -2) ,(/ pi -2))))
                         (funcall fn :scale 3 3
                                  #'(lambda (fn)
                                      (funcall fn :line 50 0)
                                      (funcall fn :line 50 50)
                                      (funcall fn :arc 0 -50 100 100 (/ pi -2) (/ pi -2))))
                         (funcall fn :rotate .2
                                  #'(lambda (fn)
                                      (funcall fn :line 50 0)
                                      (funcall fn :line 50 50)
                                      (funcall fn :arc 0 -50 100 100 (/ pi -2) (/ pi -2))))
                         (funcall fn :translate 80 80
                                  #'(lambda (fn)
                                      (funcall fn :move 0 0)
                                      (funcall fn :line 50 0)
                                      (funcall fn :line 50 50)
                                      (funcall fn :arc 0 -50 100 100 (/ pi -2) (/ pi -2))))
                         (funcall fn :transform (gp:make-transform 2 .8 .8 2 0 80)
                                  #'(lambda (fn)
                                      (funcall fn :move 0 0)
                                      (funcall fn :line 50 0)
                                      (funcall fn :line 50 50)
                                      (funcall fn :arc 0 -50 100 100 (/ pi -2) (/ pi -2)))))
                20
                20))

(define-paths-example simple-shape-repeated-in-a-circle (pane)
  (gp:draw-path pane
                #'(lambda (fn)
                    (dotimes (sector 6)
                      (funcall fn :rotate (* 2 pi (/ sector 6))
                               #'(lambda (fn)
                                   (funcall fn :line 50 0)
                                   (funcall fn :line 50 50)
                                   (funcall fn :arc 0 -50 100 100 (/ pi -2) (/ pi -2))))))
                80
                80))

(define-paths-example many-figures-closed-and-filled (pane)
  (dotimes (row 4)
    ;; Row 0: not closed at all.
    ;; Row 1: closed before the rectangle.
    ;; Row 2: closed at the end (after the bezier figure).
    ;; Row 3: filled,
    (dotimes (col 6)
      ;; Cols 0,3: the arc is 1/4 circle
      ;; Cols 1,4: the arc is 1/8 circle
      ;; Cols 2,5: the arc starts with a diagonal line.
      ;; Cols 3,4,5: the arc is part of a disconnected figure
      (let ((movep (>= col 3))
            (full-arc-p (eql (mod col 3) 0))
            (skip-arc-start-p (eql (mod col 3) 2)))
        (gp:draw-path pane `((:line 100 0)
                             ,@(if movep `((:move 80 16)))
                             (:line 100 100)
                             (:arc 0 -100 200 200
                              ,(+ (/ pi -2) (if skip-arc-start-p (/ pi -8) 0))
                              ,(if full-arc-p (/ pi -2) (/ pi -4)))
                             ,@(and (eql row 1) `((:close)))
                             (:rectangle 6 6 16 6)
                             (:line 60 60)
                             (:arc -20 -120 240 240 ,(/ pi -2) ,(/ pi -4) t)
                             (:ellipse 16 80 8 6)
                             (:move 60 10)
                             (:line 90 10)
                             (:bezier 90 40 90 60 40 30)
                             (:bezier 20 20 30 40 14 26)
                             )
                      (+ 20 (* col 140))
                      (+ 20 (* row 140))
                      :closed (eql row 2)
                      :filled (eql row 3))))))

;; None of the scaling is applied to the path.
(define-paths-example simple-mask-without-transform (pane)
  (gp:draw-rectangle pane 0 0 1000 1000 :filled t :foreground (capi:simple-pane-background pane))
  (gp:with-graphics-scale (pane 2 2)
    (gp:draw-rectangle pane 0 0 60 60 :filled t :foreground :black)
    (gp:with-graphics-mask (pane `(:path ((:line 50 0)
                                          (:line 50 50)
                                          (:arc 0 -50 100 100 ,(/ pi -2) ,(/ pi -2))))
                                 :mask-transform nil)
      (gp:with-graphics-scale (pane 2 2)
        (gp:draw-rectangle pane 10 10 30 200 :filled t :foreground :red)))))

;; Capture the transform before the second scaling is applied.
(define-paths-example simple-mask-with-captured-transform (pane)
  (gp:draw-rectangle pane 0 0 1000 1000 :filled t :foreground (capi:simple-pane-background pane))
  (gp:with-graphics-scale (pane 2 2)
    (gp:draw-rectangle pane 0 0 60 60 :filled t :foreground :black)
    (gp:with-graphics-mask (pane `(:path ((:line 50 0)
                                          (:line 50 50)
                                          (:arc 0 -50 100 100 ,(/ pi -2) ,(/ pi -2))))
                                 :mask-transform t)
      (gp:with-graphics-scale (pane 2 2)
        (gp:draw-rectangle pane 10 10 30 200 :filled t :foreground :red)))))

;; This looks the same as simple-mask-with-current-transform-prescaled
(define-paths-example simple-mask-with-current-transform-postscaled (pane)
  (gp:draw-rectangle pane 0 0 1000 1000 :filled t :foreground (capi:simple-pane-background pane))
  (gp:with-graphics-scale (pane 2 2)
    (gp:draw-rectangle pane 0 0 60 60 :filled t :foreground :black)
    (gp:with-graphics-mask (pane `(:path ((:line 50 0)
                                          (:line 50 50)
                                          (:arc 0 -50 100 100 ,(/ pi -2) ,(/ pi -2))))
                                 :mask-transform :dynamic)
      (gp:with-graphics-scale (pane 2 2)
        (gp:draw-rectangle pane 10 10 30 200 :filled t :foreground :red)))))

;; This looks the same as simple-mask-with-current-transform-postscaled
(define-paths-example simple-mask-with-current-transform-prescaled (pane)
  (gp:draw-rectangle pane 0 0 1000 1000 :filled t :foreground (capi:simple-pane-background pane))
  (gp:with-graphics-scale (pane 2 2)
    (gp:draw-rectangle pane 0 0 60 60 :filled t :foreground :black)
    (gp:with-graphics-scale (pane 2 2)
      (gp:with-graphics-mask (pane `(:path ((:line 50 0)
                                            (:line 50 50)
                                            (:arc 0 -50 100 100 ,(/ pi -2) ,(/ pi -2))))
                                   :mask-transform :dynamic)
        (gp:draw-rectangle pane 10 10 30 200 :filled t :foreground :red)))))

(define-paths-example mask-repeated-in-a-circle (pane)
  (gp:draw-rectangle pane 0 0 1000 1000 :filled t :foreground (capi:simple-pane-background pane))
  (gp:with-graphics-mask
      (pane `(:path ,#'(lambda (fn)
                         (dotimes (x 6)
                           (funcall fn :rotate (* 2 pi (/ x 6))
                                    #'(lambda (fn)
                                        (funcall fn :line 50 0)
                                        (funcall fn :line 50 50)
                                        (funcall fn :arc 0 -50 100 100 (/ pi -2) (/ pi -2)))))))
            :mask-transform (gp:make-transform 1 0 0 1 80 80))
    (gp:draw-rectangle pane 0 0 200 200 :filled t :foreground :red)))

(defun paths-example ()
  (capi:display (make-instance 'paths-example)))

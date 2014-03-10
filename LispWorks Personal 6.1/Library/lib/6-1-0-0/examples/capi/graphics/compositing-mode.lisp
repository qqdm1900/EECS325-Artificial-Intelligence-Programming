;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:compositing-mode.lisp,v 1.5.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/compositing-mode.lisp
;;
;; This example shows the effects of using :COMPOSITING-MODE 
;; in graphics operations. 
;; To see the example, compile and load this file and then execute:
;;
;; (CL-USER::COMPOSITING-MODE-EXAMPLE)

;; It really works only on GTK+ (with Cairo, which requires GTK 2.8)
;; and Cocoa.  On Microsoft Windows, all the modes behave the same as
;; the default (:OVER) mode, except the :COPY mode, so the example is
;; quite limited.

;; The example creates an image, with background that is a gradient from completely
;; transparent on the left to almost opaque blue on the right. On top
;; of the background there are four solid color ellipses. It then uses this
;; image repeatedly to make example drawings via a pixmap. Each example drawing
;; is made by filling the background of the pixmap (by gp:draw-rectangle), drawing the 
;; image with some :COMPOSITING-MODE into the pixmap, and then copying the 
;; pixmap to the pane. 

;; In each "line", the :COMPOSITING-MODE that is used is shown on the left. In the left
;; column, the background in the pixmap is the solid color
;; (*compositing-mode-example-solid-color*), and in the right column
;; it is a color with alpha (*compositing-mode-example-semi-opaque-color*). 


;; For explanation of what the different modes do, see:
;; Cairo (GTK+) (they call it operators)
;;    http://cairographics.org/operators/
;; Cocoa (they call it BlendMode)
;;    http://developer.apple.com/library/mac/#documentation/GraphicsImaging/Reference/CGContext/Reference/reference.html

;; In this example we use :COMPOSITING-MODE mainly for images,
;; but it can be used for other graphics operations.  You can see a very simple
;; (current-pathname "compositing-mode-simple.lisp")
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

;; The colors for the background 

(defconstant *compositing-mode-example-solid-color* (color:make-rgb 0.8 0.4 0.4 1))
(defconstant *compositing-mode-example-semi-opaque-color* (color:make-rgb 0.8 0.4 0.4 0.3))


(defun compositing-mode-example-create-image (pane)
  (gp:with-pixmap-graphics-port
      (pixmap  pane 180 40 )
    ;; Create a gradient of transparency and blueness. 
    (dotimes (x 180)
      (let ((val (float (/ x 255))))
        (gp:draw-rectangle pixmap x 0 1 40
                           :foreground (color:make-rgb 0 0 val val)
                           :filled t
                           :compositing-mode :copy)))
    ;; Draw some ellipses
    (loop for color in '(:red :yellow :blue :green)
          for x from 27 by 42
          do (gp:draw-ellipse pixmap x 20 15 8 :foreground color :filled t))
    
    
    (gp:make-image-from-port pixmap)))

;;; Ensure that we have an image to use, and draw the top
;;; bit of the pane. 

(defun compositing-mode-example-ensure-image-and-draw-titles (pane)
  (let ((image (or (capi:capi-object-property pane 'the-image)
                   (setf (capi:capi-object-property pane 'the-image)
                         (compositing-mode-example-create-image pane)))))
    (gp:draw-image pane image 10 10) )
    (gp:draw-string pane "Original image" 200 (+ 10 22))
    
    (gp:draw-string pane "with alpha background" 510 82)
    (gp:draw-string pane "Solid background" 160 82))

;;; This function does the interesting bit. 
;;; It first fills the pixmap with the given background, using :compositing-mode :copy
;;; which ignores anything that was already there. Then it draws the image
;;; using the given compositing-mode, and copies the pixmap to the screen. 

(defun compositing-mode-example-draw-image-with-compositing-mode (pane pixmap mode background x y)
  (gp:draw-rectangle pixmap 0 0 200 50 :filled t 
                     :foreground background
                     :compositing-mode :copy)
  (gp:draw-image pixmap (capi:capi-object-property pane 'the-image)
                 10 5 :compositing-mode mode)
  (gp:copy-area pane pixmap x y 200 50 0 0))


(defun compositing-mode-example-draw-lines (pane size)
  (loop for num from 0 by 40 below size
        do 
        (gp:draw-line pane 0 num size (- size num) :foreground :green)
        (gp:draw-line pane 0 (+ num 20) size (- size num 20) :foreground :yellow)
        (gp:draw-line pane  num 0 (- size num) size :foreground :yellow)
        (gp:draw-line pane (+  num 20) 0 (- size num 20) size :foreground :green)))

;;; This does the main drawing. 
;;; Create a pixmap (we could cache on the pane), and then loop for each
;;; mode showing one "line". Each "line" contains the name of the left,
;;; and then two example of drawing the image with this mode into the pixmap.
;;; One of the examples with solid background, the other with a background that
;;; has alpha too. 

(defun compositing-mode-example-draw-modes (pane)
  (gp:with-pixmap-graphics-port
      (pixmap pane 200 50)
    (let ((y 100))
      (dolist (mode '(:clear
                      :copy
                      :over
                      :in
                      :out
                      :atop
                      :dest-over
                      :dest-in
                      :dest-out
                      :dest-atop
                      :xor
                      :add))
        ;; Show the name on the left
        (gp:draw-string pane (prin1-to-string mode) 40 (+ y 30))
        
        ;; Show drawing on solid-color
        (compositing-mode-example-draw-image-with-compositing-mode 
         pane pixmap mode 
         *compositing-mode-example-solid-color*
         150 y)
        
        ;; Show drawing on color with alpha. 
        (compositing-mode-example-draw-image-with-compositing-mode 
         pane pixmap mode 
         *compositing-mode-example-semi-opaque-color* 
         500 y)

        (incf y 60)))))

(defun compositing-mode-example-draw-callback (pane &rest x)
  (declare (ignore x))
  ;; Draw a background of lines to make it easy to see where 
  ;; the drawing is transparent
  (compositing-mode-example-draw-lines pane 850)

  ;; Ensure we have an image and draw the top bit of the pane. 
  (compositing-mode-example-ensure-image-and-draw-titles pane)

  ;; Do the main drawing. 
  (compositing-mode-example-draw-modes pane))



(defun compositing-mode-example ()
  (capi:contain (make-instance 'capi:output-pane
                               :draw-with-buffer t
                               :display-callback 
                               'compositing-mode-example-draw-callback
                               :background :lightcyan
                               :initial-constraints
                               '(:min-width 850 :min-height 850))
                :title "Compositing-mode example"
                ))




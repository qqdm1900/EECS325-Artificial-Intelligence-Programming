;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:compositing-mode-simple.lisp,v 1.2.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/compositing-mode-simple.lisp
;;
;; This is a very simple of using :COMPOSITING-MODE :COPY in drawing
;; to create transparent and semi-transparent areas. To see it compile
;; this file and execute:
;;
;;   (CL-USER::COMPOSITING-MODE-SIMPLE-EXAMPLE)
;;
;; This creates a pixmap filled with pink. It then draws four ellipses
;; in blue and alpha 0.5 (left) or 0 (right) on the pixmap. The top
;; ellipses are drawn normally, so when alpha is is 0.5 it blends with
;; the background to give a solid purple, and when alpha is zero it
;; does nothing.  The bottom ellipses are drawn with :COMPOSITING-MODE
;; :COPY, so with alpha 0.5 it creates a semi-transparent blue, and
;; with alpha 0 it "drills a hole" through it.
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")


;;; This just draws lines, used to make it easy to see where the
;;; pixmap is transparent. 

(defun compositing-mode-simple-example-draw-lines (pane size)
  (loop for num from 0 by 40 below size
        do 
        (gp:draw-line pane 0 num size (- size num) :foreground :green)
        (gp:draw-line pane 0 (+ num 20) size (- size num 20) :foreground :yellow)
        (gp:draw-line pane  num 0 (- size num) size :foreground :yellow)
        (gp:draw-line pane (+  num 20) 0 (- size num 20) size :foreground :green)))

;;; This is the function that does the interesting bit, by drawing ellipses
;;; with alpha 0.5 (semi-transparent-blue) or 0 (transparent), in normal 
;;; compositing-mode or :COPY.

(defun compositing-mode-simple-example-draw-ellipses (pixmap)
  (let ((semi-transparent-blue (color:make-rgb 0 0 0.5 0.5))
        (transparent (color:make-rgb 0 0 0.5 0)))
    ;; Top-left: default compositing-mode and alpha semi-transparent-blue, mixes the new color (blue) 
    ;; with the existing color (pink) to give solid purple.
    (gp:draw-ellipse pixmap 80 100 60 40 :filled t :foreground semi-transparent-blue)

    ;; Top-right: default compositing-mode and transparent, shows nothing.
    (gp:draw-ellipse pixmap 220 100 60 40 :filled t :foreground transparent)
    
    ;; Bottom-left: compositing-mode :COPY and semi-transparent-blue, ignores what
    ;; was there and creates semi-transparent-blue. 
    (gp:draw-ellipse pixmap 80 220 60 40 :filled t :foreground semi-transparent-blue
                     :compositing-mode :copy :drawing-mode :plain)
    
    ; Bottom-right: Draw with compositing-mode :COPY and transparent, "drills a hole" through it.
    (gp:draw-ellipse pixmap 220 220 60 40 :filled t :foreground transparent
                     :compositing-mode :copy :drawing-mode :plain)
    ))

;; The display-callback 
(defun compositing-mode-simple-example-draw-callback (op &rest x)
  (declare (ignore x))
  
  (let ((pixmap (gp:create-pixmap-port op 300 300 :background :pink :clear t)))
   
    (compositing-mode-simple-example-draw-ellipses pixmap)

    (compositing-mode-simple-example-draw-lines op 420) ; put lines in background to see where it is transparent

    ;; show the pixmap. 
    (gp:copy-pixels op pixmap 120 60 300 300 0 0)

    ;; add labels
    (gp:draw-string op "Normal (:Over)" 0 160)
    (gp:draw-string op ":Copy" 0 290)
    (gp:draw-string op "Alpha = 0.5" 140 40)
    (gp:draw-string op "Alpha = 0" 300 40)
    ))

(defun compositing-mode-simple-example ()
  (capi:contain  (make-instance 'capi:output-pane
                                :draw-with-buffer t
                                :display-callback 
                                'compositing-mode-simple-example-draw-callback
                                :background :lightcyan
                                :initial-constraints
                                '(:visible-min-width 420 :visible-min-height 420))
                 :title "Compositing-mode simple example"))

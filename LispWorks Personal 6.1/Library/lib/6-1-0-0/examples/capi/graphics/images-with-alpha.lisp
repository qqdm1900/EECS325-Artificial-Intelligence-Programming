;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:images-with-alpha.lisp,v 1.5.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/images-with-alpha.lisp
;;
;; This is a simple example of working with images with alpha.
;; To see it compile and load this file and execute:
;;
;;   (CL-USER::IMAGES-WITH-ALPHA-EXAMPLE)
;; 
;; On GTK you need at least 2.8
;;
;; The code starts with an image with a solid gray background.
;;
;; It makes a copy of it with alpha using GP:LOAD-IMAGE
;; with :EDITABLE :WITH-ALPHA. That adds alpha and sets it to one
;; in the entire area of the image. It then uses GP:IMAGE-ACCESS
;; operations to set the alpha to 0 in the "background" (every pixel
;; that has the same color as the top-left pixel). 

;; It then creates a pixmap, and draws the image with alpha twice into it,
;; once normal and once upside down, and then makes the combined image
;; from the pixmap. 
;;
;; It then writes the combined image to a PNG file, and loads it again. 
;; It prints the file pathname to the background output, so you can
;; copy it from there and try to look at the image by another software (e.g.
;; a web browser). 

;; To show that the images are really transparent, you can change the color
;; of the pane by clicking on it. 
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

;;; An image with a solid background. 

(defvar *image-pathname* (current-pathname "../choice/option-pane-images.bmp"))

;;; Create a "combined image", by using a pixmap-port. The port
;;; is created with solid blue background, and then it "drills" a transparent
;;; rectangle in it using :compositing-mode :copy with zero-alpha
;;; foreground, which leaves a blue frame. It then draws the image twice,
;;; once normal and once mirrored vertically. It returns an image
;;; created from the port. 

(defun images-with-alpha-example-create-combined-image (pane image)
  (let* ((image-width (gp:image-width image))
         (image-height (gp:image-height image))
         (pixmap-width (+ image-width 10))
         (pixmap-height (* image-height 3)))
    
    (gp:with-pixmap-graphics-port (pixmap pane pixmap-width pixmap-height
                                          :background :blue :clear t)
    
      ;; Drills a transparent rectangle convering the pixmap except a frame
      ;; of 3 pixels. 
      (gp:draw-rectangle pixmap 3 3 (- pixmap-width 6) (- pixmap-height 6)
                         :filled t :foreground (color:make-rgb 0 0 0 0)
                         :compositing-mode :copy
                         :shape-mode :plain)
      ;; Normal drawing
      (gp:draw-image pixmap image 5 5)

      ;; Drawing with transforms that mirrors the the image vertically
      ;; and moves it down 
      (gp:with-graphics-translation (pixmap 0 pixmap-height)
        (gp:with-graphics-transform (pixmap '(1 0 0 -1 0 0))
          (gp:draw-image pixmap image 5 5)))
      
      (gp:make-image-from-port pixmap))))

;;; This returns a list of "image-specs", where each "image-spec" 
;;; is a list of of the form (<image> . <desciption-strings>)

(defun images-with-alpha-example-create-images (pane)
  (let* ((original-image (gp:load-image pane *image-pathname*))
         (image-with-alpha (gp:load-image pane original-image :editable :with-alpha)))

    ;;; GTK before 2.8 cannot run this example. 
    (unless (gp:port-drawing-mode-quality-p pane)
      (return-from images-with-alpha-example-create-images
        `((,ORIGINAL-IMAGE ,(case (capi:default-library)
                              (:gtk "!! Need GTK 2.8 to run this example !!"))))))

    
    (images-with-alpha-example-add-alpha  pane image-with-alpha)

    (let ((COMBINED-IMAGE
           (images-with-alpha-example-create-combined-image pane image-with-alpha)))
                                                            

      (let ((file-name (make-temp-file nil "png")))
        (gp:externalize-and-write-image pane COMBINED-IMAGE file-name)
        (format t "Written image to ~a" file-name)
        (let ((LOADED-IMAGE (gp:load-image pane file-name)))

          
          `((,ORIGINAL-IMAGE "Original-image")
            (,IMAGE-WITH-ALPHA "Image with alpha")
            (,COMBINED-IMAGE "Combined image")
            (,LOADED-IMAGE "Loaded image from:" ,(namestring file-name)))
          )))))

;;; This takes an image, and replaces each pixel with the same color
;;; as the top-left pixel (at 0,0) with a transparent pixel. 
;;; The image must be editable and have alpha, which the caller
;;; ensures by using :EDITABLE :WITH-ALPHA. 

(defun images-with-alpha-example-add-alpha (pane image)
  (let* ((image-access (gp:make-image-access pane image))
         (width (gp:image-width image))
         (height (gp:image-height image))
         (pixel-num (* width height))
         (bytes-num (* 4 pixel-num))
         (vec (make-array bytes-num :element-type '(unsigned-byte 8))))
    
    (gp:image-access-transfer-from-image image-access)
    (gp:image-access-pixels-to-bgra  image-access vec)

    ;; VEC now contains the pixels of the image, so we can edit it. 

    ;; We assume the pixel at 0,0 is in the background
    (let ((bg-blue (aref vec 0))
          (bg-green (aref vec 1))
          (bg-red (aref vec 2)))

      ;; reset to zero alpha any pixel that matches the background
      (loop for x below bytes-num by 4
            when (and (= (aref vec x) bg-blue)
                      (= (aref vec (1+ x)) bg-green)
                      (= (aref vec (+ 2 x)) bg-red))
            do 
            (setf (aref vec (+ 3 x)) 0
                  (aref vec x) 0
                  (aref vec (1+ x)) 0
                  (aref vec (+ 2 x)) 0)))
    
    ;; Put back the values in VEC into the image
    (gp:image-access-pixels-from-bgra image-access vec)
    (gp:image-access-transfer-to-image image-access)
    (gp:free-image-access image-access)))

;; The display-callback 

(defun images-with-alpha-example-draw-callback (op &rest x)
  (declare (ignore x))
  (let ((images (or (capi:capi-object-property op 'images)
                    (setf (capi:capi-object-property op 'images)
                          (images-with-alpha-example-create-images op)))))
    (let ((y 10))
      
      (dolist (image-spec images)
        (let ((image (car image-spec)))
          (gp:draw-image op image 10 y) 
          (gp:draw-string op (second image-spec)
                          120 (+ y (* (gp:image-height image) 0.6)))
          (when-let (third (third image-spec))
            (gp:draw-string op third
                            20 (+ y (gp:image-height image) 20))))
        (incf y 70))
    
      (gp:draw-string op  "(Click to change background color)" 20 (+ 60 y))
      )))

(defvar *images-with-alpha-example-backgrounds* 
  '(:gold :darkgreen :pink :lightcyan :red :gray :blue :lightblue :green))

;;; This is invoked on :button-1 :press because it is in the :INPUT-MODEL.
;;; Sets the background of the pane to the car of the list and rotates it.

(defun images-with-alpha-example-change-color (pane x y)
  (declare (ignore x y))
  (let ((old-list *images-with-alpha-example-backgrounds*))
    (let ((newcolour (car old-list))
          (new-list (cdr old-list)))
      (push-end newcolour new-list)
      (setq *images-with-alpha-example-backgrounds* new-list)
      (setf (capi:simple-pane-background pane) newcolour))))


(defun images-with-alpha-example ()
  (capi:contain  (make-instance 'capi:output-pane
                                :draw-with-buffer t
                                :display-callback 
                                'images-with-alpha-example-draw-callback
                                :background (car (last *images-with-alpha-example-backgrounds*))
                                :input-model
                                '(((:button-1 :press) images-with-alpha-example-change-color))
                                :initial-constraints
                                '(:min-width 420 :min-height 420))
                 :title "Images with alpha example"))

;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:maze.lisp,v 1.7.2.1 2011/08/24 13:26:20 davef Exp $ -*-

;;---------------------------------------------------------------------
;;
;; examples/capi/applications/maze.lisp
;;
;; This example creates a 3d maze game.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::MAZE)
;;
;; Click on SQUARE or ROUND to create a maze,
;; then click on 3D to see a three dimensional representation
;; of the maze, which you can walk through using the cursor
;; keys or by clicking on the LEFT/FORWARD/BACKWARD/RIGHT buttons.
;;
;;---------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;---------------------------------------------------------------------


(in-package "CL-USER") 

(defstruct (maze (:print-function %print-maze))
  (rows 20)
  (cols 20)
  layout
  visited
  frontiers
  frontier-count
  (circularp nil)
  max-distance-from-middle
  (pixels-since-flushed 0)
  (scale 8) ;5
  (visiblep t)
  mid-row
  mid-col

  horiz
  vert
  top-level
  graphics-port
  buttons
  gp

  3d-gp
  3d-tl
  (slider 5)
  process
  rowpos colpos facing solid-walls ;; for the 3d stuff.
  floordot
)

(defun %print-maze (x s d)
  (declare (ignore d))
  (format s "#<Maze ~dx~d ~s ~s>"
          (maze-rows x)(maze-cols x)(maze-scale x)(maze-top-level x)
))           

(defvar *current-maze*)
(defvar *frontier-count*)
(defvar *frontiers*)

(defun square (x)(* x x))

;; the data structure used for the frontier cells is of 
;; importance to the efficiency of the maze creation

;(defun make-frontier (x y)(cons x y))
;(defun frontier-row (frontier) (car frontier))
;(defun frontier-col (frontier) (cdr frontier))

(defmacro make-frontier (row col)
  `(logior ,row (ash ,col 15)))

(defmacro frontier-row (frontier)
  `(logand ,frontier #x7fff))

(defmacro frontier-col (frontier)
  `(logand #x7fff (ash ,frontier -15)))

;; maze creation algorithm begins here

(defun possible-to-go (row col row-diff col-diff comparison)
  (and
   (eq (aref (maze-layout *current-maze*)
             (+ row row-diff row-diff) (+ col col-diff col-diff))
       comparison)
   (or (not (maze-circularp *current-maze*))
       (< 
        (+ (square (- (maze-mid-col *current-maze*) (+ col col-diff col-diff)))
           (square (- (maze-mid-row *current-maze*) (+ row row-diff row-diff))))
        (maze-max-distance-from-middle *current-maze*)))))

(defmacro add-dirn (dirn dirnlist directions-count)
  `(progn
    (setq ,dirnlist 
          (+ ,dirn (ash ,dirnlist 4)))
    (incf ,directions-count)))
(defun make-dirn (row col)(logior (1+ row) (ash (1+ col) 2)))
(defun dirn-row (dirn)(1- (logand 3 dirn)))
(defun dirn-col (dirn)(1- (logand 3 (ash dirn -2))))

(defun break-wall (frontier)
  (let* ((poss-directions 0)
         (row (frontier-row frontier))
         (col (frontier-col frontier))
         (directions-count 0)
         )
    (when (possible-to-go row col 0 1 :space)(add-dirn (make-dirn 0 1) poss-directions directions-count))
    (when (possible-to-go row col 0 -1 :space)(add-dirn (make-dirn 0 -1) poss-directions directions-count))
    (when (possible-to-go row col 1 0 :space) (add-dirn (make-dirn 1 0) poss-directions directions-count))
    (when (possible-to-go row col -1 0 :space)(add-dirn (make-dirn -1 0) poss-directions directions-count))
    (let* ((direction-index (random directions-count))
           (new-direction 
            (logand 15 (ash poss-directions (* -4 direction-index))))
           (row-diff (dirn-row new-direction))
           (col-diff (dirn-col new-direction)))
      (set-maze-spaces row col (+ row row-diff) (+ col col-diff))
      (when (possible-to-go row col 0 1 :wall) (add-frontier row (+ col 2)))
      (when (possible-to-go row col 0 -1 :wall)(add-frontier row (- col 2)))
      (when (possible-to-go row col 1 0 :wall) (add-frontier (+ row 2) col))
      (when (possible-to-go row col -1 0 :wall)(add-frontier (- row 2) col)))))

(defun add-frontier (x y)
  (setf (aref (maze-layout *current-maze*) x y) :frontier)
  (vector-push-extend (make-frontier x y) *frontiers*)
  (incf *frontier-count*))


(defun choose-frontier ()
  (let* ((frontier-pos (random *frontier-count*))
         (chosen-frontier (aref *frontiers* frontier-pos)))
    (setf (aref *frontiers* frontier-pos)
          (vector-pop *frontiers*))
    (decf *frontier-count*)
    chosen-frontier))


(defun visitedp (maze row col)
  (aref (maze-visited maze) row col))
(defun mark-visited (maze row col &optional (set-to t))
  (setf (aref (maze-visited maze) row col) set-to))

(defun initialize-maze (rows cols)
  (setf (maze-layout *current-maze*)
	(make-array (list rows cols) :initial-element :wall)
        (maze-visited *current-maze*)
	(make-array (list rows cols) :initial-element nil))
  (let ((mid-col (* 2 (floor cols 4)))
	(mid-row (* 2 (floor rows 4)))) ; need to be even for fast redisplay to work

    (setf (maze-mid-row *current-maze*) mid-row
          (maze-mid-col *current-maze*) mid-col)
    (set-maze-space mid-row mid-col)    ; set up initial space cell

    (add-frontier (+ mid-row  2) (+ mid-col  0)) ; set up initial frontier cells
    (add-frontier (+ mid-row -2) (+ mid-col  0))
    (add-frontier (+ mid-row  0) (+ mid-col  2))
    (add-frontier (+ mid-row  0) (+ mid-col -2))

    (setf (maze-max-distance-from-middle *current-maze*)
          (/ (square (- (min rows cols) 6)) 4))

    ;; border round edge, don't have to test for out of bounds
    (dotimes (i rows)
      (set-maze-space i 0 :border)
      (set-maze-space i 1 :border)
      (set-maze-space i (- cols 1) :border)
      (set-maze-space i (- cols 2) :border))
    (dotimes (i cols)
      (set-maze-space  0 i :border)
      (set-maze-space 1 i :border)
      (set-maze-space (- rows 1) i :border)
      (set-maze-space (- rows 2) i :border))
    nil))

;; this is the algorithm 
(defun design-maze (mz)
  (let* ((*frontier-count* 0)
         (*current-maze* mz)
         (*frontiers* 
          (make-array 32 :adjustable t :fill-pointer *frontier-count*))
         (chosen-frontier nil))

    (initialize-maze (maze-rows mz) (maze-cols mz))

    (loop until (zerop *frontier-count*) do
	  (progn
	    (setq chosen-frontier (choose-frontier))
	    (break-wall chosen-frontier)))
    (flush-maze mz)
    ))

;; display the maze (text only)
(defun print-maze (maze)
  (terpri)
  (let* ((maze-dims (array-dimensions maze))
	 (rows (first maze-dims))
         (cols (second maze-dims))
         (str (make-string cols)))
    (dotimes (r rows)
      (dotimes (c cols)
        (setf (char str c)
	      (case (aref maze r c)
		(:wall #\#)
		(:space #\Space)
		(:frontier #\F)
		(:border #\#)
                (t #\.))))
      (format t "~a~&" str))))


(defun my-slider-value (sl)
 (capi:range-slug-start sl))

;; maze graphics code begins here

(defun slider-callback (window slider-list args)
  (declare (ignore window))
  (let ((key (first args))
        (slider1 (nth 0 slider-list))
        (slider2 (nth 1 slider-list))
        (slider3 (nth 2 slider-list))
        (*current-maze* (nth 3 slider-list)))
      (if (eq key :apply-sliders)
	  (setf
	   (maze-cols *current-maze*) (my-slider-value slider1)
	   (maze-rows *current-maze*) (my-slider-value slider2)
	   (maze-scale *current-maze*)(my-slider-value slider3))
	"no match found"
        )))


;; this should really make a new maze object.
(defun show-new-maze (maze circ)
  (setf (maze-scale maze)
        (max 1 (my-slider-value (maze-slider maze))))
  (let* ((scale (maze-scale maze))
	 (gp (maze-graphics-port maze))
         (width  (floor (gp:port-width  gp) scale))
         (height (floor (gp:port-height gp) scale)))
    (setf (maze-visiblep maze) t
          (maze-circularp maze) circ
          (maze-rows maze) (max 11 height)
          (maze-cols maze) (max 11 width)
          )
    (gp:clear-graphics-port (maze-graphics-port maze))
    (design-maze maze)))

(defun maze-callback (window maze args)
  (declare (ignore window))
  (let ((key (second args))
        (*current-maze* maze))
    (case key
      (:smaller
       (when (> (maze-rows maze) 12) (decf (maze-rows maze) 4))
       (when (> (maze-cols maze) 12) (decf (maze-cols maze) 4)))
      ((:newmaze :square) (show-new-maze maze nil))
      ((:circular :round) (show-new-maze maze t))
      ((:redraw) (redraw-maze maze))
      (:3d (when (has-maze-been-drawn-yet maze)(make-3dmaze-contact)))
      ))  nil)

(defun flush-maze (mz)
  (setf (maze-pixels-since-flushed mz) 0))

(defun has-maze-been-drawn-yet (maze)
  (maze-layout maze))


;; now cache columns AND rows; looks jerkier but is quicker.
(defun redraw-maze (maze &optional x y wid hei)
  (declare (ignore x y wid hei))
  (let ((scale (maze-scale maze))
        (layout (maze-layout maze))
        (gp (maze-graphics-port maze))
        (prev-col)
        (first-col nil)
        (prev-row)
        (first-row nil))
    (unless (has-maze-been-drawn-yet maze) 
      (return-from redraw-maze)) ;; hasn't been drawn yet.
    (dotimes (col (maze-cols maze))
      (when (evenp col)
        (setq prev-row nil)
        (dotimes (row (maze-rows maze))
          (if (eq (aref layout row col) :space)
              ;; yes, log the space
	      (unless prev-row 
	        (setq prev-row t 
		      first-row row))
            ;; not a space; draw the ones which were.
            (when prev-row
              (gp:draw-rectangle gp
		                 (* scale col) (* scale first-row)
		                 scale (* scale (- row first-row))
		                 :filled t)
              (setq prev-row nil)
              )))))
    (dotimes (row (maze-rows maze))
      (when (evenp row)
        (setq prev-col nil)
        (dotimes (col (maze-cols maze))
          (if (eq (aref layout row col) :space)
              ;;; yes, log the space
	      (unless prev-col
	        (setq prev-col t 
		      first-col col))
            ;; not a space; draw the ones which were.
            (when prev-col 
              (gp:draw-rectangle gp
		                 (* scale first-col) (* scale row)
		                 (* scale (- col first-col)) scale
		                 :filled t)
              (setq prev-col nil)
              )))))
    (draw-blip maze t)
    ))

(defparameter *pixels-between-flushing-maze* 20)
(defun set-maze-space (row col &optional (set-to :space))
  (setf (aref (maze-layout *current-maze*) row col) set-to)
  (when (and (maze-visiblep *current-maze*) (eq set-to :space))
    (let ((scale (maze-scale *current-maze*)))
      (gp:draw-rectangle (maze-graphics-port *current-maze*)
                         (* col scale) (* row scale)
                         scale scale
                         :filled t)
      (when (> (incf (maze-pixels-since-flushed *current-maze*)) *pixels-between-flushing-maze*)
        (flush-maze *current-maze*)))))

(defun set-maze-spaces (row col row2 col2 &optional (set-to :space))
  ;; do two touching spaces at once
  (setf (aref (maze-layout *current-maze*) row col) set-to)
  (setf (aref (maze-layout *current-maze*) row2 col2) set-to)
  (when (and (maze-visiblep *current-maze*) (eq set-to :space))
    (let ((scale (maze-scale *current-maze*)))
      (gp:draw-rectangle (maze-graphics-port *current-maze*)
                         (* (min col col2) scale) (* (min row row2) scale)
                         (if (eq col col2) scale (+ scale scale))
                         (if (eq row row2) scale (+ scale scale))
                         :filled t)
      (when (> (incf (maze-pixels-since-flushed *current-maze*)) *pixels-between-flushing-maze*)
        (flush-maze *current-maze*)))))

(defparameter *maze-contact-buttons-at-top* t)

(defun make-maze-contact (mz)
  (let* (buttons
         slider
         gp
         (callback #'(lambda (x interface)
                       (maze-callback (maze-top-level mz) mz (list interface x))))
         (ml (make-instance 
              (if *maze-contact-buttons-at-top*
                  'capi:column-layout 'capi:row-layout)
	      :gap 1
              :x-adjust :centre
              :description
              (list
               (setq buttons
                     (make-instance
                      'capi:push-button-panel
                      :interaction :spring-loaded
                      :callbacks (list callback callback callback)
                      :layout-class (if *maze-contact-buttons-at-top*
                                        'capi:row-layout
                                      'capi:column-layout)
                      :items '(:Square :Round :3d)
                      ))
               (setq slider
                     (make-instance
                      'capi:slider
                      :slug-start (maze-scale mz)
                      :start 1 
                      :end 20
                      :orientation :horizontal
                      :horizontal-scroll :without-bar))
	       (setq gp
                     (make-instance 'capi:output-pane
                                    :display-callback 
                                    #'(lambda(gp2 &optional x y w h)
                                        (declare (ignore gp2))
                                        (redraw-maze mz x y w h))
                                    :background :black
                                    :foreground :white))
               )))
         (tl (capi:display
              (make-instance 'capi:interface
                             :layout ml
                            :title "Maze"
                            :width (+ (if *maze-contact-buttons-at-top* 0 160)
                                      (truncate
                                       (* (maze-scale mz) (+ 4 (maze-cols mz)))))
                            :height (truncate 
                                     (+ 
                                      (if *maze-contact-buttons-at-top* 160 0)
                                      (* (maze-scale mz) (maze-rows mz))))))))
    (declare (ignore buttons))
    (setf (maze-top-level mz) tl)
    (setf (maze-graphics-port mz) gp)
    (setf (maze-slider mz) slider)
    gp))



(defun make-maze-internal (rows cols scale-factor circle floordot)
  (let* ((mz (make-maze
              :circularp circle 
              :scale  scale-factor
              :rows rows ;(* 4 (truncate rows 4))
              :cols cols ;(* 4 (truncate cols 4))
              :visiblep t
              :floordot floordot ; hansel & gretel - leave a trail
              )))
    (make-maze-contact mz)
    mz))


;; 3d graphics begins here

(defparameter *maxsize* 20) ; can increase this to affect 'depth of focus' up to about 35
(defparameter *recipmaxsize* (/ *maxsize*))

(defparameter *wallpos* 
  #(0 18/10 34/10 48/10 6 7 78/10 84/10 88/10 92/10 94/10 96/10 97/10 98/10 99/10))
(defparameter *max-dist*  (length *wallpos*))
(defparameter *move-by* 1) ;; cannot change this yet.

(defun convert (x) (values (round x)))

(defun my-draw-rectangle (gp x y wid hei &optional filled)
  (let ((width (* (gp:port-width gp) *recipmaxsize*))
	(height (* (gp:port-height gp) *recipmaxsize*)))
    (gp:draw-rectangle gp
                       (convert (* x width))(convert (* y height))
                       (convert (* wid width))(convert (* hei height))
                       :filled filled)))

(defun my-draw-line (gp x y wid hei)
  (let ((width (* (gp:port-width gp) *recipmaxsize*))
	(height (* (gp:port-height gp) *recipmaxsize*)))
    (gp:draw-line gp
                  (convert (* x width))(convert (* y height))
                  (convert (* wid width))(convert (* hei height)))))

(defun 3dmaze-callback (window maze args)
  (declare (ignore window))
  (let ((key (second args))
        (*current-maze* maze))
    (case key 
      ((:left #\l #\L) (3d-turn-left maze))
      ((:right #\r #\R) (3d-turn-right maze))
      ((:up :forward #\f #\F) (3d-forward maze *move-by*))
      ((:down :backward #\b #\B) (3d-forward maze (- *move-by*)))
      ((#\w #\W) (try-to-move maze 0 -1))
      ((#\e #\E) (try-to-move maze 0  1))
      ((#\n #\N) (try-to-move maze -1 0))
      ((#\s #\S) (try-to-move maze  1 0))
      (t (format t "~S ~s" args key))))
  nil)

(defun draw-blip (maze filled) ; show where we are
  (when (maze-colpos maze) 
    (let* ((gp (maze-graphics-port maze))
           (scale (maze-scale maze))
           (halfscale (floor (- scale 0.1) 2)) ; make it less than half so it won't leave stray pixels
           (state (gp:port-graphics-state gp)))
      (when filled
        (setf (gp:graphics-state-foreground state) :red))
      (if (or (not filled) (< (maze-scale maze) 3))
          (gp:draw-rectangle (maze-graphics-port maze)
		             (* (maze-colpos maze) scale)
                             (* (maze-rowpos maze) scale)
		             scale scale
                             :filled t)
        ;; if it's big enough, try to draw some sort of pointing shape.
        (gp:with-graphics-translation
            (gp (+ halfscale (* (maze-colpos maze) scale))
		(+ halfscale (* (maze-rowpos maze) scale)))
          (gp:with-graphics-rotation (gp 
                                      (case (maze-facing maze)
                                        (:north 0)
                                        (:east (/ pi 2))
                                        (:south pi)
                                        (:west (/ pi -2))))
	    (gp:draw-line gp halfscale halfscale 0 (- halfscale))
	    (gp:draw-line gp 0 (- halfscale) (- halfscale) halfscale)
	    )))
      (setf (gp:graphics-state-foreground state) :white))))

(defun 3d-forward (maze &optional (offset 1))
  (when (eq :space (square-in-view maze offset 0))
    (draw-blip maze nil)
    (dotimes (i (abs offset))
      (incf (maze-rowpos maze) (* offset (rowoffset (maze-facing maze))))
      (incf (maze-colpos maze) (* offset (coloffset (maze-facing maze))))
      (mark-visited maze (maze-rowpos maze) (maze-colpos maze))
      ))
  (draw-blip maze t)
  (draw-3d-maze maze))

(defun 3d-turn-left (maze)
  (setf (maze-facing maze)
        (ecase (maze-facing maze)
          (:west :south)
          (:south :east)
          (:east :north)
          (:north :west)))
  (draw-blip maze nil)
  (draw-blip maze t)
  (draw-3d-maze maze))

(defun 3d-turn-right (maze)
  (setf (maze-facing maze)
        (ecase (maze-facing maze)
          (:south :west)
          (:east :south)
          (:north :east)
          (:west :north)))
  (draw-blip maze nil)
  (draw-blip maze t)
  (draw-3d-maze maze))

(defun rowoffset (direction)
  (ecase direction
    (:north -1)
    (:west  0)
    (:south 1)
    (:east  0)))

(defun coloffset (direction)
  (ecase direction
	(:north 0)
	(:west  -1)
	(:south 0)
	(:east  1)))


(defun try-to-move (maze rowoffset coloffset)
  (when (maze-can-go-to maze rowoffset coloffset)
    (draw-blip maze nil)
    (incf (maze-rowpos maze) rowoffset)
    (incf (maze-colpos maze) coloffset)
    (draw-blip maze t)
    (draw-3d-maze maze)))

(defun maze-can-go-to (maze rowoffset coloffset)
  (eq :space
      (aref (maze-layout maze)
            (+ (maze-rowpos maze) rowoffset)
            (+ (maze-colpos maze) coloffset))))

(defun square-in-view (mz how-far-in-front how-far-to-right)
  (let* ((rowoffset (rowoffset (maze-facing mz)))
	 (coloffset (coloffset (maze-facing mz)))
	 (row (maze-rowpos mz))
	 (col (maze-colpos mz))
	 (erow (+ row (* how-far-in-front rowoffset)))
	 (ecol (+ col (* how-far-in-front coloffset)))
         (actualrow (+ erow (* how-far-to-right coloffset)))
         (actualcol (- ecol (* how-far-to-right rowoffset))))
    (values
     (aref (maze-layout mz) actualrow actualcol)
     (aref (maze-visited mz) actualrow actualcol))
    ;;(maze-rowpos mz) (* how-far-in-front rowoffset) (+ maze-colpos)
    ))

(defun draw-endwall (maze distance)
  (let* ((posn (aref *wallpos* distance))
         (width (- *maxsize* (* 2 posn)))
         )
    (my-draw-rectangle (maze-3d-gp maze)
                       posn posn
                       width width
                       (maze-solid-walls maze))))

(defun draw-floordot (maze distance)
  (when (maze-floordot maze)
    (let* ((posn (* 1/2 (+ (aref *wallpos* distance)
                           (aref *wallpos* (1+ distance)))))
           (width (- *maxsize* (* 2 posn)))
           )
      (my-draw-rectangle (maze-3d-gp maze)
                         (- 10 (/ width 8)) (- 20 posn) ;1
                         (/ width 4) 0 ;;width,height
                         nil ;(maze-solid-walls maze)
                         ))))

(defun draw-sidewall (maze distance side)
  ;;(setq *global-gp* (maze-3d-gp maze))
  (let ((this (aref *wallpos* distance))
        (next (aref *wallpos* (1+ distance))))
    (unless (eq side :left)
      (setq this (- *maxsize* this)
            next (- *maxsize* next)))
    (my-draw-line (maze-3d-gp maze) ;; top
		  this this
		  next next)
    (my-draw-line (maze-3d-gp maze) ;; bottom
		  this (- *maxsize* this)
		  next (- *maxsize* next))))

(defun draw-door (maze distance side)
  (let* ((left (eq side :left))
         (near (aref *wallpos* distance))
         (far (aref *wallpos* (1+ distance)))
         (realfar (if left far (- *maxsize* far)))
         (realnear (if left near (- *maxsize* near)))
         (leftmost (if left near realfar))
         )
    (unless (= 0 distance)
      (my-draw-line (maze-3d-gp maze)    ; vertical, near edge
		    realnear realnear
		    realnear (- *maxsize* realnear)))
    (my-draw-rectangle (maze-3d-gp maze) ; top (if left), horiz.
		       leftmost far
		       (- far near)
                       (- *maxsize* (* 2 far))
                       (maze-solid-walls maze))))

(defun draw-3d-maze-part (gp &optional x y width height)
  (declare (ignore gp x y width height))
  (when (boundp '*current-maze*)
    (draw-3d-maze *current-maze* t)))

(defun draw-3d-maze (maze &optional noclear)
  (let* ((gp (maze-3d-gp maze)))
    (let* ((corridor nil)
	   (end-of-corridor nil))
      (dotimes (i (1- *max-dist*))
	(unless end-of-corridor
          (multiple-value-bind 
              (ahead visited)
              (square-in-view maze (* i *move-by*)  0)
	    (push (list 
		   i
		   (square-in-view maze (* i *move-by*) -1)
                   ahead
		   (square-in-view maze (* i *move-by*) 1)
                   visited
		   )
		  corridor))
	  (unless (eq :space (square-in-view maze i 0))

	    (setq end-of-corridor t))))
      (setq corridor (nreverse corridor))
      (setf (maze-solid-walls maze)
            (member (maze-facing maze) '(:north :south)))
      (unless noclear (gp:clear-graphics-port gp))
      (dolist (l corridor)
	(let ((i (first l))
	      (to-left (second l))
	      (ahead (third l))
	      (to-right (fourth l)))
	  (if (eq :space ahead)
              (unless (= i (- *max-dist* 2))
	        (if (eq :space to-left)
	            (draw-door maze i :left)
	          (draw-sidewall maze i :left))
	        (if (eq :space to-right)
	            (draw-door maze i :right)
	          (draw-sidewall maze i :right))
                (when (fifth l) ; has been visited before
                    (draw-floordot maze i)
                    )
                )
	    (draw-endwall maze i))
	  )) 
      (gp:draw-string gp
		      (concatenate 'string
				   "FACING " (string (maze-facing maze)))
		      50 10
                      :foreground :red)
      )))



(defun 3dmaze-input (self x y gesture mz)
  (declare (ignore self x y))
  (3dmaze-callback (maze-top-level mz) mz
                   (list nil (sys::gesture-spec-data gesture))))

(defun make-3dmaze-contact ()
  (let ((mz *current-maze*))
    (setf (maze-rowpos mz) (maze-mid-row mz)
          (maze-colpos mz) (maze-mid-col mz)
          (maze-facing mz) :north)
    (let*
        ((callback
          #'(lambda (x interface) 
              (3dmaze-callback (maze-top-level mz) mz (list interface x))))
         (ml (make-instance
              'capi:column-layout
              :gap 5
              :description (list
                            (setf (maze-3d-gp mz) 
                                  (make-instance
                                   'capi:output-pane
                                   :display-callback #'(lambda (gp &optional x y w h)
                                                         (declare (ignore gp x y w h))
                                                         (draw-3d-maze mz t))
                                   :resize-callback #'(lambda (pane &rest args)
                                                        (declare (ignore args))
                                                        (gp:invalidate-rectangle pane)
                                                        )
                                   :background :black
                                   :foreground :white
                                   :input-model `((:up 3dmaze-input ,mz)
                                                  (:down 3dmaze-input ,mz)
                                                  (:left 3dmaze-input ,mz)
                                                  (:right 3dmaze-input ,mz))))
                            (make-instance 'capi:push-button
                                           :data :forward
                                           :callback callback
                                           )
                            (make-instance 'capi:push-button-panel
                                           :callbacks (list callback callback)
                                           :items '(:left :right))
                            (make-instance 'capi:push-button
                                           :data :backward
                                           :callback callback
                                           )
	                    )
              :x-adjust :centre))
         (tl (capi:display (make-instance 'capi:interface
                                          :layout ml
                                          :title "3d-Maze"
                                          :width 320
                                          :height 380))))
      (setf (maze-3d-tl mz) tl)
      (maze-3d-gp mz))))


(defun maze (&optional (rows 30)(cols rows) &key (scale-factor 6) circle floordot)
  (make-maze-internal rows cols scale-factor circle floordot))

;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:maze-multi.lisp,v 1.2.2.1 2011/08/24 13:26:21 davef Exp $ -*-

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

(defstruct xmaze-client
  (id 0)
  stream
  colpos
  rowpos
  facing
  (3d-color :red))

(defstruct (xmaze (:print-function %print-xmaze))
  (rows 20)
  (cols 20)
  layout
  visited
  frontiers
  frontier-count
  (circularp nil)
  max-distance-from-middle
  (pixels-since-flushed 0)
  (scale 16) ;5
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
  solid-walls ;; for the 3d stuff.
  floordot
  server-stream
  (current-client (make-xmaze-client))
  client-process
  clients
)

(defun %print-xmaze (x s d)
  (declare (ignore d))
  (format s "#<Xmaze ~dx~d ~s ~s>"
          (xmaze-rows x)(xmaze-cols x)(xmaze-scale x)(xmaze-top-level x)
))

(defmacro def-xmaze-client-trampoline (xmaze-accessor client-accessor)
  `(dspec:def (def-xmaze-client-trampoline ,xmaze-accessor)
     (defun ,xmaze-accessor (xmaze)
       (,client-accessor (xmaze-current-client xmaze)))
     (defun (setf ,xmaze-accessor) (value xmaze)
       (setf (,client-accessor (xmaze-current-client xmaze)) value))))

(def-xmaze-client-trampoline xmaze-colpos xmaze-client-colpos)
(def-xmaze-client-trampoline xmaze-rowpos xmaze-client-rowpos)
(def-xmaze-client-trampoline xmaze-facing xmaze-client-facing)
(def-xmaze-client-trampoline xmaze-3d-color xmaze-client-3d-color)

(defvar *current-xmaze*)
(defvar *frontier-count*)
(defvar *frontiers*)

(defun square (x)(* x x))

;; the data structure used for the frontier cells is of 
;; importance to the efficiency of the xmaze creation

;(defun make-frontier (x y)(cons x y))
;(defun frontier-row (frontier) (car frontier))
;(defun frontier-col (frontier) (cdr frontier))

(defmacro make-frontier (row col)
  `(logior ,row (ash ,col 15)))

(defmacro frontier-row (frontier)
  `(logand ,frontier #x7fff))

(defmacro frontier-col (frontier)
  `(logand #x7fff (ash ,frontier -15)))

;; xmaze creation algorithm begins here

(defun possible-to-go (row col row-diff col-diff comparison)
  (and
   (eq (aref (xmaze-layout *current-xmaze*)
             (+ row row-diff row-diff) (+ col col-diff col-diff))
       comparison)
   (or (not (xmaze-circularp *current-xmaze*))
       (< 
        (+ (square (- (xmaze-mid-col *current-xmaze*) (+ col col-diff col-diff)))
           (square (- (xmaze-mid-row *current-xmaze*) (+ row row-diff row-diff))))
        (xmaze-max-distance-from-middle *current-xmaze*)))))

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
      (set-xmaze-spaces row col (+ row row-diff) (+ col col-diff))
      (when (possible-to-go row col 0 1 :wall) (add-frontier row (+ col 2)))
      (when (possible-to-go row col 0 -1 :wall)(add-frontier row (- col 2)))
      (when (possible-to-go row col 1 0 :wall) (add-frontier (+ row 2) col))
      (when (possible-to-go row col -1 0 :wall)(add-frontier (- row 2) col)))))

(defun add-frontier (x y)
  (setf (aref (xmaze-layout *current-xmaze*) x y) :frontier)
  (vector-push-extend (make-frontier x y) *frontiers*)
  (incf *frontier-count*))


(defun choose-frontier ()
  (let* ((frontier-pos (random *frontier-count*))
         (chosen-frontier (aref *frontiers* frontier-pos)))
    (setf (aref *frontiers* frontier-pos)
          (vector-pop *frontiers*))
    (decf *frontier-count*)
    chosen-frontier))


(defun visitedp (xmaze row col)
  (aref (xmaze-visited xmaze) row col))
(defun mark-visited (xmaze row col &optional (set-to t))
  (setf (aref (xmaze-visited xmaze) row col) set-to))

(defun pre-initialize-xmaze (xmaze rows cols layout)
  (let ((mid-col (* 2 (floor cols 4)))
	(mid-row (* 2 (floor rows 4)))) ; need to be even for fast redisplay to work

    (setf (xmaze-layout xmaze) layout
          (xmaze-visited xmaze) (make-array (list rows cols) :initial-element nil))
    (setf (xmaze-mid-row xmaze) mid-row
          (xmaze-mid-col xmaze) mid-col)
    (values mid-col mid-row)))

(defun initialize-xmaze (rows cols)
  (multiple-value-bind (mid-col mid-row)
      (pre-initialize-xmaze *current-xmaze*
                           rows cols
                           (make-array (list rows cols) :initial-element :wall))
    (set-xmaze-space mid-row mid-col)    ; set up initial space cell

    (add-frontier (+ mid-row  2) (+ mid-col  0)) ; set up initial frontier cells
    (add-frontier (+ mid-row -2) (+ mid-col  0))
    (add-frontier (+ mid-row  0) (+ mid-col  2))
    (add-frontier (+ mid-row  0) (+ mid-col -2))

    (setf (xmaze-max-distance-from-middle *current-xmaze*)
          (/ (square (- (min rows cols) 6)) 4))

    ;; border round edge, don't have to test for out of bounds
    (dotimes (i rows)
      (set-xmaze-space i 0 :border)
      (set-xmaze-space i 1 :border)
      (set-xmaze-space i (- cols 1) :border)
      (set-xmaze-space i (- cols 2) :border))
    (dotimes (i cols)
      (set-xmaze-space  0 i :border)
      (set-xmaze-space 1 i :border)
      (set-xmaze-space (- rows 1) i :border)
      (set-xmaze-space (- rows 2) i :border))
    nil))

;; this is the algorithm 
(defun design-xmaze (mz)
  (let* ((*frontier-count* 0)
         (*current-xmaze* mz)
         (*frontiers* 
          (make-array 32 :adjustable t :fill-pointer *frontier-count*))
         (chosen-frontier nil))

    (initialize-xmaze (xmaze-rows mz) (xmaze-cols mz))

    (loop until (zerop *frontier-count*) do
	  (progn
	    (setq chosen-frontier (choose-frontier))
	    (break-wall chosen-frontier)))
    (flush-xmaze mz)
    ))

;; display the xmaze (text only)
(defun print-xmaze (xmaze)
  (terpri)
  (let* ((xmaze-dims (array-dimensions xmaze))
	 (rows (first xmaze-dims))
         (cols (second xmaze-dims))
         (str (make-string cols)))
    (dotimes (r rows)
      (dotimes (c cols)
        (setf (char str c)
	      (case (aref xmaze r c)
		(:wall #\#)
		(:space #\Space)
		(:frontier #\F)
		(:border #\#)
                (t #\.))))
      (format t "~a~&" str))))


(defun my-slider-value (sl)
 (capi:range-slug-start sl))

;; xmaze graphics code begins here

(defun slider-callback (window slider-list args)
  (declare (ignore window))
  (error "ajsdj")
  (let ((key (first args))
        (slider1 (nth 0 slider-list))
        (slider2 (nth 1 slider-list))
        (slider3 (nth 2 slider-list))
        (*current-xmaze* (nth 3 slider-list)))
      (if (eq key :apply-sliders)
	  (setf
	   (xmaze-cols *current-xmaze*) (my-slider-value slider1)
	   (xmaze-rows *current-xmaze*) (my-slider-value slider2)
	   (xmaze-scale *current-xmaze*)(my-slider-value slider3))
	"no match found"
        )))


;; this should really make a new xmaze object.
(defun show-new-xmaze (xmaze circ)
  (setf (xmaze-scale xmaze)
        (max 1 (my-slider-value (xmaze-slider xmaze))))
  (let* ((scale (xmaze-scale xmaze))
	 (gp (xmaze-graphics-port xmaze))
         (width  (floor (gp:port-width  gp) scale))
         (height (floor (gp:port-height gp) scale)))
    (setf (xmaze-visiblep xmaze) t
          (xmaze-circularp xmaze) circ
          (xmaze-rows xmaze) (max 11 height)
          (xmaze-cols xmaze) (max 11 width)
          )
    (gp:clear-graphics-port (xmaze-graphics-port xmaze))
    (design-xmaze xmaze)))

(defun xmaze-callback (window xmaze args)
  (declare (ignore window))
  (let ((key (second args))
        (*current-xmaze* xmaze))
    (case key
      (:smaller
       (when (> (xmaze-rows xmaze) 12) (decf (xmaze-rows xmaze) 4))
       (when (> (xmaze-cols xmaze) 12) (decf (xmaze-cols xmaze) 4)))
      ((:newxmaze :square) (show-new-xmaze xmaze nil))
      ((:circular :round) (show-new-xmaze xmaze t))
      ((:redraw) (redraw-xmaze xmaze))
      (:3d (when (has-xmaze-been-drawn-yet xmaze)(make-3dxmaze-contact)))
      ))  nil)

(defun flush-xmaze (mz)
  (setf (xmaze-pixels-since-flushed mz) 0))

(defun has-xmaze-been-drawn-yet (xmaze)
  (xmaze-layout xmaze))


;; now cache columns AND rows; looks jerkier but is quicker.
(defun redraw-xmaze (xmaze &optional x y wid hei)
  (declare (ignore x y wid hei))
  (let ((scale (xmaze-scale xmaze))
        (layout (xmaze-layout xmaze))
        (gp (xmaze-graphics-port xmaze))
        (prev-col)
        (first-col nil)
        (prev-row)
        (first-row nil))
    (unless (has-xmaze-been-drawn-yet xmaze) 
      (return-from redraw-xmaze)) ;; hasn't been drawn yet.
    (dotimes (col (xmaze-cols xmaze))
      (when (evenp col)
        (setq prev-row nil)
        (dotimes (row (xmaze-rows xmaze))
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
    (dotimes (row (xmaze-rows xmaze))
      (when (evenp row)
        (setq prev-col nil)
        (dotimes (col (xmaze-cols xmaze))
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
    (draw-blip xmaze t)
    (dolist (client (xmaze-clients xmaze))
      (draw-blip-for-client xmaze t client))
    ))

(defparameter *pixels-between-flushing-xmaze* 20)
(defun set-xmaze-space (row col &optional (set-to :space))
  (setf (aref (xmaze-layout *current-xmaze*) row col) set-to)
  (when (and (xmaze-visiblep *current-xmaze*) (eq set-to :space))
    (let ((scale (xmaze-scale *current-xmaze*)))
      (gp:draw-rectangle (xmaze-graphics-port *current-xmaze*)
                         (* col scale) (* row scale)
                         scale scale
                         :filled t)
      (when (> (incf (xmaze-pixels-since-flushed *current-xmaze*)) *pixels-between-flushing-xmaze*)
        (flush-xmaze *current-xmaze*)))))

(defun set-xmaze-spaces (row col row2 col2 &optional (set-to :space))
  ;; do two touching spaces at once
  (setf (aref (xmaze-layout *current-xmaze*) row col) set-to)
  (setf (aref (xmaze-layout *current-xmaze*) row2 col2) set-to)
  (when (and (xmaze-visiblep *current-xmaze*) (eq set-to :space))
    (let ((scale (xmaze-scale *current-xmaze*)))
      (gp:draw-rectangle (xmaze-graphics-port *current-xmaze*)
                         (* (min col col2) scale) (* (min row row2) scale)
                         (if (eq col col2) scale (+ scale scale))
                         (if (eq row row2) scale (+ scale scale))
                         :filled t)
      (when (> (incf (xmaze-pixels-since-flushed *current-xmaze*)) *pixels-between-flushing-xmaze*)
        (flush-xmaze *current-xmaze*)))))

(defparameter *xmaze-contact-buttons-at-top* t)

(defun make-xmaze-contact (mz server-host server-port)
  (let* (buttons
         slider
         gp
         (callback #'(lambda (x interface)
                       (xmaze-callback (xmaze-top-level mz) mz (list interface x))))
         (ml (make-instance 
              (if *xmaze-contact-buttons-at-top*
                  'capi:column-layout 'capi:row-layout)
	      :gap 1
              :x-adjust :centre
              :description
              (append
               (if server-host
                   (list (make-instance 'capi:title-pane
                                        :text (format nil "Connected to ~A:~D"
                                                      server-host server-port)))
                 (list (setq buttons
                             (make-instance
                              'capi:push-button-panel
                              :interaction :spring-loaded
                              :callbacks (list callback callback callback)
                              :layout-class (if *xmaze-contact-buttons-at-top*
                                                'capi:row-layout
                                              'capi:column-layout)
                              :items '(:Square :Round :3d)
                              :layout-args '(:uniform-size-p t)
                              ))
                       (setq slider
                             (make-instance
                              'capi:slider
                              :slug-start  (xmaze-scale mz)
                              :start 1 
                              :end 20
                              :orientation :horizontal
                              :horizontal-scroll :without-bar))))
	       (list (setq gp
                           (make-instance 'capi:output-pane
                                          :display-callback 
                                          #'(lambda(gp2 &optional x y w h)
                                              (declare (ignore gp2))
                                              (redraw-xmaze mz x y w h))
                                          :background :black
                                          :foreground :white)))
               )))
         (tl (make-instance 'capi:interface
                            :layout ml
                            :title "Xmaze"
                            :width (+ (if *xmaze-contact-buttons-at-top*
                                          0
                                        160)
                                      (truncate
                                       (* (xmaze-scale mz) (+ 2 (xmaze-cols mz)))))
                            :height (truncate 
                                     (+ 
                                      (if *xmaze-contact-buttons-at-top*
                                          (if server-host 54 160)
                                        0)
                                      (* (xmaze-scale mz) (xmaze-rows mz))))
                            :create-callback (and server-host
                                                  #'(lambda (interface)
                                                      (declare (ignore interface))
                                                      (connect-to-xmaze-server
                                                       server-host
                                                       server-port
                                                       mz)))
                            :destroy-callback (if server-host
                                                  #'(lambda (interface)
                                                      (declare (ignore interface))
                                                      (disconnect-from-xmaze-server
                                                       mz))))))
    (declare (ignore buttons))
    (setf (xmaze-top-level mz) tl)
    (setf (xmaze-graphics-port mz) gp)
    (setf (xmaze-slider mz) slider)
    (capi:display tl)
    gp))



(defun make-xmaze-internal (rows cols scale-factor circle floordot server)
  (let* ((mz (make-xmaze
              :circularp circle 
              :scale  scale-factor
              :rows rows ;(* 4 (truncate rows 4))
              :cols cols ;(* 4 (truncate cols 4))
              :visiblep nil
              :floordot floordot ; hansel & gretel - leave a trail
              )))
    (multiple-value-bind (host port)
        (cond ((stringp server)
               (let ((split (position #\: server)))
                 (values (subseq server 0 split)
                         (if split
                             (parse-integer server :start (1+ split))
                           12434))))
              ((fixnump server)
               (let ((server-mz (copy-xmaze mz)))
                 (setf (xmaze-current-client server-mz) nil)
                 (ensure-xmaze-server server server-mz))
               (values "localhost" server)))
      (make-xmaze-contact mz host port)
      mz)))

(defun connect-to-xmaze-server (host port mz)
  (let* ((stream (comm:open-tcp-stream host port :errorp t))
         (id-and-3d-color (read stream))
         (id (car id-and-3d-color))
         (client (xmaze-current-client mz)))
    (setf (xmaze-visiblep mz) t)
    (setf (xmaze-server-stream mz) stream)
    (setf (xmaze-client-id client) id
          (xmaze-client-3d-color client) (cdr id-and-3d-color))
    (setf (xmaze-client-process mz)
          (mp:process-run-function
           (format nil "Xmaze Client ~D Connection ~D"
                   (nth-value 1 (comm:socket-stream-peer-address stream))
                   id)
           '()
           'run-xmaze-connection
           stream
           client
           mz))
    (send-to-xmaze-connection-no-reply mz :get-info)))

(defun disconnect-from-xmaze-server (mz)
  (when-let (process (xmaze-client-process mz))
    (mp:process-kill process)))

(defvar *xmaze-server* nil)

(defun ensure-xmaze-server (port mz)
  (unless (and *xmaze-server* (mp:process-alive-p *xmaze-server*))
    (setq *xmaze-server*
          (comm:start-up-server
           :function #'(lambda (fd)
                         (accept-xmaze-connection fd port mz))
           :service port
           :process-name (format nil "Xmaze Server ~D" port)))))

(defun accept-xmaze-connection (fd port mz)
  (mp:process-run-function (format nil "Xmaze Server ~D Connection ~D" port fd)
                           '()
                           'run-xmaze-server-connection
                           (make-instance 'comm:socket-stream
                                          :socket fd
                                          :direction :io
                                          :element-type 'base-char)
                           fd
                           mz))

(defparameter *colors*
  '(:red :green :blue :cyan :magenta :darkgreen :darkorange))

  
(defun allocate-xmaze-colour (mz)
  (dolist (x *colors* :red)
    (unless (find x (xmaze-clients mz)
                  :key 'xmaze-client-3d-color )
      (return x))))

(defun run-xmaze-server-connection (stream id mz)
  (let* ((3d-color (allocate-xmaze-colour mz))
         (client (make-xmaze-client :id id
                                   :3d-color 3d-color
                                   :stream stream)))
    (unwind-protect
        (progn
          (let ((other-client-info '()))
            (dolist (other-client (xmaze-clients mz))
              (let ((other-stream (xmaze-client-stream other-client)))
                (print (list :new-client id 3d-color)
                       other-stream)
                (force-output other-stream))
              (push (cons (xmaze-client-id other-client)
                          (xmaze-client-3d-color other-client))
                    other-client-info))
            (push client (xmaze-clients mz))
            (print (cons id 3d-color) stream)
            (print (list :all-clients other-client-info)
                   stream))
          (run-xmaze-connection stream client mz))
      (setf (xmaze-clients mz)
            (delete id (xmaze-clients mz) :key 'xmaze-client-id))
      (dolist (other-client (xmaze-clients mz))
        (let ((other-stream (xmaze-client-stream other-client)))
          (print (list :delete-client id)
                 other-stream)
          (force-output other-stream))))))

(defun run-xmaze-connection (stream client mz)
  (with-open-stream (stream stream)
    (loop (unless (run-xmaze-connection-1  stream client mz)
            (return)))))

(defun run-xmaze-connection-1 (stream client mz)
  (force-output stream)
  (let ((command (read stream nil :eof)))
    (and (consp command)
         (progn
           (when-let (res (handle-xmaze-command client command mz))
             (print res stream))
           t))))

(defun handle-xmaze-command (current-client command mz)
  (let ((type (car command)))
    (case type
      (:get-info
       (unless (xmaze-layout mz)
         (design-xmaze mz))
       (list :return-info
             :rows (xmaze-rows mz)
             :cols (xmaze-cols mz)
             :scale (xmaze-scale mz)
             :circularp (xmaze-circularp mz)
             :layout (xmaze-layout mz)))
      (:return-info
       (destructuring-bind (&key rows cols scale circularp layout)
           (cdr command)
         (setf (xmaze-rows mz) rows
               (xmaze-cols mz) cols
               (xmaze-scale mz) scale
               (xmaze-circularp mz) circularp)
         (pre-initialize-xmaze mz rows cols layout)
         (make-3dxmaze-contact mz)
         nil))
      (:position-and-locate
       (destructuring-bind (colpos rowpos facing)
           (cdr command)
         (setf (xmaze-client-colpos current-client) colpos
               (xmaze-client-rowpos current-client) rowpos
               (xmaze-client-facing current-client) facing)
         (loop for client in (xmaze-clients mz)
               for stream = (xmaze-client-stream client)
               unless (eq client current-client)
               do
               (print (list :locate
                            (xmaze-client-id current-client)
                            colpos
                            rowpos
                            facing)
                      stream)
               (force-output stream)))
       nil)
      (:locate
       (destructuring-bind (id colpos rowpos facing)
           (cdr command)
         (unless (eql id (xmaze-client-id current-client))
           (when-let (client (find id (xmaze-clients mz) :key 'xmaze-client-id))
             (draw-blip-for-client mz nil client)
             (setf (xmaze-client-colpos client) colpos
                   (xmaze-client-rowpos client) rowpos
                   (xmaze-client-facing client) facing)
             (draw-blip-for-client mz t client))))
       nil)
      (:all-clients
       (destructuring-bind (other-client-info)
           (cdr command)
         (let ((clients '())
               (old-clients (xmaze-clients mz))
               (current-id (xmaze-client-id (xmaze-current-client mz))))
           (dolist (info other-client-info)
             (destructuring-bind (id . 3d-color)
                 info
               (unless (eq id current-id)
                 (let ((old-client (find id old-clients :key 'xmaze-client-id)))
                   (if old-client
                       (push old-client clients)
                     (push (make-xmaze-client :id id :3d-color 3d-color) clients))))))
           (setf (xmaze-clients mz) clients))))
      (:new-client
       (destructuring-bind (id 3d-color)
           (cdr command)
         (push (make-xmaze-client :id id :3d-color 3d-color)
               (xmaze-clients mz)))
       nil)
      (:delete-client
       (destructuring-bind (id)
           (cdr command)
         (setf (xmaze-clients mz)
               (delete id (xmaze-clients mz) :key 'xmaze-client-id)))
       nil))))


(defun send-to-xmaze-connection (mz command &rest args)
  (when-let (stream (xmaze-server-stream mz))
    (print (cons command args) stream)
    (force-output stream)
    (read stream)))

(defun send-to-xmaze-connection-no-reply (mz command &rest args)
  (when-let (stream (xmaze-server-stream mz))
    (print (cons command args) stream)
    (force-output stream)))


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

(defun 3dxmaze-callback (window xmaze args)
  (declare (ignore window))
  (let ((key (second args))
        (*current-xmaze* xmaze))
    (case key 
      ((:left #\l #\L) (3d-turn-left xmaze))
      ((:right #\r #\R) (3d-turn-right xmaze))
      ((:up :forward #\f #\F) (3d-forward xmaze *move-by*))
      ((:down :backward #\b #\B) (3d-forward xmaze (- *move-by*)))
      ((#\w #\W) (try-to-move xmaze 0 -1))
      ((#\e #\E) (try-to-move xmaze 0  1))
      ((#\n #\N) (try-to-move xmaze -1 0))
      ((#\s #\S) (try-to-move xmaze  1 0))
      (t (format t "~S ~s" args key))))
  nil)

(defun draw-blip (xmaze filled &optional
                       (colpos (xmaze-colpos xmaze))
                       (rowpos (xmaze-rowpos xmaze))
                       (facing (xmaze-facing xmaze))
                       (color (xmaze-3d-color xmaze)))
  ;; show where we are
  (when colpos
    (let* ((gp (xmaze-graphics-port xmaze))
           (scale (xmaze-scale xmaze))
           (halfscale (floor (- scale 0.1) 2)) ; make it less than half so it won't leave stray pixels
           (state (gp:port-graphics-state gp)))
      (when filled
        (setf (gp:graphics-state-foreground state) color))

      (if (or (not filled) (< scale 3))
          (gp:draw-rectangle gp
		             (* colpos scale)
                             (* rowpos scale)
		             scale scale
                             :filled t)
        ;; if it's big enough, try to draw some sort of pointing shape.
        (let ((width (truncate scale 4)))
          
        (gp:with-graphics-translation
            (gp (+ halfscale (* colpos scale))
		(+ halfscale (* rowpos scale)))
          (when (> width 1)
            (decf halfscale (- width 1)))
          (gp:with-graphics-rotation (gp 
                                      (case facing
                                        (:north 0)
                                        (:east (/ pi 2))
                                        (:south pi)
                                        (:west (/ pi -2))))
	    (gp:draw-lines gp (list halfscale halfscale 0 (- halfscale)  0 (- halfscale) (- halfscale) halfscale) :thickness width :line-end-style :round)
	    ))))
      (setf (gp:graphics-state-foreground state) :white))))

(defun 3d-forward (xmaze &optional (offset 1))
  (when (eq :space (square-in-view xmaze offset 0))
    (draw-blip xmaze nil)
    (dotimes (i (abs offset))
      (incf (xmaze-rowpos xmaze) (* offset (rowoffset (xmaze-facing xmaze))))
      (incf (xmaze-colpos xmaze) (* offset (coloffset (xmaze-facing xmaze))))
      (mark-visited xmaze (xmaze-rowpos xmaze) (xmaze-colpos xmaze))
      ))
  (draw-blip xmaze t)
  (update-xmaze-server-position xmaze)
  (draw-3d-xmaze xmaze))

(defun 3d-turn-left (xmaze)
  (setf (xmaze-facing xmaze)
        (ecase (xmaze-facing xmaze)
          (:west :south)
          (:south :east)
          (:east :north)
          (:north :west)))
  (draw-blip xmaze nil)
  (draw-blip xmaze t)
  (update-xmaze-server-position xmaze)
  (draw-3d-xmaze xmaze))

(defun 3d-turn-right (xmaze)
  (setf (xmaze-facing xmaze)
        (ecase (xmaze-facing xmaze)
          (:south :west)
          (:east :south)
          (:north :east)
          (:west :north)))
  (draw-blip xmaze nil)
  (draw-blip xmaze t)
  (update-xmaze-server-position xmaze)
  (draw-3d-xmaze xmaze))

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


(defun try-to-move (xmaze rowoffset coloffset)
  (when (xmaze-can-go-to xmaze rowoffset coloffset)
    (draw-blip xmaze nil)
    (incf (xmaze-rowpos xmaze) rowoffset)
    (incf (xmaze-colpos xmaze) coloffset)
    (draw-blip xmaze t)
    (update-xmaze-server-position xmaze)
    (draw-3d-xmaze xmaze)))

(defun update-xmaze-server-position (xmaze)
  (if (xmaze-server-stream xmaze)
      (let* ((current-client (xmaze-current-client xmaze)))
        (send-to-xmaze-connection-no-reply
         xmaze
         :position-and-locate
         (xmaze-client-colpos current-client)
         (xmaze-client-rowpos current-client)
         (xmaze-client-facing current-client)))))

(defun draw-blip-for-client (xmaze filled client)
  (capi:execute-with-interface
   (xmaze-top-level xmaze)
   'draw-blip
   xmaze filled
   (xmaze-client-colpos client)
   (xmaze-client-rowpos client)
   (xmaze-client-facing client)
   (xmaze-client-3d-color client)))

(defun xmaze-can-go-to (xmaze rowoffset coloffset)
  (eq :space
      (aref (xmaze-layout xmaze)
            (+ (xmaze-rowpos xmaze) rowoffset)
            (+ (xmaze-colpos xmaze) coloffset))))

(defun square-in-view (mz how-far-in-front how-far-to-right)
  (let* ((rowoffset (rowoffset (xmaze-facing mz)))
	 (coloffset (coloffset (xmaze-facing mz)))
	 (row (xmaze-rowpos mz))
	 (col (xmaze-colpos mz))
	 (erow (+ row (* how-far-in-front rowoffset)))
	 (ecol (+ col (* how-far-in-front coloffset)))
         (actualrow (+ erow (* how-far-to-right coloffset)))
         (actualcol (- ecol (* how-far-to-right rowoffset))))
    (values
     (aref (xmaze-layout mz) actualrow actualcol)
     (aref (xmaze-visited mz) actualrow actualcol))
    ;;(xmaze-rowpos mz) (* how-far-in-front rowoffset) (+ xmaze-colpos)
    ))

(defun draw-endwall (xmaze distance)
  (let* ((posn (aref *wallpos* distance))
         (width (- *maxsize* (* 2 posn)))
         )
    (my-draw-rectangle (xmaze-3d-gp xmaze)
                       posn posn
                       width width
                       (xmaze-solid-walls xmaze))))

(defun draw-floordot (xmaze distance)
  (when (xmaze-floordot xmaze)
    (let* ((posn (* 1/2 (+ (aref *wallpos* distance)
                           (aref *wallpos* (1+ distance)))))
           (width (- *maxsize* (* 2 posn)))
           )
      (my-draw-rectangle (xmaze-3d-gp xmaze)
                         (- 10 (/ width 8)) (- 20 posn) ;1
                         (/ width 4) 0 ;;width,height
                         nil ;(xmaze-solid-walls xmaze)
                         ))))

(defun draw-sidewall (xmaze distance side)
  ;;(setq *global-gp* (xmaze-3d-gp xmaze))
  (let ((this (aref *wallpos* distance))
        (next (aref *wallpos* (1+ distance))))
    (unless (eq side :left)
      (setq this (- *maxsize* this)
            next (- *maxsize* next)))
    (my-draw-line (xmaze-3d-gp xmaze) ;; top
		  this this
		  next next)
    (my-draw-line (xmaze-3d-gp xmaze) ;; bottom
		  this (- *maxsize* this)
		  next (- *maxsize* next))))

(defun draw-door (xmaze distance side)
  (let* ((left (eq side :left))
         (near (aref *wallpos* distance))
         (far (aref *wallpos* (1+ distance)))
         (realfar (if left far (- *maxsize* far)))
         (realnear (if left near (- *maxsize* near)))
         (leftmost (if left near realfar))
         )
    (unless (= 0 distance)
      (my-draw-line (xmaze-3d-gp xmaze)    ; vertical, near edge
		    realnear realnear
		    realnear (- *maxsize* realnear)))
    (my-draw-rectangle (xmaze-3d-gp xmaze) ; top (if left), horiz.
		       leftmost far
		       (- far near)
                       (- *maxsize* (* 2 far))
                       (xmaze-solid-walls xmaze))))

(defun draw-3d-xmaze-part (gp &optional x y width height)
  (declare (ignore gp x y width height))
  (when (boundp '*current-xmaze*)
    (draw-3d-xmaze *current-xmaze* t)))

(defun draw-3d-xmaze (xmaze &optional noclear)
  (let* ((gp (xmaze-3d-gp xmaze)))
    (let* ((corridor nil)
	   (end-of-corridor nil))
      (dotimes (i (1- *max-dist*))
	(unless end-of-corridor
          (multiple-value-bind 
              (ahead visited)
              (square-in-view xmaze (* i *move-by*)  0)
	    (push (list 
		   i
		   (square-in-view xmaze (* i *move-by*) -1)
                   ahead
		   (square-in-view xmaze (* i *move-by*) 1)
                   visited
		   )
		  corridor))
	  (unless (eq :space (square-in-view xmaze i 0))

	    (setq end-of-corridor t))))
      (setq corridor (nreverse corridor))
      (setf (xmaze-solid-walls xmaze)
            (member (xmaze-facing xmaze) '(:north :south)))
      (unless noclear (gp:clear-graphics-port gp))
      (dolist (l corridor)
	(let ((i (first l))
	      (to-left (second l))
	      (ahead (third l))
	      (to-right (fourth l)))
	  (if (eq :space ahead)
              (unless (= i (- *max-dist* 2))
	        (if (eq :space to-left)
	            (draw-door xmaze i :left)
	          (draw-sidewall xmaze i :left))
	        (if (eq :space to-right)
	            (draw-door xmaze i :right)
	          (draw-sidewall xmaze i :right))
                (when (fifth l) ; has been visited before
                    (draw-floordot xmaze i)
                    )
                )
	    (draw-endwall xmaze i))
	  )) 
      (gp:draw-string gp
		      (concatenate 'string
				   "FACING " (string (xmaze-facing xmaze)))
		      50 10
                      :foreground (xmaze-3d-color xmaze))
      )))



(defun 3dxmaze-input (self x y gesture mz)
  (declare (ignore self x y))
  (3dxmaze-callback (xmaze-top-level mz) mz
                   (list nil (sys::gesture-spec-data gesture))))

(defun make-3dxmaze-contact (&optional (mz *current-xmaze*))
  (setf (xmaze-rowpos mz) (xmaze-mid-row mz)
        (xmaze-colpos mz) (xmaze-mid-col mz)
        (xmaze-facing mz) :north)
  (let*
      ((callback
        #'(lambda (x interface) 
            (3dxmaze-callback (xmaze-top-level mz) mz (list interface x))))
       (ml (make-instance
            'capi:column-layout
            :gap 5
            :description (list
                          (setf (xmaze-3d-gp mz) 
                                (make-instance
                                 'capi:output-pane
                                 :display-callback #'(lambda (gp &optional x y w h)
                                                       (declare (ignore gp x y w h))
                                                       (draw-3d-xmaze mz t))
                                 :resize-callback #'(lambda (pane &rest args)
                                                      (declare (ignore args))
                                                      (gp:invalidate-rectangle pane)
                                                      )
                                 :background :black
                                 :foreground :white
                                 :input-model `((:up 3dxmaze-input ,mz)
                                                (:down 3dxmaze-input ,mz)
                                                (:left 3dxmaze-input ,mz)
                                                (:right 3dxmaze-input ,mz))))
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
                                        :title "3d-Xmaze"
                                        :width 320
                                        :height 380
                                        :create-callback
                                        #'(lambda (interface)
                                            (declare (ignore interface))
                                            (update-xmaze-server-position mz))))))
    (setf (xmaze-3d-tl mz) tl)
    (xmaze-3d-gp mz)))


(defun maze (&key (rows 30) (cols rows) (scale-factor 12) circle floordot server)
  (make-xmaze-internal rows cols scale-factor circle floordot server))

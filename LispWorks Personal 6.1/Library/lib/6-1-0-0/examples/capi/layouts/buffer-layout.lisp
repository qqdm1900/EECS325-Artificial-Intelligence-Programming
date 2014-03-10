;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/layouts:buffer-layout.lisp,v 1.3.12.1 2011/08/24 13:26:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;----------------------------------------------------------------------------
;;
;; buffer-layout.lisp
;;
;; A layout which positions its children in a row with alternate children aligned
;; at the top and buttom of the layout.  The width of each child is an equal
;; proportion of the width of the layout.  If any child does not fit its
;; constraints, its size is set to the appropriate constraint and the remaining
;; space is divided amongst the remaining children.  A gap of size THROW is placed
;; above/below each child in alternation.
;; 
;; 
;; 	         -------------------------------    -
;;    	        |	|	|	|	|   |
;; 	        |  c	|  gap	|  c	|  gap	| throw
;; 	        |  h	|	|  h	|	|   |
;; 	        |  i	|-------|  i	|-------|   -
;; 	        |  l	|	|  l	|	|
;; 	        |  d	|  c	|  d	|  c	|
;; 	        |	|  h	|	|  h	|
;; 	        |  1	|  i	|  3	|  i	|
;; 	        |	|  l	|	|  l	|
;;     -	|-------|  d	|-------|  d	|
;;     |	|	|	|	|	|
;;   throw	|  gap	|  2	|  gap	|  4	|
;;     |	|	|	|	|	|
;;     -	 -------------------------------
;;
;;----------------------------------------------------------------------------
;; To run this demo, compile and load this file and then execute:
;;
;;  (CL-USER::MAKE-BUFFER-LAYOUT)
;; 
;;----------------------------------------------------------------------------

(in-package "CL-USER")

(capi:define-layout buffer-layout ()
  ((throw :initarg :throw :initform 30)))


;; Parse each layout descriptor and return a list of the geometry objects.
(defmethod capi:interpret-description ((self buffer-layout) desc interface)
  (loop for child in desc
        collect (capi:parse-layout-descriptor child interface self)))

(defmacro do-all-children (&body body)
  `(capi:map-pane-children
    self
    #'(lambda (child)
        (capi:with-geometry child
          ,@body))))
          
;; Combine the constraints of the children to form the constraints of the
;; layout.
(defmethod capi:calculate-constraints ((self buffer-layout))
  (let ((total-min-width 0)
        (total-min-height 0)
        (total-max-width most-positive-fixnum)
        (total-max-width-t-p t)
        (total-max-height most-positive-fixnum)
        (throw (slot-value self 'throw)))

    ;; Compute the constraints from those of the children.
    (do-all-children

      ;; All the children must fit horizontally within the layout, so the
      ;; min-width of the layout is the sum of the min-widths of the chilren.
      (incf total-min-width capi:%min-width%)

      ;; Until we find a child with no max-width, sum the max-widths using the
      ;; min-width when the max-width is T.  Remember if all children have
      ;; max-width T.
      (when total-max-width
        (if capi:%max-width%
            (incf total-max-width
                  (if (eq capi:%max-width% t)
                      capi:%min-width%
                    (progn
                      (setq total-max-width-t-p nil)
                      capi:%max-width%)))
          (setq total-max-width nil)))

      ;; All the children must fit vertically within the layout so the
      ;; min-height of the layout is the maximum of the min-heights of the
      ;; children.
      (when (> capi:%min-height% total-min-height)
	(setq total-min-height capi:%min-height%))

      ;; Until we find a child with no max-height, the max-height of the layout
      ;; is the minimum of the max-heights of the children.
      (when total-max-height
	(if capi:%max-height%
	    (let ((max-height (if (eq capi:%max-height% t)
				  capi:%min-height%
				capi:%max-height%)))
	      (when (< max-height total-max-height)
		(setq total-max-height max-height)))
	  (setq total-max-height nil))))

    ;; Set the constraints.
    (capi:with-geometry self
      (setf ;; Min-width as summed above.
	    capi:%min-width% total-min-width

            ;; If any child has no max-width then the layout has no max-width,
            ;; otherwise if all children have max-width T, the layout has
            ;; max-width is T, otherwise max-width is the sum of the max-widths
            ;; of the children.
            capi:%max-width% (and total-max-width
                             (or total-max-width-t-p
                                 total-max-width))
            
            ;; Min-height as maximized above plus the required throw.
            capi:%min-height% (+ total-min-height throw)

            ;; Max-height is NIL if any child has no max-height, otherwise as
            ;; minimumize above plus the required throw.
            capi:%max-height% (and total-max-height
                              (+ total-max-height throw))))))


;; Set the position and size of the chilren based on the given position and
;; size of the layout.
(defmethod capi:calculate-layout ((self buffer-layout) x y width height)
  (let ((num-children 0))
  
  ;; Reset all the widths to flag the children as having no assigned width.
  (do-all-children
    (incf num-children)
    (setf capi:%width% nil))

  ;; Loop round until all widths have been assigned.  In each loop, children
  ;; with a non NIL width have will have had their width fixed in a previous
  ;; loop.  Eventually, no constraints are broken an all remaining children are
  ;; assigned an equal proportion of the remaining width.
  (loop named set-widths
	with split-width = width
	for split-count from num-children downto 1
        for child-width = (floor split-width split-count)
	do
	(block fixed-child

          ;; Remove any children which are fixed at their min-width or for
          ;; which CHILD-WIDTH is too large.
	  (do-all-children
	    (when (and (null capi:%width%)
                       (or (eq capi:%max-width% t)
			   (< child-width capi:%min-width%)))
	      (decf split-width (setf capi:%width% capi:%min-width%))
	      (return-from fixed-child)))

          ;; Remove any children for which CHILD-WIDTH is too small.
	  (do-all-children
	    (when (and capi:%max-width% (null capi:%width%) (> child-width capi:%max-width%))
	      (decf split-width (setf capi:%width% capi:%max-width%))
	      (return-from fixed-child)))

          ;; Assign CHILD-WIDTH to the remaining children
	  (do-all-children
	    (when (null capi:%width%)
	      (setf capi:%width% child-width)))

          ;; Exit now that all widths have been assigned.
	  (return-from set-widths)))

  (let* ((throw (slot-value self 'throw))
         (child-height (- height throw)) ;try to make all children this size
         (top-p t) ;start at the top
         (xpos x) ;start at the given x coord
         )
    ;; Position all children and set their heights.
    (do-all-children
      (let ((chosen-height (cond ((or (< child-height capi:%min-height%)
                                      (eq capi:%max-height% t))
                                  ;; use the min-height if required by the size
                                  ;; or by the max-height constraint
				  capi:%min-height%)
                                 ((or (null capi:%max-height%)
                                      (< child-height capi:%max-height%))
                                  ;; use the child-height if not too big
			          child-height)
                                 (t
                                  ;; use the max-height otherwise
                                  capi:%max-height%))))
        (setf capi:%height% chosen-height
              capi:%x% xpos
              capi:%y% (if top-p 
                      y ;at the top
		    (+ y (- height chosen-height)))) ;at the bottom
        (incf xpos capi:%width%) ;move across the layout
        (setq top-p (not top-p)) ;toggle between top and bottom aligned
        )))))

(defun make-buffer-layout ()
  (capi:contain 
   (make-instance 'buffer-layout 
                  :description (list (make-instance 'capi:output-pane)
                                     "a" "b" "c" "d"
                                     (make-instance 'capi:output-pane)
                                     "e"))))

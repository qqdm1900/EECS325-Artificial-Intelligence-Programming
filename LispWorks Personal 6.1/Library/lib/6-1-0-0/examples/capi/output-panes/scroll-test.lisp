;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/output-panes:scroll-test.lisp,v 1.3.1.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; ----------------------------------------------------------------------
;;;
;;; This example demonstrates OUTPUT-PANE scrolling.
;;; To try it, compile and load this file and then execute:
;;;
;;;      (CL-USER::TEST-SCROLLING-OUTPUT-PANE)
;;;
;;; ----------------------------------------------------------------------

(in-package "CL-USER")

#|
    OUTPUT-PANE test and illustration of:

      o usage of PANE-CAN-SCROLL == T & PANE-CAN-SCROLL == NIL
      o programatic scrolling
      o user specified initial data range, step and page size
      o programmatic setting of data range, step and page size
      o drag specific redisplay
      o drag scroll on the pane
      o drag scroll when outside of the visible area
      o input model
      o using graphics ports transforms to draw into the visible area

    Note the use of the following scroll parameters and functions.
    Their use in the code is highlighted by upcasing the symbols.

      o Initargs and hints in the interface definition for specifing initial values:
        - :SCROLL-START-X size hint for specifing the initial minimum data range value
        - :SCROLL-WIDTH size hint for specifing the initial width of the data
        - :SCROLL-INITIAL-X for specifing the initial scroll position
        - :SCROLL-HORIZONTAL-PAGE-SIZE/:SCROLL-HORIZONTAL-STEP-SIZE size hint for specifing the initial scroll increment
        - :PANE-CAN-SCROLL initarg for specifing that the pane will do its own drawing into
          the visible area according to scroll parameters
      o CAPI:SCROLL - programmatic control of scroll position and the passing of arbitrary
        user data thru to the scroll callback.
      o CAPI:SET-HORIZONTAL-SCROLL-PARAMETERS - programmatic control of scroll parameters.
      o CAPI:GET-HORIZONTAL-SCROLL-PARAMETERS - programmatic query of scroll parameters.
        - :slug-position (current scroll position)
        - :min-range (minimum data coordinate)
        - :max-range (maximum data coordinate)
        - :step-size (scroll step size)
        - :page-size (scroll page size)
      o CAPI:SIMPLE-PANE-VISIBLE-WIDTH/CAPI:SIMPLE-PANE-VISIBLE-HEIGHT for obtaining the
        current size of the visible area of the pane (i.e. the viewport size).
      o CAPI::COMPUTE-INTERNAL-SCROLL-POSITION for computing an X/Y scroll position from the
        abstract scroll operation and the current scroll position and increment parameters.
      o The scroll callback.
|#

;;; ======================================================================
;;;
;;; Base pane classes.
;;;
;;; ======================================================================

;;; Base pane class for all rulers...
(defclass ruler-pane (capi:output-pane)
  ()

  (:default-initargs
   :draw-with-buffer t
   :display-callback 'RULER-EXPOSE-CALLBACK
   :resize-callback 'RULER-RESIZE-CALLBACK

   :input-model `(((:button-1 :press)
                   ruler-button-input-callback
                   (:button-1 :press))
                  ((:button-1 :motion)
                   ruler-button-input-callback
                   (:button-1 :motion))
                  ((:button-1 :release)
                   ruler-button-input-callback
                   (:button-1 :release))
                  ((:button-2 :press)
                   ruler-button-input-callback
                   (:button-2 :press))
                  ((:button-2 :motion)
                   ruler-button-input-callback
                   (:button-2 :motion))
                  ((:button-2 :release)
                   ruler-button-input-callback
                   (:button-2 :release)))

   :background :white

   ;; Minimum size of the visible area.
   :visible-min-width 400
   :visible-min-height 100

   :horizontal-scroll t

   ;; Initial X range of the data: 50 -> 5000
   :SCROLL-START-X 50
   :SCROLL-WIDTH 4950

   :SCROLL-INITIAL-X 100

   ;; Scroll increments
   :SCROLL-HORIZONTAL-STEP-SIZE 1
   :SCROLL-HORIZONTAL-PAGE-SIZE 50

   :scroll-callback 'RULER-CAPI-DOES-SCROLL-CALLBACK

   ;; CAPI is responsible for scrolling over the data range.
   :PANE-CAN-SCROLL nil))

(defclass ruler-pane-does-scroll (ruler-pane)
  ()

  (:default-initargs

   :scroll-callback 'RULER-PANE-DOES-SCROLL-CALLBACK

    ;; Pane is responsible for drawing the proper data
    ;;  in the visible area according to scroll parameters.
    :PANE-CAN-SCROLL t))

;;; ----------------------------------------------------------------------

;;; Base pane class for all design panes.
(defclass design-pane (capi:output-pane)
  ()

  (:default-initargs
   :draw-with-buffer t
   :display-callback 'DESIGN-EXPOSE-CALLBACK
   :resize-callback 'DESIGN-RESIZE-CALLBACK

   :background :white

   ;; Minimum size of the visible area.
   :visible-min-width 400
   :visible-min-height 100

   :horizontal-scroll t

   ;; Initial X range of the data: 50 -> 5000
   :SCROLL-START-X 50
   :SCROLL-WIDTH 4950

   :scroll-callback 'DESIGN-CAPI-DOES-SCROLL-CALLBACK

   ;; CAPI is responsible for scrolling over the data range.
   :PANE-CAN-SCROLL nil))


(defclass design-pane-does-scroll (design-pane)
  ()

  (:default-initargs

   :scroll-callback 'DESIGN-PANE-DOES-SCROLL-CALLBACK

   ;; Pane is responsible for drawing the proper data
   ;;  in the visible area according to scroll parameters.
   :PANE-CAN-SCROLL t))


;;; ======================================================================
;;;
;;; Main interface definition.
;;;
;;; ======================================================================

(capi:define-interface scrolling-output-pane  ()

  ;; Interface state variables.
  ((ruler-last-x
    :initform nil
    :accessor ruler-last-x)
   (ruler-last-scroll-operation
    :initform nil
    :accessor ruler-last-scroll-operation)

   (design-last-x
    :initform nil
    :accessor design-last-x)
   (design-last-scroll-operation
    :initform nil
    :accessor design-last-scroll-operation)

   ;; Drag scroll state parameters.
   (scroll-process
    :initform nil)
   (scroll-params
    :initform nil))

  (:panes

   (ruler-pane-does-scroll
    ruler-pane-does-scroll
    :reader ruler-pane-does-scroll)

   (ruler-capi-does-scroll
    ruler-pane
    :reader ruler-capi-does-scroll)

   (ruler-message
    capi:display-pane
    :reader ruler-message

    :min-height '(:character 2)

    ;; So display-pane width resizes with interface.
    :max-width nil)

   (design-pane-does-scroll
    design-pane-does-scroll
    :reader design-pane-does-scroll)

   (design-capi-does-scroll
    design-pane
    :reader design-capi-does-scroll)

   (design-message
    capi:display-pane
    :reader design-message

    :min-height '(:character 2)

    ;; So display-pane width resizes with interface.
    :max-width nil)

   ;; Scroll parameters...

   (scroll-position
    capi:display-pane
    :reader scroll-position
    :title "Scroll Position"
    :title-position :left
    :max-width nil)
   (range-min
    capi:text-input-pane
    :reader range-min
    :title "Range Minimum"
    :callback 'range-min-callback)
   (range-max
    capi:text-input-pane
    :reader range-max
    :title "Range Maximum"
    :callback 'range-max-callback)
   (ruler-step-size
    capi:text-input-pane
    :reader ruler-step-size
    :title "Ruler Step Size"
    :callback 'ruler-step-size-callback)
   (ruler-page-size
    capi:text-input-pane
    :reader ruler-page-size
    :title "Ruler Page Size"
    :callback 'ruler-page-size-callback)
   (design-step-size
    capi:text-input-pane
    :reader design-step-size
    :title "Design Step Size"
    :callback 'design-step-size-callback)
   (design-page-size
    capi:text-input-pane
    :reader design-page-size
    :title "Design Page Size"
    :callback 'design-page-size-callback)
   )

  (:layouts
   (scroll-parameters
    capi:column-layout
    '(scroll-position range-min range-max ruler-page-size ruler-step-size design-page-size design-step-size)
    :title "Scroll Parameters"
    ;;:title-position :frame
    )

   ;; Rulers...

   (ruler-pane-does-scroll-area
    capi:column-layout
    '(ruler-pane-does-scroll ruler-message)
    :title "Scroll Ruler (pane does scrolling)"
    ;;:title-position :frame
    )

   (ruler-capi-does-scroll-area
    capi:column-layout
    '(ruler-capi-does-scroll ruler-message)
    :title "Scroll Ruler (CAPI does scrolling)"
    ;;:title-position :frame
    )

   (ruler-pane-does-scroll-layout
    capi:column-layout
    '(scroll-parameters ruler-pane-does-scroll-area))

   (ruler-capi-does-scroll-layout
    capi:column-layout
    '(scroll-parameters ruler-capi-does-scroll-area))

   ;; Designs...

   (design-pane-does-scroll-layout
    capi:column-layout
    '(design-pane-does-scroll design-message)
    :title "Design Area (pane does scrolling)"
    ;;:title-position :frame
    )

   (design-capi-does-scroll-layout
    capi:column-layout
    '(design-capi-does-scroll design-message)
    :title "Design Area (CAPI does scrolling)"
    ;;:title-position :frame
    )

   ;; The top level views...

   (pane-does-scroll-view
    capi:column-layout
    '(ruler-pane-does-scroll-layout design-pane-does-scroll-layout)
    :reader pane-does-scroll-view
    :initial-focus 'ruler-pane-does-scroll)

   (capi-does-scroll-view
    capi:column-layout
    '(ruler-capi-does-scroll-layout design-capi-does-scroll-layout)
    :reader capi-does-scroll-view
    :initial-focus 'ruler-capi-does-scroll)

   ;; The main switchable layout for switching between the types of OUTPUT-PANEs.

   (main-layout
    capi:switchable-layout
    '(pane-does-scroll-view capi-does-scroll-view)
    :reader main-layout
    :visible-child 'pane-does-scroll-view)

   ;; A tab layout for visual mechanism to change the view.

   (tab-layout
    capi:tab-layout
    '(main-layout)
    :items '(pane-does-scroll-view capi-does-scroll-view)
    :print-function 'print-tab-layout-item
    :selection-callback 'select-layout-from-tab-layout-item))

  (:default-initargs
   :title "Scrolling OUTPUT-PANE"
   :layout 'tab-layout))

;;; ----------------------------------------------------------------------

(defun test-scrolling-output-pane (&rest options)
  (apply 'capi:display (make-instance 'scrolling-output-pane) options))

;;; ----------------------------------------------------------------------

(defmethod capi:interface-display :after ((interface scrolling-output-pane))
  ;; Set proper initial values for display once parameters have been computed.
  (update-scroll-parameter-display interface))

;;; ======================================================================
;;;
;;; Switchable layout manipulation.
;;;
;;; ======================================================================

(defun print-tab-layout-item (item)
  (ecase item
    (pane-does-scroll-view "Pane Does Scroll")
    (capi-does-scroll-view "CAPI Does Scroll")))

(defmethod select-layout-from-tab-layout-item (item (interface scrolling-output-pane))
  (setf (current-view interface) item))

(defmethod current-view ((interface scrolling-output-pane))
  (let ((child (capi:switchable-layout-visible-child (main-layout interface))))
    (unless (symbolp child)
      (setf child (capi:capi-object-name child)))
    child))

;;; Change the view.
(defmethod (setf current-view) (new-view (interface scrolling-output-pane))
  (setf (capi:switchable-layout-visible-child (main-layout interface))
        (ecase new-view
          (pane-does-scroll-view (pane-does-scroll-view interface))
          (capi-does-scroll-view (capi-does-scroll-view interface))))
  (update-scroll-parameter-display interface))

;;; ======================================================================
;;;
;;; Ruler and design pane readers.
;;; Return the appropriate pane as a function of the current layout.
;;;
;;; ======================================================================

(defmethod ruler ((interface scrolling-output-pane))
  (ecase (current-view interface)
    (pane-does-scroll-view (ruler-pane-does-scroll interface))
    (capi-does-scroll-view (ruler-capi-does-scroll interface))))

(defmethod design ((interface scrolling-output-pane))
  (ecase (current-view interface)
    (pane-does-scroll-view (design-pane-does-scroll interface))
    (capi-does-scroll-view (design-capi-does-scroll interface))))

;;; ======================================================================
;;;
;;; Message pane handlers.
;;; Display messages in the appropriate pane.
;;;
;;; ======================================================================

(defun display-message (message-pane message-format message-format-args)
  (flet ((string-to-string-list (string) ;convert NewLine separated string into list of strings
            (loop with strings = nil
                  with start = 0
                  with length = (length string)
                  for end = (position #\newline string :start start)
                  do
                  (cond (end
                         (push (subseq string start end) strings))
                        (t
                         ;; Don't collect a trailing newline.
                         (unless (= start length)
                           (push (subseq string start) strings))
                         (return (nreverse strings))))
                  (setf start (1+ end)))))

    (setf (capi:display-pane-text message-pane)
          (string-to-string-list (apply #'format nil message-format message-format-args)))))

(defun ruler-display-message (interface message-format &rest message-format-args)
  (display-message (ruler-message interface) message-format message-format-args))

(defun design-display-message (interface message-format &rest message-format-args)
  (display-message (design-message interface) message-format message-format-args))

;;; ======================================================================
;;;
;;; Automatic drag scroll when outside of the visible area...
;;;
;;; ======================================================================

(defun start-scroll (pane scroll-dimension scroll-value scroll-operation)
  (with-slots (scroll-process scroll-params) (capi:element-interface pane)
    (let ((current-scroll-params (list scroll-dimension scroll-value scroll-operation)))

      ;; Take care to make a new process only when the parameters have changed...
      (when (or (null scroll-process)
                (not (equal scroll-params current-scroll-params)))

        (when scroll-process
          (mp:process-kill scroll-process))
        (setf scroll-process (mp:process-run-function "Drag Scroller"
                                                      nil
                                                      'scroll-top-level
                                                      pane scroll-dimension scroll-value scroll-operation))
        (setf scroll-params current-scroll-params)))))

(defun stop-scroll (pane)
  (with-slots (scroll-process) (capi:element-interface pane)
    (when scroll-process
      (mp:process-kill scroll-process)
      (setf scroll-process nil))))

(defmacro with-periodic-invocation ((&key (interval 500)) &body body)
  `(invoke-with-periodic-invocation ,interval #'(lambda () ,@body)))

(defun invoke-with-periodic-invocation (millisecond-interval closure)
  (flet ((timer-callback ()
           (funcall closure)))
    (let ((timer (mp:make-timer #'timer-callback)))
      (unwind-protect
          (progn
            ;; Schedule the repetitive timer.
            (mp:schedule-timer-relative-milliseconds timer millisecond-interval millisecond-interval)
            ;; Wait forever...
            (mp:process-wait "Periodic wait" #'(lambda () nil)))
        (when timer
          (mp:unschedule-timer timer))))))

(defun scroll-top-level (pane scroll-dimension scroll-value scroll-operation)
  ;; Queue scroll messages to the interface process on a periodic basis.
  (with-periodic-invocation (:interval 100)
    (capi:apply-in-pane-process pane 'CAPI:SCROLL pane scroll-dimension scroll-operation scroll-value)))

;;; ======================================================================
;;;
;;; Scroll parameters display.
;;;
;;; ======================================================================

(defun range-min-callback (text interface)
  (let ((ruler (ruler interface))
        (design (design interface))
        ;; T or number
        (new-value (read-from-string text)))

    (CAPI:SET-HORIZONTAL-SCROLL-PARAMETERS ruler :min-range new-value)
    (CAPI:SET-HORIZONTAL-SCROLL-PARAMETERS design :min-range new-value)

    ;; Redisplay panes.
    (ruler-redraw ruler)
    (design-redraw design)
    (update-scroll-parameter-display interface)))

(defun range-max-callback (text interface)
  (let ((ruler (ruler interface))
        (design (design interface))
        ;; T or number
        (new-value (read-from-string text)))

    (CAPI:SET-HORIZONTAL-SCROLL-PARAMETERS ruler :max-range new-value)
    (CAPI:SET-HORIZONTAL-SCROLL-PARAMETERS design :max-range new-value)

    ;; Redisplay panes.
    (ruler-redraw ruler)
    (design-redraw design)
    (update-scroll-parameter-display interface)))

;;; ----------------------------------------------------------------------

(defun ruler-page-size-callback (text interface)
  (let ((ruler (ruler interface))
        (new-value (parse-integer text)))

    (CAPI:SET-HORIZONTAL-SCROLL-PARAMETERS ruler :page-size new-value)))

(defun ruler-step-size-callback (text interface)
  (let ((ruler (ruler interface))
        (new-value (parse-integer text)))

    (CAPI:SET-HORIZONTAL-SCROLL-PARAMETERS ruler :step-size new-value)))

;;; ----------------------------------------------------------------------

(defun design-page-size-callback (text interface)
  (let ((design (design interface))
        (new-value (parse-integer text)))

    (CAPI:SET-HORIZONTAL-SCROLL-PARAMETERS design :page-size new-value)))

(defun design-step-size-callback (text interface)
  (let ((design (design interface))
        (new-value (parse-integer text)))

    (CAPI:SET-HORIZONTAL-SCROLL-PARAMETERS design :step-size new-value)))

;;; ----------------------------------------------------------------------

;;; Update all scroll parameters.
(defun update-scroll-parameter-display (interface)

  (multiple-value-bind (slug-position range-min range-max page-size step-size)
      (CAPI:GET-HORIZONTAL-SCROLL-PARAMETERS (ruler interface)
                                             :slug-position
                                             :min-range :max-range
                                             :page-size :step-size)

    (update-scroll-position-display interface slug-position)
    (update-scroll-range-min-display interface range-min)
    (update-scroll-range-max-display interface range-max)
    (update-scroll-ruler-page-size-display interface page-size)
    (update-scroll-ruler-step-size-display interface step-size))

  (multiple-value-bind (page-size step-size)
      (CAPI:GET-HORIZONTAL-SCROLL-PARAMETERS (design interface) :page-size :step-size)

    (update-scroll-design-page-size-display interface page-size)
    (update-scroll-design-step-size-display interface step-size)))

(defun update-scroll-position-display (interface x)
  (setf (capi:display-pane-text (scroll-position interface))
        (format nil "~d" x)))

(defun update-scroll-range-min-display (interface range-min)
  (setf (capi:text-input-pane-text (range-min interface))
        (format nil "~d" range-min)))

(defun update-scroll-range-max-display (interface range-max)
  (setf (capi:text-input-pane-text (range-max interface))
        (format nil "~d" range-max)))

(defun update-scroll-ruler-page-size-display (interface page-size)
  (setf (capi:text-input-pane-text (ruler-page-size interface))
        (format nil "~d" page-size)))

(defun update-scroll-ruler-step-size-display (interface step-size)
  (setf (capi:text-input-pane-text (ruler-step-size interface))
        (format nil "~d" step-size)))

(defun update-scroll-design-page-size-display (interface page-size)
  (setf (capi:text-input-pane-text (design-page-size interface))
        (format nil "~d" page-size)))

(defun update-scroll-design-step-size-display (interface step-size)
  (setf (capi:text-input-pane-text (design-step-size interface))
        (format nil "~d" step-size)))

;;; ======================================================================
;;;
;;; Ruler...
;;;
;;; ======================================================================

(defvar *debug-ruler* nil)

;;; Draw the ruler from (lx, ly) to (lx+width, ly+height)
;;; Ruler ticks are drawn every 50 units.
(defun ruler-draw (pane lx ly width height &key dragp nearest-rule)

  ;; Adjust so that we draw starting at the nearest rule to the left.
  ;; This makes sure that we redraw a partially exposed label or rule.
  (when nearest-rule
    (setf width (+ width (mod lx 50)))
    (setf lx (- lx (mod lx 50))))

  (loop for x from lx to (+ lx width)
        when (zerop (mod x 50))
        do
        (gp:draw-line pane x ly x (+ ly height) :dashed dragp)
        (gp:draw-string pane (format nil "~d" x) x 10 :block dragp :background :grey)))

;;; Redraw the ruler in the visible area of PANE.
(defun ruler-redraw (pane &key dragp expose-x expose-width)
  (if (CAPI::PANE-CAN-SCROLL pane)
      (ruler-redraw-pane-does-scroll pane dragp expose-x expose-width)
    (ruler-redraw-capi-does-scroll pane dragp expose-x expose-width)))

(defun ruler-redraw-pane-does-scroll (pane dragp expose-x expose-width)
  (let (
        ;; Data coordinates at viewport origin.
        (x (CAPI:GET-HORIZONTAL-SCROLL-PARAMETERS pane :slug-position))
        ;; No vertical scroll
        (y 0)

        (visible-width (CAPI:SIMPLE-PANE-VISIBLE-WIDTH pane))
        (visible-height (CAPI:SIMPLE-PANE-VISIBLE-HEIGHT pane)))

    ;; Clear the visible area background.
    ;; If its already damaged, forget it.
    (unless (and expose-x expose-width)
      (gp:clear-rectangle pane 0 0 visible-width visible-height))

    ;; Apply scroll position transform so that the data coordinates
    ;; at the origin of the viewport are drawn at the upper left
    ;; corner of the visible area.
    (gp:with-graphics-translation (pane (- x) (- y))

      ;; Redraw damaged area, or full visible area.
      (if (and expose-x expose-width)
          (ruler-draw pane expose-x y expose-width visible-height :nearest-rule t)
        (ruler-draw pane x y visible-width visible-height :dragp dragp)))))

(defun ruler-redraw-capi-does-scroll (pane dragp expose-x expose-width)
  (let (
        ;; Data coordinates at viewport origin.
        (x (CAPI:GET-HORIZONTAL-SCROLL-PARAMETERS pane :slug-position))
        ;; No vertical scroll so capi:%scroll-y% is NIL.
        (y 0)

        (visible-width (CAPI:SIMPLE-PANE-VISIBLE-WIDTH pane))
        (visible-height (CAPI:SIMPLE-PANE-VISIBLE-HEIGHT pane)))

    ;; Clear the visible area background.
    ;; If its already damaged, forget it.
    (unless (and expose-x expose-width)
      (gp:clear-rectangle pane 0 0 visible-width visible-height))

    ;; Redraw damaged area, or full visible area.
    (if (and expose-x expose-width)
        (ruler-draw pane expose-x y expose-width visible-height :nearest-rule t)
      (ruler-draw pane x y visible-width visible-height :dragp dragp))))
     
;;; The coordinates define a rectangle for the damaged area
;;;  from (expose-x, expose-y) to (expose-x+expose-width, expose-y+expose-height).
;;; Coordinates are user coordinates of the scrolled area exposed,
;;;  not parent relative pixel coordinates.
(defun RULER-EXPOSE-CALLBACK (pane expose-x expose-y expose-width expose-height)

  (ruler-display-message (capi:element-interface pane)
                         "Expose: X/Y: ~s/~s Width/Height: ~s/~s"
                         expose-x expose-y expose-width expose-height)

  (when *debug-ruler*
    (format *terminal-io* "~&Ruler Expose: ~s~%~4tX/Y: ~s/~s~%~4tWidth/height: ~s/~s~%"
            pane expose-x expose-y expose-width expose-height))

  ;; Redraw the damaged area.
  (ruler-redraw pane :expose-x expose-x :expose-width expose-width))

;;; The coordinates define a rectangle of the resized area from
;;;  (x, y) to (x+width, y+height).
;;; Coordinates are user coordinates of the scrolled area in the visible window.
(defun RULER-RESIZE-CALLBACK (pane x y width height)

  (ruler-display-message (capi:element-interface pane)
                         "Resize: X/Y: ~s/~s Width/height: ~s/~s"
                         x y width height)

  (when *debug-ruler*
    (format *terminal-io* "~&Ruler Resize: ~s~%~4tX/Y: ~s/~s~%~4tWidth/height: ~s/~s~%"
            pane x y width height)))

;;; When ruler implements its own scroll to viewport...
(defun RULER-PANE-DOES-SCROLL-CALLBACK (pane scroll-dimension scroll-operation scroll-value
                                             &key
                                             ;; T iff invoked as a result of scroll bar input
                                             interactive
                                             ;; Remaining are user defined.
                                             (do-other-scroll t)
                                             &allow-other-keys)

  ;; We could do this computation ourselves, but use the CAPI mechanism
  ;;  for computing an X/Y position according to the step and page size
  ;;  constrained to be within the data range.
  (multiple-value-bind (new-x new-y)
      (CAPI::COMPUTE-INTERNAL-SCROLL-POSITION pane scroll-dimension scroll-operation scroll-value)
    (declare (ignore new-y))

    (let ((interface (capi:element-interface pane)))

      (ruler-display-message interface
                             "~a ~s ~s ~s (new position: ~s)"
                             (if interactive "User Scroll" "Programatic Scroll")
                             scroll-dimension scroll-operation scroll-value new-x)

      (when *debug-ruler*
        (format *terminal-io* "~&Ruler Scroll: ~s~%~4tDimension: ~s~%~4tOperation: ~s~%~4tValue: ~s~%~4tComputed Position: ~s"
                pane scroll-dimension scroll-operation scroll-value new-x))

      ;; No need to do anything if the position hasn't changed.
      (when (or (eq (ruler-last-scroll-operation interface) :drag) ;dropped into place
                (null (ruler-last-x interface))
                (not (eql (ruler-last-x interface) new-x)))

        (setf (ruler-last-x interface) new-x)
        (setf (ruler-last-scroll-operation interface) scroll-operation)

        ;; This updates the internal coordinates as well as the scroll bar.
        (CAPI:SET-HORIZONTAL-SCROLL-PARAMETERS pane :slug-position new-x)

        (update-scroll-position-display interface new-x)

        ;; Redraw the entire visible area with the proper data.
        (ruler-redraw pane :dragp (eq scroll-operation :drag))

        ;; Track ruler position in the design.
        ;; Since there are differing step and page sizes, do a move to the computed coordinate.
        (when do-other-scroll
          (CAPI:SCROLL (design interface) :horizontal :move new-x :do-other-scroll nil))))))

;;; When ruler relies on CAPI to scroll to viewport.
(defun RULER-CAPI-DOES-SCROLL-CALLBACK (pane scroll-dimension scroll-operation scroll-value
                                             &key
                                             ;; T iff invoked as a result of scroll bar input
                                             interactive
                                             ;; Remaining are user defined.
                                             (do-other-scroll t)
                                             &allow-other-keys)

  (let ((interface (capi:element-interface pane))
        (new-x (CAPI:GET-HORIZONTAL-SCROLL-PARAMETERS pane :slug-position)))

    (ruler-display-message interface
                           "~a ~s ~s ~s (new position: ~s)"
                           (if interactive "User Scroll" "Programatic Scroll")
                           scroll-dimension scroll-operation scroll-value new-x)

    (when *debug-ruler*
      (format *terminal-io* "~&Ruler Scroll: ~s~%~4tDimension: ~s~%~4tOperation: ~s~%~4tValue: ~s~%~4tComputed Position: ~s"
              pane scroll-dimension scroll-operation scroll-value new-x))

    ;; No need to do anything if the position hasn't changed.
    (when (or (eq (ruler-last-scroll-operation interface) :drag) ;dropped into place
              (null (ruler-last-x interface))
              (not (eql (ruler-last-x interface) new-x)))

      (setf (ruler-last-x interface) new-x)
      (setf (ruler-last-scroll-operation interface) scroll-operation)

      (update-scroll-position-display interface new-x)

      ;; User applications could just blindly redraw the entire range and CAPI will
      ;;  ensure that the proper clip to viewport is done.
      ;; Here we rely on the expose callback for redrawing proper area.
      ;;(ruler-redraw pane (eq scroll-operation :drag))

      ;; Track ruler position in the design.
      ;; Since there are differing step and page sizes, do a move to the computed coordinate.
      (when do-other-scroll
        (CAPI:SCROLL (design interface) :horizontal :move new-x :do-other-scroll nil)))))

(defun ruler-button-input-callback (pane x y gesture)
  (let ((interface (capi:element-interface pane)))

    (destructuring-bind (button action) gesture
      (case button

        ;; Pointer drag scroll.
        (:button-1
         (case action
           (:press
            (capi::start-pane-drag-operation pane x y))
           (:motion
            (capi::pane-drag-operation pane x y))
           (:release
            (capi::end-pane-drag-operation pane x y))))

        ;; Drag scroll outside of visible area.
        (:button-2
         (case action
           (:press
            (ruler-display-message interface "Drag pointer outside of visible area to scroll."))
           (:motion
            ;; Start repetitive scroll step operations as long as we are outside of
            ;;  the visible area.
            (let ((position (CAPI:GET-HORIZONTAL-SCROLL-PARAMETERS pane :slug-position)))
              (cond ((< x position)
                     (start-scroll pane :horizontal -1 :step))
                    ((> x (+ position (CAPI:SIMPLE-PANE-VISIBLE-WIDTH pane)))
                     (start-scroll pane :horizontal 1 :step))
                    (t
                     (stop-scroll pane)))))
           (:release
            (stop-scroll pane)
            (ruler-display-message interface ""))))))))

;;; ======================================================================
;;;
;;; Design...
;;;
;;; ======================================================================

(defvar *debug-design* nil)

;;; This should be in GP...
(defun draw-x-y-adjusted-string (port string x y &rest options &key (x-adjust :left) (y-adjust :bottom) &allow-other-keys)
  (multiple-value-bind (left top right bottom)
      (gp:get-string-extent port string)

    (let ((width (- right left))
          (height (- bottom top)))
      (without-properties (draw-options options :x-adjust :y-adjust)
        (apply 'gp:draw-string
               port
	       string
	       (- x (ecase x-adjust
		      (:left 0)
		      ((:centre :center) (floor width 2))
		      (:right width)))
	       (+ y (ecase y-adjust
                      (:top height)
                      ((:centre :center) (floor height 2))
                      (:bottom 0)))
	       draw-options)))))

(defun design-draw (pane lx ly width height &key dragp )


  (let* ((radius 5)
        (y-offset 2)
        (x-margin (max radius (gp:port-string-width pane "00")))
        (first-site (* 50 (ceiling (- lx x-margin) 50)))
        (x-limit (+ lx width x-margin))
        ;; The callers always call this function with the full height, so these
        ;; calculatios are just preted. 
        (y-string-margin (+ radius y-offset (gp:port-string-height pane "00")))
        (first-y (* 50 (ceiling (- ly y-string-margin) 50)))
        (y-limit  (+ ly height radius)))

        

    (loop for x from first-site below x-limit by 50

          do
          (loop for y from first-y below y-limit by 50
	        do
                (let ((fillp (oddp (floor (+ x y) 50))))
                  (if dragp
                      (gp:draw-rectangle pane x y radius radius :filled fillp)
                    (gp:draw-circle pane x y radius :filled fillp))
                  (unless fillp
                    ;; Draw label at the top of the circle.
                    (draw-x-y-adjusted-string pane
                                              (format nil "~d" x)
                                              x (- y radius y-offset)
                                              :x-adjust :center :y-adjust :bottom)))))))

(defun design-redraw (pane &key dragp expose-x expose-width)
  (if (CAPI::PANE-CAN-SCROLL pane)
      (design-redraw-pane-does-scroll pane dragp expose-x expose-width)
    (design-redraw-capi-does-scroll pane dragp expose-x expose-width)))

(defun design-redraw-pane-does-scroll (pane dragp expose-x expose-width)
  (let (
        ;; Data coordinates at viewport origin.
        (x (CAPI:GET-HORIZONTAL-SCROLL-PARAMETERS pane :slug-position))
        ;; No vertical scroll.
        (y 0)

        (visible-width (CAPI:SIMPLE-PANE-VISIBLE-WIDTH pane))
        (visible-height (CAPI:SIMPLE-PANE-VISIBLE-HEIGHT pane)))

    ;; Clear the visible area background.
    ;; If its already damaged, forget it.
    (unless (and expose-x expose-width)
      (gp:clear-rectangle pane 0 0 visible-width visible-height))

    ;; Apply scroll position transform so that the data coordinates
    ;; at the origin of the viewport are drawn at the upper left
    ;; corner of the visible area.
    (gp:with-graphics-translation (pane (- x) (- y))

      ;; Redraw damaged area, or full visible area.
      (if (and expose-x expose-width)
          (design-draw pane expose-x y expose-width visible-height)
        (design-draw pane x y visible-width visible-height :dragp dragp)))))

(defun design-redraw-capi-does-scroll (pane dragp expose-x expose-width)
  (let (
        ;; Data coordinates at viewport origin.
        (x (CAPI:GET-HORIZONTAL-SCROLL-PARAMETERS pane :slug-position))
        ;; No vertical scroll.
        (y 0)

        (visible-width (CAPI:SIMPLE-PANE-VISIBLE-WIDTH pane))
        (visible-height (CAPI:SIMPLE-PANE-VISIBLE-HEIGHT pane)))

    ;; Clear the visible area background.
    ;; If its already damaged, forget it.
    (unless (and expose-x expose-width)
      (gp:clear-rectangle pane 0 0 visible-width visible-height))

    ;; Redraw damaged area, or full visible area.
    (if (and expose-x expose-width)
        (design-draw pane expose-x y expose-width visible-height)
      (design-draw pane x y visible-width visible-height :dragp dragp))))

(defun DESIGN-EXPOSE-CALLBACK (pane expose-x expose-y expose-width expose-height)

  (design-display-message (capi:element-interface pane)
                          "Expose: X/Y: ~s/~s Width/Height: ~s/~s"
                          expose-x expose-y expose-width expose-height)

  (when *debug-design*
    (format *terminal-io* "~&Design Expose: ~s~%~4tX/Y: ~s/~s~%~4tWidth/height: ~s/~s~%"
            pane expose-x expose-y expose-width expose-height))

  ;; Redraw the damaged area.
  (design-redraw pane :expose-x expose-x :expose-width expose-width))

(defun DESIGN-RESIZE-CALLBACK (pane x y width height)

  (design-display-message (capi:element-interface pane)
                          "Resize: X/Y: ~s/~s Width/height: ~s/~s"
                          x y width height)

  (when *debug-design*
    (format *terminal-io* "~&Design Resize: ~s~%~4tX/Y: ~s/~s~%~4tWidth/height: ~s/~s~%"
            pane x y width height)))

(defun DESIGN-PANE-DOES-SCROLL-CALLBACK (pane scroll-dimension scroll-operation scroll-value
                                              &key
                                              ;; T iff invoked as a result of scroll bar input
                                              interactive
                                              ;; Remaining are user defined.
                                              (do-other-scroll t)
                                              &allow-other-keys)

  ;; We could do this computation ourselves, but use the CAPI mechanism
  ;;  for computing an X/Y position according to the step and page size
  ;;  constrained to be within the data range.
  (multiple-value-bind (new-x new-y)
      (CAPI::COMPUTE-INTERNAL-SCROLL-POSITION pane scroll-dimension scroll-operation scroll-value)
    (declare (ignore new-y))

    (let ((interface (capi:element-interface pane)))

      (design-display-message interface
                              "~a: ~s ~s ~s ~s~%"
                              (if interactive "User Scroll" "Programatic Scroll")
                              scroll-dimension scroll-operation scroll-value new-x)

      (when *debug-design*
        (format *terminal-io* "~&Design Scroll: ~s~%~4tDimension: ~s~%~4tOperation: ~s~%~4tValue: ~s~%~4tComputed Position: ~s"
                pane scroll-dimension scroll-operation scroll-value new-x))

      (when (or (eq (design-last-scroll-operation interface) :drag) ;dropped into place
                (null (design-last-x interface))
                (not (eql (design-last-x interface) new-x)))

        (setf (design-last-x interface) new-x)
        (setf (design-last-scroll-operation interface) scroll-operation)

        ;; This updates the internal coordinates as well as the scroll bar.
        (capi:set-horizontal-scroll-parameters pane :slug-position new-x)

        ;; Redraw the entire visible area with the proper data.
        (design-redraw pane :dragp (eq scroll-operation :drag))

        ;; Track ruler position in the design.
        ;; Since there are differing step and page sizes, do a move to the computed coordinate.
        (when do-other-scroll
          (CAPI:SCROLL (ruler interface) :horizontal :move new-x :do-other-scroll nil))))))

(defun DESIGN-CAPI-DOES-SCROLL-CALLBACK (pane scroll-dimension scroll-operation scroll-value
                                              &key
                                              ;; T iff invoked as a result of scroll bar input
                                              interactive
                                              ;; Remaining are user defined.
                                              (do-other-scroll t)
                                              &allow-other-keys)

  (let ((interface (capi:element-interface pane))
        (new-x (CAPI:GET-HORIZONTAL-SCROLL-PARAMETERS pane :slug-position)))

    (design-display-message interface
                            "~a: ~s ~s ~s ~s~%"
                            (if interactive "User Scroll" "Programatic Scroll")
                            scroll-dimension scroll-operation scroll-value new-x)

    (when *debug-design*
      (format *terminal-io* "~&Design Scroll: ~s~%~4tDimension: ~s~%~4tOperation: ~s~%~4tValue: ~s~%~4tComputed Position: ~s"
              pane scroll-dimension scroll-operation scroll-value new-x))

    (when (or (eq (design-last-scroll-operation interface) :drag) ;dropped into place
              (null (design-last-x interface))
              (not (eql (design-last-x interface) new-x)))

      (setf (design-last-x interface) new-x)
      (setf (design-last-scroll-operation interface) scroll-operation)

      ;; User applications can just blindly redraw the entire range and CAPI will
      ;;  ensure that the proper clip to viewport is done.
      ;; Here we rely on the expose callback for redrawing proper area.
      ;;(design-redraw pane :dragp (eq scroll-operation :drag))

      ;; Track ruler position in the design.
      ;; Since there are differing step and page sizes, do a move to the computed coordinate.
      (when do-other-scroll
        (CAPI:SCROLL (ruler interface) :horizontal :move new-x :do-other-scroll nil)))))

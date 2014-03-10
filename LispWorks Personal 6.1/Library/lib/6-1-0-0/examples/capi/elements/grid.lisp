;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:grid.lisp,v 1.18.1.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;; ----------------------------------------------------------------------
;;;
;;; An example using the grid widget (see grid-impl.lisp).
;;;
;;; Load the grid-impl.lisp first and then run using 
;;;
;;;    (CAPI:DISPLAY (MAKE-INSTANCE 'EMPLOYEES-INTERFACE))
;;;
;;; ----------------------------------------------------------------------

(unless (find-class 'grid-widget nil)
  (cerror "Load grid-impl.lisp later."
          "The file grid-impl.lisp must be loaded to make this example work."))


;;; ******************************************************************
;;;                     Example code
;;; ******************************************************************

(defclass employee ()
    ;; The user domain object for an employee grid,
    ;; whose values will be represented in a grid row.
    (
     ;; The name of the employee
     (name :initarg :name
        :reader employee-name
        :writer set-employee-name
        :initform nil)
     
     ;; Whether the employee works full-time
     (full-time-p :initarg :full-time-p
        :reader employee-full-time-p
        :writer set-employee-full-time-p
        :initform nil)
     
     ;; Current department
     (department :initarg :department
        :reader employee-department
        :writer set-employee-department
        :initform nil)
     
     ;; Departments to choose from
     (departments :initarg :departments
        :accessor departments
        :initform (list :tech :sales :marketing :management
                     :admin :none_of_the_above))
     
     ;; Current bar color
     (bar-color :initarg :bar-color
        :reader employee-bar-color
        :writer set-employee-bar-color
        :initform :red)
     
     ;; Bar colors to choose from
     (bar-colors :initarg :bar-colors
        :reader employee-bar-colors
        :writer set-employee-bar-colors
        :initform '(:red :yellow :green :blue))
     
     ;; Job Description
     (duties :initarg :duties
        :reader employee-duties
        :writer set-employee-duties
        :initform "None")
     
     ;; Experience
     (experience :initarg :experience
        :accessor employee-experience
        :initform "None")
     
     ;; Generic monthly data for this employee
     (bar-values :initarg :bar-values
        :accessor employee-bar-values
        :initform nil)
     ))

(defparameter *example-employees*
   (list
      (make-instance 'employee
         :name "Sheryl"
         :department :admin
         :duties "Performs any activity that everyone else says is not in their job description"
         :experience "Has done lots of differents sorts of things for various employers"
         :bar-values '(3 4 5 6  7 8 9 8  7 6 5 4))
      (make-instance 'employee
         :name "Skipper"
         :department :shipping
         :departments '(:shipping :support)
         :duties "Labels, lifts, and lugs large loads"
         :experience "Has lifted over 1800 tons of boxes"
         :bar-color ':yellow
         :bar-values '(2 4 3 5  4 6 5 7  6 8 7 9 ))
      (make-instance 'employee
         :name "Newt"
         :full-time-p t
         :department :management
         :duties "Locks programmers in small rooms until their code works"
         :experience "Has allowed only 3 escapes in 15 years of experience"
         :bar-color :blue
         :bar-values
         '(5 3 6 4  7 5 8 6  9 7 8 6))
      (make-instance 'employee
         :name "Elvis"
         :full-time-p t
         :department :tech
         :duties "Entertains the other employess so they can motivate themselves to come to work"
         :experience "Has sung in 37 different showers, and has only been kicked out of a few of them"
         :bar-color :green
         :bar-values
         '(8 9 7 9  10 8 9 7  9 10 7 9))
      (make-instance 'employee
         :name "Grandma"
         :department :marketing
         :duties "Comes in late at night and corrects everyone's mistakes"
         :experience "Two generations of mistake correction"
         :bar-values
         '(2 7 2 7  2 7 2 7  2 7 2 7))
      (make-instance 'employee
         :name "The Dog"
         :department :none
         :duties "Sleeps; eats; provides moral support"
         :experience "Has consumed much food and learned to sleep in most any location in previous and current lives"
         :bar-color ':yellow
         :bar-values

        '(2 4 3 5  4 6 5 7  6 8 7 9 ))
      (make-instance 'employee
         :name "Uncle Fester"
         :full-time-p t
         :department :sales
         :duties "Frightens customers into purchasing product"
         :experience "Has sat in an attic and ponderred doing many remarkle things"
         :bar-color :green
         :bar-values
         '(8 9 7 9  10 8 9 7  9 10 7 9))
      (make-instance 'employee
         :name "Beavis"
         :duties "Creates silly sentences for use in demos"
         :experience "Has answered the question \"Beavis?  What kind of name is Beavis?  Is that some kind of joke\" frequently for many years"
         :bar-color :blue
         :bar-values
         '(1 10 2 9  3 7 4 6  5 5 6 4))
      ))

(defun grid-example-rows ()
   (let* ((people *example-employees*))
      (mapcar #'(lambda (employee)
                  (make-instance 'grid-data-row
                                 :data-object employee
                                 :focus-function 'set-experience-text
                                 :size 40))
        people)))




(defun set-experience-text (row column on)
  (when on 
    (let* ((emp (cell-data-object row column))
           (interface (capi:element-interface (top-level-widget row)))
           (editor-pane (slot-value interface 'experience-pane)))

      (capi:modify-editor-pane-buffer editor-pane :contents 
                                      (format  nil "~a : ~a" (employee-name emp)(employee-experience emp))))))


(defun employee-full-time-function (employee &optional (value nil valp))
  (if valp
      (set-employee-full-time-p value employee)
    (employee-full-time-p employee)))

(defun full-time-p-title-reader (name)
   ;; An example of returning custom strings 
   (format nil "~:(~a~)'s full-time" name))


(defun employee-inspect-string (employee)
   ;; An example of returning custom strings for a column of buttons
   (and employee
        (format nil "Inspect ~:(~a~)"
           (employee-name employee))))

(defun inspect-button-function (  row column )
   (lw-tools::inspect-object (cell-data-object row column)))




(defun make-employee-widget ()
  (make-instance 
   'grid-widget
    :name :employee-1
    :title "Employee Info"
    :font (gp:make-font-description :family "courier" :weight :medium :slant :roman :size 12)
    
    :column-sections
    (list
       
       ;; The first column section is the row header, simply displaying
       ;; a number for each row
       (make-instance 'grid-column-section
                      :name :row-header
                      :scrollable-p nil
                      ;;:proportional-p nil
                      :size 20
                      :bands
                      (list (make-instance 'grid-display-column
                                           :header-title "#"
                                           :name :row-number
                                           :data-getter :other-band-position
                                           :print-function #'(lambda (x)   ;;; make it 1 based
                                                               (format nil "~a" (1+ x)))
                                           :band-dragging t 
                                           :right-border-spec 1
                                           ;:selectable-p nil
                                           :size 20)))
       
       ;; The second column section has a single column, which is the
       ;; editable name of each employee
       (make-instance 'grid-column-section
                      :name :name
                      :scrollable-p nil
                      :size 80
                      :bands
                      (list (make-instance 'grid-display-column
                                           :name :name
                                           :header-title "Name"
                                           :size 80
                                           :proportional-p t
                                           :data-reader 'employee-name
                                           :data-writer 'set-employee-name
                                           :editing-pane-type 'capi:text-input-pane
                                           :dependents '(:full-time-p)

                                           ))
                      :right-border-spec 3)
       
       ;; The third columnm section is a scrollable set of cells
       ;; conveying miscellaneous info about each employee
       (make-instance 'grid-column-section
                      :name :worksheet
                      :scrollable-p t
                      :size 300
                      :right-border-spec nil
                      :bands
                      (list 
                       (make-instance 'grid-display-column
                                      :name :full-time-p
                                      :header-title "Full Time?"
                                      :data-reader 'employee-name
                                      :print-function 'full-time-p-title-reader
                                      :check-box-function 'employee-full-time-function
                                      :size 100
                                      )
                       (make-instance 'grid-display-column
                                      :name :department
                                      :header-title "Department"
                                      :size 90
                                      :data-reader 'employee-department
                                      :data-writer 'set-employee-department
                                      :print-function 'capitalize-if-symbol
                                      :range-reader 'departments
                                      :test-function 'string-equal
                                      :editing-pane-type 'capi:option-pane
                                      )
                       (make-instance 'grid-display-column
                                      :data-reader  'employee-inspect-string
                                      :action-function 'inspect-button-function
                                      :name :inspect
                                      :header-title "Inspect"
                                      
                                      :size 75

                                      )
                       (make-instance 'grid-display-column
                                      :name :bar-color
                                      :header-title "Bar Color"
                                      :size 75
                                      :data-reader 'employee-bar-color
                                      :data-writer 'set-employee-bar-color
                                      :print-function 'capitalize-if-symbol
                                      :range-reader 'employee-bar-colors
                                      :test-function 'string-equal
                                      :editing-pane-type 'capi:option-pane
                                      :dependents '(:bars)
                                      )
                       (make-instance 'grid-display-column
                                      :name :duties
                                      :header-title "Duties"
                                      :size 250
                                      :data-reader 'employee-duties
                                       :data-reader 'employee-duties
                                       :data-writer 'set-employee-duties
                                       :editing-pane-type 'capi:text-input-pane)
                       ))
       
       ;; The fourth column section is a bar graph.
       ;; Add the actual bar columns later, just to show the other
       ;; way of adding columns.
       (make-instance 'grid-column-section
                      :name :bar-chart
                      :scrollable-p t
                      :resizable-p nil
                      :size 200
                      :bands nil
                      :left-border-spec 2
                      ))
    
    :row-sections
    (list
     
     ;; The first row section consists of the column headers
     (make-instance 'grid-special-row
                    :name :header
                    
                    :data-getter :other-band  ;; so sw get the column as the data-object
                    :data-reader 'header-title ;; and reads the column title
                    :band-dragging t
                    :size 30
                    :bottom-border-spec 4)
     
     ;; The second row section contains the actual employee rows.
     ;; Add the data rows to this row section at runtime individually,
     ;; since the domain data will typically vary from one run to the
     ;; next.
     (make-instance 'grid-row-section
                    :name :body
                    :scrollable-p t
                    :size 200
                    :resizable-p nil
                    :top-border-spec '(:thickness 4 :foreground :green) ;; show that it works
                    :bands (grid-example-rows)))))



(defun create-grid ()
  (let ((grid-widget (make-employee-widget)))
    (add-column (column-section grid-widget :bar-chart)
                (make-instance 'grid-display-column
                               :data-reader 'employee-bar-total ;; <3>
                               :name :total
                               :size 44
                               ;:right-border-spec 1
;                               :selectable-p nil
                               :header-title "Total"
                               :proportional-p nil))
    (loop for title in '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
          for index from 0 by 1
          do
          (let* ((ii index)
                 (month-column  (make-instance 'grid-display-column
                                :data-reader #'(lambda (emp)
                                                 (nth ii (employee-bar-values emp)))
                                :data-writer #'(lambda (val emp)
                                                 (setf (nth ii (employee-bar-values emp)) val))
                                :header-title title
                               :name :bars
                               :size 36

                               :right-border-spec nil

                               :print-function nil
                               :drawing-function 'draw-bar-cell
                               :motion-function 'do-bar-motion
                               :dependents '(:total)
;                               :selectable-p nil
                               )))
            (add-column (column-section grid-widget :bar-chart)
                        month-column)))
    grid-widget))

(capi:define-interface employees-interface ()
  ((employee-grid-widget :initform (create-grid)))
  (:panes
   (experience-pane
    capi:editor-pane 
   :contents ""
   :enabled nil ;; don;t want the user to edit this
   :buffer-name "Experience"))
  (:layouts
   (main-layout
    capi:column-layout
    '(employee-grid-widget experience-pane))))

(defparameter *grid-bar-cell-margin* 4)
(defparameter *grid-example-bar-max* 10)
(defparameter *grid-example-bar-middle* 4)
(defparameter *grid-example-bar-top-margin* 3)
(defparameter *grid-example-bar-bottom-margin* 1) ; match the bottom-border-spec (default to 1)
(defmacro bar-len (height)
  `(- ,height *grid-example-bar-bottom-margin* *grid-example-bar-top-margin*))

(defun draw-bar-cell (row column 
                      pane width height)
  (let* ((employee (data-object row))
         (max *grid-example-bar-max*)
         (bar-value  (read-cell-value row column)))
    (let ((coord
           (+ ;; *grid-bar-cell-margin*
              (round (*  (bar-len height )
                           
                        (min max (max 0
                                      (or bar-value
                                          (floor max 2)))))
                     max))))
      (gp:draw-rectangle pane
                         *grid-bar-cell-margin*
                         (- height coord *grid-example-bar-bottom-margin*)
                         (- width (* 2 *grid-bar-cell-margin*))
                         coord
                         :foreground  (employee-bar-color employee)
                         :filled t)
      (gp:draw-string pane (prin1-to-string bar-value)
                      *grid-bar-cell-margin*
                      (+ 10 *grid-bar-cell-margin*)))))

(defun do-bar-motion (row column  event)
    (let ((y-offset (- (grid-event-y event) (grid-event-cell-y event)))
          (height (grid-event-height event)))


  (when (and (<= *grid-example-bar-top-margin* y-offset (- height *grid-example-bar-bottom-margin*)))
    (let ((new-value  (round (-    height y-offset *grid-example-bar-bottom-margin*)
                            (/ (bar-len height)
                               *grid-example-bar-max*))))

      (write-cell-value row column new-value)))))



(defun employee-bar-total (employee) ;; <3>
   (apply #'+ (employee-bar-values employee)))




(defun capitalize-if-symbol (x)
  (string-capitalize
   (if (symbolp x)
       (symbol-name x)
     x)))

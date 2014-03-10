;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:multi-threading.lisp,v 1.2.1.1 2011/08/24 13:26:21 davef Exp $" -*-



;;;----------------------------------------------------------------------------
;;;
;;; examples/capi/applications/multi-threading.lisp

;;; This is an example of a "multi-threaded application", and
;;; one way to incorporate cpu-intensive work 
;;; with updating the GUI in "real" time. 

;;; It demonstrates how the GUI can display in "real time" 
;;; and responds to the user input while the process is perfoming
;;; work in the background. 

;;; The "work" that the  "application" does is counting the number of
;;; double characters (two consecutive occurrences of the same English
;;; alphabetic character, irrespective of case) in files. It displayes 
;;; the average number of such occurrence for each character. 

;;; Compile the file and then eval
;;;
;;;   (cl-user::test)

;;; This display a "controller" window with 4 buttons:

;;; Run - 
;;;    Raises a dialog for entering a directory. The input should
;;;    be a valid string input to DIRECTORY. In particular, it 
;;;    can include wild char characters (*). To see the point of
;;;    the "application" you need many files (thousands or tens
;;;    of thousands unless they are very big), so you probably 
;;;    want to include ** in the directory. 
;;;    It opens the files using :external-format :latin-1, so can 
;;;    work on any file. 
;;;    Once you you confirm the directory string, it is scheduled
;;;    for running in the background. You will not see anything
;;;    unless you already did "Show" (you can see the "Worker" 
;;;    process in the Process Browser tool). 

;;; Abort Current - 
;;;   Abort the current search. If there are still scheduled directories,
;;;   it goes to the next one. 

;;; Show -
;;;    Create and display an interface that shows the results. For each letter
;;;    it shows a bar corresponding to the average number of occurrence per file of 
;;;    doubles of  this character, and the actual number. It allows the user to
;;;    scale the bars up and down. The bars are in fixed ratio to the dimensions of 
;;;    window, so resize when the window resizes. 
;;;    The interface shows the results in "real time" (actually updated after each
;;;    file).
;;;    You can open (and close) more than one display at the same time.

;;; Kill -
;;;     Kill the worker.

;;;----------------------------------------------------------------------------
;;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;;----------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overall structure.

;;; The work is done in a specific process, which is stored in *worker-thread*. 
;;; In a real application there will be more than one workers threads, which
;;; may or may not work on the same data. 

;;; The "results" of the work are stored in a structure call MY-DATA-STRUCT. 
;;; This corresponds to in-memory data of the real application, which can be
;;; accessed quickly, and it is in a form that does not require much work
;;; for displaying. 
;;; Below the MY-DATA-STRUCT is associated with the work process as a private
;;; property. This is useful when when each worker process work on its own data.
;;; The MY-DATA-STRUCT includes a lock, which is used to protect against two
;;; problems:
;;;  1) Two (or more) threads updating it in the same time.
;;;  2) A thread reading data out of it which is modified by another thread
;;;     and threfore is incosistent. 
;;; MY-DATA-STRUCT also has update-functions list, which the *worker-thread*
;;; uses whenever something changes. The *worker-thread* just call them, without
;;; any knowledge of what they do. As a result, the *worker-thread* and all
;;; the code that it runs doesn't need to know anything about what is being
;;; updated. 

;;; The display is done by other process(es), in a "standard" CAPI interface,
;;; which is called MY-INTERFACE. The interface has a structure which mirror
;;; the data in the my-data-struct (slot PRIVATE-DATA), which it updates
;;; from the MY-DATA-STRUCT with the lock held, thus ensuring that the data in the
;;; private-data is always consistent. It then uses this to display.
;;; When the interface is displayed, it add an update-function the data-struct,
;;; so it gets called when the data-struct changes. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "USER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Data structures and utilities 
;;; These are intended to be data structures that in real
;;; application are in memory, and are reasonably fast
;;; to access. 

;;; MY-BASE-DATA contains the "interetsing" data. 

(defstruct my-base-data
  directory      ; Directory we look at
  (status :idle) ; status of the worker thrad
  (count 0)      ; count of file swe looked at
  (total 0)      ; cunt of characters we looked at 
  (count-vector (make-array 26 :initial-element 0))) ; index number of double-character

;;; MY-DATA-STRUCT contains MY-BASE-DATA (and hence the "interetsing" data),
;;; and also added slots for "administration". 

(defstruct (my-data-struct (:include my-base-data))
  (lock (mp:make-lock :name "MY-DATA-STRUCT lock"))
  update-functions  ; a list of no-argument functions
  abort-flag
  )

  
(defmacro with-my-data-struct-lock (data &body body)
  `(mp:with-lock ((my-data-struct-lock ,data))
     ,@body))

;;; If the directory is still EQ, then if TOTAL-COUNT
;;; is the same only the STATUS cacn change, and maybe the COUNT
;;; for empty files. 
;;; Needs to be inside the lock to give a reliable result. 

(defun compare-my-base-data (bd1 bd2)
  (and (eq (my-base-data-directory bd1) (my-base-data-directory bd2))
       (eq (my-base-data-status bd1) (my-base-data-status bd2))
       (eq (my-base-data-count bd1) (my-base-data-count bd2))
       (eq (my-base-data-total bd1) (my-base-data-total bd2))))

;;; The source must be locked, The target is assumed to be private.
(defun copy-my-base-data-from-database (bd1 my-data-struct)
  (setf (my-base-data-directory bd1) (my-base-data-directory my-data-struct))
  (setf (my-base-data-status bd1) (my-base-data-status my-data-struct))
  (setf (my-base-data-count bd1) (my-base-data-count my-data-struct))
  (setf (my-base-data-total bd1) (my-base-data-total my-data-struct))
  (replace (my-base-data-count-vector bd1) (my-base-data-count-vector my-data-struct)))

;;; The target is assumed to be private. 
(defun update-my-base-data-from-database (bd1 my-data-struct)
  (with-my-data-struct-lock my-data-struct
    (unless (compare-my-base-data bd1 my-data-struct)
      (copy-my-base-data-from-database bd1 my-data-struct)
      t)))
     
;;; Add and remove update-function.      
           
(defun my-data-struct-add-update-function (my-data-struct update-function)
  (with-my-data-struct-lock my-data-struct
    (push update-function (my-data-struct-update-functions my-data-struct))))

(defun my-data-struct-remove-update-function (my-data-struct update-function)
  (with-my-data-struct-lock my-data-struct
    (setf  (my-data-struct-update-functions my-data-struct)
           (remove update-function (my-data-struct-update-functions my-data-struct)))))

;;; When we set the STATUS, we call the update-functions.

(defun set-my-data-struct-status (my-data-struct status)
  (let ((update-funcs
         (with-my-data-struct-lock my-data-struct
           (setf (my-data-struct-status my-data-struct) status)
           (my-data-struct-update-functions my-data-struct))))
    ;; Outside the lock 
    (dolist (func update-funcs) (funcall func))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Worker thread code

;;; The  process function, i.e. the argument to mp:process-run-function. 
;;; First setup MY-DATA-STRUCT, and then loop doing the "work".
;;; The main task of this is to receive a directory
;;; and call SCAN-DIRECTORY-CHARACTERS on it. It also does the initial
;;; and final updates. In addition it allows execution of random
;;; code if it gets sent to the worker (not used in the code here). 
 
(defun my-work-function ()
  (let ((my-data-struct (make-my-data-struct)))
    (setf (mp:process-private-property 'data-struct) my-data-struct)
    (unwind-protect
        (loop (let ((directory-or-event (mp:process-wait-for-event :wait-reason "Waiting for directory")))
                (if (pathnamep directory-or-event)
                    (progn
                      (with-my-data-struct-lock my-data-struct
                        (setf (my-data-struct-directory my-data-struct) directory-or-event
                              (my-data-struct-status my-data-struct) :running
                              (my-data-struct-count my-data-struct) 0
                              (my-data-struct-total my-data-struct) 0
                              (my-data-struct-abort-flag my-data-struct) nil)
                        (fill (my-base-data-count-vector my-data-struct) 0)
                        )
                      (scan-directory-characters my-data-struct)
                      (set-my-data-struct-status my-data-struct :idle)
                      )
                  ;; Allow random execution. 
                  (cond ((consp directory-or-event)
                         (apply (car directory-or-event) (cdr directory-or-event)))
                        ((functionp directory-or-event)
                         (funcall directory-or-event))))))
      (set-my-data-struct-status my-data-struct :dead))))



;;; The scanner of the directory. This is doing the "work" of this
;;; "application", which is scanning the files that matches the 
;;; directory string for double characters. 
;;; It is using DIRECTORY with a "trick": The TEST does the work,
;;; and then always returns NIL. Thus it is really MAP over the files
;;; that match the directory.
;;; The code here is intentionally not optimized, so it does take
;;; some time and the user can play with the GUI while it is running. 
;;; Notes: 
;;;  1. This has a built-in mechanism for soft aborting using 
;;;     the abort-flag in the data-struct. This eliminates the
;;;     any problem with aborting in a sensitive point, which may 
;;;     happen if we use mp:PROCESS-INTERRUPT to cause abort.
;;;     It would be easy to use the same mechanism to pause the worker.
;;;  2. The updating of the my-data-struct is done inside a lock. we avoid
;;;     repeated locking by accumulating the data for each file and updating
;;;     my-data-struct once per file.
;;;  3. After updating the my-data-struct we call the update-functions. We do it
;;;     outside the lock.
;;;     In the code below the update functions update the interfaces, but
;;;     they could do other things. 
;;;     In effect, the code here rely on the update-functions not to hang. Normally
;;;     they will not because they simply send something to another process. 

;;; In real code there should be better error handling. 

(defun scan-directory-characters (my-data-struct)
  (directory (my-data-struct-directory my-data-struct)
             :TEST #'(lambda (file)
                       (let ((prev nil)
                             (vec (make-array 26 :initial-element 0))
                             (total 0))
                         (declare (dynamic-extent vec))
                         (when (and (file-readable-p file)
                                    (not (file-directory-p file)))
                           (with-open-file (ff file :external-format :latin-1)
                             (loop (when (my-data-struct-abort-flag my-data-struct)
                                     (return-from scan-directory-characters))
                                   (if-let (char (read-char ff nil nil))
                                       (progn 
                                         (incf total)
                                         (if (alpha-char-p char)
                                             (let ((upcase (char-upcase char)))
                                               (when  (eq prev upcase)
                                                 (let ((int (char-int char)))
                                                   (when (>= (char-int #\Z) int (char-int #\A))
                                                     (incf (svref vec
                                                                  (- (char-int upcase) (char-int #\A)))))))
                                               (setq prev upcase))
                                           (setq prev char)))                                             
                                     (return))))
                           (let (update-functions)
                             ;; Update the data-struct inside the lock 
                             (with-my-data-struct-lock my-data-struct
                               (incf (my-data-struct-count my-data-struct))
                               (incf (my-data-struct-total my-data-struct) total)
                               (let ((t-vec (my-base-data-count-vector my-data-struct)))
                                 (dotimes (x 26)
                                   (incf (svref t-vec x) (svref vec x))))
                               (setq update-functions (my-data-struct-update-functions my-data-struct)))
                             
                             ;; Call the update-functions outside the lock
                             (dolist (func update-functions)
                               (ignore-errors
                                 (funcall func))))))
                       ;; Tell  DIRECTORY not to collect it
                       nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; GUI code. 


(defun draw-one-character-bar (pane index-from-a average)
  (let ((factor (capi:range-slug-start (my-interface-scale-pane (capi:element-interface pane)))))
    (capi:with-geometry
        pane
      (let* ((rect-height (round (*  average factor capi:%height%) 4000 ))
             (rect-width (round (/ capi:%width% 40)))
             (x-position (+ 10 (round (* (/ index-from-a 26) (- capi:%width% 10)))))
             (y-position (- capi:%height% 40 rect-height))
             (color  (svref #(:green :red :blue :yellow)
                            (mod index-from-a 4))))
        
        (gp:draw-rectangle pane x-position y-position rect-width rect-height :filled t :foreground color)
        (gp:draw-character pane (int-char (+ index-from-a (char-int #\A)))
                           (+ x-position (truncate (- rect-width 4) 2))
                           (- capi:%height% 24))
        (gp:draw-string pane (format nil "~2,2f" (float average))
                            x-position (- capi:%height% 4))))))
      
;;; The :DISPLAY-CALLBACK of the bar-chart
;;; Currently we display everything. 
;;; This function assumes that the private-data is up-to-date. 
(defun bar-chart-display-callback (pane x y width height)
  (declare (ignore width height x y))
  (let* ((tli (capi:element-interface pane))
         (private-data (my-interface-private-data tli)))
    (let ((file-count (my-base-data-count private-data))
          (count-vector (my-base-data-count-vector private-data)))
      (when (zerop file-count) (setq file-count 1)) ;prevent 0/0 error below
      (dotimes (x 26)
        (draw-one-character-bar pane x  (/ (svref count-vector x) file-count))))))

;;; The :RESIZE-CALLBACK of the bar-chart.
;;; Becuase we scale the bars with the size of the pane, we need to
;;; redraw everything. 

(defun bar-chart-resize-callback  (bar-chart x y width height)
  (declare (ignore x y width height))
  (gp:invalidate-rectangle bar-chart))

;;; That is called when the data in the has changed (maybe). First
;;; copy (maybe) from the my-data-struct (which is updated by the worker)
;;; into the private data. If this actually copy, update everything. 
 
(defun my-interface-update (interface)
  (setf (my-interface-up-to-date-p interface) t)

  (let ((private-data (my-interface-private-data interface)))
    (when (update-my-base-data-from-database private-data (my-interface-data-struct interface))
      (setf (capi:interface-title interface)
            (format nil  "Double-character occurrences for ~a"
                    (namestring (my-base-data-directory private-data))))
      (setf (capi:display-pane-text (my-interface-status-pane interface))
            (string-capitalize (my-base-data-status private-data)))
      (setf (capi:display-pane-text (my-interface-count-pane interface))
            (format nil "~d" (my-base-data-count private-data)))
      (gp:invalidate-rectangle (my-interface-bar-chart interface)))))

;;; This is called when the display needs to change, even though
;;; the data hasn't changed. Just redisplay using the data
;;; it already has. This is used when the scale changes. 

(defun my-interface-change-display (interface)
  (gp:invalidate-rectangle (my-interface-bar-chart interface)))


;;; The interface that does the display. 

(capi:define-interface my-interface ()
  ((database :reader my-interface-data-struct :initarg :data-struct)
   (private-data :initform (make-my-base-data) :reader my-interface-private-data)
   (up-to-date-p :initform t :accessor my-interface-up-to-date-p)
   (update-func))
  (:panes
   (bar-chart 
    capi:output-pane
    :draw-with-buffer t  ; On windows it flickers without this
    :display-callback 'bar-chart-display-callback
    :resize-callback 'bar-chart-resize-callback
    :initial-constraints '(:min-width 800 :visible-min-height 200)
    :reader my-interface-bar-chart)
   (status-pane
    capi:display-pane
    :title "Status:"
    :title-position :left
    :visible-min-width '(:character 30)
    :reader my-interface-status-pane)

   (count-pane 
    capi:display-pane 
    :title "File Count:"
    :title-position :left
    :visible-min-width '(:character 30)
    :reader my-interface-count-pane)
   (scale-pane
    capi:slider
    :start 100
    :end 2000
    :show-value-p nil
    :title "Scale"
    :title-position :left
    :callback #'(lambda (slider here operation)
                  (declare (ignore here operation))
                  (my-interface-change-display (capi:element-interface slider)))
    :reader my-interface-scale-pane

    ))
  (:layouts
   (main-layout
    capi:column-layout
    '(bar-chart status-layout scale-pane))
   (status-layout
    capi:row-layout
    '(status-pane count-pane)))
  (:default-initargs
   :destroy-callback 'my-interface-destroy-callback
   ))


;;; Create the update fuction that is going into the data-struct. Notes:
;;; 1. This is run on the worker process, so does minimal amount of work.
;;; 2. This uses capi:execute-with-interface-if-alive, and therefore doesn't
;;;    to worry about the interface dying while it is running. In situations
;;;    when the function just update the display, capi:execute-with-interface-if-alive
;;;    should be used in preference to capi:execute-with-interface (and
;;;    capi:apply-in-pane-process), which may run the function on the current 
;;;    thread. 
;;; 3. We use the MY-INTERFACE-UP-TO-DATE-P mechanism to avoid sending events
;;;    faster than the interface process can update. We can do it because
;;;    the events that we send (i.e. what MY-INTERFACE-UPDATE does) is not
;;;    cumulative. In the "application", it calls the update function
;;;    once per file, which is unlikely to cause much delays. But if the worker
;;;    were calling it more often (e.g. after each character), it may make a 
;;; difference. 

(defun create-my-update-function (interface)
  #'(lambda () 
      (when (my-interface-up-to-date-p interface)
        (setf (my-interface-up-to-date-p interface) nil)
        (capi:execute-with-interface-if-alive interface
                                              'my-interface-update interface))))

;;; Update the interface, and add an update-function to the data-struct
;;; so the interface is updated when it changes. This remembers the
;;; update-func so can remove it when it is destroyed (in 
;;; my-interface-destroy-callback below). 

(defmethod capi:interface-display :before ((interface my-interface))
  (let ((data (my-interface-data-struct interface))
        (update-func (create-my-update-function interface)))
    (my-interface-update interface)
    (my-data-struct-add-update-function data update-func)
    (setf (slot-value interface 'update-func) update-func)))


;;; Removes our update-function from the data-struct

(defun my-interface-destroy-callback (interface)
  (let ((my-data-struct (my-interface-data-struct interface)))
    (my-data-struct-remove-update-function my-data-struct (slot-value interface 'update-func))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;; Infrastructure 

;;; In a real application the functionaliy below may be spread 
;;; in various places. 

;;; This "application" has only one worker thread, but real applications
;;; probably would have more. 

(defglobal-variable *worker-thread* nil)


(defun ensure-worker-thread-and-data-struct ()
  (let ((work-process (or (and *worker-thread*
                               (mp:process-alive-p *worker-thread*)
                               *worker-thread*)
                          (let ((worker (mp:process-run-function "Worker" ()
                                                                 'my-work-function)))
                            (setq *worker-thread* worker)
                            worker))))
    (let ((my-data-struct (mp:get-process-private-property 'data-struct work-process)))
      ;; If we just started the process it may not have setup the data-struct
      ;; yet. 
      (unless my-data-struct
        (mp:process-wait-with-timeout "Waiting for worker to start"
                                      2
                                      'mp:get-process-private-property
                                      'data-struct
                                      work-process
                                      )
        (unless (setq my-data-struct (mp:get-process-private-property 'data-struct work-process))
          ;; Real application should tell the user that something is wrong
          (error "Worker process didn't start")))
      (values work-process my-data-struct))))

;;; Invoked by the "Run" button. 
;;; Asks for a string, and send it to the worker.

(defun run-check-directory-characater-ratio ()
  (multiple-value-bind (string ok-p)
      (capi:prompt-for-string "Directory to check character ratio")
    (when ok-p
      (let ((directory (pathname string)))
        (mp:process-send (ensure-worker-thread-and-data-struct) directory)))))

;;; Invoked by the "Abort current" Button. 
;;; Aborts the current search using the built-in mechanism. 

(defun interrupt-check-directory-characater-ratio ()
  (when-let (process *worker-thread*)
    (when-let (data-struct (mp:get-process-private-property 'data-struct process))
      ;; A single setter doesn't need a lock. 
      (setf (my-data-struct-abort-flag data-struct) t))))

;;; Invoked by the "Show" button.
;;; Creates a new interface that displays the results. 

(defun show-check-directory-characater-ratio ()
  (let ((data-struct (nth-value 1 (ensure-worker-thread-and-data-struct))))
    (capi:display (make-instance 'my-interface :data-struct  data-struct))))

;;; Invoked by the "Kill" button.
(defun kill-check-directory-characater-ratio ()
  (when-let (process *worker-thread*)
    (mp:process-kill process)))

;;; raises a (pretty dumb) "controller". 

(defun test ()
  (capi:contain (make-instance 
                 'capi:push-button-panel
                 :layout-class 'capi:column-layout
                 :callback-type :none
                 :items '("Run" "Abort current" "Show" "Kill")
                 :callbacks '(run-check-directory-characater-ratio 
                              interrupt-check-directory-characater-ratio
                              show-check-directory-characater-ratio
                              kill-check-directory-characater-ratio))
                :name "Double Character counting controller"
                :title "Double Character counting controller"))
  


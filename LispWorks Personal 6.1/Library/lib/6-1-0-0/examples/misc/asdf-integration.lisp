;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/misc:asdf-integration.lisp,v 1.18.1.1 2011/08/24 13:26:18 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; Integration of ASDF into the LispWorks IDE.
;;; This allows the Search Files and System Browser tools
;;; and Editor commands on systems to work on ASDF systems too.

;;; To use ASDF in LispWorks:
;;;
;;; 1) Do
;;;     (require "asdf")
;;;    This loads the distributed version of ASDF 2, and then this file.
;;;
;;; 2) Configure ASDF as described in its documentation.
;;;
;;; 3) Optionally, if you want your later version of ASDF, do
;;;     (asdf:load-system :asdf)
;;;
;;; 4) Then load your ASDF system definitions
;;;    Now you're all set to work with these in the IDE tools as described below.

;;; Alternatively, to use your later version of ASDF, instead of steps
;;; 1)-4) you could just load it directly. 

;;; Note that LispWorks loads this file automatically when ASDF is
;;; loaded, as long as the value of LW:*AUTOLOAD-ASDF-INTEGRATION* is
;;; true when the LispWorks IDE starts. Otherwise, you will need to
;;; load this file explicitly in order to work with your ASDF systems
;;; in the LispWorks IDE.


;;; To use this in the LispWorks IDE, load some ASDF system
;;; definitions.

;;; To use with the Search Files tool:
;;;   a) In the option pane at the top, select "System Search".
;;;   b) Move the focus to "System Names".
;;;   c) Press the Down arrow. It displays a list of system names,
;;;      both defined LW:DEFSYSTEM and by ASDF:DEFSYSTEM.
;;;      LispWorks systems are prefixed by LW, ASDF systems by ASDF.
;;;   d) Enter one or more system names (separated by a semicolon)
;;;      of either LispWorks or ASDF. You can mix the two.
;;;   e) Enter a search string in the "Regexp Search String" box (e.g.
;;;      defun) and press return. 
;;;   f) The tool searches all the files in the specified systems.

;;; To use with the System Browser tool:
;;;   a) When the tool starts or when selecting "All Systems" from the
;;;      "Systems" menu, it displays in "Tree" tab "All Systems" with
;;;      two children: LW and ASDF. Under each child it shows (when
;;;      expanded) all the system that are defined in this namespace.
;;;   b) In the text-input-pane you can enter more than one system
;;;      name, separated by semi-colon and optional whitespace. If you
;;;      don't prefix the name, it matches in both ASDF and LW. When
;;;      the names are displayed by the systems, they are prefixed.
;;;   c) You can complete in the text input pane by pressing Down arrow. 
;;;   d) You can compile or load LispWorks and ASDF systems. 


;;; Note that ASDF and LispWorks use different terminology for the same concepts:
;;;     LispWorks:  SYSTEM      MODULE      FILE
;;;          ASDF:  MODULE      COMPONENT   FILE       

;;; The example uses SCM:ADD-SYSTEM-NAMESPACE to add a system
;;; namespace for ASDF. It also defines ASDF methods for these
;;; symbols:

;;; Mapping over sources:

;;;   SCM:MAP-SYSTEM-PATHNAMES

;;; Module (component) properties: 

;;;   SCM:MODULE-IS-SYSTEM-P
;;;   SCM:MODULE-NAME
;;;   SCM:MODULE-CHILDREN 
;;;   SCM:MODULE-PATHNAME
;;;   SCM:MODULE-PRINT-NAME
;;;   SCM:MODULE-FLAG-DESCRIPTION
;;;   SCM:MODULE-NAMESPACE-NAME

;;; Plan and execute: 

;;;   SCM:SYSTEM-PLAN
;;;   SCM:EXECUTE-SYSTEM-PLAN
;;;   #+not-needed SCM:SYSTEM-TARGET-DIRECTORY

;;; Display in preview: 

;;;   #+not-needed SCM:TREEIFY-SYSTEM-PLAN
;;;   SCM:TREEIFY-PLAN-EVENT
;;;   SCM:EVENT-SOURCE-P
;;;   SCM:EVENT-MODULE

;;;   The example uses these ASDF symbols:

;;;   ASDF:COMPONENT-PATHNAME
;;;   ASDF:SYSTEM
;;;   ASDF:MODULE
;;;   ASDF:SOURCE-FILE
;;;   ASDF:CL-SOURCE-FILE
;;;   ASDF:OOS
;;;   ASDF:PERFORM
;;;   ASDF:COMPONENT-PATHNAME
;;;   ASDF:MODULE-COMPONENTS
;;;   ASDF:OPERATION-DONE-P
;;;   ASDF:LOAD-SOURCE-OP
;;;   ASDF:COMPILE-OP
;;;   ASDF:LOAD-OP
;;;   ASDF:FIND-SYSTEM
;;;   ASDF:COMPONENT
;;;   ASDF:COMPONENT-NAME
;;;   ASDF:MAP-SYSTEMS  ; on ASDF 2
;;;
;;;   ASDF::*DEFINED-SYSTEMS* ; except on ASDF 2
;;;   ASDF::COMPONENT-OPERATION-TIMES
;;;   ASDF::TRAVERSE
;;;   ASDF::*VERBOSE-OUT*



(in-package "CL-USER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;      Namespace definition       ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; In ASDF 2 we have proper interface for listing systems, so 
;;; use it if it is available. 

(defun map-systems-symbol ()
  (let ((maps-systems-symbol (find-symbol "MAP-SYSTEMS" 'asdf)))
    (and maps-systems-symbol (fboundp maps-systems-symbol)
         maps-systems-symbol)))

;;; The name-lister takes no argument and must return a list of strings.
;;; This one uses an internal symbol in ASDF on earlier versions,
;;; use ASDF:MAP-SYSYTEMS if available. 

(defun asdf-list-system-names ()
  (let ((list nil))
    (if-let (maps-systems-symbol (map-systems-symbol))
        (funcall maps-systems-symbol #'(lambda(asdf-system)
                                         (push (scm:module-name asdf-system) list)))
      (maphash #'(lambda (key value)
                   (declare (ignore value))
                   (push key list))
               asdf::*defined-systems*))
    ;;; currently ignoring systems not yet loaded
    ;;; maybe should sort them 
    list))

;;; system-lister takes no argument and returns the system (modules
;;; in ASDF) objects. 

(defun asdf-list-systems ()
  (let ((list nil))
    (if-let (maps-systems-symbol (map-systems-symbol))
        (funcall maps-systems-symbol #'(lambda(asdf-system)
                                         (push asdf-system list)))
      (maphash #'(lambda (key value)
                   (declare (ignore key))
                   (push (if (consp value)
                             (cdr value)
                           value)
                         list))
               asdf::*defined-systems*))
    ;;; currently ignoring ones on the disk.
    ;;; maybe should sort them 
    list))

;;; The finder takes a string, and returns a system or a list of
;;; systems. Must not give an error. The match needs to be exact, i.e.
;;; don't do completions. 
 
;;; We need the IGNORE-ERRORS because ASDF:FIND-SYSTEM gives an 
;;; error on a system name that is a wild filename, inside
;;; ASDF:SYSDEF-CENTRAL-REGISTRY-SEARCH. This needs to be changed
;;; to call WILD-PATHNAME-P before calling PROBE-FILE. 

(defun find-system-no-error (name)
  (ignore-errors
    (asdf:find-system name nil)))

;;; Install the listers and finder for use by LispWorks tools.
(scm:ADD-SYSTEM-NAMESPACE "ASDF" 
                          :name-lister 'asdf-list-system-names
                          :system-lister 'asdf-list-systems
                          :finder 'find-system-no-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;     mapping over sources        ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SYSTEM      - the result of a call to a finder. 
;;; FUNCTION    - function to map: takes one argument,  a pathname.
;;; TYPE        - Either :SOURCE or :OBJECT. In the Search Files tool
;;;               it is always :SOURCE.
;;; MUST-EXIST  - If non-nil, only existsing pathnames should be passed
;;;               to the function. 
;;; SKIP-BINARY - If non-nil, skip components where the :SOURCE is
;;;               is in a binary format (LispWorks allows this).  

;;; The current definition is pretty minimal, which only copes with
;;; the way it is called by the Search Files tool. 

(defmethod scm:MAP-SYSTEM-PATHNAMES ((system asdf:system) function &key (type :source) 
                                      (must-exist nil)
                                      skip-binary)
  (declare (ignore type must-exist skip-binary))
  (asdf:oos 'map-source-op system :function function))

(defclass map-source-op (asdf:load-source-op) ((function :initarg :function)))
(defmethod asdf:operation-done-p ((o map-source-op) (c asdf:source-file)) nil)
(defmethod asdf:perform ((o map-source-op) (c asdf:cl-source-file))
  (let ((source (asdf:component-pathname c)))
    (when (probe-file source)
      (funcall (slot-value o 'function) source))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;   Module (component) properties   ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Predicate for whether the module (component in ASDF) is a system
;;; (module in ASDF). We define a method on ASDF:MODULE to return T.
;;; There is a default method on T returning nil, which will apply
;;; to all other components. 
;;; When this returns non-nil, it tells the caller
;;; that there are applicable methods
;;; for SCM:MAP-SYSTEM-PATHNAMES, SCM:MODULE-NAMESPACE-NAME, 
;;; SCM:SYSTEM-PLAN and SCM:EXECUTE-SYSTEM-PLAN on the same object.
 
(defmethod scm:module-is-system-p ((module asdf:module)) t)

;;; Return the name of a module.

(defmethod scm:module-name ((module asdf:component))
  (asdf:component-name module))

;;; Return the parent of a module.

(defmethod scm:module-parent ((module asdf:component))
  (asdf:component-parent module))

;;; Return the children of a module. 
;;; There is a default method on T returning nil 
;;; The results is a list of "children". Each child
;;; is a "module", but provided there is either
;;; SCM:MODULE-PRINT-NAME or SCM:MODULE-NAME defined for it, 
;;; it can be anything. 

(defmethod scm:module-children ((module asdf:module))
  (asdf:module-components module))

;;; Returns the pathname of the module. 
;;; There is a default method that returns NIL. 

(defmethod scm:module-pathname ((module asdf:component) parent)
  (declare (ignore parent))
  (asdf:component-pathname module))

;;; Must return a string or NIL. The string is not interpreted. 
;;; Probably should contain at least the version. 

(defmethod scm:module-flag-description ((module asdf:module))
  nil)

;;; What the user sees in the IDE, which is not necessarily the same
;;; as the name.

(defmethod scm:module-print-name ((module asdf:component))
  (asdf:component-name module))

;;; This needs to match the name given to SCM:ADD-SYSTEM-NAMESPACE above. 

(defmethod scm:module-namespace-name ((module asdf:component))
  "ASDF")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;      Plan and execute     ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
(defstruct asdf-event
  op
  component)


;;; Returns a plan, which is interpreted by SCM:EXECUTE-SYSTEM-PLAN below.
;;; ACTIONS is a list contain :COMPILE and :LOAD or both. 
;;; In ASDF LOAD also COMPILEs, so this map both '(:LOAD) and
;;; '(:COMPILE :LOAD) to asdf:load-op.
;;; FORCE and SOURCE are booleans.

(defmethod scm:system-plan ((system asdf:module) actions
                            &key
                            force
                            source
                            )
  (when-let (op-type  (cond (source 'asdf:load-source-op)
                            ((member :load actions) 'asdf:load-op)
                            ((member :compile actions) 'asdf:compile-op)))
    (let* ((initargs (when force '(:force t)))
           (op (apply 'make-instance op-type
                      :force force
                      :original-initargs initargs
                      initargs))
           (asdf::*verbose-out* *standard-output*))
      (mapcar #'(lambda (cons)
                  (make-asdf-event :op (car cons) :component (cdr cons)))
              (asdf::traverse op system)))))

;;; This receives the results of scm SCM:SYSTEM-PLAN above, and needs
;;; to execute it. 
;;; This is the bottom half of ASDF:OPERATE. 

(defmethod scm:execute-system-plan ((system asdf:module) plan)
  (let ((asdf::*verbose-out* *standard-output*))
    (with-compilation-unit ()
      (dolist (event plan)
        (let ((op (asdf-event-op event))
              (component (asdf-event-component event)))
          (loop
           (restart-case 
               (progn (asdf:perform op component)
                 (return))
             (retry ()
               :report
               (lambda (s)
                 (format s "~@<Retry performing ~S on ~S.~@:>"
                         op component)))
             (accept ()
               :report
               (lambda (s)
                 (format s
                         "~@<Continue, treating ~S on ~S as ~
                               having been successful.~@:>"
                         op component))
               (setf (gethash (type-of op)
                              (asdf::component-operation-times component))
                     (get-universal-time))
               (return)))))))))



;;; This is where the results of compilation go. In ASDF
;;; it seems it cannot be defined in the module. The
;;; default method just takes the module-pathname, so
;;; this is not needed
#+not-needed
(defmethod scm:system-target-directory ((system asdf:module)) 
  (scm:module-pathname system nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;        Display in preview     ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This is needed only if the plan that SCM:SYSTEM-PLAN
;;; returns is not a list of events. It needs to return
;;; two values:  a string describing the plan (one line, 
;;; which is displayed in the preview tree), and a list of the
;;; events in the plan. In ASDF the plan is a list, so this 
;;; is not needed. 

#+not-needed
(defmethod scm:treeify-system-plan ((module asdf:module) plan)
  (values "a plan" (plan-events plan)))

;;; This takes the module on which the plan was made (not the one
;;; in the event) and an event in a plan. Needs to return two values:
;;; a string describing the event to show in the preview tree, and 
;;; children events. 
;;; In ASDF there are no sub-events, so the second value always NIL. 

(defmethod scm:treeify-plan-event ((module asdf:module) event)
  (values (format nil "~a on ~a" (type-of (asdf-event-op event))
                  (asdf:component-name (asdf-event-component event)))
          nil))

;;; Returns non-nil if the event is a "source" event.  The preview
;;; tree decides which icon to use based on the result. 

(defmethod scm:event-source-p ((event asdf-event))
  (eq (type-of (asdf-event-op event)) 'asdf:load-source-op))

;;; Returns the module of the event.

(defmethod scm:event-module ((event asdf-event))
  (asdf-event-component event))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;        Editor                 ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(editor:define-file-type-hook ("asd")
    (buffer type)
  (declare (ignore type))
  (setf (editor:buffer-major-mode buffer) "Lisp"))

;; Use ASDF as the default package for asd files (not quite right: it
;; should be a package that uses ASDF).
(defun set-asdf-package (buffer new-mode-p)
  (when new-mode-p
    (let ((pathname (editor:buffer-pathname buffer)))
      (when (and pathname (pathname-match-p pathname "*.asd"))
        (editor::set-buffer-current-package buffer (find-package "ASDF"))))))
(editor:add-global-hook editor::LISP-MODE-HOOK 'set-asdf-package)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;        Dspecs                 ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dspec:define-dspec-alias asdf::defun* (name)
    `(defun ,name))

(dspec:define-dspec-alias asdf::defgeneric* (name)
    `(defgeneric ,name))

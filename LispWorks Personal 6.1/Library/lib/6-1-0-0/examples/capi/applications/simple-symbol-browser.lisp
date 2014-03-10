;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:simple-symbol-browser.lisp,v 1.6.2.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;; This a stripped-down version of the code of the Symbol Browser
;;; tool in the LispWorks IDE. It demonstrates the usage of some CAPI
;;; elements and shows how they are integrated.  To see what it does,
;;; compile the file and evaluate:
;;;
;;; (CL-USER::SHOW-SIMPLE-SYMBOL-BROWSER)
;;;
;;; Selecting a symbol should display its value and function in the
;;; description. You can set the type and accessibility to limit which
;;; symbols are displayed ("visible").  You can filter by typing into
;;; the filter.  You can search again by typing into the Regexp: pane.
;;; "Select packages:" is not implemented.  Double-clicking on a
;;; symbol will try to edit the source (that is, where it is defined
;;; in some way). You will need to have the source of the definition
;;; for that to work.
;;; The toolbar items at the top do nothing, they jsut show how to add a
;;; toolbar to an interface using the :TOOLBAR-ITEMS keyword.


(defvar *default-type-filter* :all)
(defvar *symbol-types*
    '(:all :functions :classes :structures
      :variables :constants :keywords :others))


;;; These two are used when sorting the symbols by the
;;; package (click on the Home-Package header)

(defun symbol-lessp (symbol1 symbol2)
  (declare (optimize (safety 0)))
  (let ((p1 (symbol-package symbol1))
        (p2 (symbol-package symbol2)))
    (if p1
        (if p2
            (if (eq p1 p2)
                (string-lessp (symbol-name symbol1) (symbol-name symbol2))
              (string-lessp (package-name p1) (package-name p2)))
          t)
      (If p2
          nil
        (string-lessp (symbol-name symbol1) (symbol-name symbol2))))))
          
(defun symbol-greaterp (symbol1 symbol2)
  (declare (optimize (safety 0)))
  (let ((p1 (symbol-package symbol1))
        (p2 (symbol-package symbol2)))
    (if p1
        (if p2
            (if (eq p1 p2)
                (string-greaterp (symbol-name symbol1) (symbol-name symbol2))
              (string-greaterp (package-name p1) (package-name p2)))
          t)
      (If p2
          nil
        (string-greaterp (symbol-name symbol1) (symbol-name symbol2))))))

;;; These are the sorting types for the symbol pane
;;; (selected by clicking on the headers). 

(defvar *symbol-browser-sorting-types*
  (list
   (capi:make-sorting-description :type :Home-Package
                             :sort 'symbol-lessp
                             :reverse-sort 'symbol-greaterp)
   (capi:make-sorting-description :type :Name
                             :sort 'string-lessp
                             :reverse-sort 'string-greaterp)
  
   ))


(defmacro symbol-browser-accessibility (self)
  `(capi:choice-selected-item (slot-value ,self 'accessibility-pane)))

;;; LispWorks 5 does not have exported interface to the filter
;;; LispWorks 6 has an exported interface. 

(defmacro symbol-browser-filter (self)
  #+lispworks5 `(capi::filtered-pane-filter (slot-value ,self 'symbols-pane))
  #-lispworks5 `(capi:list-panel-filter-state (slot-value ,self 'symbols-pane)))

;;----------------------------------------------------------------------------
;; The symbol browser itself
;;----------------------------------------------------------------------------


(capi:define-interface simple-symbol-browser ()
  ((visible-symbols :initform nil :accessor symbol-browser-visible-symbols)

   (display-type-filter :initform *default-type-filter*
                        :initarg :display-type-filter
                        :accessor symbol-browser-display-type-filter)
   (list-up-to-date :initform nil)
   (state :initform nil :accessor symbol-browser-state)
   (packages :initform :all
             :initarg :packages 
             :accessor symbol-browser-packages))
  (:panes
   (apropos-pane
     capi:text-input-pane
    :title "Regexp:"
    :title-position :left
    :buttons `(:ok t :cancel t)
    :callback-type :interface-data
    :callback 'symbol-browser-new-apropos)
   (matches-pane
     capi:display-pane
    :title "Total Matches:"
    :title-position :left
    :title-adjust :center
    :visible-min-width '(character 6)
    :visible-max-width t)
   (select-packages
     capi:push-button
    :data "Select Packages..."
    :callback 'change-symbol-browser-packages
    :callback-type :interface)
   (symbols-pane
     capi:multi-column-list-panel
    :filter t
    :color-function 'color-symbol-if-defined
    :visible-min-height '(:character 6)
    :interaction :extended-selection
    :columns '((:title :Home-Package) (:title :Name))
    :column-function #'(lambda (symbol)
                         (let ((p (symbol-package symbol)))
                           (list (and p (package-name p))
                                 (symbol-name symbol))))
    :sort-descriptions *symbol-browser-sorting-types*
    :header-args (list :print-function 'string-capitalize
                       :selection-callback #'(lambda (interface data)
                                               (let ((slist (slot-value interface 'symbols-pane)))
                                                 (capi:sorted-object-sort-by slist data))))
   
    :print-function #'(lambda (symbol)
                        (symbol-browser-print-symbol capi:interface symbol))
    :selection-callback 'symbol-browser-display-symbol-description
    :retract-callback #'(lambda (ignore self)
                          (declare (ignore ignore))
                          (symbol-browser-display-symbol-description nil self)
                          (capi:redisplay-menu-bar self))
    :action-callback 'symbol-browser-edit-source
    :callback-type :data-interface)
  
   (description-pane
     capi:multi-column-list-panel
    :visible-max-width nil
    :columns '((:title "Attribute" :visible-min-width 50) 
               (:title "value" :visible-min-width 50))
    :visible-min-height '(character 2))

   (symbol-types
    capi:option-pane
    :items *symbol-types*
    :print-function 'string-capitalize
    :interaction :single-selection
    :callback-type :data-interface
    :selected-item (symbol-browser-display-type-filter capi:interface)
    :selection-callback #'(setf symbol-browser-display-type-filter)
    :visible-max-width t
    :title "Type:"
    :title-position :left
    )
   (accessibility-pane
     capi:option-pane
    :print-function 'string-capitalize
    :items '(:all :present :externals-only :internals-only)
    :callback-type :interface
    :selection-callback 'symbol-browser-update-visible-symbols 
    :title "Accessibility:"
    :title-position :left
    )
   )
  
  (:layouts
   (options-layout
     capi:row-layout
    '(show-layout nil select-packages ;matches-pane
                  )
    :y-adjust :center
    )
   (show-layout
     capi:row-layout
    '(symbol-types accessibility-pane)
    :title "Show"
    :title-position :frame)
   (text-layout
    capi:column-layout
    '(symbols-pane))
   (select-layout
     capi:column-layout
    '(apropos-pane  options-layout)
    :title-position :frame
    :title "Search Settings")
    
   (main-layout
     capi:column-layout
    '(select-layout text-layout :divider  description-pane )
    :ratios '(  3 9 nil 1))
   )
  (:extra-initargs '(:accessibility))

  (:default-initargs
   :layout 'main-layout
   :title "Simple Symbol Browser"
   :best-height 600
   :toolbar-items  (make-simle-symbol-browser-toolbar-items)
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;       Interface Toolbar Items      ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *an-image-set*
  (gp:read-external-image
   (lispworks-file "examples/capi/elements/images/toolbar-radio-images.bmp")
   :transparent-color-index 7)
  "An image set for use in a toolbar")

;;; A callback that just shows that there was a callback
(defun simple-symbol-browser-toolbar-callback (interface item)
  (capi:popup-confirmer nil (format nil "Clicked item ~s " item)
                        :owner interface
                        :cancel-button "Close"  ; create a dialog that goes away
                        :ok-button nil          ; both by esacpe and return
                        ))

;; Random set of toolbars that just call the call back above. 

(defun make-simle-symbol-browser-toolbar-items ()
  (list    ;; These two don't have images, so use the default image set.
           ;; See documentation for capi:toolbar-component and capi:toolbar-button
           (make-instance 'capi:toolbar-component
                          :items '(:std-cut :std-copy :std-paste)
                          :tooltips '("Tooltip for Cut" "Tooltip for Copy" "Tooltip for Paste")
                          #-lispwork5 :titles #-lispwork5 '("Cut" "Copy" "Paste")
                          :selection-callback 'simple-symbol-browser-toolbar-callback
                          :callback-type :interface-data)
           (make-instance 'capi:toolbar-component
                          :items '(:std-undo :std-redo)
                          :tooltips '("Tooltip for Undo" "Tooltip for Redo")
                          #-lispwork5 :titles #-lispwork5 '("Undo" "Redo")
                          :selection-callback 'simple-symbol-browser-toolbar-callback
                          :callback-type :interface-data)
           ;; This one does have an image-set
           (make-instance 'capi:toolbar-component
                          :items '(:on :off :help)
                          :tooltips '("Tooltip for On" "Tooltip for Off" "Tooltip for Help")
                          #-lispwork5 :titles #-lispwork5 '("On" "Off" "Help")
                          :images '(3 5 4)   ; indices into the :default-image-set 
                          :default-image-set 
                          (capi:make-general-image-set :id *an-image-set* :image-count 6)
                          :selection-callback 'simple-symbol-browser-toolbar-callback
                          :callback-type :interface-data)
           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Colouring of the items in the list-panel. Works only on Windows and GTK. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *sb-fbound-and-special-color* :green)
(defvar *sb-fbound-and-class-color* :purple)
(defvar *sb-fbound-color* :red)
(defvar *sb-specialp-color* :blue)
(defvar *sb-specialp-and-class-color* :white)
(defvar *sb-class-color* :orange)

;;; This is the :COLOR-FUNCTION of the symbols pane.
;;; See the doc for COLOR-FUNCTION in the manual entry for CAPI:LIST-PANEL

(defun color-symbol-if-defined (lp symbol state)
  (declare (ignore lp))
  (when (eq state :normal)
    (let ((fboundp (fboundp symbol))
          (specialp (sys:declared-special-p symbol))
          (classp (find-class symbol nil)))
      (cond (fboundp
             (cond (specialp *sb-fbound-and-special-color*)
                   (classp *sb-fbound-and-class-color*)
                   (t *sb-fbound-color*)))
            (specialp (if classp 
                          *sb-specialp-and-class-color*
                        *sb-specialp-color*))
            (classp *sb-class-color*)
            (t nil)))))
          

;;----------------------------------------------------------------------------
;; symbol-browser-state
;;----------------------------------------------------------------------------

;;; The SYMBOL-BROWSER-STATE object keeps the current state of the interface. 
;;; The IDE tool keeps a record of these to implement the History. 

(defstruct (symbol-browser-state
            (:print-function print-symbol-browser-state))
  search-string
  (symbols :unknown))

(defun print-symbol-browser-state (self stream level)
  (declare (ignore level))
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~S" (symbol-browser-state-search-string self))))

;;; Accessors that access values on the state. 

(defun symbol-browser-search-string (self)
  (when-let (state (symbol-browser-state self))
    (symbol-browser-state-search-string state)))

(defun symbol-browser-symbols (self)
  (when-let (state (symbol-browser-state self))
    (let ((symbols (symbol-browser-state-symbols state)))
      (when (listp symbols)
        symbols))))

(defun (setf symbol-browser-symbols) (symbols self)
  (when-let (state (symbol-browser-state self))
    (setf (symbol-browser-state-symbols state) symbols)
    (symbol-browser-update-visible-symbols self)))


;;----------------------------------------------------------------------------


(defmethod initialize-instance :after  ((self simple-symbol-browser)
                                   &key
                                   (regexp nil regexpp)
                                   (packages :all  packagesp)
                                   (accessibility nil accessibility-p)
                                   filter
                                   (display-type-filter *default-type-filter*
                                                        display-type-filter-p)
                                   &allow-other-keys)
  (when accessibility-p
    (setf (symbol-browser-accessibility self)
          accessibility))
  (when filter
    (setf (symbol-browser-filter self) filter))
  (when display-type-filter-p
    (setf (symbol-browser-display-type-filter self) display-type-filter))
  (when packagesp
    (setf (slot-value self 'packages) packages))
  (when regexpp
    (setf (symbol-browser-state self) 
          (symbol-browser-coerce-to-state regexp self))))


;;; The IDE has a mechanism to keep a history of states. Here
;;; interface-history is just a dummy. 

(defun interface-history (self)
  (declare (ignore self))
  nil)

;;; Convert to object to a symbol-browser-state, which
;;; keeps the state of the interface. 
(defun symbol-browser-coerce-to-state (object self)
  (typecase object
    (symbol-browser-state object)
    (string
     (let ((old-state (find object (interface-history self)
                            :test 'string=
                            :key 'symbol-browser-state-search-string)))
       (if old-state
           (progn
             ;; Make it redo the regexp-find-symbols when given a string.
             (setf (symbol-browser-state-symbols old-state) :recompute)
             old-state)
         (make-symbol-browser-state
          :search-string object))))))

;;; Dummy. The IDE has a more complex mechanism to take various
;;; settings into account.

(defun symbol-browser-print-symbol (self symbol)
  (declare (ignore self)) 
  (prin1-to-string symbol))


;;; That is the callback for the "Regexp:" text-input-pane. 

(defun symbol-browser-new-apropos (self string)
  (setf (symbol-browser-state self) 
        (symbol-browser-coerce-to-state string self)))

;;; Update the description pane at the bottom.

(defun update-description-pane (desc-pane symbol)
  (setf (capi:collection-items desc-pane)
        (list (list "Function" (when (fboundp symbol) (symbol-function symbol)))
              (list "Value" (when (boundp symbol) (symbol-value symbol))))))

;;; This is the selection callback in the symbols pane,
;;; and also used elsewhere to update the description pane. 

(defun symbol-browser-display-symbol-description (symbol self)
  (with-slots (description-pane) self
    (update-description-pane description-pane symbol)))

;;; This is where it actually matches symbols, using
;;; REGEXP-FIND-SYMBOLS.

(defun symbol-browser-matching-symbols (self)
  (let ((packages (symbol-browser-packages self)))
    (regexp-find-symbols (symbol-browser-search-string self)
                         :packages packages)))

;;; Update functions. 
(defmethod interface-update (self)
  (with-slots (apropos-pane) self
    (let ((search-string (symbol-browser-search-string self)))
      (setf (capi:text-input-pane-text apropos-pane) 
            search-string)
      (when (> (length search-string) 0)
        (symbol-browser-update-symbols self)))))


(defun symbol-browser-update-current-view (self)
  (with-slots (apropos-pane) self
    (let ((search-string (symbol-browser-search-string self)))
      (setf (capi:text-input-pane-text apropos-pane) 
            search-string)
      (symbol-browser-update-visible-symbols self))))

;;; This matches the string, which can take time,
;;; so do it inside capi:with-busy-interface. In real application
;;; this may need to be done asynchronously on another thread. 

(defun symbol-browser-update-symbols (self)
  (capi:with-busy-interface (self)
    (setf (symbol-browser-symbols self)
          (symbol-browser-matching-symbols self))))

;;; This computes which of the symbols that matches the
;;; regexp also fits the "accessibility" and the "type"

(defun symbol-browser-compute-visible-symbols (self)
  (let* ((type (symbol-browser-display-type-filter self))
         (packages (symbol-browser-packages self))
         (accessibility (symbol-browser-accessibility self)))
    (loop for symbol in (symbol-browser-symbols self)
      for name = (symbol-name symbol)
      for package = (symbol-package symbol)
      when (and (symbol-of-type symbol type)
                (or (eq accessibility :all)
                    (and (or (eq packages :all)
                             (find package packages))
                         (or (eq accessibility :present)
                             (if (eq (nth-value 1 (find-symbol name package))
                                     :external)
                                 (eq accessibility :externals-only)
                               (eq accessibility :internals-only))))))
      collect symbol)))

;;; The "visible symbols" are the symbols that match the 
;;; "accessibility" and "type" settings. 

(defun symbol-browser-update-visible-symbols (self )
  (with-slots (matches-pane list-up-to-date) self
    (let ((visible-symbols (symbol-browser-compute-visible-symbols self)))
      (setf (symbol-browser-visible-symbols self) visible-symbols)
      (setf (capi:display-pane-text matches-pane)
            (format nil "~D" (length visible-symbols)))
      (setf  list-up-to-date nil)
      (update-symbols-list-pane self t)
      (symbol-browser-display-symbol-description (car visible-symbols) self)
      (capi:redisplay-menu-bar self))))


;;; Setters that also need to do updating 

(defmethod (setf symbol-browser-display-type-filter)
     :after ((options t) (self simple-symbol-browser))
  (symbol-browser-update-visible-symbols self))

(defmethod (setf symbol-browser-packages)
     :after ((options t) (self simple-symbol-browser))
  (interface-update self))

(defmethod (setf symbol-browser-state) :after ((string t)
                                           (self simple-symbol-browser))
  (let ((state (symbol-browser-state self)))
    (if (and state
             (listp (symbol-browser-state-symbols state)))
        (symbol-browser-update-current-view self)
      (interface-update self))))



(defun symbol-of-type (symbol type)
  (or (eq type :all)
      (case type
        (:keywords
         (keywordp symbol))
	(:classes
	 (find-class symbol nil))
	(:functions
	 (fboundp symbol))
        (:constants
         (and (boundp symbol)
              (constantp symbol)
              (not (keywordp symbol))))
	(:variables
         (and (boundp symbol)
              (not (constantp symbol))))
	(:structures
	 (when-let (class (find-class symbol nil))
	   (typep class 'structure-class)))
	(t
	 (not (or (find-class symbol nil)
		  (fboundp symbol)
		  (boundp symbol)))))))




(defun update-symbols-list-pane (self update)
  (when (or update
            (not (slot-value self 'list-up-to-date)))
    (with-slots (symbols-pane) self
      (let* ((visible-symbols (symbol-browser-visible-symbols self))
             (old-selection (capi:choice-selected-items symbols-pane))
             (new-list visible-symbols))
        (setf (capi:collection-items symbols-pane) new-list)
        (setf (capi:choice-selected-items symbols-pane) old-selection)
        (setf (slot-value self 'list-up-to-date) t)))))


;;; Dummy. The IDE has a complex dialog to do that. 

(defun change-symbol-browser-packages (self)
  (declare (ignore self))
  (capi:display-message "Changing packages not implemented"))


;;; Dummies for implementing editing the source.  Rely on 
;;; having the LispWorks IDE, in particular on having an interface of type 
;;; lw-tools::editor that capi:call-editor can be called with it. 

(defun my-find-interface  () (capi:find-interface 'lw-tools::editor))
(defun my-call-function (interface data)
  (capi:call-editor interface (list 'editor:find-source-command
                                    nil data)))

;;; The action callback of the list of symbols

(defun symbol-browser-edit-source  (data self)  
  (declare (ignore self))
  (when-let (interface (my-find-interface))
    (capi:execute-with-interface interface
                                 'my-call-function interface
                                 data)))

;;; Entry point

(defun show-simple-symbol-browser ()
  (capi:display  (make-instance 'simple-symbol-browser 
                                :regexp "DEFAULT")))

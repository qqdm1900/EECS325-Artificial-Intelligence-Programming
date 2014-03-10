;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:rich-text-editor.lisp,v 1.15.1.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;; ----------------------------------------------------------------------
;;; Example of creating a simple editor using capi:rich-text-pane
;;; To run the test
;;;  (CL-USER::MAKE-RICH-TEXT-EDITOR)


(defvar *initial-text*
  "This is the initial text. ")

(defvar *subscript-offset* -3)
(defvar *superscript-offset* 3)

(defvar *default-font-sizes* '(8 9 10 11 12 14 16 18 20 22 24 26 28 36 48 72))

;;; Register bmp files for use with the demo.
;;; RICH-TEXT-EDITOR-TOOLBAR-IMAGES will contain the image set for use
;;; with the toolbar buttons. RICH-TEXT-EDITOR-INFO-IMAGE contains an
;;; image for use with the 'Save modified file...' dialog.
;;; At compile time the external images will be embedded into the binary file.
(gp:register-image-translation
 'rich-text-editor-toolbar-images
 #.(gp:read-external-image (current-pathname "images/richtext.bmp")
                           :transparent-color-index 7))
(gp:register-image-translation
 'rich-text-editor-info-image
 #.(gp:read-external-image (current-pathname "images/info.bmp")
                           :transparent-color-index 7))



;;; Create the image set from 'rich-text-editor-toolbar-images
(defvar *rich-text-editor-toolbar-image-set*
  (capi:make-general-image-set :id 'rich-text-editor-toolbar-images
                               :image-count 10))


(defvar *default-character-format* nil)
(defvar *default-paragraph-format*
  `(:tab-stops (36 72 108 144)))


(defun check-op-enable (interface op)
  (with-slots (text-pane) interface
    (capi:rich-text-pane-operation text-pane op)))

;;; Currently (as of 1st May 1997) in order to add toolbar as part of an interface
;;; then they must be added in top down order, otherwise CAPI will fail to reference
;;; them by name properly.

(capi:define-interface rich-text-editor ()
  ((current-pathname :initform nil :accessor rich-text-editor-current-pathname)
   (character-attributes :initform nil :reader character-attributes)  ;;;caches for the current values
   (paragraph-attributes :initform nil :reader paragraph-attributes)  ;;; updated by the change callback
   )
  (:panes 
   (text-pane capi:rich-text-pane
              :pane-menu popup-edit-menu
              :change-callback 'rich-text-editor-changed-callback
              :character-format *default-character-format*
              :paragraph-format *default-paragraph-format*
              :text *initial-text*
              :horizontal-scroll t)
   (toolbar capi:toolbar
            :items (list font-names font-size char-attribute-buttons
                         select-color-button paragraph-attribute-buttons
                         paragraph-bullets tabbing-component)
            :initial-constraints '(:visible-min-width 600)
            :visible-min-width 1    ; so window can now be made smaller than toolbar
            :visible-max-width nil  ; by default toolbar max size is min size 
            )

   ;; Basic toggle buttons for the character format control: bold,
   ;; italic, and underline.
   (char-attribute-buttons
    capi:toolbar-component
   
    :items (let ((list '((bold-button . :bold)  
                         (italic-button . :italic)
                         (underline-button . :underline))))
             (loop for pair in list
                   for index from 0
                   collect (make-instance 'capi:toolbar-button
                                          :remapped (car pair)
                                          :data (cdr pair)
                                          :image (capi:make-image-locator :image-set *rich-text-editor-toolbar-image-set*
                                      :index index))))
    :tooltips '("Toggle Bold" "Toggle Italic" "Toggle Underline")
    :interaction :multiple-selection
    ;:retract-callback 'rich-text-editor-retract-char-attributes
   ; :selection-callback 'rich-text-editor-select-char-attributes
    )

   (select-color-button 
    capi:toolbar-button
    :image (capi:make-image-locator :image-set *rich-text-editor-toolbar-image-set*
                                    :index 9)
    :tooltip "Modify Color"
    :callback 'rich-text-editor-modify-selection-color
    :callback-type :interface)

   ;; Align paragraph  left, center, or right
   (paragraph-attribute-buttons
    capi:toolbar-component
    :images (list
             (capi:make-image-locator :image-set *rich-text-editor-toolbar-image-set*
                                      :index 4)
             (capi:make-image-locator :image-set *rich-text-editor-toolbar-image-set*
                                      :index 6)
             (capi:make-image-locator :image-set *rich-text-editor-toolbar-image-set*
                                      :index 5))
    :items '(:left :center :right)
    :tooltips '("Align Left" "Align Center" "Align Right")
    :interaction :single-selection
    :selection-callback 'rich-text-editor-select-paragraph-attributes)

   (tabbing-component
    capi:toolbar-component
    :images (list
             (capi:make-image-locator :image-set *rich-text-editor-toolbar-image-set*
                                      :index 7)
             (capi:make-image-locator :image-set *rich-text-editor-toolbar-image-set*
                                      :index 8))
    :items '(:indent :outdent)
    :tooltips '("Indent Paragraph" "Outdent Paragraph")
    :selection-callback 'rich-text-editor-in/outdent-paragraph)

   ;; toggle bullet points
   (paragraph-bullets
    capi:toolbar-component
    :images (list
             (capi:make-image-locator :image-set *rich-text-editor-toolbar-image-set*
                                      :index 3)
             )
    :items '(:bullets)
    :tooltips '("Use Bullets")
    :interaction :multiple-selection
    :retract-callback 'rich-text-editor-retract-paragraph-bullets
    :selection-callback 'rich-text-editor-select-paragraph-bullets)

   ;; toolbar element
   (font-size
    capi:text-input-choice
    :items *default-font-sizes*
    :callback 'rich-text-editor-set-callback-font-size
    :popdown-callback 'rich-text-editor-give-focus-to-text-pane
    :selection-callback 'rich-text-editor-set-selection-callback-font-size)
   
   ;; font-names collection items is filled in at the time the editor is
   ;; created (as we need to query the screen for the available fonts)
   (font-names
    capi:option-pane
    :selection-callback 'rich-text-editor-set-selection-callback-font-name
    :popdown-callback 'rich-text-editor-give-focus-to-text-pane
    :test-function 'string=
    :visible-min-width '(character 20))
   )

  ;; Very basic layout
  (:layouts
   (main-layout
    capi:column-layout
    '(toolbar text-pane)))

  (:menu-bar file-menu edit-menu format-menu)
  (:menus 
   ;; ------------------------------
   ;; File Menus
   (file-menu
    "File"
    (("New" :callback 'rich-text-editor-new-file)
     ("Open..." :callback 'rich-text-editor-load-file)
     ("Insert File..." :callback 'rich-text-editor-insert-file)
     ("Save" :callback 'rich-text-editor-save-file
             :enabled-function 'rich-text-editor-current-pathname)
     ("Save As..." :callback 'rich-text-editor-save-as-file)
     ("Save Selection..." :callback 'rich-text-editor-save-selection
                          :enabled-function 'rich-text-editor-selection-enabled)
     print-component
     ("Exit" :callback 'capi:quit-interface))
    :callback-type :interface)
   (print-component
    :component
    (("Print Setup ..." :callback 'capi:page-setup-dialog :callback-type :none)
     ("Print Preview ..." :enabled-function (constantly nil))
     ("Print ..." :callback #'(lambda (interface)
                                (capi:print-rich-text-pane
                                 (slot-value interface 'text-pane))))
     ("Print Selection ..." :callback #'(lambda (interface)
                                          (capi:print-rich-text-pane
                                           (slot-value interface 'text-pane)
                                           :selection t))
                            :enabled-function 'rich-text-editor-selection-enabled)
     ))

   ;; ------------------------------
   (edit-menu
    "Edit"
    (undo-component standard-edit-component selection-component)
    :callback-type :interface)
   (standard-edit-component
     :component
     (("Cut" :callback 'capi:active-pane-cut
             :enabled-function 'capi:active-pane-cut-p)
      ("Copy" :callback 'capi:active-pane-copy
              :enabled-function 'capi:active-pane-copy-p)
      ("Paste" :callback 'capi:active-pane-paste
               :enabled-function 'capi:active-pane-paste-p)))



    (selection-component
     :component
      (
       ("Select All" :callback 'capi:active-pane-select-all)))
     
    (undo-component
     :component
     (("Undo" :data :undo
              :enabled-function #'(lambda (interface)
                                    (check-op-enable interface :can-undo))
             
              )
      ("Redo" :data :redo
              :enabled-function #'(lambda (interface)
                                    (check-op-enable interface :can-redo))
             
              ))
     :callback-type :data-interface
     :callback #'(lambda (data interface)
                   (with-slots (text-pane) interface
                     (capi:rich-text-pane-operation text-pane data))))

   
   ;; Duplicate for popup menu
   (popup-edit-menu
    "Edit"
    (("Cut" :data :cut
            :enabled-function #'(lambda (interface)
                                  (with-slots (text-pane) interface
                                    (capi:rich-text-pane-operation text-pane :cutp))))
     ("Copy" :data :copy
             :enabled-function #'(lambda (interface)
                                   (with-slots (text-pane) interface
                                     (capi:rich-text-pane-operation text-pane :copyp))))
     ("Paste" :data :paste
              :enabled-function #'(lambda (interface)
                                    (with-slots (text-pane) interface
                                      (capi:rich-text-pane-operation text-pane :pastep))))
     ("Select All" :data :select-all))
    :callback #'(lambda (data interface)
                  (with-slots (text-pane) interface
                    (capi:rich-text-pane-operation text-pane data))))

   ;; ------------------------------
   (format-menu
    "Format"
    (text-format-menu
     alignment-menu-component
     indent-first-component
     character-format-menu
     #-cocoa
     word-wrap-menu))
   (character-format-menu
    :component
    (("Italic"  
                 :data :italic
                 :accelerator #\control-\i
                 :name 'italic-button
                 )
     ("Bold"  
                 :data :bold
                 :accelerator #\control-\b
                 :name 'bold-button)
     ("Underline"  
                 :data :underline
                 :accelerator #\control-\u
                 :name 'underline-button)
     ("strikethrough"  
                 :data :strikeout
                 :accelerator #\control-\k
                 :name 'strikeout-button)
     )
    :selection-callback 'rich-text-editor-select-char-attributes
    :retract-callback 'rich-text-editor-retract-char-attributes
    :callback-type :data-interface
    :popup-callback 'update-character-buttons-selected
    :interaction :multiple-selection
    )
                 
   (text-format-menu
    :component
    (("Superscript" :data :superscript)
     ("Subscript" :data :subscript))
    :selected-item-function #'(lambda (interface)
                                  (let ((offset (getf (character-attributes interface)
                                                      :offset 0)))
                                    (cond ((< 0 offset) :superscript)
                                          ((> 0 offset) :subscript)
                                          ((zerop offset) :none))))
    :selection-callback #'(lambda (data interface)
                            (with-slots (text-pane) interface
                              (let ((offset (if (eq data :superscript)
                                                *superscript-offset*
                                              *subscript-offset*)))
                                (capi:set-rich-text-pane-character-format
                                 text-pane :attributes-plist `(:offset ,offset)))))
    :retract-callback  #'(lambda (data interface)
                           (declare (ignore data))
                            (with-slots (text-pane) interface
                              (capi:set-rich-text-pane-character-format
                               text-pane :attributes-plist `(:offset 0))
                              ))
    :interaction :multiple-selection)
   #-cocoa
   (word-wrap-menu
    "Word Wrap" 
    (visual-component))
   #-cocoa
   (visual-component
    :component 
    (("None" :data :none)
     ("Window" :data :window)
     ("Printer" :data :printer))
    :callback #'(lambda (data interface)
                   (with-slots (text-pane) interface
                     (capi:rich-text-pane-operation
                            text-pane
                            :set-word-wrap data)))
    :interaction :single-selection
    :selected-item-function #'(lambda (interface)
                                 (with-slots (text-pane) interface
                                  (let ((keyword (capi:rich-text-pane-operation text-pane :get-word-wrap)))
                                    (if (keywordp keyword) keyword :printer))))
    )
   (alignment-menu-component
    :component
    (("Align Left" :data :left)
     ("Align Right" :data :right)
     ("Align Center" :data :center))
    :interaction :single-selection
    :selected-item-function #'(lambda (interface)
                                (with-slots (text-pane) interface
                                  (let ((values (capi:rich-text-pane-paragraph-format
                                                 text-pane `(:alignment :numbering))))
                                    (getf values :alignment :left))))
    :selection-callback 'rich-text-editor-select-paragraph-attributes)
   (indent-first-component
    :component
    (("Indent First" :data :indent)
     ("Outdent First" :data :outdent))
    :interaction :multiple-selection
    :selected-item-function #'(lambda (interface)
                                (with-slots (text-pane) interface
                                    (let ((value (getf (capi:rich-text-pane-paragraph-format
                                                        text-pane '(:offset)) :offset 0)))
                                      (cond ((zerop value) :none)
                                            ((> 0 value) :indent)
                                            ((< 0 value) :outdent)))))
    :selection-callback 'rich-text-text-in/outdent-first
    :retract-callback #'(lambda (data interface)
                          (declare (ignore data))
                          (rich-text-text-in/outdent-first :none interface))
    )
   )
  
  (:default-initargs
   :auto-menus nil
   :title "RichTextTest"
   :best-height 300
   :best-width 400
   :message-area t
   :confirm-destroy-callback 'rich-text-editor-confirm-destroy-callback
   )
  )


;;; This is the popup-callback of the component with items for binary
;;; character attributes. Set the selected value for each one of
;;; them 

(defun update-character-buttons-selected (component)
  (capi:map-collection-items 
   component
   #'(lambda (x)
       (let ((data (capi:item-data x))
             (interface (capi:element-interface-for-callback x)))
         (setf (capi:item-selected x)
               (getf (character-attributes interface) data))))))
  
;;; Whenever we set the current-pathname slot we should also update the message area.
(defmethod (setf rich-text-editor-current-pathname)
           :after (pathname (self rich-text-editor))
  (setf (capi:titled-pane-message self)
        (format nil "File - ~A" (or pathname "Unknown"))))

;; Force update of the message area.
(defmethod initialize-instance :after ((self rich-text-editor) &rest initargs)
  (setf (rich-text-editor-current-pathname self)
        (rich-text-editor-current-pathname self)))


;;; ----------------------------------------------------------------------
;;; Predicate to be used by the printing functions

(defun rich-text-editor-selection-enabled (interface)
  (with-slots (text-pane) interface
    (multiple-value-bind (start end)
        (capi:rich-text-pane-operation text-pane :get-selection)
      (/= start end))))


;;; ----------------------------------------------------------------------
;;; Indenting outdenting routines

(defvar *indent-step-size* 18)

(defun rich-text-text-in/outdent-first (data interface)
  (with-slots (text-pane) interface
    (let* ((values ( paragraph-attributes interface))
           (start-indent (+ (getf values :start-indent 0) (getf values :offset 0)))
           (offset (case data
                     (:indent (- *indent-step-size*))
                     (:outdent *indent-step-size*)
                     (:none 0)))
           (new-indent (- start-indent offset)))
      (capi:set-rich-text-pane-paragraph-format text-pane `(:offset ,offset :start-indent ,new-indent)))))
  
(defun rich-text-editor-in/outdent-paragraph (data interface)
  (with-slots (text-pane) interface
    (let* ((values ( paragraph-attributes interface))
           (offset (getf values :offset 0))
           (start-indent (getf values :start-indent 0)))
      (when-let (new-indent (case data
                              (:outdent (when (<= *indent-step-size* (+ offset start-indent))
                                          (- start-indent *indent-step-size*)))
                              (:indent (+ start-indent *indent-step-size*))))
        (capi:set-rich-text-pane-paragraph-format text-pane `(:start-indent ,new-indent))))))

      

;;; ----------------------------------------------------------------------
;;; Modify color callback

(defun rich-text-editor-modify-selection-color (interface)
  (let ((current-color (getf (character-attributes interface) :color)))
    (multiple-value-bind (new-color okp)
        (capi:prompt-for-color "Select color" :color current-color)
      (when okp
        (rich-text-editor-set-char-attributes :color interface new-color)))))

;;; ----------------------------------------------------------------------
;;; Paragraph Formatting

(defun rich-text-editor-select-paragraph-attributes (type interface)
  (with-slots (text-pane) interface
    (capi:set-rich-text-pane-paragraph-format text-pane `(:alignment ,type))))


(defun rich-text-editor-select-paragraph-bullets (type interface)
  (declare (ignore type))
  (with-slots (text-pane) interface
    (capi:set-rich-text-pane-paragraph-format text-pane `(:numbering t))))

(defun rich-text-editor-retract-paragraph-bullets (type interface)
  (declare (ignore type))
  (with-slots (text-pane) interface
    (capi:set-rich-text-pane-paragraph-format text-pane `(:numbering nil))))


;;; ----------------------------------------------------------------------
;;; Character formatting

(defun rich-text-editor-set-char-attributes (type interface on)
   (with-slots (text-pane) interface
    (capi:set-rich-text-pane-character-format text-pane
                                              :attributes-plist `(,type ,on))
    (capi:set-pane-focus text-pane)))

(defun rich-text-editor-give-focus-to-text-pane (pane)
  (with-slots (text-pane) (capi:top-level-interface pane)
    (capi:set-pane-focus text-pane)))

(defun rich-text-editor-select-char-attributes (type interface)
  (rich-text-editor-set-char-attributes type interface t))

(defun rich-text-editor-retract-char-attributes (type interface)
    (rich-text-editor-set-char-attributes type interface nil))

(defun rich-text-editor-toggle-char-attributes (type interface)
  (let ((new-value 
         (not (getf ( character-attributes interface) type))))
    (rich-text-editor-set-char-attributes type interface new-value)))


;;; ------------------------------
;;; Font manipulation

(defun rich-text-editor-set-selection-callback-font-name (data interface)
  (rich-text-editor-set-char-attributes :face interface data))

(defun rich-text-editor-set-selection-callback-font-size (data interface)
  (rich-text-editor-set-char-attributes :size interface data))

(defun rich-text-editor-set-callback-font-size (data interface)
  (multiple-value-bind (size index)
      (parse-integer data :junk-allowed t :radix 10)
    (if (or (not size)
            (find-if-not 'whitespace-char-p data
                         :start index))
        (capi:beep-pane interface)
      (rich-text-editor-set-selection-callback-font-size size interface))))


;;; ----------------------------------------------------------------------
;;; Toolbar updater


(defun rich-text-editor-changed-callback (pane type)
  (declare (ignorable type args))
  (let ((interface (capi:top-level-interface pane)))

    ;; Character formatting

    (with-slots (font-size font-names char-attribute-buttons) interface
      (let* ((values (capi:rich-text-pane-character-format pane
                                                          ))
             (current-size (getf values :size))
             (toolbar-size (capi:choice-selected-item font-size))
             (current-face (getf values :face))
             (toolbar-face (capi:choice-selected-item font-names)))
        (capi:map-collection-items char-attribute-buttons
                                   #'(Lambda(x)
                                       (setf (capi:item-selected x)
                                             (getf values (capi:item-data x)))))

        (unless (string= toolbar-face current-face)
          (setf (capi:choice-selected-item font-names) current-face))
        (unless (eq toolbar-size current-size)
          (setf (capi:choice-selected-item font-size) current-size)
          (unless (capi:choice-selected-item font-size)
            (setf (capi:text-input-pane-text font-size) (if current-size
                                                            (prin1-to-string current-size)
                                                          ""))))

        (setf (slot-value interface 'character-attributes) values )))

    ;; Paragraph formatting

    (with-slots (paragraph-attribute-buttons paragraph-bullets) interface
      (let* ((values (capi:rich-text-pane-paragraph-format pane ))
             (alignment (getf values :alignment :left))
             (numbering (getf values :numbering nil)))
        (setf (capi:choice-selected-item paragraph-attribute-buttons) alignment
              (capi:choice-selected-items paragraph-bullets) (when numbering :bullets))
        (setf (slot-value interface 'paragraph-attributes) values )))
    ))



;;; ----------------------------------------------------------------------
;;; File Open/Save


(defvar *rich-text-editor-filters* `("Rich Text Format" "*.rtf"
                                   "Plain Text" "*.txt"
                                   "Any File" "*.*"))

(defun rich-text-pane-open-file (text-pane &key (format :rtf) selection)
  (multiple-value-bind (filename res)
      (capi:prompt-for-file "Load File" :filter (if (eq format :rtf)
                                               "*.rtf"
                                             "*.txt")
                       :filters *rich-text-editor-filters*)
    (when res
      (capi:rich-text-pane-operation text-pane :load-file filename
                                      :format format
                                      :selection selection)
      filename)))

(defun rich-text-pane-save-file (text-pane &key filename (format :rtf) selection)
  (multiple-value-bind (filename res)
      (if filename
          (values filename t)
        (capi:prompt-for-file "Save File" :filter (if (eq format :rtf)
                                                      "*.rtf"
                                                    "*.txt")
                              :operation :save
                              :filters *rich-text-editor-filters*))
    (when res
      (let ((format (if (string-equal (pathname-type filename) "txt")
                        :text
                      :rtf)))
        (capi:rich-text-pane-operation text-pane :save-file filename
                                        :format format
                                        :selection selection)
        filename))))


    
(defun rich-text-editor-new-file (interface)
  (with-slots (text-pane) interface
    (when (ok-to-destroy-text-pane-contents interface text-pane)
      (setf (capi:rich-text-pane-text text-pane
            :character-plist :default  
            :paragraph-plist :default)
            "")
      (setf (rich-text-editor-current-pathname interface) nil)
      (capi:rich-text-pane-operation text-pane :set-modified nil))))

(defun rich-text-editor-load-file (interface)
  (with-slots (text-pane) interface
    (when (ok-to-destroy-text-pane-contents interface text-pane)
      (setf (rich-text-editor-current-pathname interface)
            (rich-text-pane-open-file text-pane))
       (capi:rich-text-pane-operation text-pane :set-modified nil))))

(defun rich-text-editor-save-as-file (interface)
  (with-slots (text-pane) interface
    (when-let (filename (rich-text-pane-save-file text-pane))
      (setf (rich-text-editor-current-pathname interface) filename)
      (capi:rich-text-pane-operation text-pane :set-modified nil))))

(defun rich-text-editor-save-file (interface)
  (with-slots (text-pane current-pathname) interface
    (when (rich-text-pane-save-file text-pane :filename current-pathname)
      (capi:rich-text-pane-operation text-pane :set-modified nil))))

(defun rich-text-editor-insert-file (interface)
  (with-slots (text-pane) interface
    (rich-text-pane-open-file text-pane :selection t)))

(defun rich-text-editor-save-selection (interface)
  (with-slots (text-pane) interface
    (rich-text-pane-save-file text-pane :selection t)))


;;; ----------------------------------------------------------------------
;;; Save modified contents prompter dialog

(defun ok-to-destroy-text-pane-contents (interface text-pane)
  (or 
   (not (capi:rich-text-pane-operation text-pane :get-modified))
   (let* ((current-filename (rich-text-editor-current-pathname interface))
          (layout (make-instance
                   'capi:row-layout
                   :description (list
                                 (make-instance 'capi:simple-pinboard-layout
                                                :description
                                                (list (make-instance 'capi:image-pinboard-object
                                                                     :image 'rich-text-editor-info-image)))
                                 (make-instance 'capi:title-pane
                                                :text (format nil "Save changes to [~A]"
                                                              (or current-filename "Unknown"))))
                   :gap 10
                   :adjust :center
                   :internal-border 20)))
     (multiple-value-bind (res okp)
         (capi:popup-confirmer layout nil
                               :ok-button "Yes"
                               :no-button "No"
                               :owner interface)
       (when okp
         (if res
             (rich-text-pane-save-file text-pane :filename current-filename)
           t))))))


;;; The above function is also use to verify that quiting the tool isn't going to
;;; lose any data.

(defun rich-text-editor-confirm-destroy-callback (interface)
  (with-slots (text-pane) interface
    (ok-to-destroy-text-pane-contents interface text-pane)))    


;;; ----------------------------------------------------------------------
;;; Tool Creation - (MAKE-RICH-TEXT-EDITOR)
;;; Because we cannot calculate the fonts unless connection to the windows
;;; screen is initiated we need to call ensure-interface-screen.


;;; General tool for listing all applicable font face names (for a given pane)

(defun get-all-font-names (pane)
  (let* ((fonts (graphics-ports:find-matching-fonts pane (graphics-ports:make-font-description :family :wild)))
         (all-font-names (append '("MS Shell Dlg" "MS Shell Dlg 2")   ;; Miscrosoft hacks
                                 (loop for font in fonts
                              for description = (gp:font-description font)
                              collect (graphics-ports:font-description-attribute-value description :family)))))
    (sort (remove-duplicates all-font-names :test 'string=) 'string<)))

(defmethod capi:interface-display :after ((interface rich-text-editor))
  (with-slots (font-names) interface
    (setf (capi:collection-items font-names) (get-all-font-names interface))))

(defun make-rich-text-editor ()
  (let ((interface (make-instance 'rich-text-editor) ))
    (capi:display interface)))


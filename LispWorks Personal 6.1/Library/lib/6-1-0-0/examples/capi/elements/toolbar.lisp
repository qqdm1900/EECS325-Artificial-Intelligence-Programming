;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:toolbar.lisp,v 1.15.1.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/elements/toolbar.lisp
;;
;; This example demonstrates the uses of toolbars in the CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-TOOLBARS)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

;; Images in the toolbar-radio-images bitmap:
;;  0  = One
;;  1  = Two
;;  2  = Red
;;  3  = On
;;  4  = Query
;;  5  = Off
(gp:register-image-translation
 'toolbar-radio-images
 #.(gp:read-external-image (current-pathname "images/toolbar-radio-images.bmp")
                           :transparent-color-index 7))

;; Images in the toolbar-search-images bitmap:
;;  0  = Clear
;;  1  = Enable
(gp:register-image-translation
 'toolbar-search-images
 #.(gp:read-external-image (current-pathname "images/toolbar-search-images.bmp")
                           :transparent-color-index 7))



(capi:define-interface toolbar-test ()
  ()
  (:panes
   (simple-toolbar
    capi:toolbar
    :items '(:new :open :save)
    :images '(:std-file-new :std-file-open :std-file-save)
    :selection-callback 'simple-toolbar-callback
    :callback-type :interface-data
    :title "Files Horizontal"
    :title-position :left
    :help-key "File operations"
    )

   (simple-toolbar-vertical
    capi:toolbar
    :orientation :vertical 
    :items '(:new :open :save)
    :images '(:std-file-new :std-file-open :std-file-save)
    :selection-callback 'simple-toolbar-callback
    :callback-type :interface-data
    :title "Files Vertical"
    :title-position :left
    :help-key "File operations"
    )
   (random-toolbar
    capi:toolbar)
   (random-toolbar-component
    capi:toolbar-component)
   (random-toolbar-setter
    capi:push-button
    :text "Randomize Buttons"
    :help-key "Randomize the images in the buttons on the left."
    :callback 'random-toolbar-randomize
    :callback-type :interface)
   (change-image-toolbar-setter
    capi:push-button
    :text "Swap Images"
    :help-key "Swap the images in the two pairs on the left."
    :callback 'change-image-toolbar-swap
    :callback-type :interface)
   (options-toolbar
    capi:toolbar
    :items
    (list (make-instance
           'capi:toolbar-component
           :items
           (list (make-instance 'capi:toolbar-button :image 0
                                :help-key "Radio button 1"
                                :selection-callback 'radio-toolbar-callback-1)
                 (make-instance 'capi:toolbar-button :image 1
                                :help-key "Radio button 2"
                                :selection-callback 'radio-toolbar-callback-2)
                 (make-instance 'capi:toolbar-button :image 2
                                :help-key "Radio button with red circle"
                                :selection-callback 'radio-toolbar-callback-3))
           :interaction :single-selection)
          (make-instance
           'capi:toolbar-component
           :items
           (list (make-instance
                  'capi:toolbar-button :image 5 :selected-image 3
                  :selection-callback 'toggle-toolbar-callback-radio-buttons-on
                  :help-key "Enable or disable the buttons on the left."
                  :retract-callback 'toggle-toolbar-callback-radio-buttons-off
                  :selected t)
                 (make-instance
                  'capi:toolbar-button :image 4
                  :help-key "Mystery button"
                  :selection-callback 'toggle-toolbar-callback-2-on
                  :retract-callback 'toggle-toolbar-callback-2-off))
           :interaction :multiple-selection))
    :callback-type :interface
    :title "Options"
    :title-position :frame
    :default-image-set (capi:make-general-image-set
                        :id 'toolbar-radio-images
                        :image-count 6)
    )
   (radio-change-toolbar
    capi:toolbar
    :items
    (list (make-instance
           'capi:toolbar-component
           :items
           (list (make-instance 'capi:toolbar-button :image 5 :selected-image 0
                                :help-key "Radio button 1 or Off"
                                :selection-callback 'radio-toolbar-callback-1)
                 (make-instance 'capi:toolbar-button :image 5 :selected-image 1
                                :help-key "Radio button 2 or Off"
                                :selection-callback 'radio-toolbar-callback-2)
                 (make-instance 'capi:toolbar-button :image 5 :selected-image 2
                                :help-key "Radio button red cricle or Off"
                                :selection-callback 'radio-toolbar-callback-3))
           :interaction :single-selection
           :selection 1))
    :callback-type :interface
    :title "Image Swap"
    :title-position :frame
    :default-image-set (capi:make-general-image-set
                        :id 'toolbar-radio-images
                        :image-count 6)
    )
   (self-enable-toolbar
    capi:toolbar
    :items
    (list (make-instance 'capi:toolbar-button :image 0
                         :selection-callback 'self-enable-toolbar-callback-1)
          (make-instance 'capi:toolbar-button :image 1
                         :selection-callback 'self-enable-toolbar-callback-2
                         :enabled nil))
    :callback-type :interface
    :title "Enable 1"
    :title-position :frame
    :default-image-set (capi:make-general-image-set
                        :id 'toolbar-radio-images
                        :image-count 6))
   (selection-change-toolbar
    capi:toolbar
    :items
    (list (make-instance
           'capi:toolbar-component
           :items
           (list (make-instance 'capi:toolbar-button :image 0
                                :help-key "Selection button 1"
                                :selection-callback 'selection-toolbar-selection-callback
                                :retract-callback 'selection-toolbar-retract-callback
                                :data '(0 0 0))
                 (make-instance 'capi:toolbar-button :image 1
                                :help-key "Radio button 2"
                                :selection-callback 'selection-toolbar-selection-callback
                                :retract-callback 'selection-toolbar-retract-callback
                                :data '(0 1 1)))
           :interaction :multiple-selection
           :selection 1)
          (make-instance
           'capi:toolbar-component
           :items
           (list (make-instance 'capi:toolbar-button :image 0
                                :help-key "Selection button 1"
                                :selection-callback 'selection-toolbar-selection-callback
                                :retract-callback 'selection-toolbar-retract-callback
                                :data '(1 0 1))
                 (make-instance 'capi:toolbar-button :image 1
                                :help-key "Radio button 2"
                                :selection-callback 'selection-toolbar-selection-callback
                                :retract-callback 'selection-toolbar-retract-callback
                                :data '(1 1 0)))
           :interaction :multiple-selection
           :selection 1))
    :callback-type :interface-data
    :title "Selection Set"
    :title-position :frame
    :default-image-set (capi:make-general-image-set
                        :id 'toolbar-radio-images
                        :image-count 6)
    )
   (relayout-toolbar
    capi:toolbar
    :items
    (list (make-instance 'capi:toolbar-button :image 0
                         :selection-callback 'relayout-toolbar-callback
                         :dropdown-menu-kind :delayed
                         :dropdown-menu
                         (make-instance 'capi:menu
                                        :items '(1 2))))
    :callback-type :interface)
   (selection-function-toolbar
    capi:toolbar
    :items
    (list (make-instance 'capi:toolbar-component
                         :items (loop for item in '(0 1 2)
                                      collect
                                      (make-instance 'capi:toolbar-button
                                                     :image item
                                                     :data item
                                                     :help-key "Control the selection of the corresponding button in the right-hand half of this toolbar."))
                         :interaction :multiple-selection
                         :selection-callback 'selection-function-toolbar-update-other-toolbar
                         :retract-callback 'selection-function-toolbar-update-other-toolbar
                         :callback-type :interface)
          (make-instance 'capi:toolbar-component
                         :items (loop for item in '(0 1 2)
                                      collect
                                      (make-instance 'capi:toolbar-button
                                                     :image item
                                                     :data item
                                                     :help-key "Selection is changed by the corresponding button in the left-hand half of this toolbar."))
                         :interaction :multiple-selection
                         :selection-function 'selection-function-toolbar-selection-function))
    :title "Selection Function"
    :title-position :frame
    :default-image-set (capi:make-general-image-set
                        :id 'toolbar-radio-images
                        :image-count 6))
   (relayout-other-pane
    capi:title-pane
    :text "Label")
   (menu-toolbar
    capi:toolbar
    :items (list (make-instance
                  'capi:toolbar-component
                  :items
                  (list (make-instance
                         'capi:toolbar-button
                         :image 1
                         :help-key "A toolbar button with a menu"
                         :selection-callback 'menu-toolbar-callback
                         :dropdown-menu (make-instance
                                         'capi:menu
                                         :items '(1 2 3)
                                         :callback 'menu-toolbar-menu-callback
                                         :callback-type :interface-data))))
                 (make-instance 'capi:toolbar-button
                                :dropdown-menu-kind :only
                                :help-key  "A Toolbar button which contains only a menu"
                                :dropdown-menu (make-instance 'capi:menu
                                                              :items '(4 5 6)
                                                              :callback 'menu-toolbar-menu-callback
                                                              :callback-type :interface-data))
                 (make-instance 'capi:toolbar-button
                                :image 2
                                :help-key "A toolbar button with a callback and a delayed menu (click and hold to get the menu)"
                                :selection-callback 'menu-toolbar-callback
                                :dropdown-menu-kind :delayed
                                :dropdown-menu (make-instance 'capi:menu
                                                              :items '(7 8 9)
                                                              :callback 'menu-toolbar-menu-callback
                                                              :callback-type :interface-data))
                 (make-instance
                  'capi:toolbar-component
                  :items
                  (list (make-instance 'capi:toolbar-button
                                       :image 3
                                       :help-key "A toolbar button with a popup-interface"
                                       :popup-interface (make-popup-toolbar-slider)))))
    :callback-type :interface
    :title "Menus"
    :title-position :frame)
   (element-toolbar
    capi:toolbar
    :items (list (make-instance 'capi:text-input-pane
                                :callback 'element-toolbar-search-callback)
                 :clear
                 :enable)
    :images '(nil 0 1)
    :default-image-set (capi:make-general-image-set
                        :id 'toolbar-search-images
                        :image-count 2)
    :selection-callback 'element-toolbar-callback
    :callback-type :interface-data
    :title "Search"
    :title-position :frame)
   #+comment
   (element-toolbar
    capi:toolbar
    :items (list (make-instance 'capi:toolbar-component
                                :items (list (make-instance 'capi:text-input-pane
                                                            :callback 'element-toolbar-search-callback)
                                             :clear
                                             :enable)
                                :images '(nil 0 1)
                                :selection-callback 'element-toolbar-callback
                                :callback-type :interface-data))
    :title "Search"
    :title-position :frame)
   (change-image-toolbar
    capi:toolbar
    :items (list (make-instance 'capi:toolbar-button
                                :data :left
                                :image 0)
                 (make-instance 'capi:toolbar-button
                                :data :right
                                :image 1))
    :default-image-set (capi:make-general-image-set
                        :id 'toolbar-radio-images
                        :image-count 5)
    :selection-callback 'simple-toolbar-callback
    :callback-type :interface-data)
   (change-selected-image-toolbar
    capi:toolbar
    :items (list (make-instance 'capi:toolbar-component
                                :interaction :multiple-selection
                                :items (list (make-instance 'capi:toolbar-button
                                                            :data :left
                                                            :image 0
                                                            :selected-image 2)
                                             (make-instance 'capi:toolbar-button
                                                            :data :right
                                                            :image 1
                                                            :selected-image 3))
                                :selection '(0 1)))
    :default-image-set (capi:make-general-image-set
                        :id 'toolbar-radio-images
                        :image-count 5)
    :selection-callback 'change-selected-image-toolbar-callback-on
    :retract-callback 'change-selected-image-toolbar-callback-off
    :callback-type :interface-data)
   (text-toolbar
    capi:toolbar
    :items (list (make-instance 'capi:toolbar-component
                                :items (list (make-instance
                                              'capi:toolbar-button
                                              :text "Toggle"
                                              :selection-callback 'text-toolbar-toggle-callback
                                              :retract-callback 'text-toolbar-toggle-callback))
                                :interaction :multiple-selection)
                 (make-instance 'capi:toolbar-button
                                :text "Randomize Number"
                                :selection-callback 'text-toolbar-switch-callback)
                 (make-instance 'capi:toolbar-button
                                :data 1
                                :selection-callback 'text-toolbar-number-callback)
                 (make-instance 'capi:toolbar-component
                                :items (list change-text-toolbar-button)))
    :callback-type :interface
    :title "Text"
    :title-position :frame)
   (change-text-toolbar-button
    capi:toolbar-button
    :text "Menu 10"
    :callback-type :interface
    :selection-callback #'(lambda (self)
                            (capi:display-popup-menu
                             (make-change-text-toolbar-button-menu self)))
    :dropdown-menu-kind :delayed
    :dropdown-menu (make-change-text-toolbar-button-menu capi:interface))
   (message-pane
    capi:title-pane
    :text "Click on a button."
    :max-width nil))
  (:layouts
   (random-toolbar-layout
    capi:row-layout
    '(random-toolbar random-toolbar-setter)
    :title "Random"
    :title-position :frame
    :y-adjust :center
    :x-gap 20)
   (change-image-toolbar-layout
    capi:row-layout
    '(change-image-toolbar change-selected-image-toolbar change-image-toolbar-setter)
    :title "Change Images"
    :title-position :frame
    :y-adjust :center
    :x-gap 20)
   (sub-layout 
    capi:row-layout
    '(options-toolbar radio-change-toolbar)
    :x-gap 10)
   (relayout-toolbar-layout
    capi:row-layout
    '(relayout-other-pane relayout-toolbar)
    :title "Relayout"
    :title-position :frame
    :y-adjust :center)
   (crazy-layout 
    capi:row-layout
    '(self-enable-toolbar selection-change-toolbar relayout-toolbar-layout)
    :x-gap 10)
   (element-menu-layout
    capi:row-layout
    '(element-toolbar menu-toolbar)
    :x-gap 10)
   (files-layout
    capi:row-layout
    '(simple-toolbar simple-toolbar-vertical)
    :title "Files"
    :title-position :frame
    :y-adjust :center
    :x-gap 30)
   (main-layout
    capi:column-layout
    '(files-layout sub-layout
                     crazy-layout
                     selection-function-toolbar
                     element-menu-layout
                     random-toolbar-layout
                     change-image-toolbar-layout
                     text-toolbar
                     message-pane)
    :default t))
  (:default-initargs
   :title "Toolbar test"
   :help-callback 'do-tooltip-help
   :best-width 300))

(defmethod initialize-instance :after ((self toolbar-test) &key)
  (with-slots (random-toolbar random-toolbar-component) self
    (setf (capi:collection-items random-toolbar-component)
          (make-random-toolbar-items))
    (setf (capi:collection-items random-toolbar)
          (cons random-toolbar-component (make-random-toolbar-items)))))

(defun make-random-toolbar-items ()
  (loop repeat 3
        collect (random 5)))

(defun set-toolbar-test-message (self message)
  (with-slots (message-pane) self
    (setf (capi:title-pane-text message-pane)
          (format nil "~A was selected." message))))

(defun make-change-text-toolbar-button-menu (self)
  (make-instance
   'capi:menu
   :items '("Menu 10" "Menu 100" "Menu 1000")
   :callback-type :data
   :callback (lambda (num)
               (with-slots (change-text-toolbar-button) self
                 (setf (capi:item-text change-text-toolbar-button)
                       num)))))

(capi:define-interface popup-toolbar-slider ()
  ()
  (:panes
   (slider capi:slider
           :orientation :vertical
           :callback #'(lambda (slider where operation)
                         (declare (ignore where))
                         (when (eq operation :move)
                           (capi:hide-interface (capi:element-interface slider) nil)))))
  (:default-initargs
   :title "Slider"))

(defun make-popup-toolbar-slider ()
  (make-instance 'popup-toolbar-slider))


;;----------------------------------------------------------------------------
;; Callbacks
;;----------------------------------------------------------------------------

(defun simple-toolbar-callback (self item)
  (set-toolbar-test-message self (string-capitalize item)))

(defun simple-toolbar-callback-1 (self)
  (set-toolbar-test-message self "New file"))

(defun simple-toolbar-callback-2 (self)
  (set-toolbar-test-message self "Open file"))

(defun simple-toolbar-callback-3 (self)
  (set-toolbar-test-message self "Save file"))

(defun radio-toolbar-callback-1 (self)
  (set-toolbar-test-message self "Radio 1"))

(defun radio-toolbar-callback-2 (self)
  (set-toolbar-test-message self "Radio 2"))

(defun radio-toolbar-callback-3 (self)
  (set-toolbar-test-message self "Radio Red"))

(defun toggle-toolbar-callback-radio-buttons-on (self)
  (toggle-toolbar-callback-radio-buttons-on-off self t))

(defun toggle-toolbar-callback-radio-buttons-off (self)
  (toggle-toolbar-callback-radio-buttons-on-off self nil))

(defun toggle-toolbar-callback-radio-buttons-on-off (self on-p)
  (with-slots (options-toolbar) self
    (let ((radio-toolbar-comonent (elt (capi:collection-items options-toolbar) 0)))
      (setf (capi:simple-pane-enabled radio-toolbar-comonent) on-p)
      (set-toolbar-test-message self (if on-p "Radio buttons on" "Radio buttons off")))))

(defun toggle-toolbar-callback-2-on (self)
  (set-toolbar-test-message self "Mystery toggle on"))

(defun toggle-toolbar-callback-2-off (self)
  (set-toolbar-test-message self "Mystery toggle off"))

(defun self-enable-toolbar-callback-1 (self)
  (self-enable-toolbar-callback-aux self 0 "Self Enable 1"))

(defun self-enable-toolbar-callback-2 (self)
  (self-enable-toolbar-callback-aux self 1 "Self Enable 2"))

(defun self-enable-toolbar-callback-aux (self index message)
  (with-slots (self-enable-toolbar) self
    (let ((items (capi:collection-items self-enable-toolbar)))
      (setf (capi:simple-pane-enabled (elt items index)) nil
            (capi:simple-pane-enabled  (elt items (- 1 index))) t)
      (set-toolbar-test-message self message))))

(defun selection-toolbar-selection-callback (self item)
  (with-slots (selection-change-toolbar message-pane) self
    (destructuring-bind (comp-index button-index set-button-index) item
      (let* ((components (capi:collection-items selection-change-toolbar))
             (component (elt components (- 1 comp-index))))
        (pushnew set-button-index (capi:choice-selection component))
        (setf (capi:title-pane-text message-pane)
              (format nil "Selection ~D of ~A selected ~D of ~A"
                      (1+ button-index) (elt '("left" "right") comp-index)
                      (1+ set-button-index) (elt '("left" "right") (- 1 comp-index))))))))

(defun selection-toolbar-retract-callback (self item)
  (with-slots (selection-change-toolbar message-pane) self
    (destructuring-bind (comp-index button-index set-button-index) item
      (let* ((components (capi:collection-items selection-change-toolbar))
             (component (elt components (- 1 comp-index))))
        (setf (capi:choice-selection component)
              (remove set-button-index (capi:choice-selection component)))
        (setf (capi:title-pane-text message-pane)
              (format nil "Retraction ~D of ~A retracted ~D of ~A"
                      (1+ button-index) (elt '("left" "right") comp-index)
                      (1+ set-button-index) (elt '("left" "right") (- 1 comp-index))))))))

(defun relayout-toolbar-callback (self)
  (with-slots (relayout-toolbar-layout) self
    (setf (capi:layout-description relayout-toolbar-layout)
          (reverse (capi:layout-description relayout-toolbar-layout)))))

(defun selection-function-toolbar-update-other-toolbar (self)
  (with-slots (selection-function-toolbar) self
    (capi:update-toolbar selection-function-toolbar)))

(defun selection-function-toolbar-selection-function (self)
  (with-slots (selection-function-toolbar) self
    (let ((controlling-component
           (capi:get-collection-item selection-function-toolbar 0)))
      (capi:choice-selection controlling-component))))

(defun element-toolbar-callback (self item)
  (with-slots (element-toolbar) self
    (case item
      (:clear
       (setf (capi:text-input-pane-text (elt (capi:collection-items element-toolbar) 0))
             "")
       (set-toolbar-test-message self "Text cleared"))
      (:enable
       (let* ((tip (elt (capi:collection-items element-toolbar) 0))
              (enabled (not (capi:simple-pane-enabled tip))))
         (setf (capi:simple-pane-enabled tip) enabled)
         (set-toolbar-test-message self (if enabled
                                            "Text enabled"
                                          "Text disabled")))))))

(defun random-toolbar-randomize (self)
  (with-slots (random-toolbar random-toolbar-component message-pane) self
    (let ((items (make-random-toolbar-items)))
      (if (zerop (random 2))
          (progn
            (setf (capi:collection-items random-toolbar)
                  (cons random-toolbar-component items))
            (setf (capi:title-pane-text message-pane)
                  "Randomized the toolbar items"))
        (progn
          (setf (capi:collection-items random-toolbar-component)
                items)
          (setf (capi:title-pane-text message-pane)
                "Randomized the toolbar component"))))))



(defun change-image-toolbar-swap (self)
  (with-slots (change-image-toolbar change-selected-image-toolbar message-pane)
      self
    (let ((buttons (capi:collection-items change-image-toolbar)))
      (capi:with-atomic-redisplay (change-image-toolbar)
        (rotatef (capi:toolbar-button-image (elt buttons 0))
                 (capi:toolbar-button-image (elt buttons 1)))))
    (let ((buttons (capi:collection-items
                    (elt (capi:collection-items change-selected-image-toolbar)
                         0))))
      (capi:with-atomic-redisplay (change-selected-image-toolbar)
        (rotatef (capi:toolbar-button-selected-image (elt buttons 0))
                 (capi:toolbar-button-selected-image (elt buttons 1)))))
    (setf (capi:title-pane-text message-pane)
          "Swapped the toolbar images")))

(defun change-selected-image-toolbar-callback-on (self item)
  (set-toolbar-test-message self (string-append (string-capitalize item)
                                                " toggle on")))

(defun change-selected-image-toolbar-callback-off (self item)
  (set-toolbar-test-message self (string-append (string-capitalize item)
                                                " toggle off")))

(defun text-toolbar-toggle-callback (self)
  (with-slots (text-toolbar) self
    (let ((toggle-button (elt (capi:collection-items
                               (elt (capi:collection-items text-toolbar) 0))
                              0)))
    (set-toolbar-test-message
     self (string-append "Text Toggle "
                         (if (capi:item-selected toggle-button)
                             "On"
                           "Off"))))))

(defun text-toolbar-switch-callback (self)
  (with-slots (text-toolbar) self
    (let ((number-button (elt (capi:collection-items text-toolbar) 2)))
      (setf (capi:item-data number-button)
            ;; A random number that often changes in length.
            (+ 1 (random 9) (* (1+ (random 9)) (expt 10 (random 5))))))
    (set-toolbar-test-message self "Randomize Number")))

(defun text-toolbar-number-callback (self)
  (with-slots (text-toolbar) self
    (let ((number-button (elt (capi:collection-items text-toolbar) 2)))
      (set-toolbar-test-message self
                                (format nil "Number ~D"
                                        (capi:item-data number-button))))))

(defun menu-toolbar-callback (self)
  (set-toolbar-test-message self "Menu button"))

(defun menu-toolbar-menu-callback (self item)
  (set-toolbar-test-message self
                            (format nil "Menu item ~S" item)))




;;; Simple help-callback, do only :TOOLTIP and only
;;; if the button has a key that is a string, which 
;;; must have been specified by :help-key. 

(defun do-tooltip-help (interface pane type key)
  (declare (ignorable interface pane))
  (and (eq type :tooltip) 
       (stringp key)
       key))

;;----------------------------------------------------------------------------
;; test-toolbars
;;----------------------------------------------------------------------------

(defun test-toolbars ()
  (capi:display (make-instance 'toolbar-test)))





;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:help.lisp,v 1.6.1.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;; ----------------------------------------------------------------------
;;;
;;; Example use of help callbacks for processing context help,
;;;  tooltip help, and pointer documentation.
;;;
;;; To try it, compile and load this file and then execute:
;;;
;;;      (CL-USER::TEST-HELP-WITH-HELP)
;;;
;;; or
;;;
;;;      (CL-USER::TEST-HELP-NO-HELP)
;;;
;;;
;;; ----------------------------------------------------------------------

#|

Interfaces must have a help callback specified in order to process any
type of help.  A help callback can be specified by supplying the
:HELP-CALLBACK initarg to an interface or using the method (SETF
CAPI:INTERFACE-HELP-CALLBACK) to set the callback on an existing
interface.  If the interface has no help callback, then all help
processing is disabled for the interface.

The help callback receives the following arguments:

  INTERFACE    The top level interface.
  PANE         The target pane that the help is requested for.
  TYPE         The type of help.
  KEY          A key that can be used to distinquish the help requested.

There are 3 types of help: 

 context help (such as that requested from pressing F1 on Windows and Motif)
 tooltip help (i.e. hints, clues, tips, bubble help, balloon help)
 pointer documentation

TYPE may be one of the following:

  :HELP                           Context help.
  :TOOLTIP                        Tooltip help.
  :POINTER-DOCUMENTATION-ENTER    Pointer documentation enter
  :POINTER-DOCUMENTATION-LEAVE    ... leave

Context help typically posts detailed help.  The callback is expected
to perform this operation asynchronously with respect to the running
interface.  For example, the callback might arrange to popup a dialog
to allow the user to choose or confirm a help topic and then start a
WEB browser to display the topic.

Tooltip help is posted in physical proximity to the target pane.  The
callback is expected to return a string to be displayed in the tooltip
popup pane.  If the callback returns NIL, no tooltip popup is
displayed for the target pane.

Pointer documentation typically describes operations that can be
performed on the target pane.  The callback should set/clear the text
of a pane dedicated for pointer documentation when
:POINTER-DOCUMENTATION-ENTER and :POINTER-DOCUMENTATION-LEAVE types
are received respectively.  

KEY can be used to distinguish the help topic independent of the PANE
parameter passed to the help callback.  The default key for a pane is
its name as returned by CAPI:CAPI-OBJECT-NAME.  An alternate key can
be specified by using the :HELP-KEY initarg to a CAPI pane.  If
:HELP-KEY is NIL, then no help (i.e. context help, tooltip help, or
pointer documentation) is available for the pane.  If the :HELP-KEY is
T, then the default key is used.

The default value for :HELP-KEY is T on all panes except
CAPI:DISPLAY-PANE, CAPI:TITLE-PANE, and layouts where the default
value is NIL.

Tooltips and pointer documentation can be enabled and disabled for an
interface.  The initarg :ENABLE-TOOLTIPS or the method (SETF
CAPI:INTERFACE-ENABLE-TOOLTIPS) can be used to enable or disable
tooltips for the interface.  The default value for :ENABLE-TOOLTIPS is
T The initarg :ENABLE-POINTER-DOCUMENTATION or the method (SETF
CAPI:INTERFACE-ENABLE-POINTER-DOCUMENTATION) can be used to enable or
disable pointer documentation for the interface.  The default value
for :ENABLE-POINTER-DOCUMENTATION is T.

The mechanism above does not work for PINBOARD-OBJECTs. The example
below shows how the mechanism can be implemented using the :motion
gesture in the :input-model and calling CAPI:DISPLAY-TOOLTIP, and
normally that what the application does. The example actually gets the string
for the tooltip using the function that is used as the help-callback of the 
interface (HELP-EXAMPLE-HELP-CALLBACK), but it can do it in any other way.

|#

;;; ----------------------------------------------------------------------

(capi:define-interface help-example ()
  ()
  (:menus
   (color-menu "Colours" (:red :green :blue)
               :print-function 'string-capitalize))
  (:menu-bar color-menu)
  (:panes

   (button1
    capi:push-button
    :text "Normal"
    :callback-type :interface-item
    :selection-callback 'help-example-activate)

   (button2
    capi:push-button
    :text "Multiple Lines"
    :callback-type :interface-item
    :selection-callback 'help-example-activate)

   ;; This pane has no tooltip help because the
   ;; help-callback returns NIL for the tooltip
   ;; help string.
   (button3
    capi:push-button
    :text "No tooltip help"
    :callback-type :interface-item
    :selection-callback 'help-example-activate)

   (button4
    capi:push-button
    :text "No context help"
    :callback-type :interface-item
    :selection-callback 'help-example-activate)

   (button5
    capi:push-button
    :text "No pointer documentation"
    :callback-type :interface-item
    :selection-callback 'help-example-activate)

    (button6
     capi:push-button
    :text "Pretty long tooltip documentation"
    :callback-type :interface-item
    :selection-callback 'help-example-activate)

   (button7
    capi:push-button
    :text "Longer than the screen width?"
    :callback-type :interface-item
    :selection-callback 'help-example-activate)

   ;; With default button names and help-keys...
   (check-panel
    capi:check-button-panel
    :items '(:red :orange :yellow :blue :indigo :violet)
    :print-function #'string-capitalize
    :title "Default button names and help keys")

   ;; With help-keys...
   (radio-panel
    capi:radio-button-panel
    :items '(:red :orange :yellow :blue :indigo :violet)
    :print-function #'string-capitalize
    :help-keys '(:help-red :help-orange :help-yellow :help-blue
                 :help-indigo :help-violet)
    :title "With button help keys")

   ;; With button names...
   (push-panel
    capi:push-button-panel
    :items '(:red :orange :yellow :blue :indigo :violet)
    :print-function #'string-capitalize
    :names '(red orange yellow blue indigo violet)
    :title "With button names")

   (push-panel2
    capi:push-button-panel
    :items '(:active :inactive :active :inactive)
    :print-function #'string-capitalize
    :help-keys '(:active nil :active nil)
    :title "Some buttons with help")

   (text1
    capi:text-input-pane
    :text "Hello World"
    :title "Help on a TEXT-INPUT-PANE"
    ;; Override default help-key (i.e. text1 as returned by CAPI-OBJECT-NAME).
    :help-key :text)

   (text2
    capi:text-input-pane
    :text "The quick brown fox"
    :visible-min-width '(:character 50)
    :title "No help, tooltip, or pointer documentation"
    :title-position :frame
    ;; A help-key of NIL means no context help, tooltip help, or
    ;; pointer documentation for this pane.
    :help-key nil)

   (yow
    capi:display-pane
    :text "YOW Phrase"
    :visible-min-width '(:character 50)
    :title "Enable help with :HELP-KEY of T for DISPLAY-PANE."
    :help-key t)

   (output
    capi:output-pane
    :title "Output Pane"
    :background :red)

   (option
    capi:option-pane
    :title "Option Pane"
    :items '(:red :green :blue))

   (pinboard-line
    capi:line-pinboard-object
    :x 50
    :y 50
    :width 50
    :height 50
    :help-key :pinboard-line)
   (pinboard-title
    capi:title-pane
    :x 10
    :y 10
    :text "Pinboard"
    :help-key :pinboard-title)

   ;; These are panes that don't have help.
   ;; You can enable help for these panes by specifying a non-NIL :HELP-KEY initarg.
   
   (display-pane
    capi:display-pane
    :text "DISPLAY-PANE")
   (title-pane
    capi:title-pane
    :text "TITLE-PANE"))

  (:layouts

   (pinboard
    capi:pinboard-layout
    '(pinboard-title pinboard-line)
    :background :white
    :title "Pinboard"
    :visible-min-width 150
    :visible-min-height 150
    :input-model '((:motion my-pinboard-motion-function)))

   (no-help-layout
    capi:column-layout
    '(display-pane title-pane)
    :title "Panes with no help"
    :title-position :frame)

   (button-layout
    capi:column-layout
    '(button1 button2 button3 button4 button5 button6 button7)
    :title "General"
    :title-position :frame)

   (button-panels-layout
    capi:column-layout
    '(check-panel radio-panel push-panel push-panel2)
    :title "Button Panels"
    :title-position :frame)

   (column-1-layout
    capi:column-layout
    '(button-layout
      output
      option
      no-help-layout))

   (column-2-layout
    capi:column-layout
    '(button-panels-layout
      text1
      text2
      yow
      pinboard))

   (main-layout
    capi:row-layout
    '(column-1-layout column-2-layout)))

  (:default-initargs
   :layout 'main-layout
   :title "Help Example"
   ;; For displaying pointer documentation.
   :message-area t))

;;; ----------------------------------------------------------------------

(defun help-example-help-callback (interface pane type key)
  (ecase type

    ;; CONTEXT HELP
    (:help

     (unless (eq key 'button4)
       (capi:display-message "Help for pane ~a (Key=~s)" pane key)))

    ;; TOOLTIP HELP
    (:tooltip

     (typecase key

       (string

        (cond
         ;; For the button-panel with default button names.
         ((member key '("button-0" "button-1" "button-2"
                                   "button-3" "button-4" "button-5")
                  :test 'string-equal)
          (string-upcase key))

         (t
          (error "Don't know about key ~s~%" key))))


       (symbol

        (ecase key

          (button1
           (string-upcase key))
          ;; Multiline.
          (button2
           (format nil "Multiline tooltip help.~%Line two of multiline tooltip help."))
          ;; No tooltip help.
          (button3
           nil)
          (button4
           (string-upcase key))
          (button5
           (string-upcase key))
          ;; Long line.
          (button6
           "Now is the time for all good men to come to the aid of their country.  The quick brown fox jumped over the moon.  I sot I saw a puddy cat!")
          ;; Line hopefully longer than the screen width.
          (button7
           "Now is the time for all good men to come to the aid of their country.  The quick brown fox jumped over the moon.  I sot I saw a puddy cat!|1|Now is the time for all good men to come to the aid of their country.  The quick brown fox jumped over the moon.  I sot I saw a puddy cat!|2|Now is the time for all good men to come to the aid of their country.  The quick brown fox jumped over the moon.  I sot I saw a puddy cat!")

          (:text
           "Input some text")

          (yow
	   (fetch-yow))

          (output
           "Output Pane")

          (option
           "Option Pane")

          ;; For the :HELP-KEYS initarg to button panels.
          ((:help-red :help-orange :help-yellow :help-blue :help-indigo :help-violet)
           (string-upcase key))

          ;; For button-panel with specified button names.
         ((red orange yellow blue indigo violet)
          (string-upcase key))

         (:active
          "This button has help")

          (:pinboard-title
           "Pinboard TITLE-PANE")
          (:pinboard-line
           "Pinboard LINE object")

          ;; +++
          ;; Currently button-panel buttons don't have names.
          ;; This causes NIL to be passed as the key unless
          ;;  the HELP-KEYS patch to button panels is applied.
          ;; Maybe this should be fixed?
          ;;((nil) (format nil "Tooltip for ~s" pane))

          ))))

    ;; POINTER DOCUMENTATION
    (:pointer-documentation-enter
     (setf (capi:titled-pane-message interface)
           (cond ((eq key 'button5) "")
                 (t (string-upcase key)))))

    (:pointer-documentation-leave
     (setf (capi:titled-pane-message interface) ""))))


(defun my-pinboard-motion-function (layout x y) 
  (let ((string (when-let (po (capi:pinboard-object-at-position layout x y))
                  (when-let (key (capi:help-key po))
                    (help-example-help-callback
                     (capi:top-level-interface layout)
                     layout
                     :tooltip
                     key)))))
    (capi:display-tooltip layout :text string :x x :y y)))
    


(defun help-example-activate (interface item)
  (setf (capi:titled-pane-message interface)
        (format nil "Activate button ~s" item)))

;;; ----------------------------------------------------------------------

(defun test-help-with-help ()
  (capi:display (make-instance 'help-example :help-callback 'help-example-help-callback)))

;;; No help callback.
(defun test-help-no-help ()
  (capi:display (make-instance 'help-example)))

;;; ----------------------------------------------------------------------

;;; Dynamic changing of help parameters.

(defun enable-help-callback (interface)
  (setf (capi:interface-help-callback interface) 'help-example-help-callback))

(defun disable-help-callback (interface)
  (setf (capi:interface-help-callback interface) nil))

(defun enable-tooltips (interface)
  (setf (capi:interface-tooltips-enabled interface) t))

(defun disable-tooltips (interface)
  (setf (capi:interface-tooltips-enabled interface) nil))

(defun enable-pointer-documentation (interface)
  (setf (capi:interface-pointer-documentation-enabled interface) t))

(defun disable-pointer-documentation (interface)
  (setf (capi:interface-pointer-documentation-enabled interface) nil))

;;; ----------------------------------------------------------------------

(defun fetch-yow ()
  (let* ((phrases '(
                    ;; from YOW...
                    "if it GLISTENS, gobble it!!"
                    "Like I always say -- nothing can beat the BRATWURST  here in DUSSELDORF!!"
                    "A can of ASPARAGUS, 73 pigeons, some LIVE ammo, and a FROZEN DAQUIRI!!"
                    "ANN JILLIAN'S HAIR makes LONI ANDERSON'S HAIR look like RICARDO MONTALBAN'S HAIR!"
                    "Do I have a lifestyle yet?"
                    "HOORAY, Ronald!!  Now YOU can marry LINDA RONSTADT too!!"
                    "I guess you guys got BIG MUSCLES from doing too much STUDYING!"
                    "I request a weekend in Havana with Phil Silvers!"
                    "I'm reporting for duty as a modern person.  I want to do the Latin Hustle now!"
                    "Is this going to involve RAW human ecstasy?"
                    "Mary Tyler Moore's SEVENTH HUSBAND is wearing my DACRON TANK TOP in a cheap hotel in HONOLULU!"
                    "Oh my GOD -- the SUN just fell into YANKEE STADIUM!!"
                    "PARDON me, am I speaking ENGLISH?"
                    "SHHHH!!  I hear SIX TATTOOED TRUCK-DRIVERS tossing ENGINE BLOCKS into empty OIL DRUMS.."
                    "This PIZZA symbolizes my COMPLETE EMOTIONAL RECOVERY!!"
                    "Used staples are good with SOY SAUCE!"
                    "Wow!  Look!!  A stray meatball!!  Let's interview it!"
                    "Zippy's brain cells are straining to bridge synapses..."))
	 (index (random (length phrases))))

    (elt phrases index)))

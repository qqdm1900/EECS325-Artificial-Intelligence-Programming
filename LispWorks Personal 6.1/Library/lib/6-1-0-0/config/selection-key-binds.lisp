;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/63/LISPeditor/RCS/selection-key-binds.lisp,v 1.10.4.1 2011/08/24 13:26:12 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.


(in-package "CL-USER")

;;; Key bindings shared between MSW and Mac emulations.

;;; This file is loaded automatically the first time that the emulation is used. 
;;; The modes  "Pc Echo" and "Pc Execute" replace the modes "Echo Area" 
;;; and "Execute" respectively. 
;;; All the Emacs commands are available in the other emulations by prefixing them with Ctrl-e


(defvar *selection-bind-key*
  (lambda (&rest args)
    (declare (ignore args))
    (error "Bind *selection-bind-key* while loading this file.")))

(flet ((selection-bind-key (command key &rest specific)
         (if specific
             (apply 'editor:bind-key command key specific)
           (funcall *selection-bind-key* command key))))

(editor:defmode "Pc Echo" :major-p t :no-redefine t)
(editor:defmode "Pc Execute"  :no-redefine t)


;; yank in Emacs
(selection-bind-key  "Un-Kill"                                                  "F16")
(selection-bind-key  "Save Region Preserving Selection"                         "F18")
(selection-bind-key  "Kill Region"                                              "F20")



;;; Prefix Arguments

(selection-bind-key "Negative Argument" "Control-\-")
(selection-bind-key "Argument Digit" "Control-0")
(selection-bind-key "Argument Digit" "Control-1")
(selection-bind-key "Argument Digit" "Control-2")
(selection-bind-key "Argument Digit" "Control-3")
(selection-bind-key "Argument Digit" "Control-4")
(selection-bind-key "Argument Digit" "Control-5")
(selection-bind-key "Argument Digit" "Control-6")
(selection-bind-key "Argument Digit" "Control-7")
(selection-bind-key "Argument Digit" "Control-8")
(selection-bind-key "Argument Digit" "Control-9")


;;; Help

(selection-bind-key "Help" :help)

(selection-bind-key "What Cursor Position" "Control-=")


;;; Executing Commands

(selection-bind-key "Extended Command" "Meta-x")




;;; Marking

(selection-bind-key "Set Mark" "Control-\@")
(selection-bind-key "Set Mark" "Control-Space")


;;; Deletion

(selection-bind-key "Delete Next Character" "Delete")
(selection-bind-key "Delete Previous Character" "Backspace")
(selection-bind-key "Kill Previous Word" "Control-Backspace")


;;; Insertion

(selection-bind-key "Overwrite Mode" :insert)

(selection-bind-key "Un-Kill" "Shift-Insert")
(selection-bind-key "New Line" "Return")


;;; Transposition

(selection-bind-key "Transpose Characters" "Control-t")


;;; Indentation

(selection-bind-key "Indent" "Tab")
(selection-bind-key "Indent" "F8")
(selection-bind-key "Indent Region" "Ctrl-F8")
(selection-bind-key "Indent Rigidly" "Ctrl-Shift-F8")
(selection-bind-key "Indent New Line" "Linefeed")


;;; Regular Expression Searching

(selection-bind-key "ISearch Forward Regexp" "Meta-F3")
(selection-bind-key "ISearch Backward Regexp" "Meta-Shift-F3")


;;; Buffers

(selection-bind-key "Select Buffer" "Meta-F11")
(selection-bind-key "List Buffers" "Control-Shift-F11")
(selection-bind-key "Select Previous Buffer" "F11")
(selection-bind-key "Kill Buffer" "control-F4")
(selection-bind-key "Circulate Buffers" "Control-F11")



;;; Recursive Editing

(selection-bind-key "Abort Recursive Edit" "Control-\]")


;;; Lisp Operations

(selection-bind-key "Indent Form" "Shift-F8")
(selection-bind-key "Forward List" "Control-\)")
(selection-bind-key "Backward List" "Control-\(")
(selection-bind-key "Down List" "Control-+")



;;; Evaluation & Lisp stuff

(selection-bind-key "Evaluate Defun" "F8")
(selection-bind-key "Evaluate Expression" "Control-F8")
(selection-bind-key "Evaluate Last Form" "Shift-F8")
(selection-bind-key "Evaluate Region" "Control-Shift-F8")
(selection-bind-key "Macroexpand Form" "F9")
(selection-bind-key "Walk Form" "Ctrl-F9")

(selection-bind-key "Compile Defun" "F7")
(selection-bind-key "Compile Region" "Control-Shift-F7")
(selection-bind-key "Compile Buffer" "Control-F7")


;;; Keyboard Macros


;;; Completion

(selection-bind-key "Complete Symbol" "Meta-Control-i")
(selection-bind-key "Dynamic Completion" "Meta-\/")
(selection-bind-key "Expand File Name"  "Meta-Tab" ) 


;;;; Dspec

(selection-bind-key "Find Source" "Meta-\.")
(selection-bind-key "Rotate Active Finders" "Meta-Control-\.")


;;;; Tags

(selection-bind-key "Find Tag" "Meta-\?")
(selection-bind-key "Continue Tags Search" "Meta-\,")


;;; On line documentation.

(selection-bind-key "Function Documentation" "F2")        ; web manual page
(selection-bind-key "Show Documentation" "Meta-F1")       ; CL:DOCUMENTATION in editor help window
(selection-bind-key "Function Argument List" "Shift-F1")  ; arglist in editor echo area


;;;; Echo area

;;; Basic echo-area commands

(selection-bind-key "Help on Parse" "F1" :mode "Pc Echo")
(selection-bind-key "Help on Parse" "Help" :mode "Pc Echo")


(selection-bind-key "Help on Parse" "?" :mode "Pc Echo")
(selection-bind-key "Complete Input" "Tab" :mode "Pc Echo")
(selection-bind-key "Complete Field" "Space" :mode "Pc Echo")
(selection-bind-key "Confirm Parse" "Return" :mode "Pc Echo")

;;; Rebind some standard commands to behave better.

(selection-bind-key "Previous Parse" "Ctrl-Up" :mode "Pc Echo")
(selection-bind-key "Next Parse" "Ctrl-Down" :mode "Pc Echo")
(selection-bind-key "Beginning of Parse" "Control-a" :mode "Pc Echo")
(selection-bind-key "Beginning of Parse" "Meta-<" :mode "Pc Echo")
(selection-bind-key "Echo Area Kill Previous Word" "Meta-Backspace" :mode "Pc Echo")

(selection-bind-key "Echo Area Backward Character Cancelling Selection"                  "Left"  :mode "Pc Echo")

(selection-bind-key "Echo Area Delete Previous Character" "Backspace" :mode "Pc Echo")
(selection-bind-key "Reset Echo Area" "Meta-k" :mode "Pc Echo")

;;; Nuke some dangerous standard bindings.

(selection-bind-key "Illegal" "Control-Meta-l" :mode "Pc Echo")
(selection-bind-key "Illegal" "Meta-x" :mode "Pc Echo")

(selection-bind-key "Illegal" #("Control-x" "b") :mode "Pc Echo")
(selection-bind-key "Illegal" #("Control-x" "Control-w") :mode "Pc Echo")
(selection-bind-key "Illegal" #("Control-x" "i") :mode "Pc Echo")
(selection-bind-key "Illegal" #("Control-x" "Control-s") :mode "Pc Echo")
(selection-bind-key "Illegal" #("Control-x" "Control-v") :mode "Pc Echo")
(selection-bind-key "Illegal" #("Control-x" "k") :mode "Pc Echo")
  
;;;; Miscellaneous

#-Win32(selection-bind-key "Shell Command" "Meta-\!")
#-Win32(selection-bind-key "Shell Command On Region" "Meta-\|")






;;; Debugger

(selection-bind-key "Debugger Backtrace" "Meta-B" :mode "Pc Execute")
(selection-bind-key "Debugger Abort" "Meta-A" :mode "Pc Execute")
(selection-bind-key "Debugger Continue" "Meta-C" :mode "Pc Execute")
(selection-bind-key "Debugger Previous" "Meta-P" :mode "Pc Execute")
(selection-bind-key "Debugger Next" "Meta-N" :mode "Pc Execute")
(selection-bind-key "Debugger Edit" "Meta-E" :mode "Pc Execute")
(selection-bind-key "Debugger Print" "Meta-V" :mode "Pc Execute")


;;; Listener

(selection-bind-key "Execute or Insert Newline or Yank from Previous Prompt" "Return"
          :mode "Pc Execute")

(selection-bind-key "Throw To Top Level" "Meta-k" :mode "Pc Execute")
(selection-bind-key "Insert From Previous Prompt" "Control-j" :mode "Pc Execute")




;; Online doc on F2
(selection-bind-key "Document Command" "Ctrl-F2")
(selection-bind-key "Document Key" "Shift-F2")   
(selection-bind-key "Document Variable" "Ctrl-Shift-F2")

(selection-bind-key "Help"                                                       "F1")



;;;; using Emacs commands from other emulation

;;; calling an Emacs command starting with escape
;;; This makes MSW or Mac( Ctrl-m < ) == Emacs( Escape < )
(selection-bind-key "Emacs Command Escape" "Control-m")

) ; flet


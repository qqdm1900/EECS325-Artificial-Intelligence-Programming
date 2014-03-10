;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/63/LISPeditor/RCS/key-binds.lisp,v 1.81.1.1 2011/08/24 13:26:12 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "EDITOR")

;; This file contains the key bindings with which LispWorks was sent out.  This
;; file is NOT read in each time LispWorks runs, so any modified key bindings
;; should be put in an initialisation file or saved inside a new image.  The
;; purpose of providing this file is to give the user some examples of how key
;; bindings can be specified.



;;; Interrupt gestures that override other bindings.

(set-interrupt-keys '("Control-g" "Control-G"))


;;;; Self Insert

;; DF30June95: don't try to bind all the two-byte chars here
(loop for i from 0 below base-char-code-limit
      for c = (code-char i)
      when (or (graphic-char-p c)
               #+Lucid
               (< 159 i base-char-code-limit))
        do (bind-key "Self Insert" c :global :emacs))


;;; Prefix Arguments

(bind-key "Set Prefix Argument" "Control-u" :global :emacs)

(bind-key "Negative Argument" "Meta-\-" :global :emacs)
(bind-key "Argument Digit" "Meta-0" :global :emacs)
(bind-key "Argument Digit" "Meta-1" :global :emacs)
(bind-key "Argument Digit" "Meta-2" :global :emacs)
(bind-key "Argument Digit" "Meta-3" :global :emacs)
(bind-key "Argument Digit" "Meta-4" :global :emacs)
(bind-key "Argument Digit" "Meta-5" :global :emacs)
(bind-key "Argument Digit" "Meta-6" :global :emacs)
(bind-key "Argument Digit" "Meta-7" :global :emacs)
(bind-key "Argument Digit" "Meta-8" :global :emacs)
(bind-key "Argument Digit" "Meta-9" :global :emacs)


;;; Help

(bind-key "Help" "Control-h" :global :emacs)
(bind-key "Help" "Help" :global :emacs)

;; DF30Nov95 Removed separate bindings for the help sub-commands Task-3154.

;; 30 Mar 1995, AA - fix the control-x style ones to be control-\x to
;; ensure that they are lower case
(bind-key "What Cursor Position" #("Control-x" "=") :global :emacs) ;MJS 10Jul93: added emacs-like binding


;;; Executing Commands

(bind-key "Extended Command" "Meta-x" :global :emacs)



;;; Movement

(bind-key "Forward Character" "Control-f" :global :emacs)
(bind-key "Forward Character" "Right" :global :emacs)
(bind-key "Backward Character" "Control-b" :global :emacs)
(bind-key "Backward Character" "Left" :global :emacs)
(bind-key "Beginning of Line" "Control-a" :global :emacs)
#-Win32 (bind-key "Beginning of Line" "Begin" :global :emacs)
(bind-key "End of Line" "Control-e" :global :emacs)
(bind-key "Next Line" "Control-n" :global :emacs)
(bind-key "Next Line" "Down" :global :emacs)
(bind-key "Previous Line" "Control-p" :global :emacs)
(bind-key "Previous Line" "Up" :global :emacs)

(bind-key "Forward Word" "Meta-f" :global :emacs)
(bind-key "Forward Word" "c-Right" :global :emacs)
(bind-key "Backward Word" "Meta-b" :global :emacs)
(bind-key "Backward Word" "control-Left" :global :emacs)
(bind-key "Forward Paragraph" "Meta-]" :global :emacs)
(bind-key "Backward Paragraph" "Meta-[" :global :emacs)
(bind-key "Forward Sentence" "Meta-e" :global :emacs)
(bind-key "Backward Sentence" "Meta-a" :global :emacs)
(bind-key "Scroll Window Down" "Control-v" :global :emacs)
(bind-key "Scroll Window Down" "Next" :global :emacs)
(bind-key "Scroll Window Up" "Meta-v" :global :emacs)
(bind-key "Scroll Window Up" "Prior" :global :emacs)
(bind-key "Beginning of Buffer" "Meta-<" :global :emacs)
(bind-key "Beginning of Buffer" "Home" :global :emacs)
(bind-key "Beginning of Window" "Control-Prior" :global :emacs)
(bind-key "End of Buffer" "Meta->" :global :emacs)
(bind-key "End of Buffer" "End" :global :emacs)
(bind-key "End of Window" "Control-Next" :global :emacs)


;;; Pages

(bind-key "Previous Page" #("Control-x" "[" ) :global :emacs)
(bind-key "Next Page" #("Control-x" "]" ) :global :emacs)
(bind-key "Mark Page" #("Control-x" "Control-p") :global :emacs)
(bind-key "Count Lines Page" #("Control-x" "l") :global :emacs)


;;; Marking

(bind-key "Set Mark" "Control-@" :global :emacs)
(bind-key "Set Mark" "Control-Space" :global :emacs)
(bind-key "Pop Mark" "Meta-Control-Space" :global :emacs)  
(bind-key "Mark Word" "Meta-@" :global :emacs)
(bind-key "Mark Paragraph" "Meta-h" :global :emacs)
(bind-key "Mark Whole Buffer" #("Control-x" "h") :global :emacs)
(bind-key "Exchange Point and Mark" #("Control-x" "Control-x") :global :emacs)


;;; Registers

(bind-key "Point to Register" #("Control-x" "/") :global :emacs)
(bind-key "Jump to Register" #("Control-x" "j") :global :emacs)
(bind-key "Copy to Register" #("Control-x" "x") :global :emacs)
(bind-key "Insert Register" #("Control-x" "g") :global :emacs)


;;; Go back
(bind-key "Go Back" #("Control-x" "c") :global :emacs)
(bind-key "Go Forward" #("Control-x" "p") :global :emacs)
(bind-key "Select Go Back" #("Control-x" "m") :global :emacs)
;;; Case

(bind-key "Uppercase Word" "Meta-u" :global :emacs)
(bind-key "Lowercase Word" "Meta-l" :global :emacs)
(bind-key "Capitalize Word" "Meta-c" :global :emacs)
(bind-key "Uppercase Region" #("Control-x" "Control-u") :global :emacs)
(bind-key "Lowercase Region" #("Control-x" "Control-l") :global :emacs)


;;; Deletion

(bind-key "Delete Next Character" "Control-d" :global :emacs)
(bind-key "Delete Next Character" "Delete" :global :emacs)
(bind-key "Delete Previous Character" "Backspace" :global :emacs)
(bind-key "Kill Next Word" "Meta-d" :global :emacs)
(bind-key "Kill Next Word" "Meta-Delete" :global :emacs)
(bind-key "Kill Previous Word" "Meta-Backspace" :global :emacs)
(bind-key "Kill Line" "Control-k" :global :emacs)
(bind-key "Kill Backward Up List" "Control-K" :global :emacs)
(bind-key "Append Next Kill" "Meta-Control-w" :global :emacs)
(bind-key "Delete Blank Lines" #("Control-x" "Control-o") :global  :emacs)
(bind-key "Zap To Char" "Meta-z" :global :emacs)
(bind-key "Kill Region" "Control-w" :global :emacs)
(bind-key "Save Region" "Meta-w" :global :emacs)
(bind-key "Save Region" "Control-Insert" :global :emacs)
(bind-key "Forward Kill Sentence" "Meta-k" :global :emacs)
(bind-key "Forward Kill Sentence" #("Control-x" "Delete") :global :emacs)
(bind-key "Backward Kill Sentence" #("Control-x" "Backspace") :global :emacs)
(bind-key "Delete Horizontal Space" "Meta-\\" :global :emacs)
(bind-key "Just One Space" "Meta-Space" :global :emacs)



;;; Insertion

(bind-key "Overwrite Mode" "Insert" :global :emacs)

(bind-key "Un-Kill" "Control-y" :global :emacs)
(bind-key "Un-Kill" "Shift-Insert" :global :emacs)
(bind-key "Rotate Kill Ring" "Meta-y" :global :emacs)
(bind-key "Open Line" "Control-o" :global :emacs)
(bind-key "New Line" "Return" :global :emacs)
(bind-key "New Line" "KP-Enter" :global :emacs)
(bind-key "Quoted Insert" "Control-q" :global :emacs)


;;; Undo

(bind-key "Undo" "Control-_" :global :emacs)
(bind-key "Undo" #("Control-x" "u") :global :emacs)	


;;; Transposition

(bind-key "Transpose Characters" "Control-t" :global :emacs)
(bind-key "Transpose Words" "Meta-t" :global :emacs)
(bind-key "Transpose Lines" #("Control-x" "Control-t") :global :emacs)


;;; Indentation

(bind-key "Indent" "Tab" :global :emacs)
(bind-key "Indent" "Control-i" :global :emacs)
(bind-key "Indent Region" "Meta-Control-\\" :global :emacs)
(bind-key "Indent Rigidly" #("Control-x" "Tab") :global :emacs)
(bind-key "Indent Rigidly" #("Control-x" "Control-i" ) :global :emacs)
(bind-key "Delete Indentation" "Meta-^" :global :emacs)
(bind-key "Indent New Line" "Linefeed" :global :emacs)
(bind-key "Indent New Line" "Control-j" :global :emacs)
(bind-key "Back to Indentation" "Meta-m" :global :emacs)
(bind-key "Back to Indentation" "Meta-Control-m" :global :emacs)



;;; Filling

(bind-key "Fill Paragraph" "Meta-q" :global :emacs)
(bind-key "Fill Region" "Meta-g" :global :emacs)
(bind-key "Set Fill Prefix" #("Control-x" ".") :global :emacs)
(bind-key "Set Fill Column" #("Control-x" "f") :global :emacs)


;;; Searching

(bind-key "Incremental Search" "Control-s" :global :emacs)
(bind-key "Reverse Incremental Search" "Control-r" :global :emacs)
#|
;; 24 Sep 1993, AA - bind these to their EMACS equivalents
(bind-key "Forward Search" "Meta-s" :global :emacs)
(bind-key "Backward Search" "Meta-r" :global :emacs)
|#
(bind-key "Query Replace" "Meta-%" :global :emacs)

(bind-key "Next Search Match" #("Control-x" "`") :global :emacs)

;;; Regular Expression Searching

(bind-key "ISearch Forward Regexp" "Meta-Control-s" :global :emacs)
(bind-key "ISearch Backward Regexp" "Meta-Control-r" :global :emacs)



;;; Files

(bind-key "Wfind File" #("Control-x" "Control-f") :global :emacs)
(bind-key "Find Alternate File" #("Control-x" "Control-v") :global :emacs)
(bind-key "Write File" #("Control-x" "Control-w") :global :emacs)
(bind-key "Save File" #("Control-x" "Control-s") :global :emacs)
(bind-key "Save All Files" #("Control-x" "s") :global :emacs)
(bind-key "Save All Files and Exit" #("Control-x" "Control-c") :global :emacs)
(bind-key "Insert File" #("Control-x" "i") :global :emacs)



;;; Buffers

(bind-key "Select Buffer" #("Control-x" "b") :global :emacs)
(bind-key "List Buffers" #("Control-x" "Control-b") :global :emacs)
(bind-key "Select Previous Buffer" "Meta-Control-l" :global :emacs)
(bind-key "Kill Buffer" #("Control-x" "k") :global :emacs)
(bind-key "Buffer Not Modified" "Meta-~" :global :emacs)
(bind-key "Check Buffer Modified" #("Control-x" "~") :global :emacs)
(bind-key "Circulate Buffers" "Meta-Control-L" :global :emacs)
(bind-key "Toggle Buffer Read-Only" #("Control-x" "Control-q") :global :emacs)



;;; Windows 

(bind-key "New Window" #("Control-x" "2") :global :emacs) 

;;; Emacs use #("Control-x" "2") for split vertically, but we
;;; alreday use for "new window". C-x 4 is also used below

(bind-key "Split Window Horizontally" #("Control-x" "5") :global :emacs) 
(bind-key "Split Window Vertically" #("Control-x" "6") :global :emacs) 
(bind-key "Unsplit Window" #("Control-x" "7") :global :emacs) 

(bind-key "Next Ordinary Window" #("Control-x" "o") :global :emacs)
(bind-key "Delete Window" #("Control-x" "0") :global :emacs) ; added 13/2/89 (MSM)
(bind-key "Delete Other Windows" #("Control-x" "1") :global :emacs)
(bind-key "Refresh Screen" "Control-l" :global :emacs)
;; 24 Sep 1993, AA - add this for EMACS compatibility
(bind-key "Move To Window Line" "Meta-r" :global :emacs)


;;; Abbrevs

(bind-key "Add Mode Word Abbrev" #("Control-x" "Control-a") :global :emacs)
(bind-key "Add Global Word Abbrev" #("Control-x" "+") :global :emacs)
(bind-key "Inverse Add Mode Word Abbrev" #("Control-x" "Control-h") :global :emacs)
(bind-key "Inverse Add Global Word Abbrev" #("Control-x" "-") :global :emacs)
(bind-key "Word Abbrev Prefix Point" "Meta-'" :global :emacs)



;;; Recursive Editing

(bind-key "Exit Recursive Edit" "Meta-Control-z" :global :emacs)
(bind-key "Abort Recursive Edit" "Control-]" :global :emacs)


;;; Lisp Operations

(bind-key "Forward Form" "Meta-Control-f" :global :emacs)
(bind-key "Backward Form" "Meta-Control-b" :global :emacs)
(bind-key "Forward Kill Form" "Meta-Control-k" :global :emacs)
(bind-key "Forward Kill Form" "Meta-Control-Delete" :global :emacs)
(bind-key "Backward Kill Form" "Meta-Control-Backspace" :global :emacs)
(bind-key "End of Defun" "Meta-Control-e" :global :emacs)
(bind-key "Beginning of Defun" "Meta-Control-a" :global :emacs)
(bind-key "Indent Form" "Meta-Control-q" :global :emacs)
(bind-key "Forward List" "Meta-Control-n" :global :emacs)
(bind-key "Backward List" "Meta-Control-p" :global :emacs)
(bind-key "Mark Defun" "Meta-Control-h" :global :emacs)
(bind-key "Transpose Forms" "Meta-Control-t" :global :emacs)
(bind-key "Insert Parentheses For Selection" "Meta-(" :global :emacs)
(bind-key "Insert Double Quotes For Selection" "Meta-\"" :global :emacs)
(bind-key "Insert Multi Line Comment For Selection" "Meta-#" :global :emacs)
(bind-key "Move Over )" "Meta-)" :global :emacs)
(bind-key "Backward Up List" "Meta-Control-u" :global :emacs)
(bind-key "Down List" "Meta-Control-d" :global :emacs)

(bind-key "Indent Selection or Complete Symbol" "Tab" :mode "Lisp")
;;; Can try also  "Lisp Insert ) Indenting Top Level"
(bind-key "Lisp Insert )" #\) :mode "Lisp")


;;; Comments

(bind-key "Indent for Comment" "Meta-;" :global :emacs)
(bind-key "Set Comment Column" #("Control-x" ";") :global :emacs)
(bind-key "Kill Comment" "Meta-Control-;" :global :emacs)
(bind-key "Down Comment Line" "Meta-n" :global :emacs)
(bind-key "Up Comment Line" "Meta-p" :global :emacs)
(bind-key "Indent New Comment Line" "Meta-j" :global :emacs)
(bind-key "Indent New Comment Line" "Meta-Linefeed" :global :emacs)



;;; Evaluation & Lisp stuff

(bind-key "Evaluate Defun" "Meta-Control-x" :global :emacs)
(bind-key "Evaluate Expression" "Meta-Escape" :global :emacs)
(bind-key "Evaluate Last Form" #("Control-x" "Control-e") :global :emacs)

(bind-key "Evaluate Region" "Control-E" :global :emacs)
(bind-key "Macroexpand Form" "Control-M" :global :emacs)
(bind-key "Walk Form" "Meta-M" :global :emacs)
(bind-key "Mark Form" "meta-Control-@" :global :emacs)

(bind-key "Compile Defun" "Control-C" :global :emacs) ; KPA 9/11/92
(bind-key "Compile Region" "Control-R" :global :emacs) ;; 20 May 1993, AA
(bind-key "Compile Buffer" "Control-B" :global :emacs) ;; 20 May 1993, AA


;;;; Keyboard Macros

(bind-key "Define Keyboard Macro" #("Control-x" "(") :global :emacs)
(bind-key "End Keyboard Macro" #("Control-x" ")") :global :emacs)
(bind-key "Last Keyboard Macro" #("Control-x" "e") :global :emacs)
(bind-key "Keyboard Macro Query" #("Control-x" "q") :global :emacs)


;;; Completion 

(bind-key "Abbreviated Complete Symbol" "Meta-i" :global :emacs)
(bind-key "Complete Symbol" "Meta-Control-i" :global :emacs)
(bind-key "Dynamic Completion" "Meta-/" :global :emacs)
(bind-key "Expand File Name"  "Meta-Tab" :global :emacs) 


;;;; Dspec

(bind-key "Find Source" "Meta-." :global :emacs)
(bind-key "Rotate Active Finders" "Meta-Control-." :global :emacs)


;;;; Tags

(bind-key "Find Tag" "Meta-?" :global :emacs)
(bind-key "Continue Tags Search" "Meta-," :global :emacs)

;;; Edit next "recognizable source", i.e. text-property :actions
;;; with func-thing :location (see in capi/editor/editor-pane

(bind-key "Edit Recognized Source" #("Control-x" ",") :global :emacs)

;;; On line documentation.

(bind-key "Function Documentation" "Control-D" :global :emacs)
(bind-key "Show Documentation" "Meta-Control-A" :global :emacs)
(bind-key "Function Arglist" "Meta-=" :global :emacs)
(bind-key "Function Argument List" "Control-A" :global :emacs)


;;;; Echo area

;;; Basic echo-area commands

(bind-key "Help on Parse" "F1" :mode "Echo Area")
(bind-key "Help on Parse" "Help" :mode "Echo Area")
(bind-key "Help on Parse" "?" :mode "Echo Area")
(bind-key "Complete Input" "Tab" :mode "Echo Area")
(bind-key "Complete Field" "Space" :mode "Echo Area")
(bind-key "Confirm Parse" "Return" :mode "Echo Area")
(bind-key "Kill Parse" #("Control-c" "Control-u") :mode "Echo Area")
(bind-key "Return Default" #("Control-c" "Control-r") :mode "Echo Area")
(bind-key "Insert Parse Default" #("Control-c" "Control-p") :mode "Echo Area")
(bind-key "Insert Selected Text" #("Control-c" "Control-c") :mode "Echo Area")

;;; Rebind some standard commands to behave better.

(bind-key "Previous Parse" "Meta-p" :mode "Echo Area")
(bind-key "Next Parse" "Meta-n" :mode "Echo Area")
(bind-key "Find Matching Parse" "Meta-r" :mode "Echo Area")
(bind-key "Beginning of Parse or Line" "Control-a" :mode "Echo Area")
(bind-key "Beginning of Parse" "Meta-<":mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" "Meta-Backspace" :mode "Echo Area")
(bind-key "Echo Area Backward Character" "Control-b" :mode "Echo Area")
(bind-key "Echo Area Backward Word" "Meta-b" :mode "Echo Area")
(bind-key "Echo Area Delete Previous Character" "Backspace" :mode "Echo Area")
(bind-key "Reset Echo Area" "Meta-k" :mode "Echo Area")

;;; Remove some dangerous standard bindings.

(bind-key "Illegal" "Control-Meta-l" :mode "Echo Area")
(bind-key "Illegal" "Meta-x" :mode "Echo Area")
(bind-key "Illegal" #("Control-x" "b") :mode "Echo Area")
(bind-key "Illegal" #("Control-x" "Control-w") :mode "Echo Area")
;; DF24Jan96 trying to insert file into the echo area has to be a mistake
(bind-key "Illegal" #("Control-x" "i") :mode "Echo Area")
;; DF24Jan96 Task-1140
(bind-key "Illegal" #("Control-x" "Control-s") :mode "Echo Area")
;; DF24Jan96 unlike Ctrl-x Ctrl-f, this one would trash the entire window!
(bind-key "Illegal" #("Control-x" "Control-v") :mode "Echo Area")
;; DF02Feb96 Task-3272
(bind-key "Illegal" #("Control-x" "k") :mode "Echo Area")
  
;;;; Miscellaneous

#-Win32(bind-key "Shell Command" "Meta-!" :global :emacs)
#-Win32(bind-key "Shell Command On Region" "Meta-|" :global :emacs) ;MJS 20/02/92


(bind-key "Select Buffer Other Window" #("Control-x" "4" "b") :global :emacs)


;; Pekka 19Mar96: make sure Null isn't bound to a real command,
;; because function keys are all hacked to generate Null on LCL
#-Harlequin-Common-Lisp (bind-key "Illegal" "Null" :global :emacs)
;; DF25Mar96 This needed too. There are no mode bindings other 
;; than in Echo Area which use the chars Lucid lacks
#-Harlequin-Common-Lisp (bind-key "Illegal" "Null" :mode "Echo Area")


;;;; Listener Key bindings


(defmode "Execute" :major-p nil
  :owner 'execute-or-insert-newline-or-yank-from-previous-prompt-command) ; gleep


;;; Debugger

(bind-key "Debugger Backtrace" "Meta-B" :mode "Execute")
(bind-key "Debugger Abort" "Meta-A" :mode "Execute")
(bind-key "Debugger Continue" "Meta-C" :mode "Execute")
(bind-key "Debugger Previous" "Meta-P" :mode "Execute")
(bind-key "Debugger Next" "Meta-N" :mode "Execute")
(bind-key "Debugger Edit" "Meta-E" :mode "Execute")
(bind-key "Debugger Print" "Meta-V" :mode "Execute")


;;; Listener

(bind-key "Execute or Insert Newline or Yank from Previous Prompt" "Return"
          :mode "Execute")

(bind-key "Beginning Of Line After Prompt" "Control-a" :mode "Execute")
(bind-key "Throw To Top Level" "Meta-k" :mode "Execute")
(bind-key "Inspect Star" #("Control-c" "Control-i") :mode "Execute")
(bind-key "Insert From Previous Prompt" "Control-j" :mode "Execute")


;;; Listener History 

(bind-key "History Previous" #("Control-c" "Control-p") :mode "Execute")
(bind-key "History Next" #("Control-c" "Control-n") :mode "Execute")
(bind-key "History Yank" #("Control-c" "Control-y") :mode "Execute")
(bind-key "History Select" #("Control-c" "Control-f") :mode "Execute")
(bind-key "History Last" #("Control-c" ">") :mode "Execute")
(bind-key "History First" #("Control-c" "<") :mode "Execute")
(bind-key "History Search" #("Control-c" "Control-r") :mode "Execute")
(bind-key "History Kill Current" #("Control-c" "Control-k") :mode "Execute")

(bind-key "History Previous" "Meta-p" :mode "Execute")
(bind-key "History Next" "Meta-n" :mode "Execute")
(bind-key "History Search" "Meta-r" :mode "Execute")

;;; Diectory search

(bind-key "Search Files" #("Control-x" "*") :global :emacs)
(bind-key "Search Files Matching Patterns" #("Control-x" "&") :global :emacs)


;;; LW IDE specific bindings
;;;; Function Arglist displayers
(bind-key "Function Arglist Displayer" #("Control-`") :global :emacs)
(bind-key "Invoke Tool" #( "Ctrl-#") :global :emacs)
(bind-key "Activate Interface" #("Control-;") :global :emacs)

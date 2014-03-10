;; -*- Mode: Lisp; External-format: Latin-1; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/46/LISPconfig/RCS/a-dot-lispworks.lisp,v 1.44.1.1 2011/08/24 13:25:57 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;; This is the sample user configuration file for LispWorks.  These
;; settings are already in effect in the system as delivered.  You
;; should review them and see if you want to change any of them: if
;; you do, copy the code into your personal configuration file (by
;; default, this is ~/.lispworks).  For more info, see chapter
;; 'Release Notes' in the Installation and Release Notes.

(in-package "CL-USER")


;; First, you will want to load any patches to LispWorks you've
;; installed.  This is done by the supplied siteinit.lisp file, but if
;; you don't load that, restore this form:

; (load-all-patches)


;; Messages produced by COMPILE-FILE are controlled by the Common Lisp
;; variable *COMPILE-PRINT*. Value NIL suppresses messages. A value of
;; 1 gives one line per compiled form. Integer value > 1 generates
;; more verbose output.

(setf *compile-print* 1)


;; Turn all source debugging features on.

(toggle-source-debugging t)

;; This has the effect of setting the following variables:
#|
(setf compiler:*produce-xref-info* t
      ;; Controls whether the compiler should produce cross-referencing
      ;; information
      compiler:*load-xref-info* t
      ;; Controls whether the cross-referencing information is
      ;; loaded when a fasl file is loaded
      compiler:*notice-changed-definitions* t
      ;; Controls whether the cross-referencer notices when a function is
      ;; redefined, including an interpreted redefinition
      compiler:*source-level-debugging* t
      ;; Controls whether the compiler generates source-level debugging information
      )
|#


;; Packages on the list *HIDDEN-PACKAGES* will be excluded from backtraces.
;; You might want to add some of your own.

(pushnew (find-package "SYSTEM") dbg:*hidden-packages*)
(pushnew (find-package "COMPILER") dbg:*hidden-packages*)


;; Values of *PRINT-LEVEL* in the indicated context

(setf *describe-print-level* 10
      *inspect-print-level* 5
      *step-print-level* 20
      *trace-print-level* 5)

;; Values of *PRINT-LENGTH* in the indicated context

(setf *describe-print-length* 10
      *inspect-print-length* 5
      *step-print-length* 100
      *trace-print-length* 100)

;; When this is non-NIL, INSPECT will if possible invoke the GUI
;; inspector rather than the REPL inspector

(setf *inspect-through-gui* nil)


;; The following two variables control the handling of IN-PACKAGE,
;; which was a function in CLtL1 but is now a macro. (See Steele, 2nd
;; edition, pp. 263-4.)  They offer a choice between correct ANSI
;; Common Lisp behavior and backward compatibility.

;; *HANDLE-OLD-IN-PACKAGE* is referenced by the interpreter and
;; compiler, when a call is encountered which appears to take the form
;; of a function call (either the package name is quoted or there is a
;; :use keyword). Its value is interpreted as follows:
;;    :QUIET  - quietly ignore it, run the old function
;;    :WARN   - signal a warning and run the old function
;;    :ERROR  - signal an error

(setf *handle-old-in-package* :warn)

;; In the event that a call to IN-PACKAGE appears to be creating a new
;; package, the variable *HANDLE-OLD-IN-PACKAGE-USED-AS-MAKE-PACKAGE*
;; is referenced and interpreted as follows:
;;    :QUIET  - handle according to *HANDLE-OLD-IN-PACKAGE*
;;    :WARN   - signal a warning and create the package
;;    :ERROR  - signal a continuable error

(setf *handle-old-in-package-used-as-make-package* :quiet)


;; This variable controls what to do when DEFPACKAGE is used on
;; an existing package that is different from the definition given.
;; The value should be a list containing one or more of the following:
;;    :error     signal an error
;;    :warn      signal a warning
;;    :add       add the new symbols to the externals, imports, etc.
;;    :modify    modify the package to have only these externals, etc.
;;    :verbose   the signaled error or warning also contains details of the differences
;; You can't specify both :error and :warn at the same time; you must specify
;; either :add or :modify.  Undistinguished internals, :INTERN options, and
;; sizes are ignored when deciding whether to signal.

(setf *handle-existing-defpackage* '(:warn :modify))


;; To retain some compatibility with previous versions of LispWorks, the old
;; packages named USER and LISP have been retained as nicknames for the new
;; packages COMMON-LISP-USER and COMMON-LISP respectively.  Note therefore that
;; symbols "in" the USER and LISP packages do not reflect the "old" Common
;; Lisp (as described in Steele edition one) -- see Steele (edition two) page
;; 258.  If you wish to remove these two nicknames, you will need to uncomment
;; the following two lines:
#|
(rename-package "COMMON-LISP-USER" "COMMON-LISP-USER" '("CL-USER"))
(rename-package "COMMON-LISP" "COMMON-LISP" '("CL"))
|#


;; The variable *LOAD-FASL-OR-LISP-FILE* determines whether (LOAD "foo") should
;; load the binary file (foo.fsl, foo.ufsl, foo.nfasl etc, depending on
;; platform) or foo.lisp, when both exist.  It may take the following values:
;;
;;   :LOAD-NEWER         If the fasl is out-of-date, the lisp file is loaded,
;;                       and a warning message is output in verbose mode.
;;   :LOAD-NEWER-NO-WARN Like :LOAD-NEWER, but without the warning.
;;   :LOAD-FASL          Always choose fasl files in preference to
;;                       lisp files, but when verbose, warn if the
;;                       lisp file is newer.
;;   :LOAD-FASL-NO-WARN  Like :LOAD-FASL, but without the warning.
;;   :LOAD-LISP          Always choose lisp files in preference to fasl.
;;   :RECOMPILE          If the fasl file is out-of-date or there is none,
;;                       compile and load the new fasl.
;;   :MAYBE-RECOMPILE    If the fasl is out-of-date, queries whether to load
;;                       it, recompile and then load it, or load the lisp file.

(setf *load-fasl-or-lisp-file* :load-fasl)


;; When *WARN-ON-UNBOUND-BEFORE-SETQ* is non-NIL, a warning is signalled if the
;; interpreter sets an unbound symbol.

(setf *warn-on-unbound-before-setq* nil)


;; LispWorks can signal a warning or an error for definition of an
;; external symbol in any of the packages in this list. It is designed
;; to protect system functions from redefinition. This initial value
;; contains the package names for each package in the base system and
;; supplied modules, on all LispWorks platforms.

(setf *packages-for-warn-on-redefinition*
      '("AMD64" "ATSUI" "CAPI" "CAPI-COCOA-LIBRARY" "CAPI-GTK-LIBRARY" "CAPI-INTERNALS"
        "CAPI-LAYOUT" "CAPI-LIBRARY" "CAPI-MOTIF-LIBRARY"
        "CAPI-POSTSCRIPT-LIBRARY" "CAPI-TOOLKIT" "CAPI-WIN32-LIB"
        "CARBON" "CLIM" "CLIM-DEFSYSTEM" "CLIM-INTERNALS" "CLIM-LISP"
        "CLIM-SILICA" "CLIM-SYS" "CLIM-UTILS" "CLOG-VAR" "CLOS"
        "COCOA" "COLOR" "COM" "COMM" "COMMON-LISP" "COMMON-PROLOG"
        "COMMON-UTILITIES" "COMPILER" "CONDITIONS" "CONSTANTS" "CORBA"
        "CORBA-INTERNAL" "CORE-GRAPHICS" "COSNAMING" "DBG" "DELIVERY"
        "DSPEC" "EDITOR" "ENVIRONMENT" "ENVIRONMENT-INTERNALS"
        "EXE-PARSER" "EXTERNAL-FORMAT" "FLI" "FLI-INTERNALS" "FOREIGN"
        "FOREIGN-PARSER" "GRAPHICS-PORTS"
        "HARLEQUIN-COMMON-LISP" "HARP" "HQN-WEB" "INS"
        "INTERFACE-BUILDER" "KEYWORD" "KW" "KW-TOOLS" "LISPWORKS"
        "LISPWORKS-TOOLS" "LOOP" "LOW64" "LW-XP" "LWGTK" "MAIL" "MATCH" "MP"
        "OBJC" "ODBC-COMMON" "OMG.ORG/PORTABLESERVER" "PARSERGEN" "PC386"
        "PKG" "POSTGRESQL" "POSTSCRIPT-CLIM" "RAW" "RC-PARSER" "REG"
        "RS6K" "RUNTIME" "SAVED-STUFF" "SCM" "SERIAL-PORT" "SETF"
        "SHELL" "SNAKE" "SQL" "SQL-COMMON" "STEPPER" "STREAM"
        "STRUCTURE" "SYSTEM" "TRANS" "TYPE" "WALKER" "WIN32"
        "WIN32-LIB-CLIM" "WSPARC" "X-INTERNALS" "X-LIBRARY" "X-TOOLS"
        "X-UTILITIES" "XM-INTERNALS" "XM-LIB-CLIM" "XM-LIBRARY"
        "XMA-LIBRARY" "XT-INTERNALS" "XT-LIBRARY"
	))



;; This variable controls what to do if certain symbols (see above)
;; are redefined. Legal values:
;;   NIL, :QUIET  just do it
;;   :WARN        signal warning
;;   :ERROR       signal continuable error

(setf *handle-warn-on-redefinition* :error)


;; Controls what is done when a redefinition happens.  Legal values:
;;   NIL, :QUIET  just do it
;;   :WARN        signal warning
;;   :ERROR       signal continuable error

(setf *redefinition-action* :warn)



;; This variable controls what happens at load time if a fasl file references
;; a symbol in a package which does not exist.  There are three possibilities:
;;	:ERROR	Signals an error.  This is the standard behaviour.  Continuing
;;              from the error creates the package.
;;	:WARN	Signals a warning and creates the package.
;;	:CREATE	Silently creates the package.

(setf *unknown-package-during-fasl-load* :error)


;; This variable specifies the shell used by the Shell tool.  The value
;; should be a string containing the filename of a shell (e.g., "/bin/sh" or
;; "/usr/local/bin/bash"), or NIL (the default).  If it is NIL, the shell is
;; taken from the environment variable ESHELL; or if there is none, SHELL; if
;; there is no SHELL, a default appropriate for the OS is used.

(setf editor:*shell-shell* nil)


;; The following variable determines the maximum number of editor windows
;; which will ordinarily be permitted on the screen at any one time.

(setf editor:*maximum-ordinary-windows* 1)


;; This variable determines how definitions found by the commmands
;; "Find Source","Find Source for Dspec" and "Find Tag" are shown.
;; The value should be a list of length 2.  The first element controls
;; the positioning of the definition: when T, show it at the top of
;; the editor window; when a non-negative fixnum, position it that
;; many lines from the top; and when NIL, position it at the center of
;; the window.  The second element can be :HIGHLIGHT, meaning
;; highlight the definition, or NIL, meaning don't.

(setf editor:*source-found-action* '(nil :highlight))


;; Make the editor always look for the previous IN-PACKAGE form instead of
;; assuming the whole file is in the same package.

(setf editor:*use-previous-in-package* t)


;; This variable contains a list of strings. Files with a suffix on
;; this list are generally ignored when the editor prompts with a list
;; of files, and can be ignored by the "Directory Search" and
;; "Directory Query Replace" editor commands.

(setf editor:*ignorable-file-suffices* 
      (list #.(string-append #\. compiler:*fasl-extension-string*)
            ".lap" ".mcl"  ".xref" "~" ".o" ".diff" 
            ".lst" ".~ls" ".~li" ".obj" ".dll" ".exp" ".lib"))


;; If this variable is true, the editor interprets the Rubout (Delete)
;; character as Backspace during input.

(setf editor:*rubout-is-backspace* nil)


;; Controls checking for unmatched parentheses when saving a file and 
;; for the "Find Unbalanced Parentheses" command.  Possible values
;; are:
;;   :MOVE - move the cursor to the point of the mismatch and stop
;;   :WARN - warn about the mismatch
;;   NIL   - ignore mismatches

(setf (editor:variable-value 'editor:Check-Mismatched-Parens :mode "Lisp")
       :warn)


;; This is the format that is used by the editor when opening a
;; file. If it is set to NIL, it may be overridden by a value in the
;; preferences, otherwise it is not overriden. 
;; If the value is :DEFAULT or NIL (and not overriden by a perefernce)
;; OPEN itself determines if a file is LF or CRLF terminated. Other
;; values tells OPEN what format to us, for example the value
;; (:latin-1 :eol-style :lf) would make the editor read every file 
;; as Latin-1 LF-terminated.

(setf (editor:variable-value 'editor:Input-Format-Default) nil)


;; This is the default external format for editor buffers.
;; If it is NIL, it can be overriden by a preference. If it
;; :DEFAULT or NIL (and not overriden by a prefrence) it defaults
;; to the internal default, which is:
;;    Unix:     '(:Latin-1 :Eol-Style :Lf)  
;;    Windows:  '(:Latin-1 :Eol-Style :Crlf)

(setf (editor:variable-value 'editor:Output-Format-Default) nil)


;; This variable determines the minimum of visible lines between the
;; starting of a match in an incremental search and the closest border
;; (top or bottom). If the point is closer than this number, it is scrolled
;; to the middle of the window. The line where the match starts is included
;; in the count. Note that this has no effect when doing "fixed position"
;; search (with numeric prefix, e.g. Ctrl-u <digit> Ctrl-s), or the window
;; it too small. 

(setf (editor:variable-value 'editor:incremental-search-minimum-visible-lines) 3)

;; This variable controls how the source finding commands operate. The
;; legal values for the elements of this list are:
;;
;;   :INTERNAL  - search the internal database of definitions performed
;;                in this image
;;   :TAGS      - prompt for a tags file, when first used
;;   <pathname> - either a tags file or a tags database
;;   (<pathname> . modules) 
;;              - a tags database and a list of modules that are preloaded
;; 
;; The order of this list determines the order that the results from
;; the finders are combined in -- you would usually want :INTERNAL to
;; be the first item on this list, since it contains the up-to-date
;; information about the state of the world. More than one pathname is
;; allowed. See below for how to use this with the supplied source code.

(setf dspec:*active-finders* '(:internal)) 


;; This variable determines whether errors should enter directly into the
;; debugger without flagging the condition in a notifier first.

(setf *enter-debugger-directly* nil)


;; When set to non-NIL, tells lispworks:defsystem to print any
;; compilation errors as warnings and continue.

(setf scm:*continue-from-compile-error* nil)


;; When set to non-NIL, tells lispworks:defsystem to print any load errors as
;; warnings and continue.

(setf scm:*continue-from-load-error* nil)


;; When this variable is true on startup of the LispWorks IDE, it
;; tells LispWorks to load (sys:example-file "misc/asdf-integration")
;; automatically when ASDF is loaded, to facilitate working with
;; asdf:defsystem etc in the tools.
(setf *autoload-asdf-integration* t)


;; This flag is used by READ to tell when it's OK to treat right parentheses
;; as whitespace.  It may take the following values:
;;   NIL     Not OK, flag an error.  This is the standard value.
;;   T       OK, ignore the parenthesis.
;;   :WARN   Signal a warning and proceed.

(setf system:*right-paren-whitespace* nil)


;; This variable determines the behaviour of FILE-LENGTH when called
;; on a stream which is not associated with a file. Can be :ERROR,
;; providing the behaviour required by ANS which is to signal a
;; type-error. :WARN warns, and other values allow any argument type.

(setf system:*file-length-error-p* nil)


;; When set to non-NIL, extends the (ANSI) concept of "space" from
;; just #\Space to include other appropriate characters.

(setf system:*extended-spaces* nil)


;; The following three variables determine which keysyms are searched
;; for the Meta, Super and Hyper modifiers, on Motif.
#+:CAPI-XM-LIB
(setf capi-motif-library:*meta-keysym-search-list*  
      (list (x-lib:keysym "Meta_L") (x-lib:keysym "Meta_R")
            (x-lib:keysym "Alt_L") (x-lib:keysym "Alt_R"))
      
      capi-motif-library:*super-keysym-search-list* 
      (list (x-lib:keysym "Super_L") (x-lib:keysym "Super_R"))
      
      capi-motif-library:*hyper-keysym-search-list* 
      (list (x-lib:keysym "Hyper_L") (x-lib:keysym "Hyper_R"))
      )


;; This variable controls whether the input focus is forced to a
;; dialog the first time it is mapped. A non-nil value provides
;; behaviour which is not strictly Motif-compliant, but is useful when
;; the window manager's pointer focus policy is not click-to-type.

#+:CAPI-XM-LIB
(setf capi-motif-library:*dialog-add-focus-capture* t)



;; Source location in supplied source code
#|

;; load the logical host for the editor source code
(load-logical-pathname-translations "EDITOR-SRC") 

;; Configure source finding to know about editor source code
(setf dspec:*active-finders* 
      (append dspec:*active-finders* (list "EDITOR-SRC:editor-tags-db")))
|#



;; When set to non-NIL,, extends the (ANSI) concept of "space" from just
;; #\Space to include other appropriate characters.

(setf sys:*extended-spaces* nil)


#|
;; Setting for user only needing to view files of one particular external format
;; in the editor. The default behavior opens the file like
;;    (OPEN ... :EXTERNAL-FORMAT :DEFAULT)
;; Note that if you specify editor file encodings via the Preferences dialog,
;; then that will override an initialization file setting such as this.
(setf (editor:variable-value 'editor:Input-Format-Default)
       '(:latin-1 :eol-style :lf))
|#


;; Also see your site configuration file, #P"./configure.lisp", for
;; other configuration options you might want to change according to your 
;; personal preferences, or the configuration of your machine.

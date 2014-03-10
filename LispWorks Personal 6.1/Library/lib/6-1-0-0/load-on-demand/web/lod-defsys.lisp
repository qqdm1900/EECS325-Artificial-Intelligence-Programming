;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/54/LISPweb/RCS/lod-defsys.lisp,v 1.11.1.1 2011/08/24 13:27:54 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "SYSTEM")

(defsystem hqn-web-lod ()
  :members ( ;; main modules
             "browser"
             ("pc-browser" :features :Win32)
             ("unix-browser" :features (and :unix (not :darwin)))
             ("darwin-browser" :features :darwin)
	     "search"
	     "editor"
	     "utils"
	     "manuals"
             ("lcl-manuals" :features :lucid)
             "tools-lod"
             "initialize"
             ;; DF02Jan98: server code commented out - one problem being the SIGUSR1 handler
             ;; ("server" :features :unix)
             ;; ("send-lisp" :type :c-file :compile-only t)
	    
             )
  :rules (
          (:in-order-to :compile ("manuals" #+Lucid "lcl-manuals" "tools-lod" "initialize")
           (:caused-by (:compile "search"))
           (:requires (:load "search")))
          (:in-order-to :compile ("unix-browser")
           (:caused-by (:compile "browser"))
           (:requires (:load "browser")))
          ))


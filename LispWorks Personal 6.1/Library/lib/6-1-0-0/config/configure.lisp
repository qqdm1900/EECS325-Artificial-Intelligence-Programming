;; -*- Mode: Lisp; External-format: Latin-1; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/46/LISPconfig/RCS/configure.lisp,v 1.24.9.1 2011/08/24 13:25:57 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;; This is the site configuration file for LispWorks.  These settings
;; are already in effect in the system as delivered.  You should review them
;; before configuring LispWorks for your site.  For more info, see chapter
;; 'Release Notes' in the Installation and Release Notes.

(in-package "CL-USER")


;; Your site name.  Completely up to you: these variables are only used by the
;; Common Lisp functions short-site-name and long-site-name.

(setf (short-site-name) "Unknown")
(setf (long-site-name) "Unknown")


;; LispWorks support mailing address. 

(setf *phone-home*
      #-Lispworks-Personal-Edition "lisp-support@lispworks.com"
      #+Lispworks-Personal-Edition "lisp-feedback@lispworks.com")


;; This should be set to T for a LispWorks for Unix site (i.e.
;; network) license; the license mechanism won't work if this is not
;; done. Do not set it unless you have had specific instructions from
;; support to do so, otherwise it will cause the keyfile checker to
;; produce meaningless diagnostic messages. Does not apply on Windows,
;; Linux or Mac OS X.

#+(and :Unix (not :Linux))
(setf system:*check-network-server* nil)


;; User's initialization file loaded on startup, except in LispWorks Personal Edition

(setf *init-file-name* "~/.lispworks")


;; This variable contains a list of prefix strings for the automount
;; directories.  Typically automounters use an automount directory into which
;; symbolic links are created when a filesystem is automounted.  The function
;; TRUENAME normally follows symbolic links until a physical file is found but
;; this should not happen for the automount links themselves since they are
;; transient.  The variable SYSTEM:*AUTO-MOUNT-PREFIX-LIST* should contain a
;; list of the prefixes for which TRUENAME should not follow soft links,
;; because they are just automount directories. Use this only if the directory
;; tree in the automount directory is the same as the tree that contain links
;; to it.  E.g., if files accessed through /nfs/<hostname> are automounted on
;; /amd/<hostname> and a link is placed in /nfs/, then "/amd/" should be in
;; the list.

(setf system:*auto-mount-prefix-list*
      '(
        ;; Example private mount point.
        ;"/amd/"

        ;; RedHat 6.2 configuration.
        "/.automount/"
        ))


;;; This variable contain a a list of back-translations of paths. When TRUENAME
;;; finds a path which match the CAR of a pair in this list, it translates
;;; the pathname according to the cdr of the pair. It is intended to be used 
;;; to make sure that TRUENAME does not return pathname inside the auto-mount
;;; directory. 
;;; The commented line assumes an auto-mount directory in "/amd", which
;;; is access using "/nfs". 

(setq system:*auto-mount-reverse-translations*
      '(
        ;; Example private mapping.
        ;("/amd/**/*"     . "/nfs/**/*")

        ;; RedHat 6.2 configuration.
        ("/.automount/*/root/**/*" . "/net/*/**/*")
        ))


;; This variable controls how LispWorks finds the web browser used for
;; viewing online documentation on Motif. The value can be a string
;; specifying the location of the executable e.g. "/usr/local/bin/".
;; Don't omit the trailing slash. It can also be Nil, meaning that the
;; system uses the environment variable PATH to locate the executable.

#+:CAPI-XM-LIB
(setf *browser-location* nil)


;; There are many more options in the LispWorks system; these are just
;; the ones you must set in order to be able to successfully run
;; LispWorks.  If you want to establish site defaults for other
;; options, you can find the more common ones in the model user
;; initialization file, a-dot-lispworks.lisp.

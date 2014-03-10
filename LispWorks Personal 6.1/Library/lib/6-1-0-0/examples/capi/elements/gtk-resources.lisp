;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:gtk-resources.lisp,v 1.2.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")


;;; An example of using :WIDGET-NAME to match resource files
;;; in a GTK application. 

;;; (this file does not compile on non-GTK version)

;;; The example below generates a small resource file, add it to
;;; the resource files, and update resources. It also prints the contents 
;;; of the resource file.  It then generates a simple 
;;; interface with two panes. The interface and the botttom pane pane
;;; have widget-names. As a result each of the two panes match
;;; a different style in the resource file. The pane without the
;;; widget-name matches the "My-Application-Style", and the pane
;;; with the widget names matches the "My-Application-And-Pane-Style".

;;; Note : The calls take some time, because updating the resources causes
;;; re-computation of all existing windows.

;;; To see the example, compile this buffer and then execute
;;; the following forms. 


;; (test-widget-name)                                ; both white background 
;; (test-widget-name :red-top 180)                   ; top cyan, bottom white
;; (test-widget-name :red-top 180 :green-bottom 180) ; top cyan, bottom purple
;;
;; (test-widget-name-edit)         ;;; Edit the generated resource file            

(defvar *my-application-resources-file-name* nil)

(defparameter *my-interface-widget-name* "my-application") ;;; the interface widget-name
(defparameter *my-pane-widget-name* "my-pane")             ;;; the pane widget-name

(defun test-widget-name (&key (red-top 250) (green-top 250) (blue-top 250)
                              (red-bottom 250) (green-bottom 250) (blue-bottom 250))

  ;; Editing the resource files would be done (normally) outside
  ;; the application. Here we do "editing" programmtically just for the example

  (pretend-edit-the-gtk-resource-file red-top green-top blue-top red-bottom green-bottom blue-bottom)
  
  ;; You need to call capi-gtk-library:ADD-APPLICATION-RESOURCE-FILE if you want your application
  ;; to have a separate resource file. Normally this should happen when the application
  ;; starts. 

  (capi-gtk-library:add-application-resource-file *my-application-resources-file-name*)

  ;; We need this because we want the new resources to have effect in the
  ;; running process without restarting GTK. Normally you don't have to do this,
  ;; unless you want to allow the user to update resources while the application
  ;; runs. 

  (capi-gtk-library:update-screen-resources)

  ;; Make an interface with a widget-name, and two list-panels, one
  ;; with a widget-name and one without. The latter will default
  ;; to the class-name, i.e. "list-panel"
  
  (let ((pane1 (make-instance 'capi:list-panel :items '("first item" "second item" "giraffe")))
        (pane2 (make-instance 'capi:list-panel :items '("first item" "second item" "giraffe") 
                              :widget-name *my-pane-widget-name*)))

    (let ((interface (make-instance 
                      'capi:interface
                      :default-width 200 :default-height 200
                      :widget-name *my-interface-widget-name*
                      :layout 
                      (make-instance 'capi:column-layout
                                     :description 
                                     (list pane1 pane2)))))
      (capi:display  interface))

    (format t "Top full name   : ~a~%Bottom full name: ~a ~%"
            (capi-gtk-library:element-gtk-widget-full-name pane1)
            (capi-gtk-library:element-gtk-widget-full-name pane2))))

;;; 

(defun test-widget-name-edit ()
  (ed *my-application-resources-file-name*))




;;; PRETEND-EDIT-THE-GTK-RESOURCE-FILE is doing what normally will be done
;;; by editing the resource file outside the application. so you normally don't
;;; need to do this inside your application. 

(defun pretend-edit-the-gtk-resource-file (red-top green-top blue-top red-bottom green-bottom blue-bottom )
  (let ((string (string-append
                 "style \"My-Application-Style\" " 
                 (format nil "{ base[NORMAL] = \"#~2,'0x~2,'0x~2,'0x\" }" red-top green-top blue-top ) 
                 #\newline
                 "style \"My-Application-And-Pane-Style\" "
                 (format nil "{ base[NORMAL] = \"#~2,'0x~2,'0x~2,'0x\" }" red-bottom green-bottom blue-bottom)
                 #\newline
                 (format nil "widget \"~a.*.list-panel\" style \"My-Application-Style\"" 
                         *my-interface-widget-name*)
                 #\newline
                 (format nil "widget \"~a.*.~a\" style \"My-Application-And-Pane-Style\"" 
                         *my-interface-widget-name* *my-pane-widget-name*))))

    (format t "~%Writing out:~%++++++++++++++++++++++++++++++++++++++++~%~a~%+++++++++++++++++++++++++++++++++~%" string)
    (with-open-file (ff (or *my-application-resources-file-name*
                            (setq *my-application-resources-file-name*
                                  (make-temp-file)))
                        :direction :output
                        :if-exists :supersede)
      (write-string string ff))))

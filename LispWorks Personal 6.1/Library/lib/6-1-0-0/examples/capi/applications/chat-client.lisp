;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:chat-client.lisp,v 1.1.3.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

#|

This example contains a client for the simple line-based textual chat
program in chat.lisp.  Start the server first and then connect to it
by calling

(cl-user::show-chat-client)

|#

(in-package "CL-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

(defvar *chat-host* "localhost"
  "The TCP host on which the chat server should be listening for connections.")

(defvar *chat-port* 10243
  "The TCP port on which the chat server should be listening for connections.")

(capi:define-interface chat-client ()
  ((host :initarg :host :initform *chat-host*)
   (port :initarg :port :initform *chat-port*)
   (connect-callback :initarg :connect-callback :initform nil))
  (:layouts
   (main-layout capi:simple-layout
                '()))
  (:default-initargs
   :title "Chat Client"
   :create-callback 'chat-client-created
   :best-width 500
   :best-height 500))

(defun chat-client-created (self)
  (with-slots (host port connect-callback main-layout) self
    (let ((pane nil))
      (mp:process-run-function "Chat client reader"
                               '()
                               'make-and-run-chat-client-reader
                               host
                               port
                               #'(lambda (new-pane)
                                   (setq pane new-pane))
                               connect-callback)
      (mp:process-wait "Waiting for client reader process to start"
                       #'(lambda ()
                           pane))
      (setf (capi:layout-description main-layout)
            (list pane)))))

(defclass chat-client-pane (capi:interactive-pane)
  ((reader-process :initform nil)
   (output-copier-process :initform nil)))

(defun make-and-run-chat-client-reader (host port pane-callback connect-callback)
  (let* ((socket-stream (comm:open-tcp-stream host port))
         (reader-process mp:*current-process*)
         (interactive-pane
          (make-instance 'chat-client-pane
                         :process reader-process
                         :take-focus t
                         :destroy-callback 'chat-client-pane-destroyed
                         ))
         (output-copier-process
          (mp:process-run-function
           "Chat client output copier"
           '()
           'run-chat-client-output-copier
           interactive-pane
           socket-stream)))
      (setf (slot-value interactive-pane 'reader-process) reader-process
            (slot-value interactive-pane 'output-copier-process) output-copier-process)
      (when connect-callback
        (funcall connect-callback socket-stream))
      (funcall pane-callback interactive-pane)
      (mp:process-wait "Waiting for pane to be created"
                       'capi:editor-window
                       interactive-pane)
      (run-chat-client-reader interactive-pane socket-stream)))

(defun chat-client-pane-destroyed (pane)
  (with-slots (reader-process) pane
    (when reader-process
      (mp:process-kill (shiftf reader-process nil)))))

(defun run-chat-client-reader (pane socket-stream)
  (let ((pane-stream (capi:interactive-pane-stream pane)))
    (unwind-protect
        (loop for line = (read-line pane-stream nil nil)
              while line
              do
              (write-line line socket-stream)
              (force-output socket-stream))
      (chat-client-connection-closed pane)
      (when-let (output-copier-process (slot-value pane 'output-copier-process))
        (setf (slot-value pane 'output-copier-process) nil)
        (mp:process-kill output-copier-process)))))

(defun run-chat-client-output-copier (pane socket-stream)
  (let ((pane-stream (capi:interactive-pane-stream pane)))
    (unwind-protect
        (loop for char = (read-char socket-stream nil nil)
              while char
              do
              (write-char char pane-stream)
              (force-output pane-stream))
      (close socket-stream :abort t)
      (when-let (reader-process (slot-value pane 'reader-process))
        (setf (slot-value pane 'reader-process) nil)
        (mp:process-kill reader-process)))))

(defun chat-client-connection-closed (pane)
  (capi:apply-in-pane-process
   pane
   'capi:destroy
   (capi:top-level-interface pane)))


(defun show-chat-client (&rest args &key host port connect-callback)
  (declare (ignore host port connect-callback))
  (capi:display (apply 'make-instance 'chat-client args)))


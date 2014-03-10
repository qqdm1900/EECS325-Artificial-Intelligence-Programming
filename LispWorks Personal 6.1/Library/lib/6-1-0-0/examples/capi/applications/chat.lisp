;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:chat.lisp,v 1.1.3.1 2011/08/24 13:26:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

#|

This example contains a server for a simple line-based textual chat
program.  To start the server, compile and load this file and then
execute:

(cl-user::show-chat-window)

Clients can connect to the server and lines they send will be sent to
all other connected client.  Lines entered into the Chat window will
be sent to all connected clients.

|#

(in-package "CL-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

(defvar *chat-listening-port* 10243
  "The TCP port on which the chat window listens for connections from client machines.")

(capi:define-interface chat-window ()
  ((port :initform *chat-listening-port* :initarg :port)
   (chat-pane :initform nil))
  (:layouts
   (main-layout capi:simple-layout
                '(chat-pane)))
  (:default-initargs
   :title "Chat"
   :best-width 500
   :best-height 500))

(defmethod initialize-instance :after ((self chat-window) &key)
  (with-slots (chat-pane port) self
    (setf chat-pane (make-chat-pane :port port))))

(defmethod capi:interface-reuse-p ((self chat-window)
                                   &key port)
  (and (call-next-method)
       (or (null port)
           (eql port (slot-value self 'port)))))


(defclass chat-pane (capi:interactive-pane)
  ((port :initform *chat-listening-port* :initarg :port)
   (peers :initform nil)
   (output-copier-process :initform nil)
   (server-process :initform nil :initarg :server-process)
   (reader-process :initform nil :initarg :reader-process))
  (:default-initargs
   :destroy-callback 'chat-pane-destroyed))

(defun chat-pane-destroyed (pane)
  (with-slots (reader-process) pane
    (when reader-process
      (mp:process-kill (shiftf reader-process nil)))))

(defun make-chat-pane (&key port name close-callback)
  (unless name
    (if port
        (setq name (format nil "for port ~D" port))
      (error "Must pass either ~S or ~S to ~S."
             ':port ':name 'make-chat-pane)))
  (let* ((pane nil)
         (server-process
          (and port
               (comm:start-up-server
                :service port
                :function #'(lambda (socket)
                              (chat-pane-connected pane socket))
                :process-name (format nil "Chat server for port ~D" port))))
         (reader-process (mp:process-run-function
                          (format nil "Chat reader ~A" name)
                          '()
                          #'(lambda ()
                              (mp:process-wait "Waiting for pane to be made"
                                               #'(lambda () pane))
                              (run-chat-pane-reader pane close-callback)))))
    (setq pane (make-instance 'chat-pane
                              :port port
                              :process reader-process
                              :reader-process reader-process
                              :server-process server-process
                              :take-focus t))
    pane))

(defun chat-pane-connected (self socket)
  (chat-pane-stream-connected
   self
   (make-instance 'comm:socket-stream
                  :element-type 'base-char
                  :direction :io
                  :socket socket)))

(defun chat-pane-stream-connected (self stream &key close-on-last-peer)
  (with-slots (peers) self
    (let* ((peer-address (comm:socket-stream-peer-address stream))
           (peer-name (or (comm:get-host-entry peer-address
                                               :fields '(:name))
                          (comm:ip-address-string peer-address)))
           (output-copier-process
            (mp:process-run-function
             (format nil "Chat Pane Output Copier ~A" peer-name)
             '()
             'run-chat-pane-output-copier
             self
             stream
             peer-name
             close-on-last-peer)))
      (push (list peer-name stream output-copier-process) peers))))

(defun run-chat-pane-reader (pane close-callback)
  (mp:process-wait "Waiting for pane to be created"
                   'capi:editor-window
                   pane)
  (let ((pane-stream (capi:interactive-pane-stream pane)))
    (unwind-protect
        (loop for line = (read-line pane-stream nil nil)
              while line
              do
              (loop for (nil peer-stream nil) in (slot-value pane 'peers)
                    do
                    (write-line line peer-stream)
                    (force-output peer-stream)))
      (chat-pane-closed pane close-callback))))

(defun chat-pane-closed (pane close-callback)
  (with-slots (server-process peers) pane
    (when server-process
      (mp:process-kill server-process)
      (mp:process-wait "Waiting for server to die"
                       #'(lambda ()
                           (not (mp:process-alive-p server-process))))
      (setf server-process nil))
    (loop while peers
          do
          (let ((output-copier-process (third (car peers))))
            (mp:process-kill output-copier-process)
            (mp:process-wait "Waiting for output copier to die"
                             #'(lambda ()
                                 (not (mp:process-alive-p output-copier-process))))))
    (if close-callback
        (funcall close-callback pane)
      (capi:apply-in-pane-process
       pane
       'capi:destroy
       (capi:top-level-interface pane)))))

(defun run-chat-pane-output-copier (pane socket-stream peer-name close-on-last-peer)
  (broadcast-chat-line pane socket-stream 
                       (format nil "New connection from ~A." peer-name))
  (unwind-protect
      (loop for line = (read-line socket-stream nil nil)
            while line
            do
            (broadcast-chat-line pane socket-stream line))
    (chat-pane-connection-closed pane socket-stream peer-name close-on-last-peer)))

(defun chat-pane-connection-closed (pane socket-stream peer-name close-on-last-peer)
  (with-slots (peers reader-process) pane
    (let ((peer (find socket-stream peers :key 'second)))
      (setf peers (delete peer peers))
      (close socket-stream :abort t)
      (broadcast-chat-line pane socket-stream
                           (format nil "Closed connection from ~A" peer-name))
      (when close-on-last-peer
        (when reader-process
          (mp:process-kill (shiftf reader-process nil)))))))

(defun broadcast-chat-line (pane socket-stream line)
  (let ((pane-stream (capi:interactive-pane-stream pane)))
    (write-line line pane-stream)
    (force-output pane-stream)
    (loop for (nil peer-stream nil) in (slot-value pane 'peers)
          unless (eq peer-stream socket-stream)
          do
          (write-line line peer-stream)
          (force-output peer-stream))))


(defun show-chat-window (&key (port nil portp))
  (if portp
      (capi:find-interface 'chat-window
                           :port port)
    (capi:find-interface 'chat-window)))


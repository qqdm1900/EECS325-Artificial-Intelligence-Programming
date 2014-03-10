;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:remote-debugger.lisp,v 1.4.3.1 2011/08/24 13:26:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

#|

This example contains a server for a simple textual remote debugger.
It can be used with an auxilliary chat pane.

To compile the server with chat, compile and load chat.lisp and
compile and load this file.  To start the server, just compile and
load this file.

To start the server, execute:

(cl-user::show-remote-debugger)

To make a remote image connect to the debugger, run the following
forms in the remote image:

(example-compile-file "capi/applications/chat-client"
                      :load t)
(example-compile-file "capi/applications/remote-debugger-client"
                      :load t)
(setq *debugger-hook* (make-remote-debugger-hook))

If the remote image is on a different machine, then pass the :host
argument to make-remote-debugger-hook to specify the host of the
server.  You may need to adjust firewall software to allow connections
to the server.

|#

(in-package "CL-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

(defvar *remote-debugger-listening-port* 10243
  "The TCP port on which the remote debugger listens for connections
from client machines.")

(defvar *remote-debugger-chat-listener-p*
  t)

(capi:define-interface remote-debugger ()
  ((port :initform *remote-debugger-listening-port* :initarg :port)
   (server-process :initform nil))
  (:panes
   (initial-pane capi:title-pane
                 :text "Waiting for a connection..."
                 :visible-max-width nil
                 :visible-max-height nil))
  (:layouts
   (main-layout capi:tab-layout
                '()
                :print-function 'remote-connection-title
                :visible-child-function 'identity))
  (:default-initargs
   :create-callback 'remote-debugger-created
   :destroy-callback 'remote-debugger-destroyed
   :title "Remote Debugger"
   :best-width 500
   :best-height 500))

(defmethod initialize-instance :after ((self remote-debugger) &key)
  (with-slots (main-layout initial-pane) self
    (setf (capi:layout-description main-layout)
          (list initial-pane))))

(defmethod remote-connection-title ((self capi:title-pane))
  "No Connection")

(defmethod capi:interface-reuse-p ((self remote-debugger)
                                   &key port)
  (and (call-next-method)
       (or (null port)
           (eql port (slot-value self 'port)))))

(defun remote-debugger-created (self)
  (with-slots (port server-process) self
    (setf server-process
          (comm:start-up-server
           :service port
           :function #'(lambda (socket)
                         (remote-debugger-connected self socket))
           :process-name (format nil "Connection server for ~A"
                                 (capi:interface-title self))))))

(defun remote-debugger-destroyed (self)
  (with-slots (server-process) self
    (mp:process-kill server-process)))

(defvar *connection-name-count* 0)

(defun remote-debugger-connected (self socket)
  (let* ((stream (make-instance 'comm:socket-stream
                                :element-type 'base-char
                                :direction :io
                                :socket socket))
         (client-properties (let ((*read-eval* nil))
                              (read stream nil nil)))
         (debugger-chat-p (and *remote-debugger-chat-listener-p*
                               (fboundp 'make-chat-pane)
                               (getf client-properties :debugger-chat-p)))
         (chat-connection-name (getf client-properties :debugger-chat-start)))
    (if chat-connection-name
        ;; Client connected to one of our chats.
        (capi:apply-in-pane-process self
                                    'remote-debugger-add-client-chat
                                    self
                                    stream
                                    chat-connection-name)
      (let* ((peer-address (comm:socket-stream-peer-address stream))
             (peer-name (or (comm:get-host-entry peer-address
                                                 :fields '(:name))
                            (comm:ip-address-string peer-address)))
             (connection-name (format nil "~D" (incf *connection-name-count*))))
        (if debugger-chat-p
            ;; Tell the client that it should connect to chat using the
            ;; connection-name.
            (prin1 (list :debugger-chat-name connection-name) stream)
          (prin1 (list :debugger-no-chat t) stream))
        (force-output stream)
        (mp:process-run-function
         (format nil "Remote Debugger Reader ~A" peer-name)
         '()
         'make-and-run-remote-debugger-reader
         self
         stream
         peer-name
         connection-name)))))

(defun remote-debugger-add-connection (self connection)
  (with-slots (main-layout initial-pane) self
    (let ((new-items (concatenate 'list
                                  (remove initial-pane
                                          (capi:collection-items main-layout))
                                  (list connection))))
      (setf (capi:collection-items main-layout)
            new-items)
      (setf (capi:choice-selection main-layout)
            (1- (length new-items))))
    (capi:display-message-for-pane self "New connection from ~A."
                                   (remote-connection-peer-name connection))))

(defun remote-debugger-remove-connection (self connection socket-stream)
  (close socket-stream :abort t)
  (with-slots (main-layout initial-pane) self
    (let ((new-items (coerce (remove connection
                                     (capi:collection-items main-layout))
                             'list)))
      (setf (capi:collection-items main-layout)
            (or new-items (list initial-pane))))))

(defun remote-debugger-add-client-chat (self stream connection-name)
  (with-slots (main-layout) self
    (let* ((connection (find connection-name
                             (capi:collection-items main-layout)
                             :key 'remote-connection-name
                             :test 'equal))
           (chat-pane (make-chat-pane :name connection-name
                                      :close-callback
                                      #'(lambda (chat-pane)
                                          (remote-debugger-client-chat-closed
                                           chat-pane
                                           connection)))))
      (chat-pane-stream-connected chat-pane stream
                                  :close-on-last-peer t)
      (push chat-pane (capi:layout-description connection)))))

(defun remote-debugger-client-chat-closed (chat-pane connection)
  (capi:apply-in-pane-process
   chat-pane
   'remote-debugger-client-chat-closed-in-process
   chat-pane connection))

(defun remote-debugger-client-chat-closed-in-process (chat-pane connection)
  (setf (capi:layout-description connection)
        (remove chat-pane (capi:layout-description connection))))
  

(defclass remote-connection (capi:column-layout)
  ((peer-name :initarg :peer-name
              :reader remote-connection-peer-name)
   (connection-name :initarg :connection-name
                    :reader remote-connection-name)
   (output-copier-process :initform nil)
   (reader-process :initform nil)
   (interactive-pane :initarg :interactive-pane)))

(defmethod remote-connection-title ((self remote-connection))
  (remote-connection-peer-name self))

(defun remote-connection-interactive-stream (self)
  (with-slots (interactive-pane) self
    (capi:interactive-pane-stream interactive-pane)))

;; This makes a thread to handle user input and a pane to display the
;; connection with the client.
(defun make-and-run-remote-debugger-reader (self socket-stream peer-name
                                                 connection-name)
  (let* ((reader-process mp:*current-process*)
         (interactive-pane
          (make-instance 'capi:interactive-pane
                         :process reader-process
                         :destroy-callback 'remote-connection-pane-destroyed))
         (connection (make-instance 'remote-connection
                                    :description (list interactive-pane)
                                    :interactive-pane interactive-pane
                                    :peer-name peer-name
                                    :connection-name connection-name
                                    :take-focus t))
         (output-copier-process
          (mp:process-run-function
           (format nil "Remote Debugger Output Copier ~A" peer-name)
           '()
           'run-remote-debugger-output-copier
           connection
           socket-stream)))
      (setf (slot-value connection 'reader-process) reader-process
            (slot-value connection 'output-copier-process) output-copier-process)
      (capi:apply-in-pane-process self
                                  'remote-debugger-add-connection
                                  self
                                  connection)
      (mp:process-wait "Waiting for pane to be created"
                       'capi:editor-window
                       interactive-pane)
      (run-remote-debugger-reader connection socket-stream)))

(defun remote-connection-pane-destroyed (pane)
  (with-slots (reader-process) (capi:element-parent pane)
    (when reader-process
      (mp:process-kill (shiftf reader-process nil)))))

;; This reads lines typed into the pane and sends them to the remote client.
(defun run-remote-debugger-reader (connection socket-stream)
  (let ((pane-stream (remote-connection-interactive-stream connection)))
    (unwind-protect
        (loop for line = (read-line pane-stream nil nil)
              while line
              do
              (write-line line socket-stream)
              (force-output socket-stream))
      (remote-debugger-connection-closed connection socket-stream)
      (when-let (output-copier-process (slot-value connection 'output-copier-process))
        (setf (slot-value connection 'output-copier-process) nil)
        (mp:process-kill output-copier-process)))))

;; This displays in the pane whatever the client sends.
(defun run-remote-debugger-output-copier (connection socket-stream)
  (let ((pane-stream (remote-connection-interactive-stream connection)))
    (unwind-protect
        (loop for char = (read-char socket-stream nil nil)
              while char
              do
              (write-char char pane-stream)
              (force-output pane-stream))
      (remote-debugger-connection-closed connection socket-stream)
      (when-let (reader-process (slot-value connection 'reader-process))
        (setf (slot-value connection 'reader-process) nil)
        (mp:process-kill reader-process)))))

(defun remote-debugger-connection-closed (connection socket-stream)
  (capi:apply-in-pane-process
   connection
   'remote-debugger-remove-connection
   (capi:top-level-interface connection)
   connection
   socket-stream))


(defun show-remote-debugger (&key (port nil portp))
  (if portp
      (capi:find-interface 'remote-debugger
                           :port port)
    (capi:find-interface 'remote-debugger)))


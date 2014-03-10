;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:remote-debugger-client.lisp,v 1.3.3.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;; This example contains the client-side code for a simple textual
;; remote debugger.  See the accompanying file remote-debugger.lisp.

(in-package "CL-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

(defvar *remote-debugger-host* "localhost"
  "The TCP host on which the remote debugger should be listening for connections.")

(defvar *remote-debugger-port* 10243
  "The TCP port on which the remote debugger should be listening for connections.")

(defvar *remote-debugger-chat-p*
  t)

(defun make-remote-debugger-hook (&key (host *remote-debugger-host*)
                                       (port *remote-debugger-port*)
                                       timeout)
  #'(lambda (condition hook)
      (declare (ignore hook))
      (with-open-stream (stream (comm:open-tcp-stream host port
                                                      :direction :io
                                                      :element-type 'base-char
                                                      :errorp nil
                                                      :timeout timeout))
        (when stream
          ;; Tell the server if we want chat.
          (prin1 (list :debugger-chat-p
                       (if (and *remote-debugger-chat-p*
                                (fboundp 'show-chat-client))
                           t
                         nil))
                 stream)
          (force-output stream)
          (let ((server-properties (let ((*read-eval* nil))
                                     (read stream nil nil))))
            (when-let (chat-prefix (getf server-properties :debugger-chat-name))
              (show-chat-client :host host
                                :port port
                                :connect-callback
                                #'(lambda (chat-stream)
                                    ;; Tell the server that this
                                    ;; is the chat connection.
                                    (prin1 (list :debugger-chat-start
                                                 chat-prefix)
                                           chat-stream)
                                    (force-output chat-stream))))
            (let ((*debug-io* stream)
                  (*query-io* stream)
                  (*standard-input* stream)
                  (*standard-output* stream)
                  (*error-output* stream)
                  (*trace-output* stream))
              (invoke-debugger condition)))))
      (format *debug-io* "~&Remote debugger did not start...trying normal debugger.~%")))


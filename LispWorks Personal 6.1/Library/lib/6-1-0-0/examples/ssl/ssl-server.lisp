;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/ssl:ssl-server.lisp,v 1.5.8.2 2011/11/04 19:58:40 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; A test for the SSL server side.

;;; Check that the port in *TALK-PORT* is unused on your machine.
;;; Compile and load this file.
;;; Start the example server by calling 
;;;
;;;  (CL-USER::START-SSL-SERVER)
;;;
;;; Note: multiprocessing must have been initialized already.
;;; Connect to the server with an ssl connection, using the hostname of the
;;; machine or localhost and th eport number in *TALK-PORT*.
;;;  Every lime you send it should be returned with "You sent: " prefix.
;;; The function TALKING-TO-MYSELF can be used for testing.

;;; The server can also be set to request a certificate from the
;;; user, by calling
;;;
;;;   (CL-USER::START-SSL-SERVER T)
;;;
;;; It defines a verification callback that always returns T,
;;; so it accepts any certificate that the client returns, but the client
;;; must return some certiciate. 
;;; While it "verifies" the certificate, it prints the organization
;;; name of the current certifciate and the OK-P argument, i.e. whether
;;; the certificate looks ok. 
;;; You can test by using the example in ssl-certificates.lisp, by calling
#|
     (print-site-certificates "<hostname>" :port 10244 ;; *talk-port*
                                       :set-cert-cb t)
|#
;;; The certificate and private key files in this directory were generated
;;; by running the following in the OpenSSL distribution directory:
;;; sh apps/CA.sh -newca
;;; sh apps/CA.sh -newreq
;;; sh apps/CA.sh -sign
;;; with pass-phrase 123456

(in-package "CL-USER")

(defvar *talk-port* 10244) ; a free TCP port number

(defvar *talking-ctx* nil)

(defvar *talking-ssl-server-process* nil)
(defvar *ask-for-certificate* nil)
(defun start-ssl-server (&optional ask-for-certifcate)
  (kill-ssl)
  (setq *ask-for-certificate* ask-for-certifcate)
  (when *talking-ctx*
    (comm:destroy-ssl-ctx *talking-ctx*)
    (setq *talking-ctx* nil))
  (setq *talking-ctx* (comm:make-ssl-ctx))
  (my-configure-ssl-ctx *talking-ctx*)
  (setq *talking-ssl-server-process*
        (comm:start-up-server :function 'make-stream-and-talk
                              :service *talk-port*)))

(defun kill-ssl ()
  (when-let (p *talking-ssl-server-process*)
    (comm:server-terminate p)))

(defun make-stream-and-talk (handle)
  (mp:process-run-function (format nil "talk ~D" handle)
                           '() 'talk-on-stream handle))

(defun talk-on-stream (handle)
  (let ((stream (make-instance 'comm:socket-stream :socket handle
                               :direction :io
                               :element-type 'base-char
                               :ssl-ctx *talking-ctx*)))
    (handler-case
        (unwind-protect
            (loop for line = (read-line stream nil nil)
                  while line do (format stream "You sent: '~A'~%" line)
                  (force-output stream))
          (close stream))
      (comm:ssl-condition (cc)
                          #+win32 (capi:display-message "~a" cc)
                          #-win32 (format *error-output* "~a" cc)
                          (abort)))))

(defun allow-all-ciphers (ctx)
  (comm:set-cipher-list ctx "ALL"))

(defvar *ssl-server-current-pathname* (pathname-location (current-pathname)))

(defun filename-in-ssl-server-directory (name)
  (namestring (merge-pathnames name *ssl-server-current-pathname*)))



;;; Our "verification" function. Always return T.
;;; prints the issuer organizationName and OK-P just
;;; to show that it was called. 

(defun verify-client-certificate (ok-p xsc)
  (format (or mp:*background-standard-output* t)
          "Current ceticiate issuer : ~a [~a]~%"
          (comm::x509-name-field-string 
           (comm::x509-get-issuer-name 
            (comm::x509-store-ctx-get-current-cert xsc))
           "organizationName")
          ok-p)
  t)

;;; The function MY-CONFIGURE-SSL-CTX configures the ssl-ctx:
;;; 1) set the password callback, otherwise OpenSSL will need to
;;;    prompt for the password when reading the private key.
;;; 2) Use a dummy certiciate.
;;; 3) Use a corresponding private key.
;;; 4) Use a dh params file.
;;; 5) If we want a server that ask for a certificate,
;;;    set the verification mode and the epth to 1,
;;;    because we are nto going to really verify it. 

(defun my-configure-ssl-ctx (ssl-ctx )
  (comm:set-ssl-ctx-password-callback ssl-ctx :password "123456")

  
  (comm:ssl-ctx-use-certificate-chain-file
   ssl-ctx
   (filename-in-ssl-server-directory "newcert.pem" ))
  (comm:ssl-ctx-use-rsaprivatekey-file
   ssl-ctx
   (filename-in-ssl-server-directory "newreq.pem")
   comm:ssl_filetype_pem)
  (comm:set-ssl-ctx-dh
   ssl-ctx :filename (filename-in-ssl-server-directory "dh_param_512.pem"))

  (when *ask-for-certificate*
    (comm::set-verification-mode ssl-ctx :server :always
                                'verify-client-certificate)
    (comm::set-verification-depth ssl-ctx 1)))



;;; Example client function to connect to the ssl server.

(defun talking-to-myself (host)
  (with-open-stream (talk (comm:open-tcp-stream host *talk-port*
                                                :ssl-ctx t
                                                :errorp t))
    (dolist (monolog '("Hello self."
                       "Why don't you say something original?"
                       "Talk to you later then.  Bye."))
      (write-line monolog talk)
      (force-output talk)
      (format t "I said: \"~A\"~%" monolog)
      (format t "Self replied: \"~A\"~%" (read-line talk nil nil)))))


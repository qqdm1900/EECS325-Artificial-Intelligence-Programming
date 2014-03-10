;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/ssl:ssl-certificates.lisp,v 1.6.5.1 2011/08/24 13:26:18 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "USER")

(eval-when (compile load eval)
  (require "comm"))

;;; An example for reading information out of certifictaes. 
;;; Define a function that open some domain with SSL
;;; connection and prints information from the certifictaes
;;; that it returns. 
;;; To test, compile and load the file, and then in 
;;; a listener evaluate: 
;;;
;;; (PRINT-SITE-CERTIFICATES "login.yahoo.com")  
;;; (PRINT-SITE-CERTIFICATES "www.amazon.com") 


;;; Print a vector of '(unsigned-byte 8) in the format that
;;; browser print them normally. 

(defun print-bytes-sequence (vector)
  (let ((len (length vector)))
    (dotimes (index len)
      (when (zerop (logand index 15)) 
        (format t "~%   "))   
      (format t " ~(~2,'0x~)" (aref vector index)))))

;;; Print a x509-name object. 

(defun print-a-name (x509-name title)
  (format t ">>>>> ~a~%" title )
  (dolist (x (comm:x509-name-get-all-values x509-name))
    (format t "   ~a :  ~40t~{~a~^ ; ~}~%" (car x) (cdr x))))

;;; Print various bits of information about the certificate

(defun print-all-certificate (x509)
  (format t "~%+++ ~a +++~%" (comm:x509-name x509))
  (print-a-name (comm:x509-get-issuer-name x509) "Issuer")
  (print-a-name (comm:x509-get-subject-name x509) "Subject")
  (format t "version ~a serial number: ~x~%" 
          (comm:x509-version x509)
          (comm:x509-serial-number x509))
  (format t " Not Before: ~s~% Not After:  ~s~%"
          (comm:x509-starting-time x509)
          (comm:x509-expiry-time x509))
  (format t "subject emails : ~a~%" (comm:x509-get-email x509))
  (format t "key  algor ~a pubkey" (comm:x509-pubkey-algorithm x509))
  (print-bytes-sequence (comm:x509-pubkey-key x509))
  (format t "~2%signature algor ~a" (comm:x509-signature-algorithm x509))
  (print-bytes-sequence (comm:x509-signature-value x509))
  (format t "~2%Key usage ~s" 
          (comm:x509-key-usage x509))
  (format t "~%extended Key usage ~s" 
          (comm:x509-extended-key-usage x509))
  (format t "~%Netsacpe cert type ~s~%" 
          (comm:x509-netscape-certificate-type x509))
  
  (terpri)
  (dolist (x (comm:x509-get-extensions x509 ))
    (format t "> ~a:~% ~a~%" 
            (comm:obj-nid2ln (comm:x509-extension-nid x))
            (comm:x509-extension-string x)))
  (terpri)
  (format t "digest : ~x~%" (comm:x509-sha-digest x509)))

;
(defun print-all-stack-certificates (stack)
  (comm:map-crypto-stack-objects stack 'print-all-certificate))

;;; open a connection to the site
;;; and check the certificates it gave us. You can try:
;;; (print-site-certificates "login.yahoo.com")
;;; (print-site-certificates "www.nwolb.com")

;;; When SET-CERT-CB is nil, tis just pass :SSL-CTX T
;;; to OPEN-TCP-STREAM to tell it to use SSL. If
;;; SET-CERT-CB is non-nil, it first makes a client 
;;; SSL-CTX, sets the cert-cb function a callback
;;; that returns  "dummy" certiciate and private key, 
;;; and pass the SSL-CTX to OPEN-TCP-STREAM.  
;;; This is intended to "test" the "server" that is 
;;; implemented in ssl-server.lisp, by calling 
;;;   (PRINT-SITE-CERTIFICATES "<myshostname>"  :port 10244 ;; *talk-port*
;;;;                                            :set-cert-cb t)

(defun print-site-certificates (site &key (port 443) set-cert-cb)
  (let ((ctx (if set-cert-cb
                 (let ((ctx (comm:make-ssl-ctx :ssl-side :client)))
                   (comm:set-ssl-ctx-cert-cb ctx 'my-find-certificate)
                   ctx)
               t)))
  (with-open-stream
       (st (comm:open-tcp-stream site port :ssl-ctx  ctx))
    (if st
        (print-all-stack-certificates 
         (comm:ssl-get-peer-cert-chain (comm:socket-stream-ssl st)))
      (warn "Failed to connect to url  ~a:~a" site port )))))


(defvar *cert-key-pairs* nil)

;;; This is used above as the callback to return certificate and 
;;; a private key. We use the file in the examples directory
;;; which contain a self-signed certificate, so against 
;;; a server that really check it will cause some error. 


(defun my-find-certificate (ssl-pointer)
  (declare (ignorable ssl-pointer))
  (let ((pair (or *cert-key-pairs* 
                  (setq *cert-key-pairs* 
                        (comm:read-certificate-key-pairs
                        (example-file "ssl/cert-and-key.pem")
                        :pass-phrase "123456")))))
    (values (caar pair) (second (car pair)))))

;;; Cleanup 
(defun cleanup-cert-key-pairs ()
  (let ((ckp *cert-key-pairs*))
    (setq *cert-key-pairs* nil)
    (comm:free-certificate-key-pairs ckp)))

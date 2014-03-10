;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/ssl:ssl-client.lisp,v 1.3.1.1 2011/08/24 13:26:18 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; A test for the client side of SSL
;;; compile and load and then test by:
;;;    (try-yahoo)
;;; Should output: 

;;; 1: www.yahoo.com:80 [plain -> plain] => HTTP/1.x ....
;;; 2: www.yahoo.com:80 [ssl -> plain] => <some error> 
;;; 3: login.yahoo.com:443 [plain -> ssl] => <some error>
;;; 4: login.yahoo.com:443 [ssl -> ssl] => HTTP/1.x ....


;;; 1. The first call (plain call to www.yahoo.com:80)  succeeds.
;;; 2. The second call (SSL call to www.yahoo.com:80) fails
;;;    because www.yahoo.com:80 doesn't respond correctly to SSL calls.
;;; 3. The third call (plain call to login.yahoo.com:443)fails 
;;;    because login.yahoo.com:443 expects ssl calls.
;;; 4. The fourth call (SSL call to login.yahoo.com:443) succeeds


(in-package "CL-USER")


(defun call-get(stream)
  (format stream "GET / HTTP/1.0~2%")
  (finish-output stream))

(defun read-a-line (pp)
  (delete #\return (read-line pp)))

(defun get-first-line (pp)
      (unwind-protect
          (progn (call-get pp )
            (read-a-line pp))
     (close pp)))



(defun yahoo-ssl ()
   (let ((pp (comm::open-tcp-stream "login.yahoo.com" 443 :ssl-ctx t)))
    (get-first-line pp)))

;;; Error handler is defined in the caller. 
(defun yahoo (num &key ssl-ctx (page "www.yahoo.com") (port 80))
  (with-simple-restart (abort "abort")
    (format t ";;; ~a: ~a:~a [~a -> ~a] => " num page port (if ssl-ctx "ssl" "plain" ) (if (eq port 80) "plain" "ssl"))
    (let ((pp (comm::open-tcp-stream page port :ssl-ctx ssl-ctx :errorp t)))
      (Let ((line (get-first-line pp)))
        (format t "~a~%"  line)))))


(defun try-yahoo()
  (handler-bind ((error #'(lambda(cc) (format t "~a~%" cc ) (abort))))
    (yahoo 1)
    (yahoo 2 :ssl-ctx t)
    (yahoo 3 :page "login.yahoo.com" :port 443)
    (yahoo 4 :page "login.yahoo.com" :ssl-ctx t :port 443)))

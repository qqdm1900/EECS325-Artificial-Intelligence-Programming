;;; This file contains code to test the testapp-lw
;;; service. You need first to build, install and start 
;;; the service. See in README for details. 

;;; It works on LispWorks 6.0.1 or later. For LispWorks 6.0.1
;;; you need the patches "pipes" and "impersonating-user".

;;; To test the service once it started, compile and load this file 
;;; into a LispWorks IDE, and the go through the forms below
;;; and execute them. 

;;; For each test the comment says what it should return and what
;;; you should see in the output file.  To check that they really 
;;; work, check the output  file (first form in ""Tests" below). 

;;; It is probably more convenient to have two editor windows when 
;;; testing, one with this file and the other with the output file. 

;;; The code below tries to revert the buffer with the output file
;;; two seconds after it sends it the messgae, hopefully by then it is
;;; updated, but if you don't see the expected output you may need to 
;;; update manually. You may also need to scroll it to the end.

;;; When it fails because of bad SID or USER, the server configuration
;;; waiter sleeps for 10 seconds, and during this period you may get
;;; errors of fail to connect. 

;;; In the real application setup, you will have a  GUI program separate
;;; from the service. The GUI program  runs as an ordinary application,
;;; interact with the user to get configuration parameters, 
;;; and then use win32:connect-to-named-pipe to open
;;; a stream to the service and pass it the parameters in a way
;;; similar to what testapp-lw-set-parameter below does. 
 

;;; Tests:

;;;============================================
;;; Find the output file
;;;  

;;; (setq *output-file-buffer* (ed (output-file-full-path)))

;;; Prints the the full path of th eoutput file
;;;
;;; (print (output-file-full-path))

;;;============================================
;;; "Configuration"

;;; Change the iteration string: 

;;;     (testapp-lw-set-parameter "iteration-string" "did ~d iterations")

;;; Should retrun T, use the string to print each iteration (100 seconds
;;; by default, the value *iteration-time* in build-config.lisp). You should
;;; be able to see it immediarelt in the output file, because the configuration
;;; waiter process wakes up the main one once it reads the configuration.


;;;--------------------------------
;;; Try to change the iteration string with bad format string:

;;; (testapp-lw-set-parameter "iteration-string" "iteration ~a ~d")

;;; Should return t, but print an error message to the output
;;; file and not change the iteration string.

;;;--------------------------------
;;; Set random parameters

;;; Set PARAM1 (it is case insensitive) to "value1":

;;;      (testapp-lw-set-parameter "param1" "value1")

;;; Set PARAM1  to "value2":

;;;      (testapp-lw-set-parameter "param1" "value1")

;;; Unset PARAM1

;;;     (testapp-lw-set-parameter "param1" nil)

;;; In th eoutput fil tou see it printing the value for PARAM1, and
;;; after the last form it should disappear.

;;;============================================
;;; Tests that it gets permissions right:

;;;--------------------------------
;;; Check that giving permission to yourself works:

;;;    (testapp-lw-test-user (sys:get-user-name))

;;; Should return T. 
;;; In the output file the configuration should contain the
;;; line:
;;;     LAST_USER = <your user name> 

;;;--------------------------------
;;; Check that giving permission to a daft user name gives an error:

;;;    (testapp-lw-test-user "daft user name")

;;; Should return NIL.
;;; In the output file you should the error, and the configuration
;;; should not change.
;;; NOTE: you need to wait 10 seconds after that to test again.

;;;--------------------------------

;;; Check that giving permission to Builtin Administartions blocks 
;;; (For details, see "SID strings" in MSDN
;;;   http://msdn.microsoft.com/en-us/library/aa379602%28v=VS.85%29.aspx)

;;;    (testapp-lw-test-sid "BA")

;;; Should return NIL, and no change to the configuration
;;; NOTE: you need to wait 10 seconds after that to test again.

;;;--------------------------------

;;; Check that giving permission to everyone allows changes:

;;;    (testapp-lw-test-sid "WD")

;;; Should return T, and the configuration should have
;;;      LAST_SID = WD


;;;--------------------------------

;;; Check that giving permission interactive users allows changes:

;;;    (testapp-lw-test-sid "IU")

;;; Should return T, and the configuration should have
;;;      LAST_SID = IU

;

;;;============================================

;;; Do interaction:

;;; This opens a connection to the service via another named pipe,
;;; and then interact via this pipe with the service and using
;;; simple CAPI with the user. 
;;; In real application this will be a proper CAPI application
;;; with more elaborate GUI. 

;;; (testapp-lw-interact)

;;; It "collects information" by asking for a name of animal and
;;; how many legs it has, then "analyzes" and concludes that it
;;; has this number of legs. 
;;; Cancelling when it prompts for animal stops it. Cancelling when 
;;; it prompts for legs number asks if try another animal. 



;;;============================================

;;; Execution with impersonation using using win32:impersonating-user 
;;; and  win32:impersonating-named-pipe-client

;;;  -----------------------------------------
;;; Listing file starting with W in the root directory. 
;;; execute-process-impersonating in testapp-lw.lisp reads it and write it
;;; to the output file.  Running this ways ensures the service that input
;;; it sees is coming from a process  that it started. 

;;; (testapp-lw-set-parameter "EXECUTE" "DIR c:\\w*")

;;; The NILs are User, Password and redirect-filename. Because the user
;;; is NIL, testapp-lw-execute-with-user use "EXECUTE" as keyword, and the service
;;; impersonate the logged on user.


;;; This should return T and NIL (the second result is full-redirect-file-name)
;;; The output file should contains lines with the output of the DIR command. 

;;;--------------------------------

;;; Running a command as above, but output goes to a file
;;; "junk".  

;;; (testapp-lw-set-parameter "EXECUTE" (redirect "DIR c:\\w*" "junk"))

;;; With  "EXECUTE" as keyword,  the service
;;; impersonates the logged on user.

;;; This should return T and the command it executes, with redirection to a file.

;;; In the output file you should see the command that it executes,
;;; including the redirection.

;;; You can check the contents of the redirected file, but also check
;;; that it is owned by you rather than by the administrators (add the
;;; "owner" column in the files browser, and compare the owner for "junk"
;;; to the owner output file ("testapp-lw-out" by default).


;;;--------------------------------

;;; Same as above but impersonating daft user name.

;;; (testapp-lw-set-parameter "EXECUTE-WITH-USER"  (redirect "daft-user-name,,DIR c:\\w*" "junk-nofile")) 

;;; With "EXECUTE-WITH-USER", the service expects <user>,<password>,command. 

;;; Should return T and the command .  
;;; You should see a failure message in the output file because it fails to impersonate
;;; the user, and the file  should not be created. 

;;;--------------------------------

;;; Same as above but impersonating a user named Guest with empty password.
;;; Output goes to junk-guest. Note that you may have such
;;; user but disabled, check in the control panel -> users. Also this
;;; user may not have permission to write in the output file directory.

;;; (testapp-lw-set-parameter "EXECUTE-WITH-USER"  (redirect "Guest,,DIR c:\\w*" "junk-guest"))


;;; If you don't have a Guest user, it just gets an error as above. If
;;; you have such user, it should creates the file owned by Guest, unless
;;; it doesn't have a permission to do it. 


;;--------------------------------

;;; Starting notepad, just to show that it works.  

;;; (testapp-lw-set-parameter "EXECUTE" (format nil "notepad ~a" (namestring (current-pathname))))

;;; In the output file you should see a message that it started notepad. When
;;; you close notepad you should see a message that it finished (you will need 
;;; to revert the output file).


;;;--------------------------------

;;; Impersonate on the named pipe (using win32:impersonating-named-pipe-client)

;;; (testapp-lw-set-parameter "EXECUTE-ON-PIPE" (redirect-file-full-pathname "junk-pipe"))

;;; Writes to the output file the user name before, during and after impersonation, 
;;; which should be SYSTEM, <your user name>, SYSTEM.
;;; Also opens a file with tehgiven name and write the user name to it. Check the
;;; ownership of the file, should be yu rather than administrator. 


;;--------------------------------

;;;  Cause the service to exit:
;;; 
;;;   (testapp-lw-quit)

;;; That stops the server, so will not respond to anything anymore. To
;;; Test more you will need to start the service again. 
;;; In the output file it should say "Exiting"

;;; Note: the Services window does not update automatically, press
;;; F5 to see that the service stopped. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "USER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Updating the the output-file buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *output-file-buffer* nil)

(defvar *output-file-buffer-update-timer*
  (mp:make-timer  'maybe-update-output-file-buffer))

(defun maybe-update-output-file-buffer ()
  (when-let (buffer *output-file-buffer*)
    (when (editor:buffer-alive-p buffer)
      (editor:revert-buffer-command nil buffer nil)
      (editor:buffer-end (editor:buffer-point buffer)))))

(defun schedule-update-output-file-buffer ()
  (mp:schedule-timer-relative *output-file-buffer-update-timer* 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Pathname utilities ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun redirect-file-full-pathname (redirect-filename)
  (namestring (merge-pathnames redirect-filename (output-file-full-path))))

(defun redirect (command redirect-filename)
  (string-append command " > " 
                 (redirect-file-full-pathname redirect-filename)))

;;; Configuation shared with the service application, most importantly the pipe name
;;; in *configuration-pipe-name*

(compile-file-if-needed (current-pathname "build-config.lisp") :load t)

;;; Open the connection to a named pipe. The pipe is created by
;;; wait-for-configuration-input in testapp-lw.lisp

;;; We use errorp nil, so returns NIL on error.

(defun connect-to-service-to-update ()
  (win32:connect-to-named-pipe *configuration-pipe-name* 
                               :errorp nil :direction :output))

;;; Sets a parameter, by sending a line of the form <param-name> = <value>
;;; On the other side it is interpreted by interpret-configuration-line,
;;; which recognize them some names as commands. 

(defun testapp-lw-set-parameter (name value)
  (with-open-stream (str (connect-to-service-to-update))
    (when str
      (format str (if value
                      "~a = ~a"
                    "~a")
              name value)
      (schedule-update-output-file-buffer)
      (values t value))))


(defun testapp-lw-test-sid (sid)
  ;; Tells the service to use access with this SID 
  ;; interpret-configuration-line in testapp-lw.lisp create
  ;; a access spec ((:sid <sid>))
  (unless (testapp-lw-set-parameter "access-sid" sid)
    (error "failed to connect to the service"))

  ;; make sure that WAIT-FOR-CONFIGURATION-INPUT in testapp-lw.lisp
  ;; reached back to waiting inside win32:OPEN-NAMED-PIPE-STREAM
  ;; with the new access settings
  (sleep 1) 

  ;; If we can access when the permission is for this SID,
  ;; the next form will set the parameter SID to it and return
  ;; T, otherwise it will fail and return NIL.

  (testapp-lw-set-parameter "LAST_SID" sid))



(defun testapp-lw-test-user (user-name)
  ;; Tells the service to use access with this user name.
  ;; interpret-configuration-line in testapp-lw.lisp creates
  ;; a access spec ((:user <sid>))
  (unless (testapp-lw-set-parameter "access-user" user-name)
    (error "failed to connect to the service"))
  (sleep 1)
  ;; If we can access when the permission is for this user-name,
  ;; the next form will set the parameter USER to it and return
  ;; T, otherwise it will fail and return NIL.
  (testapp-lw-set-parameter "last_user" user-name))




;;; interpret-configuration-line in testapp-lw.lisp 
;;;  recognizes "QUIT" as a special value

(defun testapp-lw-quit ()
  (testapp-lw-set-parameter "QUIT" nil))


;;; interpret-configuration-line in testapp-lw.lisp recognizes "EXECUTE and "EXECUTE-WITH-USER"
;;; as a special value. For EXECUTE it just takes a command, for EXECUTE-WITH-USER 
;;; it takes a string of the form <user>,<password>,<command>.
;;; This function may also redirect the output. 

(defun testapp-lw-execute-with-user (user password command redirect-filename)
  (let ((full-file-name (when redirect-filename
                          (namestring (merge-pathnames redirect-filename (output-file-full-path)))))
        (keyword (if user "EXECUTE-WITH-USER" "EXECUTE"))
        (command (if user 
                     (string-append user "," password "," command)
                   command)))
    (when full-file-name
      (setq command (string-append command " > " full-file-name)))
    
    (format t "~a: ~a~%" keyword command)
  
    (values (testapp-lw-set-parameter keyword command)
            full-file-name)))


(defun testapp-lw-execute-with-user (user password command redirect-filename)
  (let ((full-file-name (when redirect-filename
                          (namestring (merge-pathnames redirect-filename (output-file-full-path)))))
        (keyword (if user "EXECUTE-WITH-USER" "EXECUTE"))
        (command (if user 
                     (string-append user "," password "," command)
                   command)))
    (when full-file-name
      (setq command (string-append command " > " full-file-name)))
    
    (format t "~a: ~a~%" keyword command)
  
    (values (testapp-lw-set-parameter keyword command)
            full-file-name)))


;;; "interactive application"

;;; Interact with the user and with the service. 
;;; Setting the "parameter" "INTERACT" causes interpret-configuration-line
;;; in testapp-lw.lisp to invoke interact-via-named-pipe with
;;; the value. This opens a named pipe stream and calls
;;; interact-via-an-open-stream, which is doing the
;;; interaction with this function. 

;;; This just reads lines, displays them to the user
;;; and sends back the result. The service does "the work".
;;; First character tells it what it is:

;;; Char      Meaning               Return

;;; #\;         ignore                empty line
;;; #\!         message               empty line
;;; #\?         binary question.      Yes or No
;;; #\=         ask for input         >input  or = for cancel.

(defun testapp-lw-interact ()
  (let ((pipe-name-for-interaction "testapp-lw-interaction"))
    (when (testapp-lw-set-parameter "INTERACT"
                                    pipe-name-for-interaction)
      (let ((stream (win32:connect-to-named-pipe pipe-name-for-interaction
                                                 :errorp nil
                                                 ;; The other process may take time
                                                 ;; to start the named pipe, so
                                                 ;; try reeatably 100 times. 
                                                 :repeat 100)))
        (when stream
          (loop
           (let ((line (read-line stream nil nil)))
             (unless line (return))
             (let ((len (length line)))
               (when (> len 0)
                 (let ((first-char (char line 0))
                       (string (subseq line 1)))
                   (case first-char
                     (#\; (terpri stream))
                     (#\! (capi:display-message string)
                          (terpri stream))
                     (#\? (let  ((yes-p (capi:prompt-for-confirmation string)))
                            (write-line (if yes-p "Yes" "No") stream)))
                     (#\= (multiple-value-bind (result-string ok)
                            (capi:prompt-for-string string)
                          (format stream
                                  (if ok
                                      ">~a~%"
                                    "=~%")
                                  result-string)))

                     (t (terpri stream)) ; shouldn't happen
                     )

                   (finish-output stream)))))))))))



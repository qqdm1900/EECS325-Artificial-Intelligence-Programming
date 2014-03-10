


(in-package "USER")



(defvar *output-stream* nil)

;;; The *waiting-barrier* is used by the main process to wait.
;;; Other processes can wake the main process by disabling it. 
;;; MY-STOP sets it to nIL, which is the signal to all other proceses to die. 
;;; We are making the barrier at load time, rather
;;; than in the init function, because the init-function is called
;;; after the service is registered, and the stop function may end
;;; up called before the init function.

(defvar *waiting-barrier* (mp:make-barrier t))


(defvar *command-line-args* nil)

;;; This contains the "configuration". In real application
;;; it have some effect, here it is just printed out. 

(defvar *configuration* nil)

(defvar *current-configuration-access* *default-access-setting*)

(defvar *iteration-string* "Iteration ~d")


;;; The init function of the service (referred to in define-service.lisp)
;;; Should be reasonably fast. 
;;; Here just setup the output stream.

(defun my-init (args)
  (setq *command-line-args* args)
  (let* ((file (output-file-full-path)))
    (let ((stream (open file :direction :output
                          :if-exists :supersede)))
        (when  stream
          (format stream   "Starting at ~a with  parameters: ~S~%" 
                  (sys::date-string)  args)
          (setq *output-stream* stream)
          t))))



;;; That is the the STOP-FN of the service. (referred to in define-service.lisp)
;;; It is Called on another process, and must return fast. 
;;; We just set the barrier to NIL to flag the other processes to exit, 
;;; and disable it to wakeup all. 

(defun my-stop ()
  (when-let (barrier *waiting-barrier*)
    (mp:barrier-disable barrier)
    (setq *waiting-barrier* nil)))




;;; That is the the MAIN-FN of the service. (referred to in define-service.lisp)
;;; It is running until the service is stopped (when MY-STOP sets the
;;; *waiting-barrier* to NIL). 

;;; This definition does nothing except printing a message to the output-file. 

(defun my-main ()

  ;; Start another process to deal with configuration

  (mp:process-run-function "Configuration waiter" () 'wait-for-configuration-input)


  (let ((count 0))
    
    (loop
     (report-it *iteration-string* (incf count))
     (when-let (barrier *waiting-barrier*)
       (mp:barrier-wait barrier :timeout *iteration-time*)
       ;; Other processes may wake the current process by disabling the barrier.
       (mp:barrier-enable barrier t))
     (unless *waiting-barrier* ; check again on wakeup
       (return)))
    
    ;; Cleanup
    (report-it "<<<<<   Exiting >>>>>")
    (close-output-stream)
    ;; Just return

))



;;; OUTPUT-STREAM stuff

;;; Make sure the output is not mixed up, and that
;;; we can safely close the output. 

(defun apply-with-ensure-stream (func &rest args)
  (when-let (stream *output-stream*)
    (stream:with-stream-output-lock stream
      (when (open-stream-p stream)
        (apply func stream args)))))

;;; "interface" that can be safely called everywhere
(defun close-output-stream ()
  (apply-with-ensure-stream 'close))

;;; This is called with the stream locked.
(defun internal-report-it (stream string args)
  (format stream "Report at ~a: ~a ~%" (sys::date-string)
          (if args (apply 'format nil string args) string))
  (finish-output stream))

;;; "interface" that can be safely called everywhere
(defun report-it (string &rest args)
  (apply-with-ensure-stream  'internal-report-it string args))

;;; This is called with the stream locked.
(defun internal-print-configuration  (stream)
  (format stream "~%Configuration at ~a:~%" (sys::date-string))
  (dolist (config *configuration*)
    (format stream "~5t~a = ~a~%" (car config) (cdr config)))
  (format stream "Access setting is ~s~%" *current-configuration-access*)
  (format stream "------------------------------~%" )
  (finish-output stream))

;;; "interface" that can be safely called everywhere
(defun print-configuration ()
  (apply-with-ensure-stream 'internal-print-configuration))

;;;;;;;;;;;;;;;;;;; end of OUTPUT-STREAM stuff


;;; Configuration stuff
;;; That is a dummy example.  It just expects lines of the form "keyword = value". 
;;; Some keywords are treated specially:

;;; "ACCESS-SID"  and "ACCESS-USER" are used to change the setting of 
;;;    the ACCESS argument to WIN32:OPEN-NAMED-PIPE-STREAM for experimenting
;;;    with it. 
;;;  "ITERATION-STRING" is used to changed the string that it prints 
;;;      each iteration, to show that it is "configured".
;;;   "QUIT" causes it to quit.
;;;  "INTERACT"  causes it to start another thread that interacts with the user
;;;     via another process that connects via (another)  named pipe. 
;;;
;;; "EXECUTE", "EXECUTE-WITH-USER" and "EXECUTE-ON-PIPE" are used
;;; to execute commands while impersonating a user. 





(defun interpret-configuration-line (stream line)
  (multiple-value-bind (keyword-string value-string)
      (let ((equal-position (position #\= line)))
        (if equal-position 
            (values (subseq line 0 equal-position)
                    (subseq line (1+ equal-position)))
          (values line nil)))

    (let ((keyword (nstring-upcase (string-trim '(#\space #\tab)keyword-string)))
          (new-val (when value-string (string-trim '(#\space #\tab) value-string))))

      (cond
       ((equal keyword "ACCESS-SID")
        (let ((ns (if new-val (list (list :sid new-val)) *default-access-setting*)))
          (report-it "new access ~s" ns)
          (setq *current-configuration-access* ns)))
       
       ((equal keyword "ACCESS-USER")
        (let ((ns  (if new-val (list (list :user new-val)) *default-access-setting*)))
          (report-it "new access ~s" ns)
          (setq *current-configuration-access* ns)))
            
       ((equal keyword "ITERATION-STRING")
        (check-iteration-string  new-val))
            

       ((equal keyword "INTERACT")
        (mp:process-run-function  "Interacting" 
                                  () 'interact-via-named-pipe new-val)
        (report-it "Starting to interact via ~a" new-val))
             
       ((equal keyword "EXECUTE")
        (mp:process-run-function 
         "Executer" ()
         'execute-process-impersonating
         new-val
         nil))

       ((equal keyword "EXECUTE-WITH-USER")
        (mp:process-run-function 
         "Executer" ()
         'execute-process-impersonating
         new-val
         t))

       ((equal keyword "EXECUTE-ON-PIPE")
        ;; That is done here rather than on another process to amke sure that
        ;; we have a live pipe. 
        (report-it "EXECUTE-ON-PIPE: user name before: ~a" (sys:get-user-name))
        (win32:impersonating-named-pipe-client
            (stream :fail-form (report-it "failed to impersonate on the configuration stream"))
          (report-it "EXECUTE-ON-PIPE: user name while impersonating: ~a" (sys:get-user-name))
          (with-open-file (ff new-val :direction :output :if-exists :supersede)
            (format ff "User name ~a~%" (sys:get-user-name))))
        (report-it "EXECUTE-ON-PIPE: user name after: ~a" (sys:get-user-name))
        )
            
       
       ((equal keyword "QUIT")
        (Report-it " -- Got QUIT")
        (my-stop))
     
       (T 
        (if new-val
            (setf (sys:cdr-assoc keyword *configuration*
                                 :test 'equalp)
                  new-val)
          (setq *configuration*
                (delete keyword *configuration* :key 'car :test 'equalp))))))))

      
;;; Wait function for the configuration waiter. We need that so
;;; it can exit on its own when the service is terminated. It will
;;; actually work without it, because the process will be just killed,
;;; but this eliminates potential problems of the killing occur in the
;;; middle of a cleanup. 

(defun wait-for-configuration-wait-function ()
  (not *waiting-barrier*))


(defun error-in-wait-for-configuration-input (cc)
  (report-it "error in configuration waiter: ~a" cc)
  (throw 'abort-in-wait-for-configuration-input nil))

;;; The process function for the process that waits for configuration input.

;;; Use win32:OPEN-NAMED-PIPE-STREAM to open a named piped. Pass it the access
;;; setting, timeout and a wait-function.
;;; :WAIT-FUNCTION is to make it return when the service is stopped. 
;;; :TIMEOUT  is needed only because we are playing with the access
;;; settings, and we want to restore the default one in case the one it is set
;;; to blocks further changes.
;;; :ACCESS is used to specify who can configure the application. In 
;;; this "application" it is a variable that we play with (it is set in
;;; interpret-configuration-line), in normal application it will
;;; be fixed value. 

;;; Because we use :TIMEOUT, win32:OPEN-NAMED-PIPE-STREAM may return even
;;; though the stream is not connected. In this case we use the same stream 
;;; the next iteration and wait using win32:WAIT-FOR-CONNECTION.

;;; Once win32:OPEN-NAMED-PIPE-STREAM or win32:OPEN-NAMED-PIPE-STREAM returned,
;;; it first check if it needs to exit by checking the *waiting-barrier* (that
;;; happens whne MY-STOP is called). 

;;; Then it checks if the stream is actually connected, if it is it reads
;;; the "configuration" out of it, close it, sets stream to NIL and then prints
;;; the configuration.

;;; If the stream was not connected, it checks if it was opened
;;; with a non-default access, and if it does closes it and reset the access.
;;; This way the stream with non-default access is there only for the
;;; TIMEOUT of the call to win32:OPEN-NAMED-PIPE-STREAM, and the ne it reverts.
;;; The tests in testapp-lw-test.lisp use this by setting the access, waiting
;;; for a second to ensure that win32:OPEN-NAMED-PIPE-STREAM is called with the
;;; non-default access, and then try to connect. 


(defun wait-for-configuration-input ()
  (let (stream connected error-object)
    (loop
     (catch 'abort-in-wait-for-configuration-input

       ;; The working application should not his handler-bind, it
       ;; is for debugging. 

       (handler-bind
           ((error 'error-in-wait-for-configuration-input))
       
         ;; open/connect the stream
    
         (if (not stream)
             (multiple-value-setq (stream connected error-object) 
                 (win32:open-named-pipe-stream 
                  *configuration-pipe-name*
                  :access *current-configuration-access*
                  :wait-function 'wait-for-configuration-wait-function
                  :errorp nil ;; prevent errors for bad access 
                  :timeout *configuration-timeout*))
           (setq error-object nil
                 connected 
                 (win32:wait-for-connection stream :timeout *configuration-timeout*
                                            :wait-function 'wait-for-configuration-wait-function)))


         ;; process

         (cond 
          ;; Somebody sends us configuration, read, close abd print new configuraton
          (connected 

           (unwind-protect
               (when-let (line (read-line stream nil nil))
                 ;; impersonating works only after reading something
                 (setq *current-configuration-access* *default-access-setting*)
                 (report-it  "Opened configuration stream with access ~s user ~a" 
                             *current-configuration-access*
                             (win32:impersonating-named-pipe-client
                                 (stream)
                               (sys:get-user-name)))

                 (loop (interpret-configuration-line stream line)
                       (unless (setq line (read-line stream nil nil))
                         (return))))
             (close stream)
             (setq stream nil))
           (print-configuration)
           ;; Wakeup main so it can "deal with the new configuration".
           ;; In this dummy application the only effect is to print
           ;; another iteration message.
           (mp:barrier-disable *waiting-barrier*) 
           )
           
          ;; MY-STOP was called, exit
          ((not *waiting-barrier*) 
           (when stream (close stream))
           (report-it "Configuration waiter exists")
           (return))

      ; Stream not connected. If no-default access, close it and reset access
          (stream 
           (unless  (eq *current-configuration-access* *default-access-setting*)
             (close stream)
             (setq stream nil)
             (setq *current-configuration-access* *default-access-setting*)))

      ; some error when trying to open the stream
          (error-object 
           (report-it "!!!!!!  Error opening configuraion stream : ~%~15t~a" error-object)
           (print-configuration)
           (setq *current-configuration-access* *default-access-setting*)
           (sleep *configuration-timeout*) ;; make the tests in in testapp-lw-test.lisp fails
           )))
       ))))



;;; ------------------------------------------------------
;;; Interaction with the user. 
;;; ------------------------------------------------------

;;; On the other side is testapp-lw-interact in testapp-lw-test.lisp,
;;; which simply read lines from the stream and display to the user,
;;; using the first character as a flag what to do. This side
;;; does "the work". 

;;; Send a line and wait for a response. 
;;; The char is a flag for the otrhe side what it is. 
;;; This relies on the format call not to generate new line,
;;; which will confuse the other side. 

(defun interact-send-and-read-line (stream char string args)
  (write-char char stream)
  (apply 'format stream string args)
  (terpri stream)
  (finish-output stream)
  (let ((line (read-line stream nil nil)))
    (unless line (report-it "Interactive pipe broken") (mP:current-process-kill))
    line))

;;; This causes the other side to display prompt-for-string. 
(defun interact-send-and-read-prompt (stream string &rest args)
  (let ((line (interact-send-and-read-line stream #\= string args)))
    (if (and (> (length line) 1)
             (eql (char line 0) #\>))
        (subseq line 1)
      :cancel)))

;;; Cause the other side to show a binary question.
(defun interact-send-and-read-question(stream string &rest args)
  (let ((line (interact-send-and-read-line stream #\? string args)))
    (cond ((equalp line "Yes") :yes)
          ((equalp line "No") :no)
          (t :cancel))))

;;; causes the other side to display-message. 
(defun interact-send-and-read-message(stream string &rest args)
  (interact-send-and-read-line stream #\! string args))
            
;;; That gets an open stream and interact through it. 
;;; It asks for a name of animal and then for the numebr of the
;;; legs, and then "concludes" that the animal has this number
;;; of legs. 

(defun interact-via-an-open-stream (stream)
  (unwind-protect
      (loop 
       (let ((animal-name (interact-send-and-read-prompt stream "Enter a name of an animal")))
         (if (eq animal-name :cancel)
             (return)
           (let ((legs-number (interact-send-and-read-prompt 
                               stream 
                               "How many legs does ~a have?" animal-name)))
             (if (eq legs-number :cancel)
                 (unless (eq :yes(interact-send-and-read-question stream "Try another animal?"))
                   (return))
               (interact-send-and-read-message stream 
                                               "~a has ~a legs" 
                                               animal-name legs-number))))))
    (close stream)))

;;; This is the process function. 
;;; Gets a pipe name to use and interact through.
;;; open and connect it, and then call interact-via-an-open-stream.

(defun interact-via-named-pipe (pipe-name)
  (multiple-value-bind (stream connected error-object) 
      (win32:open-named-pipe-stream 
       pipe-name
       :access :au
       :timeout 10)
    (if  (and stream connected)
        (progn (interact-via-an-open-stream stream)
          (report-it "Finished interacting via ~a" pipe-name))
      (progn 
        (close stream)
        (report-it "Failed to open named pipe ~a error ~a" pipe-name error-object)))))






;;; Starting another process impersonating a user. If contains-user
;;; is true, the string mmust of the format <username>,<password>,<execuatble>.
;;; In this case it impersonate the user. If contains-user is NIL,
;;; the string is the executable, and it trie sto use the user
;;; that is logged on. 

(defun execute-process-impersonating (string contains-user-p)
  (multiple-value-bind (user password process-string)
      (if contains-user-p
          (let ((list (split-sequence '(#\,) string)))
            (unless (eq (length list) 3)
              (report-it "  !!!!  Bad user,password,executable spec:~%~20t~a~%" string)
              (return-from execute-process-impersonating nil))
            (values (string-trim '(#\space)(car list))
                    (string-trim '(#\space) (second list))
                    (string-trim '(#\space) (third list))))
        (values T nil string))
    (win32:impersonating-user (user password 
                                 :fail-form (report-it " !!!!  Failed to impersonate ~a" user))
      (report-it "Impersonating ~a successful, starting ~a" user process-string)
      (with-open-stream 
          (stream (sys:open-pipe process-string))
        (loop
         (if-let (line (read-line stream nil nil))
             (report-it "Input: ~a" line)
           (return))))
      (report-it "Process ~a finished" process-string))))


;;; We are using it as argument to FORMAT, which wti all its
;;; fancy directive is a security hole, so check that it is
;;; acceptable, i.e. contains only one ~ character, with 
;;; a simple directive. In general, because the service 
;;; runs with higher privileges, it needs to check any input that 
;;; comes form the user. 

;;; This allows one ~, which must have a numeric directive. 

(defun check-iteration-string (new-val)
  (let ((count (count #\~ new-val))  )
    (if
        (and (= count 1)
             (let ((position (1+ (position #\~ new-val)))
                   (len (length new-val))
                   (char nil))
                    
               (loop 
                (setq position
                      (position-if-not 
                       #'(lambda (char)
                           (or (digit-char-p char)
                               (member char 
                                            '(#\, #\#
                                                  #\+ #\- #\: #\@)
                                              )))
                       new-val :start position))
                (unless position (return nil))
                (setq char (char new-val position))
                (if (eq char #\')
                    (progn (incf position 2)
                      (when (>= position len) (return nil)))
                  (return  (and 
                            (member (char-upcase char )
                                    '(#\D #\X #\R #\O #\B 
                                          #\F #\G #\E #\$))
                            (ignore-errors (format nil new-val 1)
                              )
                            ))))))
        (setq *iteration-string* new-val)
      (report-it "!!!! Bad iteration string : \"~a\"" new-val))))
                     
                                                                    
                  
         
  

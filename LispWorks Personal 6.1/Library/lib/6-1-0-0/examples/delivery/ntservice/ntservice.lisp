;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the Franz
;; preamble to the LGPL found in
;; http://opensource.franz.com/preamble.html.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License can be
;; found at http://opensource.franz.com/license.html.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple
;; Place, Suite 330, Boston, MA  02111-1307  USA

;; This code was also edited by Lispworks Ltd. 
;;
;; $Id: delivery:ntservice:ntservice.lisp,v 1.2.1.1 2011/08/24 13:26:19 davef Exp $


(defpackage :ntservice 
  (:add-use-defaults t)
  (:use :common-lisp)
  (:export 
   #:start-service
   #:create-service
   #:delete-service
   #:winstrerror
   #:define-service))

(in-package :ntservice)

(fli:register-module "advapi32.dll")

(fli:define-c-typedef handle (:unsigned :long))
(fli:define-c-typedef dword (:unsigned :long))
(fli:define-c-typedef word (:unsigned :short))


;; foreign types

(fli:define-c-struct SERVICE_TABLE_ENTRY 
  (lpServiceName :pointer)
  (lpServiceProc :pointer))

(fli:define-c-struct SERVICE_STATUS
  (dwServiceType dword)
  (dwCurrentState dword)
  (dwControlsAccepted dword)
  (dwWin32ExitCode dword)
  (dwServiceSpecificExitCode dword)
  (dwCheckPoint dword)
  (dwWaitHint dword))

(fli:define-c-struct ENUM_SERVICE_STATUS
  (lpServiceName :pointer)
  (lpDisplayName :pointer)
  (ServiceStatus SERVICE_STATUS))


;; foreign calls

(fli:define-foreign-function (StartServiceCtrlDispatcher "StartServiceCtrlDispatcherW")
    ((lpServiceTable (:pointer SERVICE_TABLE_ENTRY)))
  :result-type :boolean)

(fli:define-foreign-function (RegisterServiceCtrlHandler "RegisterServiceCtrlHandlerW")
    ((lpServiceName (:reference-pass (:ef-wc-string :null-terminated-p t)))
     (lpHandlerProc (:pointer :function)))
  :result-type handle)

(fli:define-foreign-function (SetServiceStatus "SetServiceStatus")
    ((hServiceStatus handle)
     (lpServiceStatus (:pointer service_status)))
  :result-type :boolean)

(fli:define-foreign-function (OpenSCManager "OpenSCManagerW")
    ((lpMachineName (:reference-pass (:ef-wc-string :null-terminated-p t) :allow-null t))
     (lpDatabaseName (:reference-pass (:ef-wc-string :null-terminated-p t) :allow-null t))
     (dwDesiredAccess dword))
  :result-type handle)

(fli:define-foreign-function (CloseServiceHandle "CloseServiceHandle")
    ((hSCObject handle))
  :result-type :boolean)

(fli:define-foreign-function (OpenService "OpenServiceW")
    ((hSCManager handle)
     (lpServiceName (:reference-pass (:ef-wc-string :limit 256 :null-terminated-p t)))
     (dwDesiredAccess dword))
  :result-type handle)


(fli:define-foreign-function (EnumServicesStatus "EnumServicesStatusW")
    ((hSCManager handle)
     (dwServiceType dword)
     (dwServiceState dword)
     (lpServices :pointer)
     (cbBufSize dword)
     (pcbBytesNeeded (:pointer dword))
     (lpServicesReturned (:pointer dword))
     (lpResumeHandle (:pointer dword)))
  :result-type :boolean)

(fli:define-foreign-function (CreateService "CreateServiceW")
    ((hSCManager handle)
     (lpServiceName (:reference-pass (:ef-wc-string :limit 256 :null-terminated-p t)))
     (lpDisplayName (:reference-pass (:ef-wc-string :limit 256 :null-terminated-p t)))
     (dwDesiredAccess dword)
     (dwServiceType dword)
     (dwStartType dword)
     (dwErrorControl dword)
     (lpBinaryPathName (:reference-pass (:ef-wc-string :limit 256 :null-terminated-p t)))
     (lpLoadOrderGroup :pointer)
     (lpdwTagId (:pointer dword))
     (lpDependencies :pointer)
     (lpServiceStartName :pointer)
     (lpPassword :pointer))
  :result-type handle)

(fli:define-foreign-function (StartService "StartServiceW")
    ((hService handle)
     (dwNumServiceArgs dword)
     (lpServiceArgVectors :pointer))
  :result-type :boolean)

(fli:define-foreign-function (DeleteService "DeleteService")
    ((hService handle))
  :result-type :boolean)

(fli:define-foreign-function (OutputDebugString "OutputDebugStringW")
     ((lpOutputString (:reference-pass (:ef-wc-string :null-terminated-p t))))
  :result-type :void)

(fli:define-foreign-function (FormatMessage "FormatMessageW")
    ((dwFlags dword)
     (lpSource :pointer)
     (dwMessageId dword)
     (dwLanguageId dword)
     (lpBuffer :pointer)
     (nSize dword)
     (Arguments :pointer))
  :result-type dword)

(fli:define-foreign-function (LocalFree "LocalFree")
    ((hMem :pointer))
  :result-type handle)


;;; Hackery to make the handler in proper code as far as the service control
;;; manager is concerned.



(fli:get-embedded-module 'service-control-handler 
                                 (current-pathname 
                                  #+lispworks-32bit "service-control-handler-32.dll"
                                  #+lispworks-64bit "service-control-handler-64.dll"))

(fli:define-foreign-function setup_service_control_handler
    ((x :pointer)) 
  :result-type (:pointer :function)
  :module 'SERVICE-CONTROL-HANDLER)

;;;;;;;;;;;;;;;;;;;;;


;;; constants

(defparameter GENERIC_READ  #x80000000)

(defparameter STANDARD_RIGHTS_REQUIRED #x000F0000)
(defparameter SC_MANAGER_CONNECT             #x0001)
(defparameter SC_MANAGER_CREATE_SERVICE      #x0002)
(defparameter SC_MANAGER_ENUMERATE_SERVICE   #x0004)
(defparameter SC_MANAGER_LOCK                #x0008)
(defparameter SC_MANAGER_QUERY_LOCK_STATUS   #x0010)
(defparameter SC_MANAGER_MODIFY_BOOT_CONFIG  #x0020)

(defparameter SC_MANAGER_ALL_ACCESS          
    (logior STANDARD_RIGHTS_REQUIRED  
	    SC_MANAGER_CONNECT     
	    SC_MANAGER_CREATE_SERVICE    
	    SC_MANAGER_ENUMERATE_SERVICE 
	    SC_MANAGER_LOCK              
	    SC_MANAGER_QUERY_LOCK_STATUS 
	    SC_MANAGER_MODIFY_BOOT_CONFIG))



(defparameter SERVICE_WIN32_OWN_PROCESS      #x00000010)
(defparameter SERVICE_WIN32_SHARE_PROCESS    #x00000020)
(defparameter SERVICE_WIN32
    (logior SERVICE_WIN32_OWN_PROCESS SERVICE_WIN32_SHARE_PROCESS))
(defparameter SERVICE_INTERACTIVE_PROCESS    #x00000100)

(defparameter SERVICE_ACTIVE                 #x00000001)
(defparameter SERVICE_INACTIVE               #x00000002)
(defparameter SERVICE_STATE_ALL         
    (logior SERVICE_ACTIVE SERVICE_INACTIVE))

(defparameter SERVICE_BOOT_START             #x00000000)
(defparameter SERVICE_SYSTEM_START           #x00000001)
(defparameter SERVICE_AUTO_START             #x00000002)
(defparameter SERVICE_DEMAND_START           #x00000003)
(defparameter SERVICE_DISABLED               #x00000004)

(defparameter SERVICE_ERROR_IGNORE           #x00000000)
(defparameter SERVICE_ERROR_NORMAL           #x00000001)
(defparameter SERVICE_ERROR_SEVERE           #x00000002)
(defparameter SERVICE_ERROR_CRITICAL         #x00000003)


;;
;; Controls
;;
(eval-when (compile load eval)
(defparameter SERVICE_CONTROL_STOP           #x00000001)
(defparameter SERVICE_CONTROL_PAUSE          #x00000002)
(defparameter SERVICE_CONTROL_CONTINUE       #x00000003)
(defparameter SERVICE_CONTROL_INTERROGATE    #x00000004)
(defparameter SERVICE_CONTROL_SHUTDOWN       #x00000005)
(defparameter SERVICE_CONTROL_PARAMCHANGE    #x00000006)
(defparameter SERVICE_CONTROL_NETBINDADD     #x00000007)
(defparameter SERVICE_CONTROL_NETBINDREMOVE  #x00000008)
(defparameter SERVICE_CONTROL_NETBINDENABLE  #x00000009)
(defparameter SERVICE_CONTROL_NETBINDDISABLE #x0000000A)
)

;;
;; Service State -- for CurrentState
;;
(defparameter SERVICE_STOPPED                #x00000001)
(defparameter SERVICE_START_PENDING          #x00000002)
(defparameter SERVICE_STOP_PENDING           #x00000003)
(defparameter SERVICE_RUNNING                #x00000004)
(defparameter SERVICE_CONTINUE_PENDING       #x00000005)
(defparameter SERVICE_PAUSE_PENDING          #x00000006)
(defparameter SERVICE_PAUSED                 #x00000007)

;;
;; Controls Accepted  (Bit Mask)
;;
(defparameter SERVICE_ACCEPT_STOP            #x00000001)
(defparameter SERVICE_ACCEPT_PAUSE_CONTINUE  #x00000002)
(defparameter SERVICE_ACCEPT_SHUTDOWN        #x00000004)
(defparameter SERVICE_ACCEPT_PARAMCHANGE     #x00000008)
(defparameter SERVICE_ACCEPT_NETBINDCHANGE   #x00000010)

;;; error codes

(defparameter NO_ERROR 0)
(defparameter ERROR_MORE_DATA 234)
(defparameter ERROR_SERVICE_SPECIFIC_ERROR 1066)

;; FormatMessage stuff
(defparameter FORMAT_MESSAGE_ALLOCATE_BUFFER #x00000100)
(defparameter FORMAT_MESSAGE_IGNORE_INSERTS  #x00000200)
(defparameter FORMAT_MESSAGE_FROM_STRING     #x00000400)
(defparameter FORMAT_MESSAGE_FROM_HMODULE    #x00000800)
(defparameter FORMAT_MESSAGE_FROM_SYSTEM     #x00001000)
(defparameter FORMAT_MESSAGE_ARGUMENT_ARRAY  #x00002000)
(defparameter FORMAT_MESSAGE_MAX_WIDTH_MASK  #x000000FF)

;; globals

(defparameter service-status nil)
(defparameter service-status-handle nil)

(defparameter service-init-func nil)
(defparameter service-main-func nil)
(defparameter service-stop-func nil)


;; macros
(defmacro ss-slot (slot) 
  `(fli:foreign-slot-value service-status ,slot))


;; code
;; (FLI::DEFINE-PRECOMPILED-FOREIGN-OBJECT-ACCESSOR-FUNCTIONS (((:POINTER :POINTER) :NO-ALLOC-P :ERROR :SIZE T)))

(fli:define-foreign-callable ("ServiceMain" :encode :source :result-type :void)
    ((argc dword)
     (argv (:pointer :pointer)))
  (block ServiceMain
    (let* ((i-service-control-handler-addr 
            (fli:make-pointer :symbol-name 'service-control-handler
                              :functionp t))
           (service-control-handler-addr (setup_service_control_handler i-service-control-handler-addr))
           err
           args)
     ; (format t "ARGC ~A ARGV ~A~%" argc argv)
      (dotimes (i argc)
        (push (fli:convert-from-foreign-string (fli:dereference argv) :external-format :unicode) args)
        (fli:incf-pointer argv))
     ; (format t "ARGS: ~S~%" args)
      (setf args (rest (reverse args))) ;; drop the service name from the list
      ;; WJH    
      (setf service-status-handle (RegisterServiceCtrlHandler (service-name)
                                                              service-control-handler-addr))
      (setf err (win32:get-last-error))
    
      (when (= service-status-handle 0)
        (debug-msg (format nil "RegisterServiceCtrlHandler failed w/ error code ~D" err))
        (return-from ServiceMain))

      (setf service-status (fli:allocate-foreign-object :type 'SERVICE_STATUS))
      (setf (ss-slot 'dwServiceType) (logior SERVICE_WIN32_OWN_PROCESS 
                                             )
            (ss-slot 'dwControlsAccepted) 0
            (ss-slot 'dwWin32ExitCode) NO_ERROR
            (ss-slot 'dwCheckPoint) 0
            (ss-slot 'dwServiceSpecificExitCode) 0
            (ss-slot 'dwWaitHint) 0)
    
      (when service-init-func
        (setf (ss-slot 'dwCurrentState) SERVICE_START_PENDING)
        (set-service-status)
        (when (null (funcall service-init-func args))
          (setf (ss-slot 'dwWin32ExitCode) ERROR_SERVICE_SPECIFIC_ERROR
                (ss-slot 'dwServiceSpecificExitCode) 1)
          (set-service-status)
          (return-from ServiceMain)))
    
      (setf (ss-slot 'dwCurrentState) SERVICE_RUNNING)
      (setf (ss-slot 'dwControlsAccepted) SERVICE_ACCEPT_STOP)
      (set-service-status)
      (unwind-protect 
          (funcall service-main-func)
        (setf (ss-slot 'dwCurrentState) SERVICE_STOPPED)
        (set-service-status)))))

(defun set-service-status ()
  (let (res err)
    (setf res (SetServiceStatus service-status-handle service-status))
    (setf err (win32:get-last-error))
    (when (not res)
      (debug-msg (format nil "SetServiceStatus failed w/ error code ~D" err))
      (big-exit))))
  
(defun big-exit ()
  (funcall service-stop-func)
  (sleep 10) ; give it a chance to exit cleanly
  (lw:quit :status 0 :ignore-errors-p t :return t))
  
(fli:define-foreign-callable (service-control-handler :result-type :void)
    ((fdwControl dword))
  (debug-msg (format nil "service-control-handler got control code ~D~%" fdwControl))
  (case fdwControl
    (#.SERVICE_CONTROL_STOP
     (when service-stop-func
       (setf (ss-slot 'dwCurrentState) SERVICE_STOP_PENDING)
       (set-service-status)
       
       (funcall service-stop-func))
     
     (setf (ss-slot 'dwCurrentState) SERVICE_STOPPED)
     (set-service-status))
    (#.SERVICE_CONTROL_INTERROGATE
     (funcall service-stop-func))
    (t
     (debug-msg "That control code is not handled."))))




(defun start-service (main &key init stop)
  (fli:install-embedded-module 'service-control-handler)

  (setf service-main-func main)
  (setf service-init-func init)
  (setf service-stop-func stop)

  (let* ((ServiceMainAddr (fli:make-pointer :symbol-name "ServiceMain" 
                                            :functionp t))
         err)
    (let* ((service-name (fli:convert-to-foreign-string (service-name)
                                                        :external-format :unicode))
           (service-table (fli:allocate-foreign-object :type 'SERVICE_TABLE_ENTRY
                                                       :nelems 2))
           (table (fli:copy-pointer service-table)))
      (setf (fli:foreign-slot-value table 'lpServiceName) service-name
            (fli:foreign-slot-value table 'lpServiceProc) ServiceMainAddr)
      (fli:incf-pointer table)
      (setf (fli:foreign-slot-value table 'lpServiceName) nil
            (fli:foreign-slot-value table 'lpServiceProc) nil)

      (unless (StartServiceCtrlDispatcher service-table)
        (setf err (win32:get-last-error))
        (debug-msg (format nil "StartServiceCtrlDispatcher got error code ~D~%" err)))
      (quit :ignore-errors-p t))))


;;;;;;;;;;

(defun debug-msg (msg)
  (OutputDebugString msg))


;;;;;;;;  Other stuff.  Not used for normal service operation.  Some of it
;;;;;;;;  is just testing.   Some of it is code to install a service.

(defun open-sc-manager (machine database desired-access)
  (let ((res (OpenSCManager machine database desired-access)))
    (if (= res 0)
        (error "OpenSCManager error ~D" (win32:get-last-error))
      res)))

(defun close-sc-manager (handle)
  (CloseServiceHandle handle))

(defmacro with-sc-manager ((handle machine database desired-access) &body body)
  `(let ((,handle (open-sc-manager ,machine ,database ,desired-access)))
     (unwind-protect 
	 (progn ,@body)
       (close-sc-manager ,handle))))

(defun open-service (smhandle name desired-access)
  (let ((shandle (OpenService smhandle name desired-access)))
    (values (if (= 0 shandle) nil shandle) (win32:get-last-error))))

(defmacro with-open-service ((handle errvar smhandle name desired-access) 
			     &body body)
  `(multiple-value-bind (,handle ,errvar)
       (open-service ,smhandle ,name ,desired-access)
     (unwind-protect
         (progn ,@body)
       (if ,handle (CloseServiceHandle ,handle)))))

;;; just a test function.
(defun enum-services ()
  (with-sc-manager (schandle nil nil SC_MANAGER_ALL_ACCESS)
    (fli:with-dynamic-foreign-objects
        ((bytes-needed dword 0)
         (services-returned dword 0)
         (resume-handle dword 0))
    (let ((buf (fli:make-pointer :address 0))
	  (bufsize 0)
	  (errcode ERROR_MORE_DATA)
	  res)
      (loop while (= errcode ERROR_MORE_DATA) do
            (setf (fli:dereference resume-handle) 0)
            (setf res (EnumServicesStatus schandle SERVICE_WIN32 SERVICE_STATE_ALL 
                                            buf bufsize bytes-needed services-returned resume-handle))
              (setf errcode (win32:get-last-error))
            (if res (setf errcode 0)
              (progn
                (if (not (= errcode ERROR_MORE_DATA))
                    (error "EnumServicesStatus error code ~D" errcode))
                (setf bufsize (fli:dereference bytes-needed))
                (setf buf (fli:alloca :type :byte :nelems bufsize)))))

      (let ((count (fli:dereference services-returned)))
        (fli:with-coerced-pointer (enum :type 'ENUM_SERVICE_STATUS)
            buf
          (dotimes (i count)
            (format t "~A -> ~A~%" 
                    (fli:convert-from-foreign-string (fli:foreign-slot-value enum 'lpServiceName) :external-format :unicode)
                    (fli:convert-from-foreign-string (fli:foreign-slot-value enum 'lpDisplayName) :external-format :unicode))
            (fli:incf-pointer enum))))))))

(defun create-service (name displaystring cmdline &key (start :manual))
  (with-sc-manager (schandle nil nil SC_MANAGER_ALL_ACCESS)
    (multiple-value-bind (res)
	(CreateService 
	 schandle 
	 name
	 displaystring
	 STANDARD_RIGHTS_REQUIRED 
	 (logior SERVICE_WIN32_OWN_PROCESS SERVICE_INTERACTIVE_PROCESS) 
	 (case start
	   (:auto SERVICE_AUTO_START)
	   (:manual SERVICE_DEMAND_START)
	   (t (error "create-service: Unrecognized start type: ~S" start)))
	 SERVICE_ERROR_NORMAL
	 cmdline
	 nil ;; no load order group
	 nil ;; no tag identifier
	 nil ;; no dependencies 
	 nil ;; use LocalSystem account
	 nil) ;; no password
      (if (/= res 0)
	  (CloseServiceHandle res))
      (if (= res 0)
	  (values nil (win32:get-last-error))
	(values t res)))))


(defun delete-service (name)
  (with-sc-manager (sc nil nil SC_MANAGER_ALL_ACCESS)
    (with-open-service (handle err sc name STANDARD_RIGHTS_REQUIRED)
      (if handle
          (let ((res (DeleteService handle)))
            (if res (values t t)
              (values nil (win32:get-last-error) "DeleteService")))
        (values nil err "OpenService")))))

;;; Error message stuff

(defun winstrerror (code)
  (fli:with-dynamic-foreign-objects
      ((stringptr (:pointer :pointer)))
    (if (zerop (FormatMessage
                (logior FORMAT_MESSAGE_FROM_SYSTEM 
                        FORMAT_MESSAGE_IGNORE_INSERTS
                        FORMAT_MESSAGE_ALLOCATE_BUFFER) ;; dwFlags
                nil       ;; lpSource
                code      ;; dwMessageId
                0         ;; dwLanguageId
                stringptr ;; lpBuffer
                0         ;; nSize
                nil))     ;; Arguments
        ""
      (let ((string (fli:dereference stringptr)))
        (prog1
            (fli:convert-from-foreign-string string :external-format :unicode)
          (LocalFree string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 2007-08-06 - Andreas Thiele - Service Definition

(defparameter *definition* nil)

(defun define-service (&rest args)
  (setf *definition* args))

(defmacro defsa (name)
  "Define a Service Accessor Function."
  `(defun ,name () 
     (getf *definition* ,(intern (symbol-name name) :keyword))))

(defsa service-name)
(defsa display-name)
(defsa main-fn)
(defsa init-fn)
(defsa stop-fn)

(defun start-mode ()
  (let ((m (getf *definition* :start-mode)))
    (if m m :manual)))

(defvar *vrb-strm* nil) ;; Output Stream in verbose mode

(defun standard-main ()
  (let ((args system:*line-arguments-list*))
    (when (member "/verbose" args :test #'equalp)
      (setf *vrb-strm* t))
    (when (member "/install" args :test #'equalp)
      (add-service)
      (return-from standard-main))
    (when (member "/remove" args :test #'equalp)
      (remove-service)
      (return-from standard-main))
    (when (main-fn)
      (start-service (main-fn) 
                     :init (init-fn) 
                     :stop (stop-fn)))
    t))

(defun add-service ()
  (format *vrb-strm* "Installing service...~%")
  (multiple-value-bind (success errcode)
      (ntservice:create-service (service-name) (display-name) (lisp-image-name) :start (start-mode))
    (if success
	(format *vrb-strm* "Installation successful.~%")
      (progn
        (format t "Installation Failed. ERROR ~A~%" (winstrerror errcode))
        (sleep 2)))))

(defun remove-service ()
  (format *vrb-strm* "Removing service...~%")
  (multiple-value-bind (success errcode errfunc) (delete-service (service-name))
    (if success
	(format *vrb-strm* "Removal successful.~%")
      (progn
        (format t "Removal Failed. ERROR ~A~%" (winstrerror errcode))
        (sleep 2)))))


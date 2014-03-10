;;; Defiinnes the service.


;;; This definition is the only way in which you need to
;;; interact with the ntservice code. 

;;; Note: here the :start-mode is commented out and default to :MANUAL.
;;; Once the service works, you may want to uncomment it. 

(in-package "USER")



;;;---------------------------------------------------------------



(ntservice:define-service
 :service-name *service-name*          ;; name for use with net start command
 :display-name *service-display-name*  ;; display name in services control
 :init-fn 'my-init                     ;; called on init, with command line args (not required)
 :main-fn 'my-main                     ;; main function - if not provide the resulting app is a intaller
 :stop-fn 'my-stop                     ;; called when stopping the service (not required)
 ;:start-mode :auto                    ;; Once it works, uncomment this so it starts automatically
 )              
 

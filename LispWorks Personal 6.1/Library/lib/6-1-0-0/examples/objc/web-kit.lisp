;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/objc:web-kit.lisp,v 1.2.8.1 2011/08/24 13:26:19 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/objc/web-kit.lisp
;;
;; This example demonstrates the use of CAPI:COCOA-VIEW-PANE, containing a
;; WebView from Apple's Web Kit and allowing an HTML page to be viewed.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-WEB-KIT)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(capi:define-interface web-kit-test ()
  (delegate)
  (:panes
   (html-pane capi:cocoa-view-pane
              :view-class "WebView"
              :init-function 'init-web-kit-test-html-pane)
   (address capi:text-input-pane
            :title "Address:"
            :text "http://www.lispworks.com/"
            :callback 'web-kit-test-go
            :callback-type :interface)
   (go-button capi:push-button
              :text "Go"
              :callback 'web-kit-test-go
              :callback-type :interface))
  (:layouts
   (controls-layout capi:row-layout
                    '(address go-button))
   (main-layout capi:column-layout
                '(controls-layout html-pane)))
  (:default-initargs
   :layout 'main-layout
   :best-width 700
   :best-height 500
   :message-area t
   :title "Web Kit Test"))

(defmethod initialize-instance :after ((self web-kit-test) &key)
  (fli:register-module "/System/Library/Frameworks/WebKit.framework/WebKit"
                       :connection-style :immediate))

(defun init-web-kit-test-html-pane (self view)
  (let ((view (objc:invoke view "init"))
        (delegate (make-instance 'web-kit-test-delegate
                                 :web-view-pane self)))
    (setf (slot-value (capi:top-level-interface self) 'delegate) delegate)
    (objc:invoke view "setFrameLoadDelegate:"
                 (objc:objc-object-pointer delegate))
    view))


;;----------------------------------------------------------------------------
;; Callbacks
;;----------------------------------------------------------------------------

(defun web-kit-test-go (self)
  (with-slots (html-pane address) self
    (objc:invoke (objc:invoke (capi:cocoa-view-pane-view html-pane)
                              "mainFrame")
                 "loadRequest:"
                 (objc:invoke "NSURLRequest"
                              "requestWithURL:"
                              (objc:invoke "NSURL"
                                           "URLWithString:"
                                           (capi:text-input-pane-text address))))))

(objc:define-objc-class web-kit-test-delegate ()
  ((web-view-pane :initarg :web-view-pane
                  :reader web-kit-test-delegate-pane))
  (:objc-class-name "WebKitTestDelegate"))

(objc:define-objc-method ("webView:didReceiveTitle:forFrame:" :void)
    ((self web-kit-test-delegate)
     (sender objc:objc-object-pointer) ; WebView *
     (title objc:objc-object-pointer)  ; NSString *
     (frame objc:objc-object-pointer)  ; WebFrame *
     )
  (when (fli:pointer-eq frame (objc:invoke sender "mainFrame"))
    (setf (capi:titled-object-title
           (capi:top-level-interface
            (web-kit-test-delegate-pane self)))
          (concatenate 'string
                       "Web Kit Test: "
                       (objc:ns-string-to-string title)))))

(objc:define-objc-method ("webView:didStartProvisionalLoadForFrame:" :void)
    ((self web-kit-test-delegate)
     (sender objc:objc-object-pointer) ; WebView *
     (frame objc:objc-object-pointer)  ; WebFrame *
     )
  (when (fli:pointer-eq frame (objc:invoke sender "mainFrame"))
    (setf (capi:text-input-pane-text
           (slot-value (capi:top-level-interface
                        (web-kit-test-delegate-pane self))
                       'address))
          (objc:ns-string-to-string
           (objc:invoke
            (objc:invoke
             (objc:invoke
              (objc:invoke frame "provisionalDataSource")
              "request")
             "URL")
            "absoluteString")))))


;;----------------------------------------------------------------------------
;; The test function
;;----------------------------------------------------------------------------

(defun test-web-kit ()
  (capi:display (make-instance 'web-kit-test)))


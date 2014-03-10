;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/editor:commands:split-line.lisp,v 1.2.1.1 2011/08/24 13:26:18 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;; This code adds a command corresponding to GNU Emacs "split-line" to
;; the LW editor
;; Demonstrates the use of: DEFCOMMAND, CURRENT-POINT,
;; WITH-CURRENT-POINT-LOCKED, COPY-POINT,
;; LINE-START, POINTS-TO-STRING, INSERT-STRING, MOVE-POINT, BIND-KEY

(in-package "CL-USER")

(editor:defcommand "Split Line" (p)
  (declare (ignore p))
  (editor:with-current-point-locked (cur-point)
    (let* ((start-point (editor:copy-point cur-point :temporary))
           (temp-point (editor:copy-point cur-point :temporary)))
      (editor:line-start temp-point)
      ;; Insert newline and correct number of spaces
      (let ((insert-length (length (editor:points-to-string temp-point start-point))))
        (editor:new-line-command nil)
        (editor:insert-string cur-point (make-string insert-length :initial-element #\Space)))
      ;; Leave point where you found it
      (editor:move-point cur-point start-point))))

(editor:bind-key "Split Line" #\C-M-\o)

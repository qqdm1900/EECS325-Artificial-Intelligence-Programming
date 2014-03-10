;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/parser-generator:expression-parser.lisp,v 1.1.2.1 2011/08/24 13:26:18 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;; An example of using the parsergen:defparser macro to create a
;; parser for infix expressions using operators like in the C
;; programming language.

;; To test, call the function PARSE-EXPRESSION.
;; E.g. (CL-USER::PARSE-EXPRESSION "1+2*3")

(in-package "CL-USER")

(eval-when (:compile-toplevel)
  (require "parsergen"))

(eval-when (:load-toplevel :execute)
  (require "parser-runtime"))

(parsergen:defparser (expression-parser (:accept-without-eoi-p t))

 ((top.level.parser expression))

 (constant
  ((:FLOAT-CONSTANT)
   $1)
  ((:INTEGER-CONSTANT)
   $1))


 (primary.expression
  ((:IDENTIFIER)
   (intern (string-upcase $1)))
  ((constant)
   $1)
  ((#\( parenthesis.expression #\))
   $2))


 (parenthesis.expression
  ((expression)
   $1)
  ((:error)
   (warn "Incorrect expression found in parentheses...using 0.")
   0))


 (unary.expression
  ((primary.expression)
   $1)
  ((#\- unary.expression)
   `(- ,$2))
  ((#\+ unary.expression)
   $2)
  ((#\~ unary.expression)
   `(lognot ,$2))
  ((#\! unary.expression)
   `(not ,$2)))


 (multiplicative.expression
  ((unary.expression)
   $1)
  ((multiplicative.expression #\* unary.expression)
   `(* ,$1 ,$3))
  ((multiplicative.expression #\/ unary.expression)
   `(/ ,$1 ,$3))
  ((multiplicative.expression #\% unary.expression)
   `(mod ,$1 ,$3)))


 (additive.expression
  ((multiplicative.expression)
   $1)
  ((additive.expression #\+ multiplicative.expression)
   `(+ ,$1 ,$3))
  ((additive.expression #\- multiplicative.expression)
   `(- ,$1 ,$3)))

 
 (shift.expression
  ((additive.expression)
   $1)
  ((shift.expression :LS additive.expression)
   `(ash ,$1 ,$3))
  ((shift.expression :RS additive.expression)
   `(ash ,$1 (- ,$3))))


 (relational.expression
  ((shift.expression)
   $1)
  ((relational.expression #\< shift.expression)
   `(< ,$1 ,$3))
  ((relational.expression #\> shift.expression)
   `(> ,$1 ,$3))
  ((relational.expression :LE shift.expression)
   `(<= ,$1 ,$3))
  ((relational.expression :GE shift.expression)
   `(>= ,$1 ,$3)))


 (equality.expression
  ((relational.expression)
   $1)
  ((equality.expression :EQ relational.expression)
   `(= ,$1, $3))
  ((equality.expression :NE relational.expression)
   `(/= ,$1 ,$3)))


 (AND.expression
  ((equality.expression)
   $1)
  ((AND.expression #\& equality.expression)
   `(logand ,$1 ,$3)))


 (exclusive.OR.expression
  ((AND.expression)
   $1)
  ((exclusive.OR.expression #\^ AND.expression)
   `(logxor ,$1 ,$3)))


 (inclusive.OR.expression
  ((exclusive.OR.expression)
   $1)
  ((inclusive.OR.expression #\| exclusive.OR.expression)
   `(logior ,$1 ,$3)))


 (logical.AND.expression
  ((inclusive.OR.expression)
   $1)
  ((logical.AND.expression :ANDAND inclusive.OR.expression)
   `(and ,$1 ,$3)))


 (logical.OR.expression
  ((logical.AND.expression)
   $1)
  ((logical.OR.expression :OROR logical.AND.expression)
   `(or ,$1 ,$3)))


 (conditional.expression
  ((logical.OR.expression)
   $1)
  ((logical.OR.expression #\? expression #\: conditional.expression)
   `(if ,$1 ,$3 ,$5)))


 (expression
  ((conditional.expression)
   $1)
  ((expression #\, conditional.expression)
   `(progn ,$1 ,$3)))


 )

;; This is a simple lexer that recognizes integers, floats,
;; identifiers and the symbols in symbol-table.
(defun make-simple-lexer (stream symbol-table)
  #'(lambda ()
      (let ((char1 (loop for char = (read-char stream nil nil)
                         while (whitespace-char-p char)
                         finally return char)))
        (if (null char1)
            (values nil nil)
          (let ((symbol1 (assoc char1 symbol-table)))
            (if symbol1
                ;; Found a symbol.
                (let ((details1 (cdr symbol1)))
                  (if (consp details1)
                      (let ((char2 (read-char stream nil nil)))
                        (if (null char2)
                            (values char1
                                    (string char1))
                          (let ((symbol2 (assoc char2 details1)))
                            (cond (symbol2
                                   (values (cdr symbol2)
                                           (string-append char1 char2)))
                                  (t
                                   (unread-char char2 stream)
                                   (values char1
                                           (string char1)))))))
                    (values char1
                            (string char1))))
              ;; Number or identifier.
              (let* ((identifierp (alpha-char-p char1))
                     (integerp (digit-char-p char1))
                     (seen-dot-p (eql char1 #\.))
                     (seen-expt-p nil)
                     (floatp (or integerp seen-dot-p))
                     (previous-digit-p integerp)
                     (chars (list char1)))

                (when (or (alphanumericp char1) seen-dot-p)
                  ;; Read and parse the token.
                  (loop for char = (read-char stream nil nil)
                        while (and char
                                   (or (alphanumericp char)
                                       (eql char #\.)))
                        do
                        (push char chars)
                        (let ((digitp (digit-char-p char)))
                          (unless digitp
                            (setq integerp nil)
                            (case (char-upcase char)
                              ((#\.)
                               (if (or seen-dot-p seen-expt-p)
                                   ;; Not a float if more than one dot
                                   ;; or expt is before the dot.
                                   (setq floatp nil)
                                 (setq seen-dot-p t)))
                              ((#\D #\E #\F)
                               (if (or seen-expt-p (not previous-digit-p))
                                   ;; Not a float if more than one expt
                                   ;; or expt is not preceded by a digit.
                                   (setq floatp nil)
                                 (setq seen-expt-p t)))
                              (otherwise
                               ;; Not a float if any other char is seen.
                               (setq floatp nil))))
                          (setq previous-digit-p digitp))
                        finally
                        (when char
                          (unread-char char stream)))
                  (when (and seen-expt-p (not previous-digit-p))
                    ;; Float expt cannot be at the end.
                    (setq floatp nil))
                  (setq chars (nreverse chars)))

                ;; Convert the token to Lisp.
                (let ((token (coerce chars 'string)))
                  (cond (integerp
                         (values :INTEGER-CONSTANT (parse-integer token)))
                        (floatp
                         (values :FLOAT-CONSTANT (parse-float token)))
                        (identifierp
                         (values :IDENTIFIER token))
                        (t (error "Bad token ~S." token)))))))))))

(defparameter *parse-expression-symbols*
  '((#\+)
    (#\-)
    (#\*)
    (#\/)
    (#\%)
    (#\& (#\& . :ANDAND))
    (#\| (#\| . :OROR))
    (#\^)
    (#\~)
    (#\= (#\= . :EQ))
    (#\! (#\= . :NE))
    (#\> (#\= . :GE))
    (#\< (#\< . :LE))
    (#\?)
    (#\:)
    (#\,)
    (#\()
    (#\))))

(defun parse-expression (string)
  (with-input-from-string (stream string)
    (expression-parser
     (make-simple-lexer stream *parse-expression-symbols*))))


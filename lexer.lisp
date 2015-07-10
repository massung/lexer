;;;; Lexing package for Common Lisp
;;;;
;;;; Copyright (c) 2012 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :lexer
  (:use :cl :re)
  (:nicknames :lex)
  (:export
   #:deflexer

   ;; utility functions
   #:push-lexer
   #:pop-lexer
   #:swap-lexer

   ;; functions
   #:scan
   #:tokenize
   #:include
   #:parse
   #:parse-with-lexer
   #:slurp

   ;; lexbuf readers
   #:lex-lexeme
   #:lex-pos
   #:lex-line
   #:lex-source
   #:lex-string

   ;; token readers
   #:token-line
   #:token-source
   #:token-class
   #:token-value
   #:token-lexeme))

(in-package :lexer)

;;; ----------------------------------------------------

(defclass lexbuf ()
  ((string :initarg :string :reader lex-string)
   (source :initarg :source :reader lex-source   :initform nil)
   (pos    :initarg :pos    :reader lex-pos      :initform 0)
   (line   :initarg :line   :reader lex-line     :initform 1)
   (lexeme :initarg :lexeme :reader lex-lexeme   :initform nil))
  (:documentation "The input source for a DEFLEXER."))

;;; ----------------------------------------------------

(defclass token ()
  ((line   :initarg :line   :reader token-line   :initform 0)
   (source :initarg :source :reader token-source :initform nil)
   (class  :initarg :class  :reader token-class  :initform nil)
   (value  :initarg :value  :reader token-value  :initform nil)
   (lexeme :initarg :lexeme :reader token-lexeme :initform ""))
  (:documentation "A parsed token from a lexbuf."))

;;; ----------------------------------------------------

(define-condition lex-error (error)
  ((reason :initarg :reason :reader lex-error-reason)
   (line   :initarg :line   :reader lex-error-line)
   (source :initarg :source :reader lex-error-source)
   (lexeme :initarg :lexeme :reader lex-error-lexeme))
  (:documentation "Signaled when an error occurs during tokenizing or parsing.")
  (:report (lambda (c s)
             (with-slots (reason line source lexeme)
                 c
               (format s "~a on line ~a~@[ of ~s~]~@[ near ~s~]" reason line source lexeme)))))

;;; ----------------------------------------------------

(defmethod print-object ((token token) s)
  "Output a token to a stream."
  (print-unreadable-object (token s :type t)
    (with-slots (class value)
        token
      (format s "~a~@[ ~s~]" class value))))

;;; ----------------------------------------------------

(defvar *lexbuf*)
(defvar *lexer*)

;;; ----------------------------------------------------

(defmacro deflexer (lexer &body patterns)
  "Create a tokenizing function."
  (let ((string (gensym "string"))
        (source (gensym "source"))
        (pos (gensym "pos"))
        (line (gensym "line"))
        (lexeme (gensym "lexeme"))
        (next-token (gensym "next-token"))
        (match (gensym "match"))
        (class (gensym "class"))
        (value (gensym "value")))
    `(defun ,lexer ()
       (symbol-macrolet ((,string (slot-value (first *lexbuf*) 'string))
                         (,source (slot-value (first *lexbuf*) 'source))
                         (,pos    (slot-value (first *lexbuf*) 'pos))
                         (,line   (slot-value (first *lexbuf*) 'line))
                         (,lexeme (slot-value (first *lexbuf*) 'lexeme)))
         (tagbody
          ,next-token
          ,@(loop for pattern in patterns collect
                  (destructuring-bind (p &body body)
                      pattern
                    (with-re (re p)
                      `(with-re-match (,match (match-re ,re ,string :offset ,pos))
                         (incf ,line (count #\newline (match-string ,match)))
                         (setf ,pos (match-pos-end ,match))
                         (setf ,lexeme (subseq ,string (match-pos-start ,match) ,pos))
                         (multiple-value-bind (,class ,value)
                             (progn ,@body)
                           (if (eq ,class :next-token)
                               (go ,next-token)
                             (return-from ,lexer (when ,class
                                                   (make-instance 'token
                                                                  :class ,class
                                                                  :value ,value
                                                                  :line ,line
                                                                  :source ,source
                                                                  :lexeme ,lexeme)))))))))

          ;; no pattern matched - error if not at the end of the buffer
          (if (< ,pos (length ,string))
              (error (make-instance 'lex-error
                                    :reason "Lexing error"
                                    :line ,line
                                    :source ,source
                                    :lexeme (string (char ,string ,pos))))
            (progn
              (pop *lexbuf*)

              ;; if there are still buffers being parsed, continue
              (when *lexbuf*
                (go ,next-token)))))))))

;;; ----------------------------------------------------

(defmacro with-lexer ((lexer string &optional source) &body body)
  "Create a new lexbuf and loop body until done."
  `(let ((*lexbuf* (list (make-instance 'lexbuf :string ,string :source ,source)))
         (*lexer* (list ,lexer)))
     ,@body))

;;; ----------------------------------------------------

(defun push-lexer (lexer class &optional value)
  "Push a new lexer and return a token."
  (multiple-value-prog1
      (values class value)
    (push lexer *lexer*)))

;;; ----------------------------------------------------

(defun pop-lexer (class &optional value)
  "Pop the top lexer and return a token."
  (multiple-value-prog1
      (values class value)
    (pop *lexer*)))

;;; ----------------------------------------------------

(defun swap-lexer (lexer class &optional value)
  "Set the current lexer and return a token."
  (multiple-value-prog1
      (values class value)
    (rplaca *lexer* lexer)))

;;; ----------------------------------------------------

(defun include (string &optional source)
  "Push a new buffer to be tokenized using the current lexer."
  (prog1
      :next-token
    (push (make-instance 'lexbuf :string string :source source) *lexbuf*)))

;;; ----------------------------------------------------

(defun make-parse-error (condition &optional token)
  "Generate an error with an optional token."
  (if (null token)
      (error condition)
    (error (make-instance 'lex-error
                          :reason condition
                          :line (token-line token)
                          :source (token-source token)
                          :lexeme (token-lexeme token)))))

;;; ----------------------------------------------------

(defun read-token ()
  "Reads the next token in the current lexbuf using the top lexer."
  (when *lexer*
    (let ((token (funcall (first *lexer*))))
      (if token
          token
        (prog1 nil
          (pop *lexer*))))))

;;; ----------------------------------------------------

(defun scan (token-function lexer string &optional source)
  "Create a lexbuf and parse, but don't collect tokens, instead, call a function with each."
  (with-lexer (lexer string source)
    (loop while *lexer* for tok = (read-token) when tok do (funcall token-function tok))))

;;; ----------------------------------------------------

(defun tokenize (lexer string &optional source)
  "Create a lexbuf and parse until there are not more tokens or lexer."
  (with-lexer (lexer string source)
    (loop while *lexer* for tok = (read-token) when tok collect tok)))

;;; ----------------------------------------------------

(defun parse (parser tokens)
  "Set the lexer and parse the source with it."
  (let (token)
    (flet ((next-token ()
             (let ((next-token (pop tokens)))
               (when next-token
                 (multiple-value-prog1
                     (values (token-class next-token)
                             (token-value next-token))
                   (setf token next-token))))))
      (handler-case
          (funcall parser #'next-token)
        (error (c)
          (make-parse-error c token))))))

;;; ----------------------------------------------------

(defun parse-with-lexer (parser lexer string &optional source)
  "Instead of parsing a list of tokens, tokenize and parse at the same time."
  (with-lexer (lexer string source)
    (let (token)
      (flet ((next-token ()
               (let ((next-token (read-token)))
                 (when next-token
                   (multiple-value-prog1
                       (values (token-class next-token)
                               (token-value next-token))
                     (setf token next-token))))))
        (handler-case
            (funcall parser #'next-token)
          (error (c)
            (make-parse-error c token)))))))

;;; ----------------------------------------------------

(defun slurp (pathname &key (element-type 'base-char))
  "Read a file into a string sequence."
  (with-open-file (stream pathname :element-type element-type)
    (let ((seq (make-array (file-length stream) :element-type element-type :fill-pointer t)))
      (prog1
          seq
        (setf (fill-pointer seq) (read-sequence seq stream))))))

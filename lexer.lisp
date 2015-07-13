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

   ;; token/parse functions
   #:tokenize
   #:parse
   #:slurp

   ;; lexing functions
   #:push-lexer
   #:pop-lexer
   #:swap-lexer

   ;; lexbuf readers
   #:lexbuf-string
   #:lexbuf-pos
   #:lexbuf-end
   #:lexbuf-line
   #:lexbuf-source

   ;; token readers
   #:token-line
   #:token-lexeme
   #:token-source
   #:token-class
   #:token-value))

(in-package :lexer)

;;; ----------------------------------------------------

(defclass lexstate ()
  ((stack  :initarg :stack  :reader lexstate-stack :initform nil)
   (lexbuf :initarg :lexbuf :reader lexstate-buf   :initform nil))  
  (:documentation "The input parameter for DEFLEXER."))

;;; ----------------------------------------------------

(defclass lexbuf ()
  ((string :initarg :string :reader lexbuf-string  :initform "")
   (source :initarg :source :reader lexbuf-source  :initform nil)
   (pos    :initarg :pos    :reader lexbuf-pos     :initform nil)
   (end    :initarg :end    :reader lexbuf-end     :initform nil)
   (line   :initarg :line   :reader lexbuf-line    :initform nil))
  (:documentation "The source string being matched against."))

;;; ----------------------------------------------------

(define-condition lex-error (error)
  ((reason :initarg :reason :reader lex-error-reason)
   (line   :initarg :line   :reader lex-error-line)
   (source :initarg :source :reader lex-error-source))
  (:documentation "Signaled when an error occurs during tokenizing or parsing.")
  (:report (lambda (c s)
             (with-slots (reason line source)
                 c
               (format s "~a on line ~a~@[ of ~s~]" reason line source)))))

;;; ----------------------------------------------------

(defclass token ()
  ((line   :initarg :line   :reader token-line     :initform 1)
   (lexeme :initarg :lexeme :reader token-lexeme   :initform "")
   (source :initarg :source :reader token-source   :initform nil)
   (class  :initarg :class  :reader token-class    :initform nil)
   (value  :initarg :value  :reader token-value    :initform nil))
  (:documentation "A parsed token from a lexbuf."))

;;; ----------------------------------------------------

(defmethod initialize-instance :after ((buf lexbuf) &key &allow-other-keys)
  "Setup the range of the buffer and validate it."
  (with-slots (string pos end line)
      buf
    (let ((len (length string)))

      ;; set default values
      (unless pos (setf pos 0))
      (unless end (setf end len))
      
      ;; validate the buffer range for parsing
      (assert (<= 0 pos end len))

      ;; set the starting line
      (setf line (1+ (count #\newline string :end pos))))))

;;; ----------------------------------------------------

(defun make-lex-error (state err)
  "Generate an error with an optional token."
  (let ((line (lexbuf-line (lexstate-buf state)))
        (source (lexbuf-source (lexstate-buf state))))
    (error (make-instance 'lex-error :reason err :line line :source source))))

;;; ----------------------------------------------------

(defmethod print-object ((token token) s)
  "Output a token to a stream."
  (print-unreadable-object (token s :type t)
    (with-slots (class value)
        token
      (format s "~a~@[ ~s~]" class value))))

;;; ----------------------------------------------------

(defmethod print-object ((buf lexbuf) s)
  "Output a lexbuf to a stream."
  (print-unreadable-object (buf s :type t)
    (with-slots (string pos end line)
        buf
      (let ((eol (or (find #\newline string :start pos :end end) end)))
        (format s "~d/~d (line ~d)... ~s" pos end line (subseq string pos eol))))))

;;; ----------------------------------------------------

(defmacro deflexer (lexer (state) &body patterns)
  "Create a tokenizing function."
  (let ((string (gensym "string"))
        (source (gensym "source"))
        (pos (gensym "pos"))
        (end (gensym "end"))
        (line (gensym "line"))
        (next-token (gensym "next-token"))
        (match (gensym "match"))
        (class (gensym "class"))
        (value (gensym "value")))
    `(defun ,lexer (,state)
       (with-slots ((,string string) (,source source) (,pos pos) (,end end) (,line line))
           (lexstate-buf ,state)
         (tagbody
          ,next-token
          ,@(loop for pattern in patterns collect
                  (destructuring-bind (p &body body)
                      pattern
                    (with-re (re p)
                      `(with-re-match (,match (match-re ,re ,string :offset ,pos :end ,end))
                         (incf ,line (count #\newline (match-string ,match)))
                         (setf ,pos (match-pos-end ,match))
                         (multiple-value-bind (,class ,value)
                             (progn ,@body)
                           (if (eq ,class :next-token)
                               (go ,next-token)
                             (return-from ,lexer
                               (when ,class
                                 (make-instance 'token
                                                :class ,class
                                                :value ,value
                                                :line ,line
                                                :source ,source
                                                :lexeme (match-string ,match))))))))))
          
          ;; no pattern matched - error if not at the end of the buffer
          (when (< ,pos ,end)
            (error "Lexing error")))))))

;;; ----------------------------------------------------

(defmacro with-lexer ((var lexer string &key source pos end) &body body)
  "Create a new lexbuf and lexer list for tokenizing."
  (let ((buf (gensym))
        (err (gensym)))
    `(let* ((,buf (make-instance 'lexbuf :string ,string :source ,source :pos ,pos :end ,end))
            (,var (make-instance 'lexstate :stack (list ,lexer) :lexbuf ,buf)))
       (handler-case
           (progn ,@body)
         (condition (,err)
           (make-lex-error ,var ,err))))))

;;; ----------------------------------------------------

(defun push-lexer (state lexer class &optional value)
  "Push a new lexer and return a token."
  (multiple-value-prog1
      (values class value)
    (push lexer (slot-value state 'stack))))

;;; ----------------------------------------------------

(defun pop-lexer (state class &optional value)
  "Pop the top lexer and return a token."
  (multiple-value-prog1
      (values class value)
    (pop (slot-value state 'stack))))

;;; ----------------------------------------------------

(defun swap-lexer (state lexer class &optional value)
  "Set the current lexer and return a token."
  (multiple-value-prog1
      (values class value)
    (rplaca (slot-value state 'stack) lexer)))

;;; ----------------------------------------------------

(defun read-token (state)
  "Reads the next token in the current lexbuf using the top lexer."
  (if (null (lexstate-stack state))
      nil
    (funcall (first (lexstate-stack state)) state)))

;;; ----------------------------------------------------

(defun tokenize (lexer string &optional source)
  "Create a lexbuf and parse until there are not more tokens or lexer."
  (with-lexer (state lexer string :source source)
    (loop for tok = (read-token state) while tok collect tok)))

;;; ----------------------------------------------------

(defun parse (parser lexer string &optional source)
  "Set the lexer and parse the source with it."
  (with-lexer (state lexer string :source source)
    (let (token)
      (flet ((next-token ()
               (setf token nil)
               (when (setf token (read-token state))
                 (values (token-class token)
                         (token-value token)))))
        (handler-case
            (funcall parser #'next-token)
          (condition (c)
            (error "~a~@[ near ~s~]" c (and token (token-lexeme token)))))))))

;;; ----------------------------------------------------

(defun slurp (pathname &key (element-type 'character))
  "Read a file into a string sequence."
  (with-open-file (stream pathname :element-type element-type)
    (let ((seq (make-array (file-length stream) :element-type element-type :fill-pointer t)))
      (prog1
          seq
        (setf (fill-pointer seq) (read-sequence seq stream))))))

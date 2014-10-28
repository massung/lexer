;;;; Lexing package for LispWorks
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
  (:use :cl :lw :re)
  (:nicknames :lex)
  (:export
   #:deflexer

   ;; utility functions
   #:push-lexer
   #:pop-lexer
   #:swap-lexer

   ;; functions
   #:tokenize
   #:parse
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

(defclass lexbuf ()
  ((string :initarg :string :reader lex-string)
   (source :initarg :source :reader lex-source   :initform nil)
   (pos    :initarg :pos    :reader lex-pos      :initform 0)
   (line   :initarg :line   :reader lex-line     :initform 1)
   (lexeme :initarg :lexeme :reader lex-lexeme   :initform nil))
  (:documentation "The input source for a DEFLEXER."))

(defclass token ()
  ((line   :initarg :line   :reader token-line   :initform 0)
   (source :initarg :source :reader token-source :initform nil)
   (class  :initarg :class  :reader token-class  :initform nil)
   (value  :initarg :value  :reader token-value  :initform nil)
   (lexeme :initarg :lexeme :reader token-lexeme :initform ""))
  (:documentation "A parsed token from a lexbuf."))

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

(defmethod print-object ((token token) s)
  "Output a token to a stream."
  (print-unreadable-object (token s :type t)
    (with-slots (class value)
        token
      (format s "~a~@[ ~s~]" class value))))

(defvar *lexbuf*)
(defvar *lexer*)

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
       (with-slots ((,string string) (,source source) (,pos pos) (,line line) (,lexeme lexeme))
           *lexbuf*
         (prog
             ()
          ,next-token
          ,@(loop :for pattern :in patterns :collect
                  (destructuring-bind (p &body body)
                      pattern
                    (with-re (re p)
                      `(with-re-match (,match (match-re ,re ,string :start ,pos))
                         (incf ,line (count #\newline (match-string ,match)))
                         (setf ,pos (match-pos-end ,match))
                         (setf ,lexeme (subseq ,string (match-pos-start ,match) ,pos))
                         (multiple-value-bind (,class ,value)
                             (progn ,@body)
                           (if (eq ,class :next-token)
                               (go ,next-token)
                             (return (when ,class
                                       (make-instance 'token
                                                      :class ,class
                                                      :value ,value
                                                      :line ,line
                                                      :source ,source
                                                      :lexeme ,lexeme)))))))))
          (unless (>= ,pos (length ,string))
            (error (make-instance 'lex-error
                                  :reason "Lexing error"
                                  :line ,line
                                  :source ,source
                                  :lexeme (string (char ,string ,pos))))))))))

(defun push-lexer (lexer class &optional value)
  "Push a new lexer and return a token."
  (multiple-value-prog1
      (values class value)
    (push lexer *lexer*)))

(defun pop-lexer (class &optional value)
  "Pop the top lexer and return a token."
  (multiple-value-prog1
      (values class value)
    (pop *lexer*)))

(defun swap-lexer (lexer class &optional value)
  "Set the current lexer and return a token."
  (multiple-value-prog1
      (values class value)
    (rplaca *lexer* lexer)))

(defun tokenize (lexer string &optional source)
  "Create a function that can be used in a parsergen."
  (loop :with *lexbuf* := (make-instance 'lexbuf :string string :source source)
        :with *lexer* := (list lexer)
        
        ;; while there is still an active lexer
        :while *lexer*

        ;; collect all the tokens for that lexer
        :nconc (loop :for token := (funcall (first *lexer*))
                     :while token
                     :collect token)

        ;; then pop it and continue tokenizing
        :do (pop *lexer*)))

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
        (condition (c)
          (if (null token)
              (error c)
            (error (make-instance 'lex-error
                                  :reason c
                                  :line (token-line token)
                                  :source (token-source token)
                                  :lexeme (token-lexeme token)))))))))

(defun slurp (pathname &key (element-type 'base-char))
  "Read a file into a string sequence."
  (with-open-file (stream pathname)
    (let ((seq (make-array (file-length stream) :element-type element-type :fill-pointer t)))
      (prog1
          seq
        (setf (fill-pointer seq) (read-sequence seq stream))))))

;;;; Pattern Matching and Lexing package for LispWorks
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "parsergen"))

(defpackage :lexer
  (:use :cl :lw :parsergen)
  (:nicknames :lex)
  (:export
   #:re
   #:re-match

   ;; macros
   #:with-re-match
   #:deflexer

   ;; pattern functions
   #:compile-re
   #:match-re
   #:find-re
   #:split-re
   #:replace-re

   ;; match functions
   #:match-string
   #:match-captures
   #:match-pos-start
   #:match-pos-end

   ;; token functions
   #:token-lexeme
   #:token-line
   #:token-source

   ;; lexer functions
   #:lex-next-token
   #:lex-lexeme
   #:lex-pos
   #:lex-line

   ;; parse error functions
   #:lex-error-source
   #:lex-error-pos
   #:lex-error-line

   ;; create a parse error
   #:lex-parse-error))

(in-package :lexer)

(defclass re ()
  ((pattern    :initarg :pattern    :reader re-pattern)
   (case-fold  :initarg :case-fold  :reader re-case-fold-p)
   (multi-line :initarg :multi-line :reader re-multi-line-p)
   (expression :initarg :expression :reader re-expression))
  (:documentation "Regular expression."))

(defclass re-match ()
  ((match     :initarg :match     :reader match-string)
   (captures  :initarg :captures  :reader match-captures)
   (start-pos :initarg :start-pos :reader match-pos-start)
   (end-pos   :initarg :end-pos   :reader match-pos-end))
  (:documentation "Matched token."))

(defclass token ()
  ((lexeme :initarg :lexeme :reader token-lexeme)
   (class  :initarg :class  :reader token-class)
   (value  :initarg :value  :reader token-value)
   (line   :initarg :line   :reader token-line)
   (source :initarg :source :reader token-source))
  (:documentation "Token created by a lexer."))

(defclass lex-state ()
  ((source   :initarg :source   :reader lex-source)
   (start    :initarg :start    :reader lex-start)
   (captures :initarg :captures :reader lex-captures)
   (newline  :initarg :newline  :reader lex-newline))
  (:documentation "Token pattern matching state."))

(defclass lexer ()
  ((next   :initarg :next-token :reader lex-next-token)
   (lexeme :initarg :lexeme     :reader lex-lexeme)
   (pos    :initarg :pos        :reader lex-pos)
   (line   :initarg :line       :reader lex-line))
  (:documentation "Created by DEFLEXER, used to tokenize a string."))

(define-condition lex-error (error)
  ((source :initarg :source :reader lex-error-source)
   (lexer  :initarg :lexer  :reader lex-error-lexer))
  (:documentation "Error signaled when a deflexer fails all patterns.")
  (:report (lambda (c s)
             (with-slots (line pos)
                 (lex-error-lexer c)
               (let ((char (char (lex-error-source c) pos)))
                 (format s "Lexing error on line ~d near '~c'" line char))))))

(define-condition lex-parse-error (lex-error)
  ()
  (:documentation "Error signaled when a parser fails.")
  (:report (lambda (c s)
             (let ((source (lex-error-source c)))
               (with-slots (pos line lexeme)
                   (lex-error-lexer c)
                 (if (and lexeme (plusp (length lexeme)))
                     (format s "Parse error on line ~d of ~s near ~s" line source lexeme)
                   (let ((ch (char source pos)))
                     (format s "Parse error on line ~d of ~s near '~c'." line source ch))))))))

(define-condition re-pattern-error (condition)
  ()
  (:documentation "Signaled when a regular expression parse fails."))

(defmethod print-object ((re re) s)
  "Output a regular expression to a stream."
  (print-unreadable-object (re s :type t)
    (format s "~s" (re-pattern re))))

(defmethod print-object ((match re-match) s)
  "Output a regular expression match to a stream."
  (print-unreadable-object (match s :type t)
    (format s "~s" (match-string match))))

(defmethod print-object ((tok token) s)
  "Output a lexer token to a stream."
  (print-unreadable-object (tok s :type t)
    (format s "~s ~a" (token-lexeme tok) (token-class tok))))

(defmethod make-load-form ((re re) &optional env)
  "Tell the system how to save and load a regular expression to a FASL."
  (with-slots (pattern case-fold multi-line)
      re
    `(compile-re ,pattern :case-fold ,case-fold :multi-line ,multi-line)))

(defvar *case-fold* nil "Case-insentitive comparison.")
(defvar *multi-line* nil "Dot and EOL also match newlines.")

(defconstant +lowercase-letter+ "abcdefghijklmnopqrstuvwxyz")
(defconstant +uppercase-letter+ "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defconstant +digit+ "0123456789")
(defconstant +hex-digit+ "0123456789abcdefABCDEF")
(defconstant +punctuation+ "`~!@#$%^&*()-+=[]{}\|;:',./<>?\"")
(defconstant +letter+ #.(concatenate 'string +lowercase-letter+ +uppercase-letter+))
(defconstant +alpha-numeric+ #.(concatenate 'string +letter+ +digit+))
(defconstant +spaces+ #.(format nil "~c~c" #\space #\tab))
(defconstant +newlines+ #.(format nil "~c~c~c" #\newline #\return #\linefeed))

(defun range-chars (from to)
  "Return an inclusive list of characters in ascii order."
  (loop :for c :from (char-code from) :to (char-code to) :collect (code-char c)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((dispatch-re (s c n)
           (declare (ignorable c n))
           (let ((re (with-output-to-string (re)
                       (loop :for c := (read-char s t nil t) :do
                         (case c
                           (#\/ (return))
                           (#\\ (let ((c (read-char s t nil t)))
                                  (princ c re)))
                           (otherwise
                            (princ c re)))))))
             (compile-re re))))
    (set-dispatch-macro-character #\# #\/ #'dispatch-re)))

(defmacro with-re-match ((v match) &body body)
  "Intern match symbols to execute a body."
  (let (($$ (intern "$$" *package*))
        ($1 (intern "$1" *package*))
        ($2 (intern "$2" *package*))
        ($3 (intern "$3" *package*))
        ($4 (intern "$4" *package*))
        ($5 (intern "$5" *package*))
        ($6 (intern "$6" *package*))
        ($7 (intern "$7" *package*))
        ($8 (intern "$8" *package*))
        ($9 (intern "$9" *package*)))
    `(let ((,v ,match))
       (when ,v
         (destructuring-bind (,$$ &optional ,$1 ,$2 ,$3 ,$4 ,$5 ,$6 ,$7 ,$8 ,$9 &rest _)
             (cons (match-string ,v) (match-captures ,v))
           (declare (ignorable ,$$ ,$1 ,$2 ,$3 ,$4 ,$5 ,$6 ,$7 ,$8 ,$9 _))
           (values (progn ,@body) t))))))

(defmacro deflexer (make-lexer (&rest options) &body patterns)
  "Create a tokenizing function."
  (let ((lexer (gensym "lexer"))
        (value (gensym "value"))
        (class (gensym "class"))
        (next-token (gensym "next-token"))
        (source (gensym "source"))
        (path (gensym "path"))
        (skip-token (gensym "skip"))
        (match (gensym "match")))
    `(defun ,make-lexer (,source &optional ,path)
       (let ((,lexer (make-instance 'lexer :lexeme nil :pos 0 :line 1)))
         (flet ((,next-token ()
                  (with-slots (pos line lexeme)
                      ,lexer
                    (tagbody
                     ,skip-token
                     (unless (< pos (length ,source))
                       (return-from ,next-token))
                     ,@(loop :for token :in patterns :collect
                             (destructuring-bind (pattern &body body)
                                 token
                               (let ((re (apply #'compile-re (cons pattern options))))
                                 `(with-re-match (,match (match-re ,re ,source :start pos))
                                    (incf line (count #\newline (match-string ,match)))
                                    (setf lexeme (match-string ,match))
                                    (setf pos (match-pos-end ,match))
                                    (multiple-value-bind (,class ,value)
                                        (progn ,@body)
                                      (if (null ,class)
                                          (go ,skip-token)
                                        (return-from ,next-token
                                          (values ,class (make-instance 'token
                                                                        :lexeme lexeme
                                                                        :line line
                                                                        :class ,class
                                                                        :value ,value
                                                                        :source ,path))))))))))
                    (error (make-condition 'lex-error :source ,source :lexer ,lexer)))))
           (prog1
               ,lexer
             (setf (slot-value ,lexer 'next) #',next-token)))))))

(defparser re-parser
  ((start compound) $1)

  ;; either expression a or b (a|b)
  ((compound compound :or exprs) (either $1 $3))
  ((compound exprs) $1)

  ;; multiple expressions bound together
  ((exprs simple exprs) (bind $1 $2))
  ((exprs simple) $1)
  ((exprs :error) (error 're-pattern-error))

  ;; simple, optional, and repition (?, *, +)
  ((simple expr :maybe) (maybe $1))
  ((simple expr :many) (many $1))
  ((simple expr :many1) (many1 $1))
  ((simple expr :to) (many $1))
  ((simple expr) $1)

  ;; capture expression (x)
  ((expr :capture :maybe compound :end-capture) $3)
  ((expr :capture compound :end-capture) (capture $2))

  ;; bounded expression
  ((expr :between) 
   (between (ch (car $1) :case-fold *case-fold*)
            (ch (cdr $1) :case-fold *case-fold*)))

  ;; single character
  ((expr :char) (ch $1 :case-fold *case-fold*))

  ;; any character and end of line/input
  ((expr :any) (any-char :match-newline-p (not *multi-line*)))
  ((expr :eol) (eol :match-newline-p *multi-line*))
  ((expr :none) (newline :match-newline-p *multi-line*))

  ;; sets of characters (named, [..], [^..])
  ((expr :one-of) (one-of $1))
  ((expr :none-of) (none-of $1))
  ((expr :set set) $2)

  ((expr :error) (error 're-pattern-error))

  ;; inclusive and exclusive sets
  ((set :none chars) (none-of $2 :case-fold *case-fold*))
  ((set chars) (one-of $1 :case-fold *case-fold*))

  ;; character set (just a list of characters)
  ((chars :one-of chars) (append (coerce $1 'list) $2))
  ((chars :char :to :char chars) (append (range-chars $1 $3) $4))
  ((chars :eol chars) (cons #\$ $2))
  ((chars :any chars) (cons #\. $2))
  ((chars :capture chars) (cons #\( $2))
  ((chars :end-capture chars) (cons #\) $2))
  ((chars :maybe chars) (cons #\? $2))
  ((chars :many chars) (cons #\* $2))
  ((chars :many1 chars) (cons #\+ $2))
  ((chars :or chars) (cons #\| $2))
  ((chars :char chars) (cons $1 $2))
  ((chars :to :end-set) (list #\-))
  ((chars :end-set) nil)
  ((chars :error) (error 're-pattern-error)))

(defun compile-re (pattern &key case-fold multi-line)
  "Create a regular expression pattern match."
  (let ((*case-fold* case-fold)
        (*multi-line* multi-line))
    (with-input-from-string (s pattern)
      (flet ((next-token ()
               (let ((c (read-char s nil nil)))
                 (when c
                   (case c
                     (#\%
                      (let ((c (read-char s)))
                        (case c
                          (#\b (let ((b1 (read-char s))
                                     (b2 (read-char s)))
                                 (values :between (cons b1 b2))))
                          (#\s (values :one-of +spaces+))
                          (#\S (values :none-of +spaces+))
                          (#\n (values :one-of +newlines+))
                          (#\N (values :none-of +newlines+))
                          (#\a (values :one-of +letter+))
                          (#\A (values :none-of +letter+))
                          (#\l (values :one-of +lowercase-letter+))
                          (#\L (values :none-of +lowercase-letter+))
                          (#\u (values :one-of +uppercase-letter+))
                          (#\U (values :none-of +uppercase-letter+))
                          (#\p (values :one-of +punctuation+))
                          (#\P (values :none-of +punctuation+))
                          (#\w (values :one-of +alpha-numeric+))
                          (#\W (values :none-of +alpha-numeric+))
                          (#\d (values :one-of +digit+))
                          (#\D (values :none-of +digit+))
                          (#\x (values :one-of +hex-digit+))
                          (#\X (values :none-of +hex-digit+))
                          (#\z (values :char #\null))
                          (otherwise
                           (values :char c)))))
                     (#\$ :eol)
                     (#\. :any)
                     (#\^ :none)
                     (#\( :capture)
                     (#\) :end-capture)
                     (#\[ :set)
                     (#\] :end-set)
                     (#\? :maybe)
                     (#\* :many)
                     (#\+ :many1)
                     (#\- :to)
                     (#\| :or)
                     (otherwise
                      (values :char c)))))))
        (let ((expr (re-parser #'next-token)))
          (when expr
            (make-instance 're 
                           :pattern pattern 
                           :case-fold case-fold
                           :multi-line multi-line
                           :expression expr)))))))

(defun match-re (re s &key (start 0) (end (length s)) exact)
  "Check to see if a regexp pattern matches a string."
  (flet ((capture (caps place)
           (cons (subseq s (car place) (cdr place)) caps)))
    (with-input-from-string (source s :start start :end end)
      (let ((st (make-instance 'lex-state 
                               :source source
                               :start start
                               :captures nil
                               :newline (or (zerop start)
                                            (char= (char s (1- start)) #\newline)))))
        (when (funcall (re-expression re) st)
          (let ((caps (reduce #'capture (lex-captures st) :initial-value nil))
                (end-pos (file-position source)))
            (when (or (not exact) (= end-pos end))
              (make-instance 're-match
                             :match (if exact s (subseq s start end-pos))
                             :captures caps
                             :start-pos start
                             :end-pos end-pos))))))))

(defun find-re (re s &key (start 0) (end (length s)) all)
  "Find a regexp pattern match somewhere in a string."
  (if (not all)
      (loop :for i :from start :below end :do
        (let ((match (match-re re s :start i :end end :exact nil)))
          (when match
            (return match))))
    (loop
     :with i := 0
     :for match := (find-re re s :start i :end end)
     :while match
     :collect (prog1
                  match
                (setf i (match-pos-end match))))))

(defun split-re (re s &key (start 0) (end (length s)) all coalesce-seps)
  "Split a string into one or more strings by regexp pattern match."
  (if (not all)
      (let ((match (find-re re s :start start :end end)))
        (if (null match)
            s
          (values (subseq s start (match-pos-start match))
                  (subseq s (match-pos-end match)))))
    (let* ((hd (list nil)) 
           (tl hd)
           (pos 0))
      (flet ((push-match (&optional m)
               (let ((s (subseq s pos (when m (match-pos-start m)))))
                 (unless (and coalesce-seps (zerop (length s)))
                   (setf tl (cdr (rplacd tl (list s)))))
                 (setf pos (when m (match-pos-end m))))))
        (loop :for sep :in (find-re re s :start start :end end :all t) :do
          (push-match sep))
        (push-match))
      (cdr hd))))

(defun replace-re (re with s &key (start 0) (end (length s)) all)
  "Split a string into one or more strings by regexp pattern match."
  (let ((matches (find-re re s :start start :end end :all all)))
    (unless all
      (setf matches (list matches)))
    (with-output-to-string (rep)
      (let ((pos 0))
        (loop :for match :in matches :do
          (let ((prefix (subseq s pos (match-pos-start match))))
            (format rep "~a~a" prefix (funcall with match))
            (setf pos (match-pos-end match))))
        (format rep (subseq s pos))))))

(defun next (st pred)
  "Read the next character, update the pos, test against predicate."
  (let ((c (read-char (lex-source st) nil nil)))
    (if (funcall pred c)
        c
      (when c
        (unread-char c (lex-source st))))))

(defun bind (&rest ps)
  "Bind parse combinators together to compose a new combinator."
  #'(lambda (st)
      (dolist (p ps t)
        (unless (funcall p st)
          (return nil)))))

(defun capture (p)
  "Push a capture of a combinator onto the lex state."
  #'(lambda (st)
      (with-slots (source captures)
          st
        (let ((capture (cons (file-position source) nil)))
          (push capture captures)
          (when (funcall p st)
            (rplacd capture (file-position source)))))))

(defun either (p1 p2)
  "Try one parse combinator, if it fails, try another."
  #'(lambda (st)
      (with-slots (source captures)
          st
        (let ((pos (file-position source))
              (caps captures))
          (or (funcall p1 st)
              (progn
                (setf captures caps)
                (file-position source pos)
                (funcall p2 st)))))))

(defun any-char (&key match-newline-p)
  "Expect a non-eoi character."
  #'(lambda (st)
      (next st #'(lambda (x)
                   (and x (if match-newline-p t (char/= x #\newline)))))))

(defun eol (&key (match-newline-p t))
  "End of file or line."
  #'(lambda (st)
      (let ((x (next st #'identity)))
        (if (and match-newline-p (char= x #\newline))
            t
          (null x)))))

(defun newline (&key (match-newline-p t))
  "Start of file or line."
  #'(lambda (st)
      (or (zerop (lex-start st)) (and match-newline-p (lex-newline st)))))

(defun ch (c &key case-fold)
  "Match an exact character."
  (let ((test (if case-fold #'unicode-char-equal #'char=)))
    #'(lambda (st)
        (next st #'(lambda (x)
                     (and x (funcall test x c)))))))

(defun one-of (cs &key case-fold)
  "Match any character from a set."
  (let ((test (if case-fold #'unicode-char-equal #'char=)))
    #'(lambda (st)
        (next st #'(lambda (x)
                     (and x (find x cs :test test)))))))

(defun none-of (cs &key case-fold)
  "Match any character not in a set."
  (let ((test (if case-fold #'unicode-char-equal #'char=)))
    #'(lambda (st)
        (next st #'(lambda (x)
                     (and x (not (find x cs :test test))))))))

(defun maybe (p)
  "Optionally match a parse combinator."
  #'(lambda (st)
      (or (funcall p st) t)))

(defun many (p)
  "Match a parse combinator zero or more times."
  #'(lambda (st)
      (do () ((null (funcall p st)) st))))

(defun many1 (p)
  "Match a parse combinator one or more times."
  (bind p (many p)))

(defun many-til (p term)
  "Match a parse combinator many times until a terminal."
  #'(lambda (st)
      (do ()
          ((funcall term st) t)
        (unless (funcall p st)
          (return nil)))))

(defun between (b1 b2 &key match-newline-p)
  "Match everything between two characters."
  (bind b1 (many-til (any-char :match-newline-p match-newline-p) b2)))


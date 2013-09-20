(defpackage :mass-lexer-asd
  (:use :cl :asdf))

(in-package :mass-lexer-asd)

(defsystem :mass-lexer
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "String tokenizing for LispWorks."
  :serial t
  :components ((:file "lexer"))
  :depends-on ("re"))

(in-package :cl-user)

(defpackage http-ink.common-util
  (:use :cl)
  (:export :make-keyword))

(in-package :http-ink.common-util)

(defun make-keyword (str)
  (values (intern (string-upcase str) "KEYWORD")))

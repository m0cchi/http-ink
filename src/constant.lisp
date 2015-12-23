(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :http-ink.constant)
    (defpackage http-ink.constant
      (:use :cl)
      (:export :+SERVER_NAME+ :+NAME+ :+VERSION+))))
(in-package :http-ink.constant)

(defvar +VERSION+ "0.0.2")
(defvar +NAME+ "http-ink")
(defvar +SERVER_NAME+ (format nil "~a ~a" +NAME+ +VERSION+))

(in-package :cl-user)
(defpackage http-ink
  (:use :cl)
  (:export ink))

(in-package :http-ink)

(defun ink (stream)
  (format t "start ink~%")
  (let ((ret ""))
    (loop for input = (read-char stream nil nil)
          while input do
          (progn
            (setq ret (format nil "~a~a" ret input))
            (format stream "~a" input)
            (format t "~A" input)
            (force-output stream))))
  (format t "end~%"))

(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :http-ink.response-util)
    (defpackage http-ink.response-util
      (:use :cl)
      (:import-from :http-ink.constant
                    :+SERVER_NAME+)
      (:import-from :local-time
                    :universal-to-timestamp
                    :format-rfc1123-timestring)
      (:import-from :trivial-mimes
                    :mime)
      (:export :response-with-file))))
(in-package :http-ink.response-util)

(defun connection (env)
  (let* ((header (getf env :header))
        (is-keep-alive (http-ink:is-keep-alive header)))
    (if is-keep-alive
        "keep-alive"
      "close")))

(defun read-file (path)
  (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
                  (let ((buf (make-array (file-length stream) :element-type '(unsigned-byte 8))))
                    (read-sequence buf stream)
                    buf)))

(defun response-with-file (env file-path)
  (let ((file (read-file file-path)))
    (list :header (list "HTTP/1.1" "200 OK"
                        :date (format-rfc1123-timestring nil 
                                                         (universal-to-timestamp (get-universal-time)))
                        :server +SERVER_NAME+
                        :connection (connection env)
                        :content-type (format nil "~A~:[~;~:*; charset=~A~]"
                                              (trivial-mimes:mime file-path) "utf-8")
                        :content-length (length file))
          :body file
          :body-is-vector T)))

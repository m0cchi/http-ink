(in-package :cl-user)
(defpackage http-ink.response-util
  (:use :cl)
  (:import-from :http-ink.constant
                :+SERVER_NAME+)
  (:import-from :local-time
                :universal-to-timestamp
                :format-rfc1123-timestring)
  (:import-from :trivial-mimes
                :mime)
  (:export :respond
           :respond-with-file
           :respond-with-html))
(in-package :http-ink.response-util)

(defun is-keep-alive (header)
  (let ((connection (getf header :connection)))
    (not (equalp connection "close"))))

(defun connection (env)
  (let* ((header (getf env :header))
        (is-keep-alive (is-keep-alive header)))
    (if is-keep-alive
        "keep-alive"
      "close")))

(defun read-file (path)
  (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
                  (let ((buf (make-array (file-length stream) :element-type '(unsigned-byte 8))))
                    (read-sequence buf stream)
                    buf)))

(defun respond (env status content-type body)
  (list :header (list "HTTP/1.1" status
                      :date (format-rfc1123-timestring nil 
                                                       (universal-to-timestamp (get-universal-time)))
                      :server +SERVER_NAME+
                      :connection (connection env)
                      :content-type content-type)
        :body body))

(defun respond-with-html (env text &key (charset "utf-8") (status "200 OK"))
  (respond env
           status
           (format nil "text/html ~a" charset)
           text))

(defun respond-with-file (env file-path &key (status "200 OK"))
  (let ((file (read-file file-path))
        (content-type (format nil "~A~:[~;~:*; charset=~A~]"
                              (mime file-path) "utf-8")))
    (respond env status content-type file)))

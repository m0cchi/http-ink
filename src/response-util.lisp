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
      (:import-from :cl-fad
                    :directory-exists-p
                    :list-directory)
      (:import-from :flexi-streams
                    :string-to-octets)
      (:import-from :http-ink
                    :defroutes)
      (:export :response
               :response-with-file
               :set-public-dir))))
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

(defun response (env status content-type body)
  (list :header (list "HTTP/1.1" status
                      :date (format-rfc1123-timestring nil 
                                                       (universal-to-timestamp (get-universal-time)))
                      :server +SERVER_NAME+
                      :connection (connection env)
                      :content-type content-type)
        :body body))

(defun response-with-file (env file-path)
  (let ((file (read-file file-path))
        (content-type (format nil "~A~:[~;~:*; charset=~A~]"
                              (trivial-mimes:mime file-path) "utf-8")))
    (response env "200 OK" content-type file)))

(defun string-size-in-octets (string)
  (length (string-to-octets string)))

(defun search-files (path)
  (let* ((all (list-directory path))
         (files (remove-if #'directory-exists-p all))
         (dirs (remove-if-not #'directory-exists-p all)))
    (loop for dir in dirs while dir do
          (setq files (append files (search-files dir))))
    files))

(defun file-path-to-uri-path (file-p base-len)
  (let ((path-string (namestring file-p)))
    (format nil "/~a" (subseq path-string base-len (string-size-in-octets path-string)))))

(defmacro set-public-dir (path)
  (let ((path-string-len (string-size-in-octets (namestring (cl-fad:directory-exists-p path))))
        (files (search-files path))
        (routes '()))
    (loop for file-p in files while file-p do
          (push `(:get ,(file-path-to-uri-path file-p path-string-len) ()
                       (response-with-file http-ink::env ,(namestring file-p))) routes))
    (append '(http-ink:defroutes) routes)))

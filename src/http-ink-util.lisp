(in-package :cl-user)

(defpackage http-ink.util
  (:use :cl)
  (:import-from :http-ink
                :defroutes)
  (:import-from :cl-fad
                :directory-exists-p
                :list-directory)
  (:import-from :flexi-streams
                :string-to-octets)
  (:import-from :http-ink
                :defroutes)
  (:import-from :http-ink.response-util
                :respond-with-file)
  (:export :set-public-dir
           :read-body))

(in-package :http-ink.util)

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
                       (respond-with-file http-ink::env ,(namestring file-p))) routes))
    (append '(defroutes) routes)))

(defun read-body (env)
  (let ((buf (make-array (parse-integer (getf (getf env :header) :content-length "0"))
                         :element-type '(unsigned-byte 8)))
        (stream (getf env :stream)))
    (read-sequence buf stream)
    buf))

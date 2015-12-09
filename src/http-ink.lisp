(in-package :cl-user)
(defpackage http-ink
  (:use :cl)
  (:use :babel)
  (:use :split-sequence)
  (:use :local-time)
  (:export :ink :defroutes))

(in-package :http-ink)

(defvar *routes* '())

(defvar +404+  (list :header (list "HTTP/1.1" "404 NotFound"
                                   :date (local-time:format-rfc1123-timestring nil 
                                                                               (local-time:universal-to-timestamp (get-universal-time)))
                                   :server "http-ink"
                                   :connection "close"
                                   :content-type "text/html; charset=utf-8")
                     :body "<html><head><title>http-ink</title></head><body>404</body></html>"))

(defmacro defroutes (&rest routes)
  (loop for route in routes while route do
        (let ((method-type (nth 0 route))
              (path (nth 1 route))
              (params (nth 2 route))
              (method (nth 3 route)))
          (push 'env params)
          (push `(:method-type ,method-type
                  :path ,path
                  :params ,(mapcar #'make-keyword params)
                  :method ,(coerce `(lambda ,params (declare (ignorable http-ink::env)) ,method) 'function))
                *routes*))))

(defun make-keyword (str)
  (values (intern (string-upcase str) "KEYWORD")))

(defun is-header (line)
  (if (= (length line) 0)
      '()
    (let ((line-head (elt line 0)))
      (not (or (eql line-head #\return)
               (eql line-head #\newline))))))

(defun read-header (stream)
  (let ((header '()))
    (loop for line = (read-line stream nil nil)
          while (is-header line) do
          (progn
            (push (string-trim '(#\return #\newline) line) header)))
    (reverse header)))

(defun parse-method-path-version (method-path-version)
  (let ((header '()))
    (if (< (length method-path-version) 3)
        (error "version"))
    (push :method-type header)
    (push (make-keyword (nth 0 method-path-version)) header)
    (push :path header)
    (push (nth 1 method-path-version) header)
    header))

(defun parse-header-field (field)
  (let* ((index (position #\: field)))
    (if index
        (let ((key (make-keyword (subseq field 0 index)))
              (value (string-trim '(#\space)
                                  (subseq field (1+ index) (length field)))))
          `(,key ,value))
      (error (format nil "invalid header field: ~a~%" field)))))

(defun parse-header (lines)
  (let* ((first-line (split-sequence:split-sequence #\space (pop lines)))
         (header (parse-method-path-version first-line)))
    (loop for field in lines do
          (loop for korv in (parse-header-field field) do
                (push korv header)))
    (reverse header)))

(defun read-body (stream content-length)
  (let ((buf (make-string content-length)))
    (read-sequence buf stream)
    buf))

(defun write-response (stream response)
  (let ((header (reverse (getf response :header)))
        (body (getf response :body)))
    (push :content-length header)
    (push (babel:string-size-in-octets body) header)
    (setq header (reverse header))
    (format stream "~a ~a~%" (pop header) (pop header))
    (format stream "~{~a: ~a~%~}~%" header)
    (format stream "~a" body))
  (force-output stream))

(defun ink (stream)
  (let* ((header (parse-header
                  (read-header stream)))
         (body "")
         (args '())
         (request-path (getf header :path))
         (request-type (getf header :method-type))
         (response-proc (car (remove-if-not #'(lambda (route)
                                                (and (equal (getf route :path)
                                                            request-path)
                                                     (equal (getf route :method-type)
                                                            request-type)))
                                            *routes*))))
      (push stream args)
      (push :stream args)
      (push header args)
      (push :header args)
      (write-response stream (if response-proc
                                 (funcall (getf response-proc :method) args)
                               +404+))))

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :http-ink)
    (defpackage http-ink
      (:use :cl)
      (:use :local-time)
      (:import-from :http-ink.parse-http
                    :parse-header
                    :parse-uri)
      (:import-from :http-ink.common-util
                    :make-keyword)
      (:export :ink :is-keep-alive :defroutes :defroute :octets-to-string))))

(in-package :http-ink)

(defvar *routes* '())

(defvar +404+ `(:method 
                ,(lambda (env)
                   (list :header (list "HTTP/1.1" "404 NotFound"
                                       :date (local-time:format-rfc1123-timestring nil 
                                                                                   (local-time:universal-to-timestamp (get-universal-time)))
                                       :server "http-ink"
                                       :connection "close"
                                       :content-type "text/html; charset=utf-8")
                         :body "<html><head><title>http-ink</title></head><body>404</body></html>"))))

(defvar +NEWLINE+ 10)
(defvar +HEADER_RESULT_FORMAT+ (format nil "~a~c~c" "~a ~a" #\return #\newline))
(defvar +HEADER_LINE_FORMAT+ (format nil "~a~c~c~a~c~c" "~{~a: ~a" #\return #\newline "~}" #\return #\newline))

(defun collect (temp buffer)
  (if (eq temp +NEWLINE+)
      (progn
        (vector-push-extend temp buffer)
        '())
    (if (or (eq temp nil)
            (eq temp 255))
        '()
      (progn
        (vector-push-extend temp buffer)
        T))))

(defun read-line-with-buffer (stream buffer)
  (setf (fill-pointer buffer) 0)
  (loop for temp = (read-byte stream nil nil)
        while (collect temp buffer))
  buffer)

(defun defroute- (route)
  (let ((method-type (nth 0 route))
        (path (nth 1 route))
        (params (nth 2 route))
        (method (nth 3 route)))
    (push 'env params)
    (push `(:method-type ,method-type
            :path ,path
            :params ,(mapcar #'make-keyword params)
            :method ,(coerce `(lambda ,params (declare (ignorable http-ink::env)) ,method) 'function))
          *routes*)))

(defmacro defroute (route)
  (defroute- route)
  nil)

(defmacro defroutes (&rest routes)
  (loop for route in routes while route
        do (defroute- route)))

(defun is-header (line)
  (if (= (length line) 0)
      '()
    (let ((line-head (elt line 0)))
      (not (or (eql line-head #\return)
               (eql line-head #\newline))))))

(defun octets-to-string (octets)
  (unless (eq (length octets) 0)
    (flexi-streams:octets-to-string octets :external-format :utf-8)))

(defun read-header (stream)
  (let* ((header '())
         (buffer (make-array 60 :fill-pointer 0)))
    (loop for line = (http-ink:octets-to-string (read-line-with-buffer stream buffer))
          while
          (progn 
            (push line header)
            (is-header line)))
    (if (car header)
        (format nil "~{~a~}" (reverse header)))))

(defun write-string-with-octets (string stream)
  (write-sequence (flexi-streams:string-to-octets string) stream))

(defun string-size-in-octets (string)
  (length (flexi-streams:string-to-octets string)))

(defun write-response (stream response)
  (let* ((header (getf response :header))
         (reverse-header (reverse header))
         (body (getf response :body))
         (write-body-func (if (stringp body)
                              #'write-string-with-octets
                            #'write-sequence))
         (content-length (if (find :content-length header)
                             (getf header :content-length)
                           (if (stringp body)
                               (string-size-in-octets body)
                             (length body)))))
    (push :content-length reverse-header)
    (push content-length reverse-header)
    (setq header (reverse reverse-header))
    (write-string-with-octets (format nil +HEADER_RESULT_FORMAT+ (pop header) (pop header)) stream)
    (write-string-with-octets (format nil +HEADER_LINE_FORMAT+ header) stream)
    (funcall write-body-func body stream))
  (force-output stream))

(defun is-keep-alive (header)
  (let ((connection (getf header :connection)))
    (not (equalp connection "close"))))

(defun ink (stream)
  (loop for header-string = (read-header stream)
        while (not (eq header-string nil)) do
        (let* ((args '())
               (header (parse-header header-string))
               (uri (parse-uri (getf header :uri)))
               (request-path (getf uri :path))
               (request-type (getf header :method-type))
               (response-proc (car (remove-if-not #'(lambda (route)
                                                      (and (equal (getf route :path)
                                                                 request-path)
                                                           (equal (getf route :method-type)
                                                                  request-type)))
                                                  *routes*))))
          (push request-path args)
          (push :path args)
          (push (getf uri :params) args)
          (push :params args)
          (push stream args)
          (push :stream args)
          (push header args)
          (push :header args)
          (unless response-proc
            (setq response-proc +404+))
          (write-response stream (funcall (getf response-proc :method) args))
          (if  (is-keep-alive header)
              (file-position stream (+ (string-size-in-octets header-string)
                                       (parse-integer (getf header :content-length "0"))))
            (loop-finish)))))

(defun read-body (stream header)
  (let ((buf (make-array (parse-integer (getf (getf env :header '()) :content-length "0"))
                         :element-type '(unsigned-byte 8)))
        (stream (getf env :stream)))
    (read-sequence buf stream)
    buf))

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :http-ink)
    (defpackage http-ink
      (:use :cl)
      (:import-from :local-time
                    :universal-to-timestamp
                    :format-rfc1123-timestring)
      (:import-from :http-ink.parse-http
                    :parse-header
                    :parse-uri)
      (:import-from :http-ink.response-util
                    :respond-with-html)
      (:import-from :http-ink.common-util
                    :make-keyword)
      (:export :ink :defroutes :defroute :defroute- :octets-to-string :search-route :dispatch :*log* :*expire-time* :*routes* :register-route))))

(in-package :http-ink)

(defvar *routes* '())
(defvar *expire-time* 0)
(defvar *log* t)
(defvar +404+ `(:method 
                ,(lambda (env)
                   (setf (getf env :connection) "close")
                   (respond-with-html env
                                      "<html><head><title>http-ink</title></head><body>404</body></html>"
                                      :status "404 NotFound"))))
(defvar +500+ `(:method 
                ,(lambda (env)
                   (setf (getf env :connection) "close")
                   (respond-with-html env
                                      "<html><head><title>http-ink</title></head><body>500 Internal Server Error</body></html>"
                                      :status "500 Internal Server Error"))))
(defvar +NEWLINE+ 10)
(defvar +HEADER_RESULT_FORMAT+ (format nil "~a~c~c" "~a ~a" #\return #\newline))
(defvar +HEADER_LINE_FORMAT+ (format nil "~a~c~c~a~c~c" "~{~a: ~a" #\return #\newline "~}" #\return #\newline))

(defun is-keep-alive (header)
  (let ((connection (getf header :connection)))
    (not (equalp connection "close"))))

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

(defun search-route (method-type path)
  (let ((route (remove-if-not #'(lambda (route)
                                  (and (equal (getf route :path)
                                              path)
                                       (equal (getf route :method-type)
                                              method-type)))
                              *routes*)))
    (or (car route) +404+)))

(defmacro dispatch (method-type path)
  `(funcall (getf (http-ink:search-route ,method-type ,path) http-ink::env)))

(defun register-route (route)
  (let* ((method (getf route :method))
         (func (coerce method 'function)))
    (setf (getf route :method) func)
    (push route *routes*)))

(defun defroute- (route)
  (let* ((method-type (nth 0 route))
         (path (nth 1 route))
         (params (nth 2 route))
         (method (nth 3 route)))
    (push 'env params)
    `(:method-type ,method-type
      :path ,path
      :params ,(mapcar #'make-keyword params)
      :method (lambda ,params
                         (declare (ignorable http-ink::env)) ,method))))


(defmacro defroute (route)
  `(register-route (quote ,(defroute- route))))

(defmacro defroutes (&rest routes)
  (let ((route- '()))
    (loop for route in routes while route
          do (push `(register-route (quote ,(defroute- route))) route-))
    (append '(progn) route-)))

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
    (push :cache-control reverse-header)
    (push (format nil "max-age=~a" *expire-time*) reverse-header)
    (push :expires reverse-header)
    (push (local-time:format-rfc1123-timestring
           nil
           (local-time:universal-to-timestamp (+ *expire-time* (get-universal-time))))
          reverse-header)
    (setq header (reverse reverse-header))
    (write-string-with-octets (format nil +HEADER_RESULT_FORMAT+ (pop header) (pop header)) stream)
    (write-string-with-octets (format nil +HEADER_LINE_FORMAT+ header) stream)
    (funcall write-body-func body stream))
  (force-output stream))

(defun ink (stream)
  (loop for header-string = (read-header stream)
        while (not (eq header-string nil)) do
        (let* ((args '())
               (header (parse-header header-string))
               (uri (parse-uri (getf header :uri)))
               (request-path (getf uri :path))
               (request-type (getf header :method-type))
               (response-proc (search-route request-type request-path)))
          (format *log* "~{~a:~a~%~}~%" header)
          (push request-path args)
          (push :path args)
          (push (getf uri :params) args)
          (push :params args)
          (push stream args)
          (push :stream args)
          (push header args)
          (push :header args)
          (write-response stream
                          (handler-case
                           (funcall (getf response-proc :method) args)
                           (error (c) (funcall (getf +500+ :method) args))))
          (if (is-keep-alive header)
              (file-position stream (+ (string-size-in-octets header-string)
                                       (parse-integer (getf header :content-length "0"))))
            (loop-finish)))))

(defun read-body (stream header)
  (let ((buf (make-array (parse-integer (getf (getf env :header '()) :content-length "0"))
                         :element-type '(unsigned-byte 8)))
        (stream (getf env :stream)))
    (read-sequence buf stream)
    buf))

(in-package :cl-user)
(defpackage http-ink
  (:use :cl)
  (:use :split-sequence)
  (:use :local-time)
  (:export :ink :is-keep-alive :defroutes :defroute :octets-to-string))

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

(defun make-keyword (str)
  (values (intern (string-upcase str) "KEYWORD")))

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

(defun parse-method-path-version (method-path-version)
  (let ((header '()))
    (if (< (length method-path-version) 3)
        (error "invalid version"))
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

(defun shap-header-string (header-string)
  (remove-if #'(lambda (x) (eq (length x) 0))
             (mapcar #'(lambda (x) (string-trim '(#\return) x))
                     (split-sequence:split-sequence #\newline header-string))))

(defun parse-header (header-string)
  (let* ((lines (shap-header-string header-string))
         (first-line (split-sequence:split-sequence #\space (pop lines)))
         (header (parse-method-path-version first-line)))
    (loop for field in lines do
          (loop for korv in (parse-header-field field) do
                (push korv header)))
    (reverse header)))

(defun read-body (stream header)
  (let ((buf (make-array (parse-integer (getf (getf env :header '()) :content-length "0"))
                         :element-type '(unsigned-byte 8)))
        (stream (getf env :stream)))
    (read-sequence buf stream)
    buf))

(defun write-string-with-octets (string stream)
  (write-sequence (flexi-streams:string-to-octets string) stream))

(defun string-size-in-octets (string)
  (length (flexi-streams:string-to-octets string)))

(defun write-response (stream response)
  (let* ((header (getf response :header))
         (reverse-header (reverse header))
         (body (getf response :body))
         (write-func (if (typep body 'simple-vector)
                         #'write-sequence
                       #'write-string-with-octets))
         (content-length (if (find :content-length header)
                             (getf header :content-length)
                            (string-size-in-octets body))))
    (push :content-length reverse-header)
    (push content-length reverse-header)
    (setq header (reverse reverse-header))
    (write-string-with-octets (format nil +HEADER_RESULT_FORMAT+ (pop header) (pop header)) stream)
    (write-string-with-octets (format nil +HEADER_LINE_FORMAT+ header) stream)
    (if (getf response :body-is-vector)
        (write-sequence body stream)
      (write-string-with-octets body stream)))
  (force-output stream))

(defun is-keep-alive (header)
  (let ((connection (getf header :connection)))
    (not (equalp connection "close"))))

(defun ink (stream)
  (loop for header-string = (read-header stream)
        while (not (eq header-string nil)) do
        (let* ((args '())
               (header (parse-header header-string))
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
          (unless response-proc
            (setq response-proc +404+))
          (write-response stream (funcall (getf response-proc :method) args))
          (if  (is-keep-alive header)
              (file-position stream (+ (string-size-in-octets header-string)
                                       (parse-integer (getf header :content-length "0"))))
            (loop-finish)))))

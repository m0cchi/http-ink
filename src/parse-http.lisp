(in-package :cl-user)

(defpackage http-ink.parse-http
  (:use :cl)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :http-ink.common-util
                :make-keyword)
  (:export :parse-header
           :parse-uri))

(in-package :http-ink.parse-http)

(defvar +HEADER_RESULT_FORMAT+ (format nil "~a~c~c" "~a ~a" #\return #\newline))
(defvar +HEADER_LINE_FORMAT+ (format nil "~a~c~c~a~c~c" "~{~a: ~a" #\return #\newline "~}" #\return #\newline))

(defun pair-to-key-value(pair)
  (if (>= (length pair) 2)
      `(,(make-keyword (car pair)) ,(cadr pair))
    pair))

(defun parse-parameter (query-string)
  (let ((parameter '()))
    (loop for part in (split-sequence #\& query-string)
          for pair = (split-sequence #\= part)
          while pair do
          (setq parameter (append parameter (pair-to-key-value pair))))
    parameter))

(defun parse-method-path-version (method-path-version)
  (let ((header '()))
    (if (< (length method-path-version) 3)
        (error "invalid version"))
    (push :method-type header)
    (push (make-keyword (nth 0 method-path-version)) header)
    (push :uri header)
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
                     (split-sequence #\newline header-string))))

(defun parse-header (header-string)
  (let* ((lines (shap-header-string header-string))
         (first-line (split-sequence #\space (pop lines)))
         (header (parse-method-path-version first-line)))
    (loop for field in lines do
          (loop for korv in (parse-header-field field) do
                (push korv header)))
    (reverse header)))

(defun parse-uri (uri)
  (let* ((len (length uri))
         (pos (position #\? uri))
         (path (subseq uri 0 (or pos len)))
         (query-string (if (and pos (< pos len)) (subseq uri (+ pos 1) len)))
         (params (if query-string (parse-parameter query-string))))
    `(:path ,path :params ,params)))

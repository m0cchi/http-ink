#|
URL: https://github.com/mocchit/http-ink
Author: mocchit
|#

(in-package :cl-user)
(defpackage http-ink-asd
  (:use :cl :asdf))
(in-package :http-ink-asd)

(defsystem http-ink
  :version "0.0.2"
  :author "mocchi"
  :license "BSD License"
  :depends-on (:flexi-streams
               :cl-fad
               :local-time
               :trivial-mimes
               :split-sequence)
  :components ((:module "src"
                        :components
                ((:file "response-util" :depends-on ("constant"))
                 (:file "http-ink" :depends-on ("parse-http" "common-util" "response-util"))
                 (:file "http-ink-util" :depends-on ("http-ink"))
                 (:file "parse-http" :depends-on("common-util"))
                 (:file "constant")
                 (:file "common-util"))))
  :description "http component")

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
  :depends-on (:toy-gun
               :flexi-streams
               :local-time
               :trivial-mimes
               :split-sequence)
  :components ((:module "src"
                        :components
                ((:file "response-util" :depends-on ("http-ink" "constant"))
                 (:file "constant")
                 (:file "http-ink"))))
  :description "http component")

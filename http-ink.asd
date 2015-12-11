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
               :split-sequence)
  :components ((:module "src"
                        :components
                ((:file "http-ink"))))
  :description "http component")

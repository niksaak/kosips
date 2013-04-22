(defpackage kosips-asd
  (:use :cl :asdf))

(in-package kosips-asd)

(defsystem kosips
  :name "KOSIPS"
  :version "0.0.905"
  :author "Mikola A Samardak"
  :license "WTFPL"
  :description "KOSIPS language compiler for DCPU-16, ver 0.1+0.9i"
  :defsystem-depends-on (optima cl-ppcre)
  :components
  ((:file "asm" :depends-on ("utils" "packages"))
   (:file "asm2" :depends-on ("utils" "packages"))
   (:file "asm3" :depends-on ("utils" "packages"))
   (:file "utils" :depends-on ("packages"))
   (:file "packages")))

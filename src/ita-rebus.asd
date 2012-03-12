;;; -*- mode: lisp -*-

(asdf:defsystem :ita-rebus
  :author ("Ritchie Cai")
  :maintainer "Ritchie Cai"
  :description "A rebus generator using a given list of images."
  :long-description "rebus generator: http://itasoftware.com/careers/work-at-ita/hiring-puzzles.html"
  :depends-on (:cl-ppcre)
  :components
  ((:file "package")
   (:file "utils"  :depends-on ("package"))
   (:file "tables" :depends-on ("package"))
   (:file "rebus-index"  :depends-on ("package" "tables" "utils"))
   (:file "rebus-search" :depends-on ("package" "tables" "utils"))))

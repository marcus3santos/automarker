;;;; automarker.asd

(asdf:defsystem #:automarker
  :description "AUTOMARKER is an automated marker for lisp program assignments where the students' assignment solutions have been exported from D2L."
  :author "Marcus Santos <m3santos@ryerson.ca>, Nabil Mansour"
  :license  "Specify license here"
  :version "0.1"
  :serial t
  :depends-on (#:zip #:rutils)
  :components ((:file "package")
               (:file "automarker")))

(asdf:defsystem #:its

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Provides convenient access to multiple values of an object in a concise, explicit and efficient way."

  :depends-on ("definitions-systems")

  :version "1.0"
  :serial cl:t
  :components ((:file "package")
               (:file "defsys")
               (:file "main")
               (:file "definitions"))

  :in-order-to ((asdf:test-op (asdf:test-op #:its_tests))))

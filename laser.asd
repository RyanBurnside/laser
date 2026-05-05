;;;; laser.asd

(asdf:defsystem #:laser
  :description "Describe laser here"
  :author "Ryan Burnside"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:nodgui #:libserialport)
  :components ((:file "package")
               (:file "laser")))

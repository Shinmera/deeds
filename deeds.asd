#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem deeds
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Deeds Extensible Event Delivery System"
  :homepage "https://github.com/Shinmera/deeds"
  :serial T
  :components ((:file "package")
               (:file "class-slots")
               (:file "toolkit")
               (:file "origin")
               (:file "forward-class-definitions")
               (:file "event")
               (:file "event-delivery")
               (:file "event-loop")
               (:file "handler")
               (:file "command")
               (:file "documentation")
               #-deeds-no-startup (:file "startup"))
  :depends-on (:closer-mop
               :simple-tasks
               :bordeaux-threads
               :lambda-fiddle
               :form-fiddle))


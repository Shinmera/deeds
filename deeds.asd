#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem deeds
  :version "3.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Deeds Extensible Event Delivery System"
  :homepage "https://github.com/Shinmera/deeds"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "event")
               (:file "handler")
               (:file "documentation"))
  :depends-on (:closer-mop
               :simple-tasks
               :bordeaux-threads))

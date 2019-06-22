#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem deeds
  :version "1.1.1"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Deeds Extensible Event Delivery System"
  :homepage "https://Shinmera.github.io/deeds/"
  :bug-tracker "https://github.com/Shinmera/deeds/issues"
  :source-control (:git "https://github.com/Shinmera/deeds.git")
  :serial T
  :components ((:file "package")
               (:file "class-slots")
               (:file "toolkit")
               (:file "origin")
               (:file "forward-class-definitions")
               (:file "event")
               (:file "standard-events")
               (:file "event-delivery")
               (:file "event-loop")
               (:file "handler")
               (:file "command")
               (:file "documentation"))
  :depends-on (:closer-mop
               :bordeaux-threads
               :lambda-fiddle
               :form-fiddle))


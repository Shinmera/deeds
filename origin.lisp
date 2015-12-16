#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

(defvar *origin* NIL)

(defmacro with-origin ((&optional (new-origin '(here))) &body body)
  `(let ((*origin* ,new-origin))
     ,@body))

(defmacro here ()
  (or *origin* *compile-file-pathname* *load-pathname* *package*))

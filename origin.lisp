(in-package #:org.shirakumo.deeds)

(defvar *origin* NIL)

(defmacro with-origin ((&optional (new-origin '(here))) &body body)
  `(let ((*origin* ,new-origin))
     ,@body))

(defmacro here ()
  (or *compile-file-pathname* *load-pathname* *package*))

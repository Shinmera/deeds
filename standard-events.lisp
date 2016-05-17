#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

(define-event blocking-event ()
  ((done :initform NIL :accessor done :mutable T)))

(define-event message-event ()
  ((message :initarg :message :reader message))
  (:default-initargs
   :message (error "MESSAGE required.")))

(define-event info-event (message-event)
  ())

(define-event warning-event (message-event)
  ())

(define-event error-event (message-event)
  ())

(define-event payload-event ()
  ((payload :initarg :payload :reader payload))
  (:default-initargs
   :payload (error "PAYLOAD required.")))

(define-event sequence-event ()
  ((index :initarg :index :reader index)
   (max-index :initarg :max-index :reader max-index))
  (:default-initargs
   :index (error "INDEX required.")
   :max-index NIL))

(define-event chunked-payload-event (payload-event sequence-event)
  ())

(define-event identified-event ()
  ((identifier :initarg :identifier :reader identifier)))

(defmethod initialize-instance :after ((identified-event identified-event) &key)
  (unless (slot-boundp identified-event 'identifier)
    (setf (slot-value identified-event 'identifier)
          identified-event)))

(define-event stream-event (identified-event)
  ())

(define-event stream-begin-event (stream-event)
  ())

(define-event stream-payload-event (stream-event payload-event)
  ()
  (:default-initargs
   :identifier (error "IDENTIFIER required.")))

(define-event stream-end-event (stream-event)
  ()
  (:default-initargs
   :identifier (error "IDENTIFIER required.")))

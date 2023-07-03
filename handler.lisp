(in-package #:org.shirakumo.deeds)

(defclass handler (event-delivery)
  ((name :initarg :name :reader name)
   (event-type :initarg :event-type :reader event-type)
   (filter :initarg :filter :reader filter)
   (before :initarg :before :reader before)
   (after :initarg :after :reader after)
   (handle-cancelled :initarg :handle-cancelled :reader handle-cancelled)
   (loops :initform NIL :accessor loops))
  (:default-initargs
   :name NIL
   :event-type 'event
   :filter NIL
   :before ()
   :after ()
   :handle-cancelled NIL))

(defmethod print-object ((handler handler) stream)
  (print-unreadable-object (handler stream :type T :identity T)
    (format stream "~s ~s" :name (name handler))))

(defmethod handle :around ((event event) (handler handler))
  (when (or (not (cancelled event))
            (handle-cancelled handler))
    (call-next-method)))

(defclass parallel-handler (handler)
  ((threads :initform () :accessor threads)
   (handler-lock :initform (bt:make-recursive-lock "parallel-handler lock") :accessor handler-lock)))

(defmethod issue ((event event) (parallel-handler parallel-handler))
  (bt:with-lock-held ((handler-lock parallel-handler))
    (let (thread)
      (setf thread (make-thread (lambda ()
                                  (unwind-protect
                                       (with-simple-restart (abort "Stop the handler thread.")
                                         (handle event parallel-handler))
                                    (bt:with-lock-held ((handler-lock parallel-handler))
                                      (setf (threads parallel-handler)
                                            (remove thread (threads parallel-handler))))))
                                (format NIL "~a thread" parallel-handler)))
      (push thread (threads parallel-handler)))))

(defmethod issue ((blocking-event blocking-event) (parallel-handler parallel-handler))
  (handle blocking-event parallel-handler))

(defmethod stop ((parallel-handler parallel-handler))
  (loop for thread = (bt:with-lock-held ((handler-lock parallel-handler))
                       (pop (threads parallel-handler)))
        while thread
        do (bt:interrupt-thread thread (lambda () (abort)))
           (loop repeat 100
                 do (unless (bt:thread-alive-p thread) (return))
                    (sleep 0.01)
                 finally (warn 'handler-thread-stop-failed-warning
                               :thread thread :handler parallel-handler))))

(defclass queued-handler (handler queued-event-delivery)
  ())

(defmethod print-object ((handler queued-handler) stream)
  (print-unreadable-object (handler stream :type T :identity T)
    (format stream "~s ~s~@[ RUNNING~]" :name (name handler) (running handler))))

(defclass locally-blocking-handler (handler)
  ())

(defclass globally-blocking-handler (handler)
  ((handler-lock :initform (bt:make-recursive-lock "globally-blocking-handler lock") :accessor handler-lock)))

(defmethod handle ((event event) (globally-blocking-handler globally-blocking-handler))
  (bt:with-recursive-lock-held ((handler-lock globally-blocking-handler))
    (call-next-method)))

(defun make-handler (&rest options &key (loop *standard-event-loop*) (class 'queued-handler) &allow-other-keys)
  (let* ((options (removef options :loop :class))
         (instance (apply #'make-instance class options)))
    (start instance)
    (if loop
        (let ((old (nth-value 1 (register-handler instance loop))))
          (when old (stop old))
          (values instance old))
        instance)))

(defmacro with-handler (event-type args &body options-and-body)
  (destructuring-bind (ev &rest args) args
    (multiple-value-bind (options body) (form-fiddle:split-body-options options-and-body)
      `(make-handler
        ,@options
        :event-type ',event-type
        ,@(when body
            `(:delivery-function
              (lambda (,ev)
                (declare (ignorable ,ev))
                (with-origin (,(getf options :name))
                  (with-fuzzy-slot-bindings ,args (,ev ,event-type)
                    ,@body)))))))))

(defmacro define-handler ((name event-type) args &body options-and-body)
  (form-fiddle:with-body-options (body options (self (gensym "SELF"))) options-and-body
    (let ((new (gensym "NEW")) (old (gensym "OLD")))
      ;; Race condition on self set.
      `(let (,self)
         (multiple-value-bind (,new ,old)
             (with-handler ,event-type ,args
               ,@options
               :name ',name
               ,@body)
           (setf ,self ,new)
           (values ,new ,old))))))

(defclass one-time-handler (handler)
  ())

(defmethod handle :around ((event event) (handler one-time-handler))
  (when (call-next-method)
    (unwind-protect
         (dolist (loop (loops handler))
           (deregister-handler handler loop))
      (stop handler))))

(defmacro with-one-time-handler (event-type args &body options-and-body)
  (multiple-value-bind (options body) (form-fiddle:split-body-options options-and-body)
    `(with-handler ,event-type ,args
       ,@options
       :class 'one-time-handler
       ,@body)))

(defclass condition-notify-handler (one-time-handler)
  ((condition-variable :initform (bt:make-condition-variable) :accessor condition-variable)
   (issue-synchronizer-lock :initform (bt:make-lock) :accessor issue-synchronizer-lock)
   (response-event :initform NIL :accessor response-event)))

(defmethod handle ((event event) (handler condition-notify-handler))
  (setf (response-event handler) event)
  ;; Quickly access lock to make sure the issuer has
  ;; entered the condition-wait.
  (bt:with-lock-held ((issue-synchronizer-lock handler)))
  (bt:condition-notify (condition-variable handler))
  ;; Satisfy the one-time-handler return.
  T)

(defmacro with-awaiting (response (&key filter timeout (loop '*standard-event-loop*)) setup-form &body body)
  (let ((handler (gensym "HANDLER"))
        (loop-g (gensym "LOOP")))
    (destructuring-bind (response-event &optional (event 'ev) &rest response-args) (ensure-list response)
      `(let ((,handler (make-instance
                        'condition-notify-handler
                        :event-type ',response-event
                        :filter ,filter))
             (,loop-g ,loop))
         (start ,handler)
         (unwind-protect
              (bt:with-lock-held ((issue-synchronizer-lock ,handler))
                (register-handler ,handler ,loop-g)
                ,setup-form
                (when (loop for condition = (bt:condition-wait (condition-variable ,handler)
                                                               (issue-synchronizer-lock ,handler)
                                                               :timeout ,timeout)
                            for event = (response-event ,handler)
                            do (cond ((not condition) (return NIL))
                                     ((and condition event) (return T)))
                               ;; Just to make sure.
                               (sleep 0.001))
                  (let ((,event (response-event ,handler)))
                    (declare (ignorable ,event))
                    (with-fuzzy-slot-bindings ,response-args (,event ,response-event)
                      ,@body))))
           (deregister-handler ,handler ,loop-g)
           (stop ,handler))))))

(defmacro with-response (issue response (&key filter timeout (issue-loop '*standard-event-loop*) (response-loop issue-loop)) &body body)
  `(with-awaiting ,response (:filter ,filter :timeout ,timeout :loop ,response-loop)
                  ,(typecase issue
                     (null)
                     (list `(do-issue ,@issue :loop ,issue-loop))
                     (T `(issue ,issue ,issue-loop)))
     ,@body))

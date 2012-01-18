(defmethod prepare-processor ((p processor))
  (maphash #'(lambda (name items)
               (declare (ignore name))
               (loop for i in items do
                     (loop for j from 1 to (resource-constraint-count i) do
                           (let ((queue (make-instance 'resource-queue)))
                             (push queue (resource-constraint-queues i))
                             (push queue
                                   (gethash (get-activity-by-typename (constraint-type i)) (processor-queues p)))
                             )))
               )
           (processor-constraints p))
)

(let ((the-processor (gensym)))

(defmacro defprocessor (name &body body)
  `(let (,the-processor)
     (declare (special ,name))
     (bootstrap)
     (defvar ,name nil)
     (setf ,the-processor
           (let ((the-processor (make-instance (quote processor))))
             ,@body
             (prepare-processor the-processor)
             the-processor))
     (setf ,name ,the-processor))
  )

(defun get-processor () `,the-processor)

)

(defmacro defconstraint (name &rest args &key constraint-type (type 'resource-constraint) &allow-other-keys)
  (let ((args (remove-pair args :constraint-type)))
    `(push (make-instance (quote ,type) :name ,name ,@args :type ,constraint-type) (gethash ,name (processor-constraints the-processor)))
    )
  )

;(defmacro defenvironment (&body body)
;  (declare (ignore body))
;)

;(defclass task-condition ()
;  ((name :initform nil :initarg :name :accessor condition-name)))

;(defmacro defcondition (condition-name &body tasks)
;  (declare (ignore condition-name tasks))
;)

;(defmacro deftask (name environment &body rules)
;  (declare (ignore name environment rules))
;)

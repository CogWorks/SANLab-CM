(defparameter *super-debug* nil)

(defmethod (setf resource-queue-tree) :before
  ((val resource) (queue resource-queue))
  (break))

(defmethod (setf resource-queue-tree) :before
  ((val number) (queue resource-queue))
  (break))

(defmethod (setf resource-dependents) :before
  ((val list) (res resource))
  (if (find-if #'(lambda (x)
		   (path-between-nodes? x res #'resource-dependents))
               val) (break)))

(defmethod (setf resource-predecessors) :before
  ((val list) (res resource))
  (if (find-if #'(lambda (x)
		   (path-between-nodes? x res #'resource-predecessors))
               val) (break)))

(defmethod reset-queue ((queue resource-queue))
  (setf (resource-queue-tree queue) nil))

(defun valid-time? (x) (and x (/= x +1D++0) (<= 0 x)))

(defmethod best-start-time ((a resource))
  (or (resource-start-time a)
      (number-not-inf (resource-earliest-start-time a))
      (number-not-inf (resource-latest-start-time a))))

(defmethod best-end-time ((a resource))
  (or (resource-end-time a)
      (number-not-inf (resource-latest-end-time a))
      (number-not-inf (resource-earliest-end-time a))))

(defmethod resource= ((a resource) (b resource))
  (and (equal (best-start-time a) (best-start-time b))
       (equal (best-end-time a) (best-end-time b))
       (equal (resource-type a) (resource-type b))))

(defmethod resource< ((a resource) (b resource))
  (< (best-end-time a) (best-end-time b)))

(defmethod print-object ((object resource) (stream stream))
  (format stream "UID ~A Operator ~S Children ~A Started ~A Ended ~A"
          (resource-id object)
          (resource-type object)
          (length (resource-dependents object))
          (or (resource-start-time object)
              (number-not-inf (resource-earliest-start-time object))
              (number-not-inf (resource-latest-start-time object))
              )
          (or (resource-end-time object)
              (number-not-inf (resource-latest-end-time object))
              (number-not-inf (resource-earliest-end-time object))
              )))

(defmethod last-scheduled-resource ((queue resource-queue))
  (first (last (resource-queue-tree queue))))

(defparameter *insert-items* nil)

(defmethod insert ((item resource) (queue list) (rest list))
  (if (null queue)
      (append (list item) rest)
    (if (resource< (first (last queue)) item)
        (append queue (list item) rest)
      (if (= 1 (length queue))
          (append (list item) queue rest)
        (let ((i (first (last queue))))
          (setf (cdr (last queue 2)) nil)
          (insert item queue (cons i rest)))))))

(defmethod schedule-resource ((item resource) (queue resource-queue))
  (print-if *super-debug*
            "Scheduled \"~A\" of type ~A from ~A to ~A~%"
            (resource-label item)
            (resource-type item)
            (best-start-time item)
            (best-end-time item))
  (if (equal (resource-type item)
	     (get-activity-by-typename "Cognitive Operator"))
      (push item *insert-items*))
  (let* ((pos (position queue
			(flatten (as-list (processor-queues
					   (get-processor)))))))
    (setf (resource-queue-tree queue)
	  (insert item (resource-queue-tree queue) nil))
    (setf (resource-queue-number item) pos)))

(defmethod compute-duration ((item resource))
  (if (and (valid-time? (resource-start-time item))
           (valid-time? (resource-end-time item)))
      (setf (resource-duration item)
	    (- (resource-end-time item)
	       (resource-start-time item))
            (first (resource-parameters item))
	    (resource-duration item)))
  (if (and (valid-time? (resource-latest-start-time item))
           (valid-time? (resource-latest-end-time item)))
      (setf (resource-duration item)
	    (- (resource-latest-end-time item)
	       (resource-latest-start-time item))
            (first (resource-parameters item))
	    (resource-duration item)))
  (if (and (valid-time? (resource-earliest-start-time item))
           (valid-time? (resource-earliest-end-time item)))
      (setf (resource-duration item)
	    (- (resource-earliest-end-time item)
	       (resource-earliest-start-time item))
            (first (resource-parameters item))
	    (resource-duration item)))
)

(defmethod scan-and-split ((item resource) (queue list) &key (method :end) (action nil))
  (cond ((null queue) queue)
        ((and (eq method :end)
              (< (best-start-time (car queue)) (best-end-time item))
              (< (best-end-time item) (best-end-time (car queue))))
          (print-if *super-debug* "Splitting \"~A\" because \"~A\" ended at ~A between ~A  and ~A~%"
                   (resource-label (car queue))
                   (resource-label item)
                   (best-end-time item)
                   (best-start-time (car queue))
                   (best-end-time (car queue)))
         (let ((act1 (car queue))
               (act2 (make-instance 'resource)))
           (setf (resource-type act2)
		 (resource-type act1)

                 (resource-start-time act2)
		 (if (valid-time? (resource-start-time act1))
		     (best-end-time item)
		   (resource-start-time act1))

                 (resource-end-time act2)
		 (resource-end-time act1)

                 (resource-latest-start-time act2)
		 (if (valid-time? (resource-latest-start-time act1))
		     (best-end-time item)
		   (resource-latest-start-time act1))

                 (resource-latest-end-time act2)
		 (resource-latest-end-time act1)

                 (resource-earliest-start-time act2)
		 (if (valid-time? (resource-earliest-start-time act1))
		     (best-end-time item)
		   (resource-earliest-start-time act1))

                 (resource-earliest-end-time act2)
		 (resource-earliest-end-time act1)

                 (resource-parameters act2)
		 (copy-list (resource-parameters act1))

                 (resource-id act2)
		 (resource-id act1)

                 (resource-iroutine act2)
		 (resource-iroutine act1)

                 (resource-iroutine-task act2)
		 (resource-iroutine-task act1)

                 (resource-queue-number act2)
		 (resource-queue-number act1)

                 (resource-label act2)
		 (resource-label act1)

                 (resource-distribution act2)
		 (resource-distribution act1)

                 (resource-dependents act2)
		 (copy-list (resource-dependents act1))

                 (resource-predecessors act2)
		 (list item act1))
           (if (valid-time? (resource-end-time act1))
               (setf (resource-end-time act1)
		     (best-end-time item)))
           (if (valid-time? (resource-latest-end-time act1))
               (setf (resource-latest-end-time act1)
		     (best-end-time item)))
           (if (valid-time? (resource-earliest-end-time act1))
               (setf (resource-earliest-end-time act1)
		     (best-end-time item)))
           (compute-duration act1)
           (compute-duration act2)
           (setf (resource-dependents act1) (list act2))
           (dolist (x (resource-dependents act2))
             (setf (resource-predecessors x)
		   (substitute act2 act1 (resource-predecessors x))))
           (if action
               (funcall action item act1 act2)
             (push act2 (resource-dependents item)))
           (setf (car queue) act1)
           (setf (cdr queue) (cons act2 (cdr queue)))
           queue))
        ((and (eq method :start)
              (< (best-start-time (car queue)) (best-start-time item))
              (< (best-end-time item) (best-end-time (car queue))))
         (print-if *super-debug* "Splitting \"~A\" because \"~A\" started at ~A between ~A and ~A~%"
                   (resource-label (car queue))
                   (resource-label item)
                   (best-start-time item)
                   (best-start-time (car queue))
                   (best-end-time (car queue)))
         (let ((act1 (car queue))
               (act2 (make-instance 'resource)))
           (setf (resource-type act2)
		 (resource-type act1)

                 (resource-start-time act2)
		 (if (valid-time? (resource-start-time act1))
		     (best-start-time item)
		   (resource-start-time act1))

                 (resource-end-time act2)
		 (resource-end-time act1)

                 (resource-latest-start-time act2)
		 (if (valid-time? (resource-latest-start-time act1))
		     (best-start-time item)
		   (resource-latest-start-time act1))

                 (resource-latest-end-time act2)
		 (resource-latest-end-time act1)

                 (resource-earliest-start-time act2)
		 (if (valid-time? (resource-earliest-start-time act1))
		     (best-start-time item)
		   (resource-earliest-start-time act1))

                 (resource-earliest-end-time act2)
		 (resource-earliest-end-time act1)

                 (resource-parameters act2)
		 (copy-list (resource-parameters act1))

                 (resource-id act2)
		 (resource-id act1)

                 (resource-iroutine act2)
		 (resource-iroutine act1)

                 (resource-iroutine-task act2)
		 (resource-iroutine-task act1)

                 (resource-queue-number act2)
		 (resource-queue-number act1)

                 (resource-label act2)
		 (resource-label act1)

                 (resource-distribution act2)
		 (resource-distribution act1)

                 (resource-dependents act2)
		 (copy-list (resource-dependents act1))

                 (resource-predecessors act2)
		 (list act1))
           (if (valid-time? (resource-end-time act1))
               (setf (resource-end-time act1)
		     (best-start-time item)))
           (if (valid-time? (resource-latest-end-time act1))
               (setf (resource-latest-end-time act1)
		     (best-start-time item)))
           (if (valid-time? (resource-earliest-end-time act1))
               (setf (resource-earliest-end-time act1)
		     (best-start-time item)))
           (compute-duration act1)
           (compute-duration act2)
           (setf (resource-dependents act1) (list item act2))
           (dolist (x (resource-dependents act2))
             (setf (resource-predecessors x)
		   (substitute act2 act1 (resource-predecessors x))))
           (if action
               (funcall action item act1 act2)
             (push act1 (resource-predecessors item)))
           (setf (car queue) act1)
           (setf (cdr queue) (cons act2 (cdr queue)))
           queue))
        (t
         (setf (cdr queue) (scan-and-split item
					   (cdr queue)
					   :method method
					   :action action))
         queue)))

(defmethod scan-and-split ((item resource) (queue resource-queue) 
			   &key (method :end) (action nil))
  (setf (resource-queue-tree queue) 
	(scan-and-split item (resource-queue-tree queue) 
			:method method :action action)))

(defmethod schedule-resource :after
  ((item resource) (queue resource-queue))
  (declare (special *interrupts*))
  (block nil
    (if (not (boundp '*interrupts*)) (return))
    (dolist (entry *interrupts*)
      (destructuring-bind (act1 act2)
	  (mapcar #'get-activity-by-typename entry)
        (if (is-supertype-of? act1 (resource-type item))
            (let (queues)
              (maphash
	       #'(lambda (k v)
		   (if (is-supertype-of? k act2)
		       (setf queues (append queues v))))
	       (processor-queues (get-processor)))
              (dolist (queue queues)
                (scan-and-split item queue)
                )))
        (if (is-supertype-of? act2 (resource-type item))
            (let (queues)
              (maphash #'(lambda (k v)
                           (if (is-supertype-of? k act1)
                               (setf queues (append queues v))))
                       (processor-queues (get-processor)))
              (dolist (queue2 queues)
                (mapcar #'(lambda (x) (scan-and-split x queue))
                        (resource-queue-tree queue2)))))
        ))))

(defmethod schedule-iroutine ((items hash-table) (proc processor))
  (let ((types nil)
        (grouped (make-hash-table)))
    (maphash #'(lambda (k v) (declare (ignore v))
                 (pushnew k types))
             (processor-queues proc))
    (maphash #'(lambda (k x) (declare (ignore k))
                 (push x (gethash (most-specific-superclass
				   (resource-type x)
				   types)
				  grouped)))
             items)
    (maphash #'(lambda (type l)
                 (let ((items (sort l #'resource<)))
                   (loop for item in items
                         do
			 (schedule-resource
			  item
			  (best-queue-for-resource proc type item)))))
             grouped)))

(defmethod best-queue-for-resource ((proc processor)
				    (type activity-type)
				    (res resource))
  (loop for queue in (gethash type (processor-queues proc)) do
        (if (null (last-scheduled-resource queue))
	    (return queue)
	  (let ((l (last-scheduled-resource queue)))
	    (cond
	     ((and (resource-end-time l)
		   (resource-start-time res)
		   (<= (resource-end-time l)
		       (resource-start-time res)))
	      (return queue))
	     ((and (resource-end-time l)
		   (resource-latest-start-time res)
		   (<= (resource-end-time l)
		       (resource-latest-start-time res)))
	      (return queue))
	     ((and (resource-end-time l)
		   (resource-earliest-start-time res)
		   (<= (resource-end-time l)
		       (resource-earliest-start-time res)))
	      (return queue))
	     ((and (resource-latest-end-time l)
		   (resource-start-time res)
		   (<= (resource-latest-end-time l)
		       (resource-start-time res)))
                      (return queue))
	     ((and (resource-latest-end-time l)
		   (resource-latest-start-time res)
		   (<= (resource-latest-end-time l)
		       (resource-latest-start-time res)))
	      (return queue))
	     ((and (resource-latest-end-time l)
		   (resource-earliest-start-time res)
		   (<= (resource-latest-end-time l)
		       (resource-earliest-start-time res)))
	      (return queue)))))                 
        finally
	(return (first (gethash type (processor-queues proc))))))

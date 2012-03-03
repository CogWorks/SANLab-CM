(defclass processor ()
  ((constraints :initform (make-hash-table :test 'equal) :accessor processor-constraints)
   (queues :initform (make-hash-table :test 'equal) :accessor processor-queues)
   (in-trial :initform nil :reader in-trial? :writer (setf trial))))

(defclass constraint ()
  ((name :initform nil :initarg :name :accessor constraint-name)
   (type :initform nil :initarg :type :accessor constraint-type)))

(defclass resource-constraint (constraint)
  ((count :initform nil :initarg :count :accessor resource-constraint-count)
   (method :initform 'serial :initarg :method :accessor resource-constraint-method)
   (queues :initform nil :accessor resource-constraint-queues)
   ))

(defclass resource-queue ()
  ((tree :initform nil :accessor resource-queue-tree)
   (priority-func :initform #'resource-latest-end-time :initarg :priority-func :reader resource-queue-priority-func)
   (comparison-func :initform #'< :initarg :comparison-func :reader resource-queue-comparison-func)
  ))

(defclass resource ()
  ((earliest-start-time :initform nil :initarg :earliest-start-time :accessor resource-earliest-start-time
                        :documentation "The earliest possible start time for this activity.")
   (earliest-end-time :initform nil :initarg :earliest-end-time :accessor resource-earliest-end-time
                      :documentation "The earliest possible end time for this activity.")
   (latest-start-time :initform nil :initarg :latest-start-time :accessor resource-latest-start-time
                      :documentation "The latest possible start time for this activity.")
   (latest-end-time :initform nil :initarg :latest-end-time :accessor resource-latest-end-time
                    :documentation "The latest possible end time for this activity.")
   (duration :initform nil :initarg :duration :accessor resource-duration
             :documentation "The duration of this activity in the log file.")
   (start-time :initform nil :initarg :start-time :accessor resource-start-time
               :documentation "Actual start time in the log file")
   (end-time :initform nil :initarg :end-time :accessor resource-end-time
             :documentation "Actual end time in the log file")
   (type :initform nil :initarg :type :accessor resource-type
         :documentation "SANLab activity-type of this activity")
   (predecessors :initform nil :initarg :predecessors :accessor resource-predecessors
                 :documentation "Resources that this activity depends on occurring")
   (dependents :initform nil :initarg :dependents :accessor resource-dependents
               :documentation "Resources the depend on this activity occurring")
   (label :initform nil :initarg :label :accessor resource-label
          :documentation "Label for this activity")
   (id :initform nil :initarg :id :accessor resource-id
       :documentation "Unique ID for this activity")
   (distribution :initform nil :initarg :distribution :accessor resource-distribution
                 :documentation "Distribution used to generate SANLab-CM activity")
   (parameters :initform nil :initarg :parameters :accessor resource-parameters
               :documentation "Paremeters used to generate the SANLab-CM activity")
   (extra :initform nil :initarg :extra :accessor resource-extra
          :documentation "Extra data for this activity for determining isomorphism (e.g. key pressed)")
   (node :initform nil :accessor resource-node
         :documentation "Node in the SANLab Model")
   (queue-number :initform nil :accessor resource-queue-number
                 :documentation "Used to compute Y-coordinate in SANLab model")
   (iroutine :initform nil :initarg :routine :accessor resource-iroutine
             :documentation "Used to tag interactive routines in SANLab")
   (iroutine-task :initform nil :initarg :task :accessor resource-iroutine-task
                  :documentation "Used to tag interactive routines in SANLab")
   (children-count :initform nil :accessor resource-children-count
                   :documentation "Used to improve isomorphism performance")
   (depth :initform 0 :accessor resource-depth
          :documentation "Depth of this resource in the graph, measured from the start node (i.e. the node with no dependencies)")
))

(defclass start-resource (resource)
  ((trial-duration :initform nil :initarg :trial-duration :accessor start-resource-trial-duration)))

(defclass parser ()
  ()
  )

(defmethod initialize-parser (p)
  nil)

(defmethod parse-item ((parser parser))
  (error (format nil "Called abstract method parse-item on %~A" parser))
)

(defmethod reset-processor ((p processor))
  (maphash #'(lambda (k v) (declare (ignore k)) (mapc #'reset-queue v)) (processor-queues p)))

(defmethod resource-constraint-for-activity ((proc processor) (type activity-type))
  (block nil
    (let (res)
      (maphash #'(lambda (k v)
                   (declare (ignore k))
                   (if (setf res (find (typename type) v :test #'equal :key #'constraint-type))
                       (return res)))
               (processor-constraints proc))
      res)))

(defmethod num-merged-trials ((r resource))
  (if (listp (resource-duration r)) (length (resource-duration r)) 1))
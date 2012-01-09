(defparameter *debug-dmap5* t)
(defparameter *save-dir* "~/sanlab/Models/DMAP5/")

(defconstant *data-files* '())

(defmacro print-if (test &rest rest)
  `(if ,test (format t ,@rest)))

(defclass dmap5-parser (parser)
  ((stream :initform nil :initarg :stream :accessor parser-stream
           :documentation "Source data file for this parser")
   (subject :initform nil :initarg :subject :accessor parser-subject
            :documentation "Subject ID for the data file")
   ))

(defmethod initialize-parser ((parser dmap5-parser))
)

(defmethod parse-item ((parser dmap5-parser))
)

(defparameter *areas-of-interest*
  '())

(defparameter *event-mapping*
  '((EG-FIXATION . (:type :routine
                    :routine "Eye Movement"
                    :event-id (2 3)
                    :distribution "Constant"))))

(defun read-tab-line (stream)
  (let* ((line (read-line stream nil ""))
         (parts (mapcar #'read-from-string (explode-tab line))))
    parts))

(defmethod read-stream-line ((parser snt-parser))
  (let ((line (read-tab-line (parser-stream parser))))
    (if line
        (setf (first line) (read-from-string (first line))
              (second line) (read-from-string (substitute #\- #\Space (second line)))
              (fourth line) (read-from-string (fourth line))
              (fifth line) (read-from-string (fifth line))
              ; All remaining items dependent on columns 4 & 5
              ))
    line))

(defun run-dmap5 (id log)
  (reset-processor (get-processor))
  (let ((parser (make-instance 'dmap5-parser
                               :stream (open (format nil "/Users/ewpatton/Research/SANLab/v3.0/DMAP-5/Human/Raw Data/~A" log))
                               :subject id)))
    (let ((result (run-protocol-analysis parser)))
      (close (parser-stream parser))
      (values result parser))))

(defmethod save-dmap5-models ((p dmap5-parser) (l list))
  (do ((l l (cdr l))
       (i 1 (1+ i)))
      ((null l) nil)
    (let ((fn (format nil "~A~A_~A_model_~A.san/" *save-dir* (num-merged-trials (car l)) (parser-subject p) i))
          (model (resource-graph-to-sanlab-model (car l))))
      (make-sanlab-bundle fn)
      (write-model-to-bundle fn model))))
(defparameter *debug-dmap5* nil)
(defparameter *save-dir* "~/sanlab/Models/DMAP5")
(defconstant *source-dir* "/Volumes/Time Machine/DMAP-5/Human/Analysis/090213")
(defconstant *data-files*
  (mapcar #'(lambda (x) (list x (format nil "~A/subj-~A.txt" *source-dir* x)))
          '(1379 1390 1418 1438 1636 1731 1735 1744 1891 2067 216 2464 2508 2537 255 2791 2834
            3010 3163 3214 3223 323 3270 3279 3397 341 3420 3422 3546 3570 4058 422 4361 4472 4700
            5272 5443 5581 5684 6181 6245 6374 6732 6768 6770 6855 6919 6926 6991
            7000 7045 714 7365 7415 7528 7666 7727 8113 8133 8228 8354 8416 8570
            9268 928 9394 941 9728 9796 9800 99 9904 9911 9917 9933)))

(setf (app-property 'current-controller) (make-instance 'controller))

(defmacro print-if (test &rest rest)
  `(if ,test (format t ,@rest)))

(defun read-tab-line (stream)
  (let* ((line (read-line stream nil ""))
         (parts (mapcar #'(lambda (x) (ignore-errors (read-from-string x))) (explode-tab line))))
    parts))

(defclass dmap5-parser (parser)
  ((stream :initform nil :initarg :stream :accessor parser-stream
           :documentation "Source data file for this parser")
   (subject :initform nil :initarg :subject :accessor parser-subject
            :documentation "Subject ID for the data file")
   (last-event :initform nil :initarg :event :accessor parser-last-event
               :documentation "Last event read from the input stream")
   (lockout :initform nil :accessor parser-lockout
            :documentation "Lockout period for current trial")
   (lockout-start :initform nil :accessor parser-lockout-start
                  :documentation "Start of the current lockout period")
   (threat-display :initform nil :accessor parser-threat-display
                   :documentation "Flag indicating that a threat is displayed")
   (decision :initform nil :accessor parser-decision
              :documentation "Flag indicating that the last event was DMT-DECISION")
   ))

(defmethod initialize-parser ((parser dmap5-parser))
  (setf *fill-gaps* nil)
  (let (event)
    (setf event (do ((x (read-tab-line (parser-stream parser))
                        (read-tab-line (parser-stream parser))))
                    ((numberp (fifth x)) x)
                  ))
    (setf (parser-last-event parser) event))
)

(defmethod parse-item ((parser dmap5-parser))
  (let (args)
    (do ((event (parser-last-event parser) (parser-last-event parser)))
        ((equal nil event) nil)
      (cond ((equal (nth 11 event) 'eye)
             (if (not (equal (nth 12 event) 'fixation)) (break))
             (setf args (make-hash-table))
             (let ((start (make-hash-table)) (end (make-hash-table)))
               (setf (gethash 3 start) (nth 13 event))
               (setf (gethash 3 end) (nth 14 event))
               (setf (gethash :start args) start)
               (setf (gethash :end args) end)
               (setf (gethash :label args)
                     (if (nth 21 event) (format nil "~A" (nth 21 event))
                       (format nil "(~A, ~A)" (nth 18 event) (nth 19 event)))))
             (setf (parser-last-event parser) (read-tab-line (parser-stream parser)))
             ;(format t "Parse fixation at ~A~%" (nth 13 event))
             ;(finish-output *standard-output*)
             (print-if *debug-dmap5* "Processed eye movement~%")
             (return (values 'eg-fixation args))
             )
            ((equal (nth 11 event) 'event)
             (cond ((parser-decision parser)
                    (setf (parser-decision parser) nil)
                    (setf args (make-hash-table))
                    (setf (gethash :end args) (nth 13 event))
                    (setf (gethash 'valid-trial args) t)
                    (setf (gethash 'condition args) (second event))
                    (setf (parser-last-event parser) (read-tab-line (parser-stream parser)))
                    (format t "End trial at ~A~%" (nth 13 event))
                    (finish-output *standard-output*)
                    (return (values 'end-trial args))
                    )
                   ((equal (nth 12 event) 'start-trial)
                    (setf args (make-hash-table))
                    (setf (gethash :start args) (nth 13 event))
                    (setf (parser-last-event parser) (read-tab-line (parser-stream parser)))
                    (format t "Start trial at ~A~%" (nth 13 event))
                    (finish-output *standard-output*)
                    (return (values 'start-trial args))
                    )
                   ((equal (nth 12 event) 'correct-response)
                    (setf args (make-hash-table))
                    (setf (gethash :start args) (nth 13 event))
                    (setf (gethash :end args) (nth 13 event))
                    (setf (gethash :label args) "New Radar Screen")
                    (setf (parser-last-event parser) (read-tab-line (parser-stream parser)))
                    ;(format t "Fixation check response at ~A~%" (nth 13 event))
                    ;(finish-output *standard-output*)
                    (return (values 'new-screen args))
                    )
                   ((equal (nth 12 event) 'select-target)
                    (setf args (make-hash-table))
                    (let ((start (make-hash-table)) (end (make-hash-table)))
                      (setf (gethash 3 end) (nth 13 event))
                      (setf (gethash 3 start) (- (nth 13 event) 125))
                      (setf (gethash :start args) start)
                      (setf (gethash :end args) end)
                      (setf (gethash :label args) (format nil "Select Tgt ~A" (nth 21 event))))
                    (setf (parser-lockout-start parser) (nth 21 event))
                    (setf (parser-lockout parser) (nth 42 event))
                    (setf (parser-last-event parser) (read-tab-line (parser-stream parser)))
                    ;(format t "Selected target at ~A~%" (nth 13 event))
                    ;(finish-output *standard-output*)
                    (print-if *debug-dmap5* "Processed select-target~%")
                    (return (values 'click args))
                    )
                   ((equal (nth 12 event) 'display-threat-value)
                    (setf args (make-hash-table))
                    ;(setf (gethash :start args) (parser-lockout-start parser))
                    (setf (gethash :start args) (nth 13 event))
                    (setf (gethash :end args) (nth 13 event))
                    (setf (parser-last-event parser) (read-tab-line (parser-stream parser)))
                    (setf (parser-threat-display parser) t)
                    ;(format t "Lockout finished at ~A~%" (nth 13 event))
                    ;(finish-output *standard-output*)
                    (print-if *debug-dmap5* "Processed lockout~%")
                    (return (values 'lockout args))
                    )
                   ((equal (nth 12 event) 'click-while-selected-target)
                    (setf args (make-hash-table))
                    (let ((start (make-hash-table)) (end (make-hash-table)))
                      (setf (gethash 3 end) (nth 13 event))
                      (setf (gethash 3 start) (- (nth 13 event) 125))
                      (setf (gethash :start args) start)
                      (setf (gethash :end args) end)
                      (setf (gethash :label args) "Invalid"))
                    (setf (parser-last-event parser) (read-tab-line (parser-stream parser)))
                    ;(format t "Invalid click at ~A~%" (nth 13 event))
                    ;(finish-output *standard-output*)
                    (print-if *debug-dmap5* "Processed invalid click~%")
                    (return (values 'click args))
                    )
                   ((equal (nth 12 event) 'removed-threat-value-display)
                    (if (not (parser-threat-display parser)) 
                        (progn
                          (setf (parser-last-event parser) (read-tab-line (parser-stream parser)))
                          (return 'continue)))
                    (setf (parser-threat-display parser) nil)
                    (setf args (make-hash-table))
                    (let ((start (make-hash-table)) (end (make-hash-table)))
                      (setf (gethash 3 end) (nth 13 event))
                      (setf (gethash 3 start) (- (nth 13 event) 125))
                      (setf (gethash :start args) start)
                      (setf (gethash :end args) end)
                      (setf (gethash :label args) (format nil "Deselect Tgt ~A" (nth 21 event))))
                    (setf (parser-last-event parser) (read-tab-line (parser-stream parser)))
                    ;(format t "Deselected target at ~A~%" (nth 13 event))
                    ;(finish-output *standard-output*)
                    (print-if *debug-dmap5* "Processed deselect target~%")
                    (return (values 'click args))
                    )
                   ((equal (nth 12 event) 'dmt-selection)
                    (setf args (make-hash-table))
                    (let ((start (make-hash-table)) (end (make-hash-table)))
                      (setf (gethash 3 end) (nth 13 event))
                      (setf (gethash 3 start) (- (nth 13 event) 125))
                      (setf (gethash :start args) start)
                      (setf (gethash :end args) end)
                      (setf (gethash :label args) (format nil "Choose Tgt ~A" (nth 21 event))))
                    (setf (parser-last-event parser) (read-tab-line (parser-stream parser)))
                    ;(format t "Choose target at ~A~%" (nth 13 event))
                    ;(finish-output *standard-output*)
                    (print-if *debug-dmap5* "Processed dmt-selection~%")
                    (return (values 'click args))
                    )
                   ((equal (nth 12 event) 'dmt-decision)
                    (setf args (make-hash-table))
                    (let ((start (make-hash-table)) (end (make-hash-table)))
                      (setf (gethash 3 end) (nth 13 event))
                      (setf (gethash 3 start) (- (nth 13 event) 125))
                      (setf (gethash :start args) start)
                      (setf (gethash :end args) end)
                      (setf (gethash :label args) "DMT Decision"))
                    (setf (parser-decision parser) t)
                    ;(format t "Submitted decision at ~A~%" (nth 13 event))
                    ;(finish-output *standard-output*)
                    (print-if *debug-dmap5* "Processed dmt-decision~$")
                    (return (values 'click args))
                    )
                   (t
                    (setf (parser-last-event parser) (read-tab-line (parser-stream parser)))
                    (return 'continue))
             )
            (t (break))))))
)

(defun run-dmap5 (id log &optional ht (merge t))
  (reset-processor (get-processor))
  (let ((parser (make-instance 'dmap5-parser
                               :stream (open log)
                               :subject id)))
    (let ((result (if ht (run-protocol-analysis parser :merge-trials merge :trials ht)
                    (run-protocol-analysis parser :merge-trials merge))))
      (close (parser-stream parser))
      (values result parser))))

(defun process-all-files ()
  (configure-default-processor)
  (let ((ht (make-hash-table)))
    (mapcar #'(lambda (i)
                (format t "Processing subject ~A~%" (first i))
                (save-models (run-dmap5 (first i) (second i)) (format nil "subj-~A" (first i)))
                (run-dmap5 (first i) (second i) ht))
            *data-files*)
    (save-models ht "AGG")))

(defun process-aggregate ()
  (configure-default-processor)
  (let ((ht (make-hash-table)))
    (mapcar #'(lambda (i)
                (format t "Processing subject ~A~%" (first i))
                (run-dmap5 (first i) (second i) ht))
            *data-files*)
    (save-models ht "AGG")))

#|
(defmethod save-models ((ht hash-table) (subject string))
  (ensure-directories-exist (format nil "~A/~A/" *save-dir* subject))
  (maphash #'(lambda (k v)
               (do ((l v (cdr l))
                    (i 1 (1+ i)))
                   ((null l) nil)
                 (setf (app-property 'current-controller) (make-instance 'controller))
                 (with-open-file (out (format nil "~A/~A/~A-~A-~A-~A-durations.txt"
                                              *save-dir* subject subject
                                              k (num-merged-trials (car l)) i)
                                      :if-exists :supersede :direction :output)
                   (dolist (dur (start-resource-trial-duration (car l)))
                     (format out "~A~%" dur)))
                 (let ((model (resource-graph-to-sanlab-model (car l)))
                       (p (format nil "~A/~A/~A-~A-~A-~A.san/"
                                  *save-dir* subject subject
                                  k (num-merged-trials (car l)) i)))
                   (make-sanlab-bundle p)
                   (write-model-to-bundle p model))))
           ht)
)
|#

(defmethod save-models ((ht hash-table) (subject string))
  (ensure-directories-exist (format nil "~A/~A/" *save-dir* subject))
  (maphash #'(lambda (k v)
               (do ((l v (cdr l))
                    (i 1 (1+ i)))
                   ((null l) nil)
                 (setf (app-property 'current-controller) (make-instance 'controller))
                 (ensure-directories-exist (format nil "~A/~A/~A/" *save-dir* subject k))
                 (with-open-file (out (format nil "~A/~A/~A/~A-~A-~A-~A-durations.txt"
                                              *save-dir* subject k subject
                                              k (num-merged-trials (car l)) i)
                                      :if-exists :supersede :direction :output)
                   (dolist (dur (start-resource-trial-duration (car l)))
                     (format out "~A~%" dur)))
                 (let ((model (resource-graph-to-sanlab-model (car l)))
                       (p (format nil "~A/~A/~A/~A-~A-~A-~A.san/"
                                  *save-dir* subject k subject
                                  k (num-merged-trials (car l)) i)))
                   (make-sanlab-bundle p)
                   (write-model-to-bundle p model))))
           ht))

(defparameter *areas-of-interest*
  '())

(defparameter *event-mapping*
  '((EG-FIXATION . (:type :routine
                    :routine "Eye Movement"
                    :event-id (3)
                    :distribution "Constant"))
    (NEW-SCREEN . (:type :activity
                   :activity "System Resource"
                   :distribution "Constant"
                   :label "New Screen"))
    (CLICK . (:type :routine
              :routine "Prepped Mouse Click"
              :event-id (3)
              :distribution "Constant"))
    (LOCKOUT . (:type :activity
                :activity "System Resource"
                :distribution "Constant"
                :label "TV Lockout"))
))

(defparameter *interrupts*
  '(("System Resource" "Perceptual Operator")))

(defparameter *utilizes*
  '(("Prepped Mouse Click" "Perceptual Operator (Visual)")))
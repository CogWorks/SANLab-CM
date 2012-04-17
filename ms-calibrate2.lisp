(defparameter *debug-calibrate* nil)
(defparameter *save-dir* "~/sanlab/Models/IBM-proc/")
(defparameter *limit-data-ingest* nil)
(defparameter *ignore-subjects* '(703 711 802 804 805 806 810 811 812 817))
(defparameter *segment* nil) ; change to nil for full aggregate models
(defparameter *condition* 'old)
(defparameter *cut-off* 4617)
(defparameter *mouse-down-time* 209)

(defconstant *w-prime-mapping*
;         1      2      3      4      5      6      7      8      9     10     11     12     13     14     15
  #2A((nil    medium long   short  short  medium long   short  short  medium long   medium long   long   long  )
      (medium nil    medium medium short  short  short  medium short  medium medium medium long   long   long  )
      (long   medium nil    long   long   medium short  long   long   medium medium long   long   long   long  )
      (short  medium long   nil    short  medium long   short  short  medium long   medium medium long   long  )
      (short  short  long   short  nil    short  medium short  short  short  medium medium medium long   long  )
      (medium short  medium medium short  nil    short  medium short  short  medium medium medium long   long  )
      (long   short  short  long   medium short  nil    long   medium short  short  medium medium long   medium)
      (short  medium long   short  short  medium long   nil    short  medium long   short  medium medium long  )
      (short  short  long   short  short  short  medium short  nil    short  medium short  medium medium medium)
      (medium medium medium medium short  short  short  medium short  nil    short  short  short  medium medium)
      (long   medium medium long   medium medium short  long   medium short  nil    medium short  long   short )
      (medium medium long   medium medium medium medium short  short  short  medium nil    short  short  medium)
      (long   long   long   medium medium medium medium medium medium short  short  short  nil    medium short )
      (long   long   long   long   long   long   long   medium medium medium long   short  medium nil    long  )
      (long   long   long   long   long   long   medium long   medium medium short  medium short  long   nil   )))

(defparameter *the-mapping* *w-prime-mapping*)

(setf (app-property 'current-controller) (make-instance 'controller))

(defun read-tab-line (stream)
  (let* ((line (read-line stream nil ""))
         (parts (mapcar
		 #'(lambda (x)
		     (ignore-errors
		       (read-from-string x)))
		 (explode-tab line))))
    parts))

(defclass calib-parser (parser)
  ((event-stream :initform nil
		 :initarg :event-stream
		 :accessor parser-event-stream)
   (fixation-stream :initform nil
		    :initarg :fixation-stream
		    :accessor parser-fixation-stream)
   (click-state :initform nil :accessor parser-click-state)
   (last-button :initform 0 :accessor parser-last-button)
   (start-time :initform 0 :accessor parser-start-time)
   (cur-target :initform 0 :accessor parser-current-target)
   (last-mouse-up :initform 0 :accessor parser-last-mouse-up)
   (clicks :initform 0 :accessor parser-clicks)
   (last-event :initform nil :accessor parser-last-event)
   (last-fixation :initform nil :accessor parser-last-fixation)))

(defmethod initialize-parser ((parser calib-parser))
  (setf *fill-gaps* '("Cognitive Operator"))
  ;(setf *fill-gaps* nil)
  (let (event fixation)
    (setf event
	  (do ((x (read-tab-line (parser-event-stream parser))
		  (read-tab-line (parser-event-stream parser))))
	      (x x)))
    (setf fixation
	  (do ((x (read-tab-line (parser-fixation-stream parser))
		  (read-tab-line (parser-fixation-stream parser))))
	      ((and x (numberp (first x))
		    (>= (floor (/ (second x) 1000))
			(second event))) x)
                     (setf fixation x)))
    (setf (second fixation) (/ (second fixation) 1000))
    (setf (parser-last-event parser) event)
    (setf (parser-last-fixation parser) fixation)))

(defmethod parse-item ((parser calib-parser))
  (let (args)
    (do ((event (parser-last-event parser)
		(parser-last-event parser))
         (fixation (parser-last-fixation parser)
		   (parser-last-fixation parser)))
        ((equal nil event) nil)
      (cond
       ((or (null fixation)
	    (< (second event) (second fixation)))
	(cond
         ((equal (third event) 'BeginBlock)
          (setf (parser-last-button parser) 0)

          (setf (parser-last-event parser)
                (read-tab-line (parser-event-stream parser)))
          (return 'continue))
	 ((equal (third event) 'StartTrial)
          ;(format t "Starting trial at ~A...~%" (second event))
	  (setf args (make-hash-table))
	  (setf (gethash :start args) (second event))
          (setf (parser-start-time parser) (first event))
          (setf (parser-current-target parser) (fourth event))
          (setf (parser-clicks parser) 0)

	  (setf (parser-last-event parser)
		(read-tab-line (parser-event-stream parser)))
	  (return (values 'start-trial args)))
	 ((equal (third event) 'EndTrial)
	  (setf args (make-hash-table))
	  (setf (gethash :end args) (parser-last-mouse-up parser))
	  (setf (gethash 'valid-trial args)
                (and (not (equal (parser-last-button parser) 0))
                     (< (- (first event) (parser-start-time parser))
                        *cut-off*)
                     (= (parser-clicks parser) 1)))
          ;(format t "Valid trial? ~A~%" (gethash 'valid-trial args))
          (incf (parser-trial parser))
          (setf (gethash 'condition args) *condition*)
          (setf (parser-clicks parser) 0
                (parser-start-time parser) 0)
          (setf (parser-last-button parser) (parser-current-target parser))
          (setf (parser-current-target parser) 0)
	  
	  (setf (parser-last-event parser)
		(read-tab-line (parser-event-stream parser)))
	  (return (values 'end-trial args)))
	 ((or (equal (third event) 'MouseMove)
              (equal (third event) 'MoveMouseToButton)
              (equal (third event) 'CorrectOvershoot))
	  (setf args (make-hash-table))
	  (let ((start (make-hash-table))
		(end (make-hash-table))
                (tgt (check-aoi (fourth event) (fifth event))))
	    (setf (gethash 2 start) (second event))
	    (setf (gethash 2 end)
		  (+ (second event) (sixth event)))
            ;(format t "~A~%" (sixth event))
	    (setf (gethash :start args) start)
	    (setf (gethash :end args) end)
	    (setf (gethash :label args)
                  (if tgt (symbol-name tgt)
                    (format nil "(~A, ~A)"
                            (fourth event)
                            (fifth event)))))
	  
	  (setf (parser-last-event parser)
		(read-tab-line (parser-event-stream parser)))
	  (if args
	      (return (values 'mouse-move args))
	    (return 'continue)))
         ((equal (third event) 'SettleMouse)
          (setf args (make-hash-table))
          (setf (gethash :start args) (second event))
          (setf (gethash :end args)
                (+ (second event)
                   (sixth event)))
          (setf (parser-last-event parser)
                (read-tab-line (parser-event-stream parser)))
          (return (values 'mouse-settle args))
          )
         ((equal (third event) 'MouseDown)
          (setf (parser-last-event parser)
                (read-tab-line (parser-event-stream parser)))
          (return 'continue)
          )
	 ((equal (third event) 'MouseUp)
	  (setf args (make-hash-table))
	  (let ((start (make-hash-table))
		(end (make-hash-table))
                (tgt (check-aoi (fourth event) (fifth event))))
            (setf (parser-last-mouse-up parser)
		  (+ (second event)
		     (sixth event)))
	    (setf (gethash 2 start) (- (second event)
                                       *mouse-down-time*))
	    (setf (gethash 2 end) (second event))
            (setf (gethash 3 start) (second event)
                  (gethash 3 end) (+ (second event)
                                     (sixth event)))
	    (setf (gethash :start args) start)
	    (setf (gethash :end args) end)
	    (setf (gethash :label args) 
                  (if tgt (symbol-name tgt)
                    "missed"))
            (incf (parser-clicks parser)))

          (setf (parser-last-event parser)
                (read-tab-line (parser-event-stream parser)))
	  
	  (if args
	      (return (values 'mouse-click args))
	    (return 'continue)))
	 (t
	  (setf (parser-last-event parser)
		(read-tab-line (parser-event-stream parser)))
	  ;(format t "Parse item at ~A~%" (second event))
	  ;(finish-output *standard-output*)
	  
	  (return 'continue))))
       (fixation
	(cond ((<= 100 (third fixation))
	       (setf args (make-hash-table))
	       (let ((start (make-hash-table))
		     (end (make-hash-table)))
		 (setf (gethash 6 start)
		       (second fixation))
		 (setf (gethash 6 end)
		       (+ (second fixation)
			  (third fixation)))
		 (setf (gethash :start args) start)
		 (setf (gethash :end args) end)
		 (setf (gethash :label args)
		       (format nil "(~A, ~A)"
			       (fourth fixation)
			       (fifth fixation))))))
	(setf (parser-last-fixation parser)
	      (read-tab-line (parser-fixation-stream parser)))
	(setf (second (parser-last-fixation parser))
	      (floor (/ (second (parser-last-fixation parser)) 1000)))
	(setf fixation (parser-last-fixation parser))
	;(format t "Parse fixation at ~A~%" (second fixation))
	;(finish-output *standard-output*)
	(do ()
	    ((or (null fixation)
		 (and (numberp (second fixation))
		      (numberp (third fixation)))) nil)
	  (setf (parser-last-fixation parser)
		(read-tab-line (parser-fixation-stream parser)))
	  (setf (second (parser-last-fixation parser))
		(floor (/ (second (parser-last-fixation parser))
			  1000)))
	  (setf fixation (parser-last-fixation parser))
	  ;(format t "Parse next fixation at ~A~%" (second fixation))
	  ;(finish-output *standard-output*)
          )
	(print-if *debug-calibrate* "Processed eye movement~%")
	(if (and (in-trial? (get-processor))
		 (<= (in-trial? (get-processor)) (second fixation))
		 args)
	    (return (values 'eg-fixation args))
	  (return 'continue)))
       (t (break))))))

(defun run-calibrate (subj mouse eye &optional ht (merge t))
  (reset-processor (get-processor))
  (let ((parser
	 (make-instance
	  'calib-parser
          :parser-subject subj
	  :event-stream (open mouse)
	  :fixation-stream (open eye))))
    (let ((result
	   (if ht
	       (run-protocol-analysis parser
				      :merge-trials merge
				      :trials ht)
	     (run-protocol-analysis parser
				    :merge-trials merge))))
      (close (parser-event-stream parser))
      (close (parser-fixation-stream parser))
      (values result parser))))

(defmethod save ((nodes list) (parser calib-parser) path)
  (do ((l nodes (cdr l))
       (i 1 (1+ i)))
      ((null l) nil)
    (setf (app-property 'current-controller) (make-instance 'controller))
    (let ((model (resource-graph-to-sanlab-model (car l)))
          (p (format nil "~A/model~A.san/" path i)))
      (make-sanlab-bundle p)
      (write-model-to-bundle p model))))

(defun get-subject-files (subj)
  (values (format nil "~~/Documents/IBM-proc/P~A-Mouse-Data.txt" subj)
          (format nil "~~/Documents/IBM-proc/P~A-Fixation-Data.tsv" subj)))

(defmethod save-models ((ht hash-table) (subject string))
  (ensure-directories-exist (format nil "~A~A/" *save-dir* subject))
  (maphash
   #'(lambda (k v)
       (do ((l v (cdr l))
	    (i 1 (1+ i)))
	   ((null l) nil)
	 (setf (app-property 'current-controller)
	       (make-instance 'controller))
         ; debug
         (if (= 158 (num-merged-trials (car l)))
             (break))
	 (with-open-file
	  (out (format nil "~A~A/~A-~A-~A-~A-durations.txt"
		       *save-dir* subject subject
		       k (num-merged-trials (car l)) i)
	       :if-exists :supersede :direction :output)
	  (dolist (dur (start-resource-trial-duration (car l)))
	    (format out "~A~C~A~C~A~%" (first dur) #\Tab (second dur)
                    #\Tab (third dur))))
	 (let ((model (resource-graph-to-sanlab-model (car l)))
	       (p (format nil "~A~A/~A-~A-~A-~A.san/"
			  *save-dir* subject subject
			  k (num-merged-trials (car l)) i)))
	   (make-sanlab-bundle p)
	   (write-model-to-bundle p model))))
   ht))

(defun process-young-subjs ()
  (configure-default-processor)
  (setf *condition* 'young)
  (setf *cut-off* 2024)
  (setf *mouse-down-time* 100)
  (let ((subjects (remove-if
		   #'(lambda (x)
		       (member x *ignore-subjects*))
		   (loop for i from 701 to 717 collect i)))
        (ht (make-hash-table)))
    (mapcar #'(lambda (i)
                (format t "Processing subject ~A~%" i)
                (multiple-value-bind (mouse eye)
		    (get-subject-files i)
                  (save-models (run-calibrate i mouse eye)
			       (format nil "P~A" i))
                  (run-calibrate i mouse eye ht)
                  ))
            subjects)
    (save-models ht "AGG-YNG")
    ))

(defun process-old-subjs ()
  (configure-default-processor)
  (setf *condition* 'old)
  (setf *cut-off* 4617)
  (setf *mouse-down-time* 209)
  (let ((subjects (remove-if
		   #'(lambda (x)
		       (member x *ignore-subjects*))
		   (loop for i from 801 to 818 collect i)))
        (ht (make-hash-table)))
    (mapcar #'(lambda (i)
                (format t "Processing subject ~A~%" i)
                (multiple-value-bind (mouse eye)
		    (get-subject-files i)
                  (save-models (run-calibrate i mouse eye)
			       (format nil "P~A" i))
                  (run-calibrate i mouse eye ht)))
            subjects)
    (save-models ht "AGG-OLD")))

(defun run-one-subject (&optional (s 701))
  (setf *super-debug* t)
  (setf *limit-data-ingest* t)
  (setf *merge-trials* nil)
  (configure-default-processor)
  (multiple-value-bind (mouse eye) (get-subject-files s)
    (save-models (run-calibrate s mouse eye nil nil)
		 (format nil "P~A" s))))
                  

(defparameter *areas-of-interest*
  '((tgt1 . (:rectangle 23 34 27 22))
    (tgt2 . (:rectangle 495 34 27 22))
    (tgt3 . (:rectangle 895 34 27 22))
    (tgt4 . (:rectangle 23 106 27 22))
    (tgt5 . (:rectangle 265 106 27 22))
    (tgt6 . (:rectangle 507 106 27 22))
    (tgt7 . (:rectangle 749 178 27 22))
    (tgt8 . (:rectangle 23 250 27 22))
    (tgt9 . (:rectangle 265 250 27 22))
    (tgt10 . (:rectangle 507 322 27 22))
    (tgt11 . (:rectangle 749 394 27 22))
    (tgt12 . (:rectangle 265 466 27 22))
    (tgt13 . (:rectangle 507 538 27 22))
    (tgt14 . (:rectangle 23 610 27 22))
    (tgt15 . (:rectangle 749 610 27 22))))

(defparameter *event-mapping*
  '((EG-FIXATION . (:type :routine
                    :routine "Fixation-old"
                    :event-id (6)
                    :distribution "Constant"))
    (MOUSE-MOVE . (:type :routine
                   :routine "Mouse Move-old"
                   :event-id (2)
                   :distribution "Constant"))
    (MOUSE-CLICK . (:type :routine
                    :routine "Prepped Mouse UpDown-old"
                    :event-id (2 3)
                    :distribution "Constant"))
    (MOUSE-SETTLE . (:type :activity
                     :activity "Right Hand Operator"
                     :label "Arrive at target"
                     :distribution "Constant"))))

(defparameter *interrupts*
  '(("System Resource" "Fixation Operator")))

(defparameter *utilizes*
  '(("Prepped Mouse Click" "Perceptual Operator (Visual)")))

(defmethod (setf resource-distribution) :after ((val (eql :same-as-parent)) (res resource))
  (break))

(defmethod scan-and-split :after ((item resource) (queue resource-queue) &key (method :end) (action nil))
  (declare (ignore method action))
  (setf queue (resource-queue-tree queue))
  (if (eql (resource-type item) (get-activity-by-typename "System Resource"))
      (let ((fix (find-if #'(lambda (x) (member item (resource-predecessors x))) queue))
            (type (get-activity-by-typename "Perceptual Operator (Visual)"))
            (type2 (get-activity-by-typename "Cognitive Operator")))
        (if fix
            (let ((new-task
		   (make-instance
		    'resource
		    :distribution (get-distribution-by-typename "Gamma CV")
		    :label "Perceive change"
		    :duration (read-from-string (first (default-params type)))
		    :type type
		    :parameters
		    (mapcar #'read-from-string
			    (default-params type))))
                  (new-task2
		   (make-instance
		    'resource
		    :distribution (get-distribution-by-typename "Gamma CV")
		    :label "Verify change"
		    :duration (read-from-string (first (default-params type2)))
		    :type type2
		    :parameters
		    (mapcar #'read-from-string
			    (default-params type2))))
                  types)
              (if (> 2 (length (resource-parameters new-task)))
                  (setf (resource-parameters new-task) (list (first (resource-parameters new-task)) 0)))
              (if (> 2 (length (resource-parameters new-task2)))
                  (setf (resource-parameters new-task2) (list (first (resource-parameters new-task2)) 0)))
              (case *condition*
                ('young
                 (setf (first (resource-parameters new-task)) 100)
                 (setf (first (resource-parameters new-task2)) 50)
                 (setf (resource-duration new-task) 100)
                 (setf (resource-duration new-task2) 50))
                ('old
                 (setf (first (resource-parameters new-task)) 178)
                 (setf (first (resource-parameters new-task2)) 118)
                 (setf (resource-duration new-task) 178)
                 (setf (resource-duration new-task2) 118)))
              (setf (resource-distribution new-task)
		    (get-distribution type)
                    (resource-earliest-start-time new-task)
		    (best-end-time item)
                    (resource-earliest-end-time new-task)
		    (+ (best-end-time item)
		       (resource-duration new-task)))
              (setf (resource-distribution new-task2)
		    (get-distribution type2)
                    (resource-earliest-start-time new-task2)
		    (resource-earliest-end-time new-task)
                    (resource-earliest-end-time new-task2)
		    (+ (resource-earliest-start-time new-task2)
		       (resource-duration new-task2)))
              (if (eql (resource-distribution new-task) :same-as-parent) (break))
              (push item (resource-predecessors new-task))
              (push new-task (resource-dependents item))
              (push new-task (resource-predecessors new-task2))
              (push new-task2 (resource-dependents new-task))
              (maphash
               #'(lambda (k v)
                   (declare (ignore v))
                   (pushnew k types))
               (processor-queues (get-processor)))
              (schedule-resource new-task
                                 (best-queue-for-resource
                                  (get-processor)
                                  (most-specific-superclass
				   (resource-type new-task)
				   types)
                                  new-task))
              (schedule-resource new-task2
                                 (best-queue-for-resource
                                  (get-processor)
                                  (most-specific-superclass
				   (resource-type new-task2)
				   types)
                                  new-task2))
              )))))

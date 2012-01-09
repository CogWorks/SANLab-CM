(defparameter *debug-snt* t)
(defparameter *save-dir* "~/sanlab/Models/SNT/")

(defconstant *data-files* '(( 8113 "mnt-01_Condition 1_2008-11-04_1614_8113.xls" "NavBack-E1-Merged.data.xls" )
                            ( 5516 "mnt-01_Condition 1_2008-11-15_1130_660435516.xls" "NavBack-E1-Merged.data.xls" )
                            ( 3422 "mnt-01_Condition 1_2008-11-19_1533_3422.xls" "NavBack-E1-Merged.data.xls" )
                            ( 1379 "mnt-01_Condition 1_2008-11-20_1621_660551379.xls" "NavBack-E1-Merged.data.xls" )
                            ( 3838 "mnt-01_Condition 1_2008-12-04_1215_3838.xls" "NavBack-E1-Merged.data.xls" )
                            ( 3838 "mnt-01_Condition 1_2008-12-04_1229_3838.xls" "NavBack-E1-Merged.data.xls" )
                            ( 7045 "mnt-01_Condition 1_2008-12-04_1413_660517045.xls" "NavBack-E1-Merged.data.xls" )
                            ( 5073 "mnt-01_Condition 1_2008-12-04_1715_5073.xls" "NavBack-E1-Merged.data.xls" )
                            ( 9800 "mnt-01_Condition 1_2008-12-05_1210_9800.xls" "NavBack-E1-Merged.data.xls" )
                            ( 1976 "mnt-01_Condition 1_2009-02-06_1414_1976.xls" "NavBack-E1-Merged.data.xls" )
                            ( 1644 "mnt-01_Condition 2_2008-11-05_1538_1644.xls" "NavBack-E1-Merged.data.xls" )
                            ( 216 "mnt-01_Condition 2_2008-11-17_1612_0216.xls" "NavBack-E1-Merged.data.xls" )
                            ( 4361 "mnt-01_Condition 2_2008-11-21_1724_4361.xls" "NavBack-E1-Merged.data.xls" )
                            ( 5581 "mnt-01_Condition 2_2008-12-01_1219_5581.xls" "NavBack-E1-Merged.data.xls" )
                            ( 7136 "mnt-01_Condition 2_2008-12-01_1322_7136.xls" "NavBack-E1-Merged.data.xls" )
                            ( 7136 "mnt-01_Condition 2_2008-12-01_1332_7136.xls" "NavBack-E1-Merged.data.xls" )
                            ( 7136 "mnt-01_Condition 3_2008-11-20_1511_660567136.xls" "NavBack-E1-Merged.data.xls" )
                            ( 7318 "mnt-01_Condition 2_2008-12-01_1812_7318.xls" "NavBack-E1-Merged.data.xls" )
                            ( 3062 "mnt-01_Condition 2_2008-12-02_1310_3062.xls" "NavBack-E1-Merged.data.xls" )
                            ( 2021 "mnt-01_Condition 2_2008-12-02_1508_2021.xls" "NavBack-E1-Merged.data.xls" )
                            ( 8959 "mnt-01_Condition 2_2008-12-05_1315_8959.xls" "NavBack-E1-Merged.data.xls" )
                            ( 6233 "mnt-01_Condition 2_2009-02-18_1615_6233.xls" "NavBack-E1-Merged.data.xls" )
                            ( 2990 "mnt-01_Condition 2_2009-02-20_1402_2990.xls" "NavBack-E1-Merged.data.xls" )
                            ( 3020 "mnt-01_Condition 2_2009-02-27_1531_3020.xls" "NavBack-E1-Merged.data.xls" )
                            ( 8111 "mnt-01_Condition 2_2009-03-17_1219_8111.xls" "NavBack-E1-Merged.data.xls" )
))

(defmacro print-if (test &rest rest)
  `(if ,test
       (format t ,@rest)))

(defclass snt-parser (parser)
  ((stream :initarg :stream :accessor parser-stream)
   (fixations :initarg :fixations :accessor parser-fixations)
   (subject :initarg :subject :accessor parser-subject)
   (sys-state-turns :accessor parser-sys-state-turns)
   (cur-block :initform 0 :accessor parser-block)
   (cur-trial :initform 0 :accessor parser-trial)
   (snt-time :initform 0 :accessor parser-time)
   (turn-lookup :initform (make-hash-table) :accessor parser-turn-lookup)
   (jitter-lookup :initform (make-hash-table) :accessor parser-jitter-lookup)
   (cbtime :initform 100 :accessor parser-cbtime)
   (last-event :initform nil :accessor parser-last-event)
   (last-fixation :initform nil :accessor parser-last-fixation)
   (last-2-fixation :initform nil :accessor parser-second-last-fixation)
   (last-display :initform nil :accessor parser-last-display)
   (last-trial-valid :initform nil :accessor parser-last-trial-valid)
   ))

(defmethod initialize-parser ((parser snt-parser))
  (setf *fill-gaps* '("Cognitive Operator"))
  (let (line fixation)

    (setf (gethash 1 (parser-turn-lookup parser)) 'left
          (gethash 2 (parser-turn-lookup parser)) 'right
          (gethash 3 (parser-turn-lookup parser)) 'forward
          (gethash 1 (parser-jitter-lookup parser)) 'left
          (gethash 2 (parser-jitter-lookup parser)) 'right)

    ; Seek to first fixation
    (setf fixation
          (do ((x (read-tab-line (parser-fixations parser))
                  (read-tab-line (parser-fixations parser))))
              ((equal (first x) (parser-subject parser)) x)
            (setf fixation x)))
    (print-if *debug-snt* "~A~%" fixation)

    ; Seek to first trial
    (setf line
          (do ((x (read-stream-line parser)
                  (read-stream-line parser)))
              ((and (equal (first x) (parser-subject parser))
                    (equal (fifth x) 'snt-display)) x)
            (setf line x)))
    (print-if *debug-snt* "~A~%" line)

    (setf (parser-sys-state-turns parser) (mapcar #'read-from-string (last line 3))
          (parser-last-event parser) line
          (parser-last-fixation parser) fixation)
    (print-if *debug-snt* "First three turns: ~A~%" (parser-sys-state-turns parser))
    parser
    )
)

(defmethod parse-item ((parser snt-parser))
  (let (args)
    (do ((event (parser-last-event parser) (parser-last-event parser))
         (fixation (parser-last-fixation parser) (parser-last-fixation parser)))
        ((equal nil event) nil)
      
      (cond (; Sync the two files if the data file trails the fixation file
             (or (null fixation) (< (third event) (third fixation)))

             (cond (; Process trial start
                    (and (= (length event) 9)
                         (equal (fifth event) 'snt-display)
                         (numberp (first (last event)))
                         (not (in-trial? (get-processor))))
                    (setf args (make-hash-table))
                    (setf (gethash :start args) (third event))

                    (setf (parser-last-event parser) (read-stream-line parser))

                    (return (values 'start-trial args)))
                   (; Process trial end
                    (and (= (length event) 9)
                         (equal (fifth event) 'snt-display)
                         (numberp (first (last event))))
                    (setf args (make-hash-table))
                    (setf (gethash :end args) (third event))
                    ;(setf (gethash 'valid-trial args) (parser-last-trial-valid parser))
                    ;(format t "Last trial was valid? ~A~%" (parser-last-trial-valid parser))
                    ;(format t "Other field? ~A~%" (first (last event 2)))
                    (setf (gethash 'valid-trial args) (= (first (last event 2)) 100))
                    (format t "Last trial was valid? ~A~%" (gethash 'valid-trial args))

                    (return (values 'end-trial args)))
                   (; Process jitter correct
                    (and (equal (fifth event) 'snt-action)
                         (/= (first (last event)) 0))
                    )
                   (; Process turn
                    (and (equal (fifth event) 'snt-action)
                         (= (first (last event)) 0))
                    (let ((turn (gethash (first (last event 2)) (parser-turn-lookup parser))))
                      (setf (parser-last-trial-valid parser) (equal turn (ninth event)))
                      (cond ((equal turn 'left)
                             (setf args (make-hash-table))

                             (let ((start (make-hash-table))
                                   (end (make-hash-table)))
                               (setf (gethash 2 start) (- (third event) 50.0))
                               (setf (gethash 2 end) (- (third event) 50.0))
                               (setf (gethash 3 start) (- (third event) 50.0))
                               (setf (gethash 3 end) (third event))
                               (setf (gethash 4 start) (third event))
                               (setf (gethash 4 end) (+ (third event) 50.0))
                               
                               (setf (gethash :start args) start)
                               (setf (gethash :end args) end))
                             
                             (setf (parser-last-event parser) (read-stream-line parser))

                             (return (values 'snt-left args))
                             )
                            ((equal turn 'right)
                             (setf args (make-hash-table))

                             (let ((start (make-hash-table))
                                   (end (make-hash-table)))
                               (setf (gethash 2 start) (- (third event) 50.0))
                               (setf (gethash 2 end) (- (third event) 50.0))
                               (setf (gethash 3 start) (- (third event) 50.0))
                               (setf (gethash 3 end) (third event))
                               (setf (gethash 4 start) (third event))
                               (setf (gethash 4 end) (+ (third event) 50.0))
                               
                               (setf (gethash :start args) start)
                               (setf (gethash :end args) end))

                             (setf (parser-last-event parser) (read-stream-line parser))

                             (return (values 'snt-right args))
                             )
                            ((equal turn 'forward)

                             (setf (parser-last-event parser) (read-stream-line parser))

                             (return 'continue)
                             )
                            (t
                             (say "Unknown turn condition at time ~A" (third event))
                             )
                            )))
                   (; Process display update
                    (equal (fifth event) 'snt-step)
                    (setf args (make-hash-table))
                    (setf (gethash :start args) (- (third event) 200.0))
                    (setf (gethash :end args) (third event))

                    (setf (parser-last-event parser) (read-stream-line parser))
                    
                    (return (values 'snt-display args)))

                   (; Process display of direction
                    (and (equal (fifth event) 'snt-display)
                         (stringp (first (last event))))
                    (setf args (make-hash-table))
                    (setf (gethash :start args) (third event))
                    (setf (gethash :end args) (third event))

                    (setf (parser-last-event parser) (read-stream-line parser))

                    (return (values 'snt-direction args)))
                   (; Debugging
                    (equal (fifth event) 'snt-display)
                    (let ((*interactive* t) *block*) (say "** HELP: ~S" event)))
                   )

             ; Move to next entry
             (setf (parser-last-event parser) (read-stream-line parser))

             (return 'continue)
             )
            (; Sync the two files if the fixation file trails the data file
             fixation
             (cond ((and (<= 100 (- (fourth fixation) (third fixation)))
                         (> 2.0 (first (last fixation))))
                    (setf args (make-hash-table))
                    (let ((start (make-hash-table))
                          (end (make-hash-table)))
                      (cond ((parser-second-last-fixation parser)
                             (setf (gethash 2 start) (fourth (parser-second-last-fixation parser)))
                             (setf (gethash 2 end) (third fixation)))
                            (t
                             (setf (gethash 2 start) (- (third fixation) 30.0))
                             (setf (gethash 2 end) (third fixation))))
                      (setf (gethash 3 start) (third fixation))
                      (setf (gethash 3 end) (fourth fixation))
                      (setf (gethash :start args) start)
                      (setf (gethash :end args) end)
                    )))
             (setf (parser-second-last-fixation parser) fixation)
             (setf fixation
                   (setf (parser-last-fixation parser)
                         (read-tab-line (parser-fixations parser))))
             (do ()
                 ((or (null fixation)
                      (and (numberp (third fixation))
                           (numberp (fourth fixation)))) nil)
               (setf fixation
                     (setf (parser-last-fixation parser)
                           (read-tab-line (parser-fixations parser))))
               (if (/= (first fixation) (parser-subject parser))
                   (setf fixation nil
                         (parser-last-fixation parser) nil))
               )
             (if args
                 (return (values 'eg-fixation args))
               (return 'continue))
             )
            )
  )
))

(defparameter *areas-of-interest*
  '((arrow . (:rectangle 550 650 100 75))
    (direction . (:rectangle 982 200 150 75))
    (feedback . (:rectangle 130 200 200 100))))

(defparameter *event-mapping* 
  '((EG-FIXATION . (:type :routine
                    :routine "Eye Movement"
                    :event-id (2 3)
                    :distribution "Constant"
                    ))
    (SNT-LEFT . (:type :routine
                 :routine "Key Press (Left Hand)"
                 :event-id (2 3 4)
                 :distribution "Constant" 
                 :label "Turn Left"
                 ))
    (SNT-RIGHT . (:type :routine
                  :routine "Key Press (Right Hand)"
                  :event-id (2 3 4)
                  :distribution "Constant"
                  :label "Turn Right"
                  ))
    (SNT-CORRECT-LEFT . (:type :routine
                         :routine "Key Press (Left Hand)"
                         :event-id (2 3 4)
                         :distribution "Constant"
                         :label "Jitter-correct Left"))
    (SNT-CORRECT-RIGHT . (:type :routine
                          :routine "Key Press (Right Hand)"
                          :event-id (2 3 4)
                          :distribution "Constant"
                          :label "Jitter-correct Right"))
    (SNT-DISPLAY . (:type :activity
                    :activity "System Resource"
                    :distribution "Constant"
                    :label "Display Update"))
    (SNT-DIRECTION . (:type :activity
                      :activity "System Resource"
                      :distribution "Constant"
                      :label "Direction - "))
))

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

(defun run-snt (id log eye)
  (reset-processor (get-processor))
  (let ((parser (make-instance 'snt-parser
                               :stream (open (format nil "/Users/ewpatton/Research/SANLab/v3.0/SNT/Data/~A" log))
                               :fixations (open (format nil "/Users/ewpatton/Research/SANLab/v3.0/SNT/Eyetrack Processed Files/~A" eye))
                               :subject id)))
    (let ((result (run-protocol-analysis parser)))
      (close (parser-stream parser))
      (close (parser-fixations parser))
      (values result parser))
)
)

(defmethod save-snt-models ((p snt-parser) (l list))
  (do ((l l (cdr l))
       (i 1 (1+ i)))
      ((null l) nil)
    (let ((fn (format nil "~A~A_~A_model_~A.san/" *save-dir* (num-merged-trials (car l)) (parser-subject p) i))
          (model (resource-graph-to-sanlab-model (car l))))
      (make-sanlab-bundle fn)
      (write-model-to-bundle fn model))))

(defun test-snt-processor ()
  (let ((parser (make-instance 'snt-parser
                               :stream (open "/Users/ewpatton/Research/SANLab/v3.0/SNT/Data/mnt-01_Condition 1_2008-11-04_1614_8113.xls")
                               :fixations (open "/Users/ewpatton/Research/SANLab/v3.0/SNT/Eyetrack Processed Files/NavBack-E1-Merged.data.xls")
                               :subject 8113))
        (turn-lookup (make-hash-table))
        (jitter-lookup (make-hash-table))
        (line nil)
        (cbtime 100)
        (fixation nil))

    (setf (gethash 1 turn-lookup) 'left
          (gethash 2 turn-lookup) 'right
          (gethash 3 turn-lookup) 'forward
          (gethash 1 jitter-lookup) 'left
          (gethash 2 jitter-lookup) 'right)

    ; Seek to first fixation
    (setf fixation
          (do ((x (read-tab-line (parser-fixations parser))
                  (read-tab-line (parser-fixations parser))))
              ((equal (first x) (parser-subject parser)) x)
            (setf fixation x)))
    (print-if *debug-snt* "~A~%" fixation)

    ; Seek to first trial
    (setf line
          (do ((x (read-stream-line parser)
                  (read-stream-line parser)))
              ((and (equal (first x) (parser-subject parser))
                    (equal (fifth x) 'snt-display)) x)
            (setf line x)))
    (print-if *debug-snt* "~A~%" line)

    (setf (parser-sys-state-turns parser) (mapcar #'read-from-string (last line 3)))
    (print-if *debug-snt* "First three turns: ~A~%" (parser-sys-state-turns parser))

    (do ()
        ((equal nil line) line)
      (cond (; Sync the two files if the data file trails the fixation file
             (or (null fixation) (< (third line) (third fixation)))
             (let ((end (last line 3)))
               (cond ((and (equal (fifth line) 'snt-display)
                           (not (member nil (mapcar #'(lambda (x)
                                                        (member x '("LEFT" "FORWARD" "RIGHT")
                                                                :test 'equal))
                                                    end))))
                      (let ((turns (mapcar #'read-from-string end)))
                        (cond ((parser-sys-state-turns parser)
                               (print-if *debug-snt* "New turn: ~A~%" (third turns))
                               )
                              (t
                               (print-if *debug-snt* "Starting turn queue: ~A~%" turns)))
                        (setf (parser-sys-state-turns parser) turns))
                      )
                     ((and (equal (fifth line) 'snt-action)
                           (/= (second end) 0))
                      (print-if *debug-snt* "Subject turned ~A~%" (gethash (second end) turn-lookup))
                      )
                     ((and (equal (fifth line) 'snt-action)
                           (= (second end) 0))
                      (print-if *debug-snt* "Subject strafed ~A~%" (gethash (third end) turn-lookup))
                      )
                     ((and (equal (fifth line) 'snt-step)
                           (< (ninth line) cbtime))
                      (setf cbtime (ninth line)
                            (parser-block parser) (seventh line)
                            (parser-trial parser) (1+ (parser-trial parser)))
                      (print-if *debug-snt* "Start block ~A, trial ~A~%" (parser-block parser) (parser-trial parser))
                      )
                     ((equal (fifth line) 'snt-step)
                      (setf cbtime (ninth line))
               ;(print-if *debug-snt* "cbtime = ~A~%" (ninth line))
                      )
                     )
               (setf line (read-stream-line parser))
               ))
            (; Sync the two files if the fixation file trails the data file
             fixation
             (if (and (<= 100 (- (fourth fixation) (third fixation)))
                      (> 2.0 (first (last fixation))))
                 (print-if *debug-snt* "Subject fixated on ~A for ~A milliseconds~%" (first (last fixation 2)) (nth 13 fixation)))
             (setf fixation (read-tab-line (parser-fixations parser)))
             (do ()
                 ((or (null fixation) (and (numberp (third fixation)) (numberp (fourth fixation)))) nil)
               (setf fixation (read-tab-line (parser-fixations parser)))
               (if (/= (first fixation) (parser-subject parser))
                   (setf fixation nil))
               )
             )))

    (close (parser-stream parser))
    (close (parser-fixations parser))
)
)

#|
Copyright © 2007-2009 Evan W. Patton

This file is part of SANLab-CM.

SANLab-CM is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with SANLab-CM. If not, see <http://www.gnu.org/license/>.
|#

(declaim (optimize (float 0) (speed 3) (space 0)))

(defclass network-processor ()
  ((num-trials :initform 0 :initarg :total-trials :accessor total-trials)
   (cur-trial :initform 0 :accessor cur-trial)
   (model :initform nil :initarg :model :accessor model)
   (callback :initform nil :initarg :callback :accessor callback)
   (update-process :initform nil :accessor update-process)
   (calc-process :initform nil :accessor calc-process)))

(defclass path-record ()
  ((data :initform nil :initarg :data :accessor data)
   (activity-durations :initform nil :initarg :activity-durations :accessor activity-durations)
   (path-data :initform nil :initarg :path-data :accessor path-data)
   (min-time :initform nil :initarg :min-time :accessor min-time)
   (max-time :initform nil :initarg :max-time :accessor max-time)
   (mean :initform nil :initarg :mean :accessor mean)
   (label :initform nil :initarg :label :accessor label)
   (cpindex :initform nil :initarg :cpindex :accessor cpindex)
   (percent :initform nil :initarg :percent :accessor percent)))

(defmethod execute-processor ((np network-processor))
  (setf (update-process np) (mp:process-run-function "network-processor Update Process" '() #'update-function np))
  (setf (calc-process np) (mp:process-run-function "network-processor Calculation Process" '() #'calculation-process-wrapper np)))
;  (calculation-process np))

(defmethod stop-processor ((np network-processor))
  (if (equal (mp:get-current-process) (calc-process np))
      (progn
        (mp:process-stop (update-process np))
        (mp:process-kill (update-process np))
        (if (callback np) (apply (callback np) (list 'stopped)))
        (mp:process-kill (calc-process np)))
    (progn
      (mp:process-stop (calc-process np))
      (mp:process-stop (update-process np))
      (if (callback np) (apply (callback np) (list 'stopped))))))

(defun update-function (np)
  (let ((s nil)
        (e nil)
        (last nil)
        (cur-trial nil)
        (callback (callback np)))
    (loop
     (when (= (total-trials np) (cur-trial np)) (return))
     (setf last (cur-trial np))
     (setf s (get-internal-real-time))
     (sleep 0.5)
     (setf e (get-internal-real-time))
     (setf cur-trial (cur-trial np))
     (if callback (apply callback (list 'update cur-trial (total-trials np) (float (/ (* 1000 (- cur-trial last)) (- e s)))))))
    (if callback (apply callback (list 'finished (total-trials np))))))

(defun max-parent (nodes)
  (let ((max-parent (first nodes)))
    (dolist (node nodes max-parent)
      (if (< (end-time max-parent) (end-time node)) (setf max-parent node)))))

(defun get-longest-path (source target)
  (declare (ignore source))
  (let ((path (list target)))
    (do ((node (slowest-parent target) (slowest-parent node)))
        ((null node) path)
      (push node path))))

(define-condition network-error (simple-error)
  ((thrown-error :initarg :thrown-error :reader thrown-error)
   (activities :initarg :activities :reader activities))
  (:report (lambda (condition stream)
             (format stream "Received an error: ~%~%~A~%~%while processing activity ~A." (thrown-error condition) (activities condition))))
)

(defun generate-sample-times (activities &optional (run 0))
  (mapcar #'(lambda (a)
              (let ((params (copy-list (parsed-parameters a))))
                (dotimes (i (length params))
                  (cond ((and (listp (nth i params)) (< 0 (fourth (nth i params))) (= 0 (mod run (fourth (nth i params)))))
                         (setf (fifth (nth i params)) (+ (second (nth i params)) (random (- (third (nth i params)) (second (nth i params))))))
                         (setf (nth i params) (fifth (nth i params))))
                        ((and (listp (nth i params)) (= 0 (fourth (nth i params))))
                         (setf (nth i params) (setf (nth i (parsed-parameters a)) (+ (second (nth i params)) (random (- (third (nth i params)) (second (nth i params))))))))
                        ((listp (nth i params))
                         (setf (nth i params) (fifth (nth i params))))))
                (multiple-value-bind (success error)
                    (ignore-errors
                      (setf (sample-duration a) (let ((x (apply (symname (distribution a)) params)))
                                                  (if (>= x 0)
                                                      x
                                                    0))))
                  (declare (ignore success))
                  (if error
                      (progn
                        ;(break)
                        (signal 'network-error :thrown-error error :activities (list a))
                        ;(error "Error!!!")
                        )
                    ))
                  ))
          activities))

(defun initialize-network (activities target)
  (mapcar #'(lambda (a) (setf (path-length a) most-positive-single-float (start-time a) nil)) activities)
  (setf (path-length target) (sample-duration target))
  )

(defun simulate-network (activities source target)
  (let ((current-layer (copy-list (edges-out source)))
        (next-layer nil))

    (initialize-network activities target)

    (mapcar #'(lambda (x) (setf (end-time x) nil (slowest-parent x) nil)) activities)
    (setf (start-time source) 0)
    (setf (end-time source) (sample-duration source))

    (loop
     (dolist (node current-layer)
       (cond ((not (member nil (mapcar #'end-time (edges-in node))))
              (setf (slowest-parent node) (max-parent (edges-in node)))
              (setf (start-time node) (end-time (slowest-parent node)))
              (setf (end-time node) (+ (start-time node) (sample-duration node)))
              (let ((new-nodes (remove-if #'(lambda (x) (member x next-layer)) (copy-list (edges-out node)))))
                (setf next-layer (append next-layer new-nodes))))
             (t
              (if (not (member node next-layer))
                  (setf next-layer (append next-layer (list node))))
              )))
     (if (null next-layer) (return))
     (let ((list1 (copy-list current-layer))
           (list2 (copy-list next-layer)))
       (setf list1 (remove-if #'(lambda (x) (if (member x list2) (setf list2 (remove x list2)))) list1))
       (if (and (= 0 (length list1)) (= 0 (length list2))) (error "It seems as though there is a cycle in this graph."))
       )
     (if (equal next-layer current-layer) (error "It seems as though there is a cycle in this graph."))
     (setf current-layer next-layer)
     (setf next-layer nil))
;    (break "End of simulate network")
    (end-time target)))

(defun compute-mean-durations (activities target trials path path-counter)
  (let ((denominator (float (/ 1 trials)))
        (total-time (end-time target)))
    (declare (ignore total-time))
    (dolist (activity activities)
      (let* ((start-time (start-time activity))
             (end-time (end-time activity)))

        (incf (mean-start activity) (* denominator start-time))
        (incf (mean-end activity) (* denominator end-time))

        (if (not (gethash path (conditional-mean-start activity)))
            (setf (gethash path (conditional-mean-start activity)) 0.0))
        (if (not (gethash path (conditional-mean-end activity)))
            (setf (gethash path (conditional-mean-end activity)) 0.0))
        
        (incf (gethash path (conditional-mean-start activity))
              (* (/ 1 path-counter) (- start-time (gethash path (conditional-mean-start activity)))))
        (incf (gethash path (conditional-mean-end activity))
              (* (/ 1 path-counter) (- end-time (gethash path (conditional-mean-end activity)))))
        )


      ))
  )

(defun calculation-process-wrapper (np)
  (multiple-value-bind (success errorp) (calculation-process np)
    (if (not success)
        (let ((callback (callback np)))
          (if callback (apply callback (list 'error errorp)))
          (mp:process-kill (update-process np))))))

(defun calculation-process (np)
  (let ((callback (callback np))
        (model (model np)))
    (cond ((changed? model)
           (if callback (apply callback (list 'error :model-not-saved)))
           (stop-processor np)
           (return-from calculation-process nil)))
    (if callback (apply callback (list 'started)))
    (let* ((start-time (get-internal-real-time))
           (trials (total-trials np))
           (activities (activities model))
           (source (remove-if #'(lambda (activity) (edges-in activity)) activities))
           (target (remove-if #'(lambda (activity) (edges-out activity)) activities))
           (simulation-results nil)
           (max-time 0.0)
           (path-records nil)
           (path-counter nil)
           (result-hash (make-hash-table :test #'equal))
           (trial-results nil)
           (trial-ucpids (make-hash-table :test #'equal))
           (trial-ucpid-current 0)
           (ct-tracker (make-instance 'criticality-tracker))
           (starting-random-state (make-random-state nil)))

      (cond
       ((not source)
        (if callback (apply callback (list 'error :no-source-node)))
        (stop-processor np)
        (return-from calculation-process))
       ((> (length source) 1)
        (if callback (apply callback (list 'error :too-many-sources)))
        (dolist (act (activities model))
          (setf (selected? act) nil))
        (dolist (act source)
          (setf (selected? act) t))
        (stop-processor np)
        (return-from calculation-process))
       ((not target)
        (if callback (apply callback (list 'error :no-target-node)))
        (stop-processor np)
        (return-from calculation-process))
       ((> (length target) 1)
        (if callback (apply callback (list 'error :too-many-targets)))
        (dolist (act (activities model))
          (setf (selected? act) nil))
        (dolist (act target)
          (setf (selected? act) t))
        (stop-processor np)
        (return-from calculation-process))
       ((< trials 1)
        (if callback (apply callback (list 'error :invalid-trial-input)))
        (stop-processor np)
        (return-from calculation-process)))

      (setf path-counter (make-hash-table :test 'equal))
      (dolist (activity activities)
        (setf (mean-start activity) 0.0)
        (setf (mean-end activity) 0.0)
        (setf (conditional-mean-start activity) (make-hash-table :test #'equal))
        (setf (conditional-mean-end activity) (make-hash-table :test #'equal))
        (cond ((not (process-parameters activity callback))
               (stop-processor np)
               (return-from calculation-process))))
      
      (multiple-value-bind (success error)
          (ignore-errors
            (generate-sample-times activities)
            (setf source (first source))
            (setf target (first target)))
        (declare (ignore success))
        (cond (error
               (if callback (apply callback (list 'error error)))
               (stop-processor np)
               (return-from calculation-process))))
      (simulate-network activities source target)

      (dotimes (i trials)
        (declare (type fixnum i))
        (setf (cur-trial np) i)
        (generate-sample-times activities i)
;        (break "Before simulate network")
        (let ((result (simulate-network activities source target))
              (path (get-longest-path source target)))
;          (break "After simulate network")
;          (declare (type single-float result))
          (mapcar #'(lambda (x) (inc-criticality x ct-tracker)) path)
          (inc-trials ct-tracker)
          
          (if (gethash path path-counter)
              (incf (gethash path path-counter))
            (setf (gethash path path-counter) 1))

          (cond ((not (gethash path trial-ucpids))
                 (setf (gethash path trial-ucpids) trial-ucpid-current)
                 (incf trial-ucpid-current)))
          
          (compute-mean-durations activities target trials path (gethash path path-counter))

          (push result simulation-results)
          (let ((temp (make-trial-result :timestamp (get-internal-real-time)
                                         :trial i
                                         :duration result
                                         :path path
                                         :ucpid (gethash path trial-ucpids)
                                         :model model)))
            (push temp trial-results))
          
          (if (gethash path result-hash)
              (push result (gethash path result-hash))
            (setf (gethash path result-hash) (list result)))))

      (setf (cur-trial np) trials)
      (push (make-instance 'path-record
                           :data simulation-results
                           :path-data nil
                           :label "< Overall >"
                           :percent "100.00"
                           :min-time (reduce #'min (mapcar #'trial-result-duration trial-results))
                           :max-time (reduce #'max (mapcar #'trial-result-duration trial-results))
                           :mean (format nil "~1,2f" (list-mean simulation-results)))
            path-records)

      (maphash
       #'(lambda (path times)
           (let ((mean (list-mean times)))
             (setf max-time (max max-time mean))
             (push (make-instance 'path-record
                                  :data times
                                  :path-data path
                                  :mean (format nil "~1,2f" mean)
                                  :min-time (reduce #'min (remove nil (mapcar #'(lambda (x)
                                                                                  (if (equal path (trial-result-path x))
                                                                                      (trial-result-duration x)
                                                                                    nil))
                                                                              trial-results)))
                                  :max-time (reduce #'max (remove nil (mapcar #'(lambda (x)
                                                                                  (if (equal path (trial-result-path x))
                                                                                      (trial-result-duration x)
                                                                                    nil))
                                                                              trial-results)))
                                  :label (print-path path)
                                  :cpindex (gethash path trial-ucpids)
                                  :percent (format nil "~1,2f" (* 100.0 (/ (length times) (length simulation-results)))))
                   path-records)))
       result-hash)
      (incf (run-number model))
      (let ((file (write-trials-to-bundle (filename model) trial-results (run-number model))))
        (write-statistics-to-bundle (filename model) path-records (run-number model))
        (write-random-state-to-bundle (filename model) starting-random-state (run-number model))
        (if callback (apply callback (list 'results trial-results path-records ct-tracker starting-random-state)))
        (display-histogram-window (view (controller model))
                                  max-time
                                  trials
                                  (list-min simulation-results)
                                  (list-max simulation-results)
                                  simulation-results
                                  path-records
                                  (- (get-internal-real-time) start-time)
                                  file))
      t
)))

(defun list-min (l)
  (let ((m (first l)))
    (dolist (x l m)
      (if (< x m)
          (setq m x))))
)

(defun list-max (l)
  (let ((m (first l)))
    (dolist (x l m)
      (if (> x m)
          (setq m x))))
)

(defmethod generate-actr-style-trace ((model model) filepath)
  (block generate-actr-style-trace
  (let ((source (remove-if #'null (mapcar #'(lambda (x) (if (edges-in x) nil x)) (activities model))))
        (target (remove-if #'null (mapcar #'(lambda (x) (if (edges-out x) nil x)) (activities model))))
        (items nil))
    (setf source (first source))
    (setf target (first target))
    (dolist (act (activities model))
      (if (not (process-parameters act))
          (return-from generate-actr-style-trace)))
    (generate-sample-times (activities model))
    (simulate-network (activities model) source target)
    (dolist (act (activities model))
      (cond ((equal (label act) "SANLab Start"))
            ((equal (label act) "dummy end")
             (push (list (start-time act) "------" "Stopped because no events left to process") items))
            ((equal (activity-type act) (get-activity-by-typename "Perceptual Operator (Auditory)"))
             (push (list (start-time act) "AUDIO" (label act)) items))
            ((equal (activity-type act) (get-activity-by-typename "Perceptual Operator (Visual)"))
             (push (list (start-time act) "VISION" (label act)) items))
            ((equal (activity-type act) (get-activity-by-typename "Eye Movement Operator"))
             (push (list (start-time act) "VISION" (label act)) items))
            ((equal (activity-type act) (get-activity-by-typename "Cognitive Operator"))
             (push (list (start-time act) "PROCEDURAL" (label act)) items))
            ((equal (activity-type act) (get-activity-by-typename "Attend Operator"))
             (push (list (start-time act) "PROCEDURAL" (label act)) items))
            ((equal (activity-type act) (get-activity-by-typename "Verify Operator")) 
             (push (list (start-time act) "PROCEDURAL" (label act)) items))
            ((equal (activity-type act) (get-activity-by-typename "Initiate Operator"))
             (push (list (start-time act) "PROCEDURAL" (label act)) items))
            ((equal (activity-type act) (get-activity-by-typename "System Resource"))
             (push (list (start-time act) "NONE" "SYSTEM EVENT" (label act)) items))
            ((equal (activity-type act) (get-activity-by-typename "Task-oriented Memory"))
             (push (list (start-time act) "GOAL" (label act)) items))
            ((equal (activity-type act) (get-activity-by-typename "Declarative Memory"))
             (push (list (start-time act) "DECLARATIVE" (label act)) items))
            ((equal (activity-type act) (get-activity-by-typename "Speech Operator"))
             (push (list (start-time act) "SPEECH" (label act)) items))
            ((equal (activity-type act) (get-activity-by-typename "Motor Operator"))
             (push (list (start-time act) "MOTOR" (label act)) items))
            ((equal (activity-type act) (get-activity-by-typename "Left Hand Operator"))
             (push (list (start-time act) "MOTOR" (label act)) items))
            ((equal (activity-type act) (get-activity-by-typename "Right Hand Operator"))
             (push (list (start-time act) "MOTOR" (label act)) items))
            ))
    (setf items (sort items #'< :key #'first))
    (with-open-file (out filepath :direction :output :if-exists :supersede :external-format '(:latin-1 :eol-style #+win32 :crlf #-win32 :lf))
      (dolist (x items)
        (format out "~&~10,3F   ~16A   ~{~A ~}~%" (/ (first x) 1000.0) (second x) (cddr x))))
)))

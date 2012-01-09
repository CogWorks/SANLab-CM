(defparameter *version* 2.5)

(defparameter *progress-stream* (capi:collector-pane-stream    ;for the progress bar
                             (make-instance 'capi:collector-pane)))

;;;==== for file loading ====
(defparameter *qualifying-string* "experiment")                ;to only include files that contain qualifying string

;contains locations of of all types of information in the log files
(defclassic col-info () tag sub-tag time-col valid diam x y w h id text)

(defclassic tag-info () obj obj-sub chg-obj chg-obj-sub eye mouse begin-seg end-seg)

(defclassic include-info () eye mouse noa assigned click remove-parens sample fix obj)

(defparameter *col-info* nil)
(defparameter *eye-col-info* nil)
(defparameter *mse-col-info* nil)
(defparameter *obj-col-info* nil)
(defparameter *mse-on-eye* nil)

(defparameter *tag-info* nil)
(defparameter *include-info* nil)
(defparameter *container-col* nil)
(defparameter *fix-radius* 35)
(defparameter *num-cameras* 1)
(defparameter *fix-threshold* (* 6 *num-cameras*))

;;;==== for file loading ====

;;;==== for file processing ====

;;; superclass
(defclassic data () file data-time end-time x y)

(defclassic obj-location (data) text w h img visible)

(defclassic eye-dwell (data) end duration obj var1 var2)
(defclassic mse-dwell (data) samples-in end duration obj var1 var2)

(defclassic eye-data (data) decency pup-diam)

(defclassic mse-data (data) decency pup-diam duration)
(defclassic mse-click (data) duration)

(defclassic gui-object (data) id obj-type container location time-gone trans-list)

(defclassic segment (data) num begin end ans)

(defclassic fixation-algorithm ()
  fix-radius 
  num-bad-data
  bad-data-threshold 
  fixation-threshold 
  outlier-threshold 
  present-fix 
  potential-fix 
  (call-count 0)
  )

(defclassic eye-fix-algorithm (fixation-algorithm)
  (fix-radius *fix-radius*)
  (num-bad-data 0) ;; 
  (bad-data-threshold (* 6 *num-cameras*))
  (fixation-threshold (* 6 *num-cameras*))
  (outlier-threshold (* 2 *num-cameras*))
  (present-fix (make-instance 'eye-fixation))
  (potential-fix (make-instance 'eye-fixation)))  

(defclassic mse-fix-algorithm (fixation-algorithm)
  (fix-radius *fix-radius*)
  (num-bad-data 0) 
  (bad-data-threshold 6)
  (fixation-threshold 6)
  (outlier-threshold 2)
  (present-fix (make-instance 'mouse-fixation))
  (potential-fix (make-instance 'mouse-fixation)))        

(defclassic fixation ()  
  (samples-in 0)      ;; number of samples in the fixation; samples >= *min-fix-samples*
  (samples-out 0)     ;; number of samples outside of fixation
  (x-fix 0.0)         ;; x-location summation
  (y-fix 0.0)         ;; y-location summation
  (x-cog 0.0)         ;; x-location of center of gravity of fixation 
  (y-cog 0.0)         ;; y-location of center of gravity of fixation
  (begin 0)           ;; beginning time of fixation
  (end 0)             ;; ending time of fixation
  (duration 0)        ;; fixation duration in milliseconds (- end begin)
  (pupil 0)
  (file NIL)
)


(defclassic eye-fixation (fixation))
(defclassic mouse-fixation (fixation))

(defclassic protomatch-prog ()
 (eye-algo (make-instance 'eye-fix-algorithm))
 (mse-algo (make-instance 'mse-fix-algorithm)))

(let ((pm (make-instance 'protomatch-prog)))

(defun pm ()
  pm)

(defun eye-fix ()
  (eye-algo pm))

(defun mouse-fix ()
  (mse-algo pm))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((all-eye-data nil)
      (all-obj-data nil)
      (all-mse-data nil)
      (all-fixations nil)
      (all-mse-fixations nil)
      (all-segments nil))
  
  (defun get-all-eye-data ()
    all-eye-data)
  
  (defun get-all-mse-data ()
    all-mse-data)

  (defun get-all-obj-data ()
    all-obj-data)

  (defun get-all-segments ()
    all-segments)
  
  (defun get-all-fixations ()
    all-fixations)

  (defun get-all-mse-fixations ()
    all-mse-fixations)

  (defun set-all-eye-data (data)
    (setf all-eye-data data))

  (defun set-all-obj-data (data)
    (setf all-obj-data data))

  (defun set-all-mse-data (data)
    (setf all-mse-data data))
  
  (defun set-all-segments (data)
    (setf all-segments data))

  (defun save-fixation (fix)
    (push fix all-fixations))

  (defun save-mse-fixation (fix)
    (push fix all-mse-fixations))

  (defun set-all-fixations (data)
    (setf all-fixations data))
  
  (defun set-all-mse-fixations (data)
    (setf all-mse-fixations data))
  
  (defun reset-all ()
    (setf all-eye-data nil)
    (setf all-fixations nil)
    (setf all-mse-fixations nil)
    (setf all-obj-data nil)
    (setf all-mse-data nil)
    (setf all-segments nil)
    ))
;;;==== for file processing ====

(defparameter path (directory-namestring (current-pathname)))